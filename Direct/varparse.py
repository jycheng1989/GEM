#!/usr/bin/python3

# This is a script to parse the global variables from a segment of Fortran code

import re
import sys

validVar = re.compile(r'\b[a-z][a-z0-9_]*\b')
parenStuff = re.compile(r'\([^\(\)]*\)')

localVars = set()
globalVars = set()
subNames = set()

# fortran builtins
# this is incomplete and needs to be updated
stopwords = set(['cos', 'exp', 'float', 'int', 'max', 'min', 'sin', 'sqrt'])

# open file or stdin
if len(sys.argv) > 1:
	try:
		infile = open(sys.argv[1], 'r')
	except IOError:
		print('File not found: ' + sys.argv[1])
		quit(1)
else:
	infile = sys.stdin

line = infile.readline()
while line:
	rawline = line # in case we need to print an error
	line = line.strip().lower()
	
	# remove comments
	if '!' in line:
		line = line[:line.index('!')].strip()
	
	# handle continuation lines
	while line and line[-1] == '&':
		line = line[:-1]
		nextLine = infile.readline()
		if nextLine:
			line += ' ' + nextLine.strip().lower()

	# remove leading labels
	line = line.lstrip('0123456789 \t')

	# blank lines
	if not line:
		line = infile.readline()
		continue
	
	firstWord = validVar.findall(line)[0]
	
	# skip block ends, cycle, goto, and continue statements
	skipList = ['end', 'endif', 'enddo', 'cycle', 'goto', 'continue']
	if not line or firstWord in skipList:
		line = infile.readline()
		continue

	# variable delcarations
	if '::' in line:
		# find compound declarations and assignments
		assignments = [tmp.strip()
		               for tmp in line.split('::')[1].split(',')
		               if '=' in tmp]
		for ass in assignments:
			names = validVar.findall(ass)
			globalVars |= set(names[1:]) - localVars
			if names[0] not in globalVars:
				localVars.add(names[0])
		line = '' # so we don't get reparsed later
				
	# if statement (and not a var name like ifstuff)
	if firstWord == 'if':
		# matches closing paren followed by new identifier
		cparenIdx = re.search(r'\)\s*[a-z]', line).start()
		conditional = line[line.index('(') + 1 : cparenIdx].strip()
		statement = line[cparenIdx + 1 : ].strip()
		# process names in conditional
		globalVars |= set(validVar.findall(conditional)) - localVars

		# if we have a statment (and not a then block), process it normally
		if statement != 'then':
			line = statement
			firstWord = line.split()[0]

	# subroutine call
	if firstWord == 'call':
		names = validVar.findall(line[4:])
		# we may overestimate some global vars
		# locals might get 
		globalVars |= set(names[1:]) - localVars
		subNames.add(names[0])

	# allocation
	if firstWord == 'allocate':
		# parse out indexing expressions, so only the array names remain
		tmpString = line[line.index('(') + 1 :]
		while '(' in tmpString:
			tmpString = parenStuff.sub('', tmpString)
		arrays = set(validVar.findall(tmpString))
		fullVars = set(validVar.findall(line)[1:])
		
		# if we're allocating something ourself, it must be local
		localVars |= arrays

		# treat indexing variables like any rvalue
		globalVars |= fullVars - localVars
		
	# do loop
	if firstWord == 'do':
		line = line[2:].lstrip('0123456789 \t') # strip 'do' and any labels

		if line[5:] == 'while' and not line[5].isalpha():
			# do-while, acts more like an if statement
			globalVars |= set(validVar.findall(line[5:])) - localVars
		
		# otherwise, we have <var> = <value>, <value> [, <value>]
		# this looks just like an assignment, so we can parse it below

	# standard assignment
	# <var> [ (<value> [, <value> [...]]) ] = <value>
	eqMatch = re.search(r'(?<!=)=(?!=)', line) # matches single =, not double
	if eqMatch:
		names = validVar.findall(line)
		if '(' in line and line.index('(') < eqMatch.start():
			# lvalue is an array index
			# arrays may have stuff in them beforehand, effectively global
			if names[0] not in localVars:
				globalVars.add(names[0])
		elif names[0] not in globalVars:
			# non-array lvalue not previously used, effectively local
			localVars.add(names[0])

		# rvalues must be global unless we've set them before
		globalVars |= set(names[1:]) - localVars

	line = infile.readline()

if infile is not sys.stdin:
	infile.close()

print('----SUBROUTINES----')
for v in subNames: print(v)
print()
print('----LOCAL VARIABLES----')
for v in localVars - stopwords: print(v)
print()
print('----GLOBAL VARIABLES----')
for v in globalVars - stopwords: print(v)
