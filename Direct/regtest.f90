! prints the greatest discrepancy betweeen two arrays (on every task)
module regtest
contains
  subroutine print_arraydiff_3(verbose, arname, ar1, ar2)
    use gem_com
    logical :: verbose
    character(len=*) :: arname
    real(8) :: ar1(:,:,:), ar2(:,:,:)

    integer :: ldims(3), udims(3), maxijk(3)
    integer :: i, j, k

    real(8) :: diff, maxdiff
    real(8) :: maxdiff_ar(0:numprocs-1)
    integer :: maxijk_ar(0:3*numprocs-1)

    ! only use overlapping array parts
    ! (should be entire arrays, but it's good to check)
    ldims = max(lbound(ar1), lbound(ar2))
    udims = min(ubound(ar1), ubound(ar2))

    ! determine location and value of max discrepancy
    maxdiff = 0
    maxijk = ldims

    do i = ldims(1), udims(1)
       do j = ldims(2), udims(2)
          do k = ldims(3), udims(3)
             diff = abs(ar1(i,j,k) - ar2(i,j,k))
             if (diff > maxdiff) then
                maxdiff = diff
                maxijk = [i,j,k]
             end if
          end do
       end do
    end do

    ! master thread gathers data
    call MPI_GATHER(maxdiff, 1, MPI_REAL8, maxdiff_ar, 1, MPI_REAL8,&
         master, MPI_COMM_WORLD, ierr)
    call MPI_GATHER(maxijk, 3, MPI_INTEGER, maxijk_ar, 3, MPI_INTEGER,&
         master, MPI_COMM_WORLD, ierr)

    ! master thread prints data
    ! <varname> max diff (<id>): <value> at <i>,<j>,<k>
    if (myid == master) then
       ! find largest error overall
       do i = 0, numprocs - 1
          if (maxdiff_ar(i) > maxdiff) maxdiff = maxdiff_ar(i)
       end do

       if (maxdiff == 0) write (*,'(A,A)') arname, ': no discrepancies found'

       do i = 0, numprocs - 1
          ! if verbose flag is set, print all
          ! else, print largest and all those within 5%
          if (verbose .or. maxdiff_ar(i) / maxdiff .ge. 0.95) then
             write (*,'(A,A,I0.4)',advance='no') arname, ' max diff (', i
             write (*,'(A,ES12.5,A)',advance='no') '): ', maxdiff_ar(i), ' at '
             write (*,'(I0,A,I0)',advance='no') maxijk_ar(3*i), ',', maxijk_ar(3*i+1)
             write (*,'(A,I0)') ',', maxijk_ar(3*i+2)
          end if
       end do
    end if
  end subroutine print_arraydiff_3

  ! this function serializes the input for some reference case of jpar0
  ! file name is <datadir>/jpar0.<tmpname>.<id>.in.regtest
  subroutine regtest_jpar0_insave(datadir, tmpname)
    use gem_com
    use equil
    implicit none
    character(len=*) :: datadir
    character(len=*) :: tmpname
    character(len=4) :: myidstring
    character(len=1024) :: regfname

    ! determine filename for input variables
    write (myidstring, '(I0.4)') myid
    regfname = datadir//'/jpar0.'//tmpname//'.'//myidstring//'.in.regtest'

    open(unit=525, file=regfname, &
         form='unformatted', action='write', status='replace')
    ! write input vars
    write (525) amie
    write (525) apar
    write (525) bfld
    write (525) br0
    write (525) cn0e
    write (525) dbdr
    write (525) delz
    write (525) den0apa
    write (525) dene
    write (525) dr
    write (525) dth
    write (525) dx
    write (525) dy
    write (525) dz
    write (525) eldu
    write (525) emass
    write (525) f
    write (525) grcgt
    write (525) grid_comm
    write (525) im
    write (525) imx
    write (525) isg
    write (525) isuni
    write (525) jac
    write (525) jfn
    write (525) jm
    write (525) jmx
    write (525) lr0
    write (525) lx
    write (525) mme
    write (525) mue3
    write (525) mykm
    write (525) n0e
    write (525) nonline
    write (525) nr
    write (525) phi
    write (525) psip
    write (525) q0
    write (525) qel
    write (525) radius
    write (525) rin
    write (525) t0e
    write (525) tge
    write (525) thfnz
    write (525) tor
    write (525) u3e
    write (525) upa0
    write (525) upa00
    write (525) upa0t
    write (525) vwidthe
    write (525) w000
    write (525) w001
    write (525) w010
    write (525) w011
    write (525) w100
    write (525) w101
    write (525) w110
    write (525) w111
    write (525) x3e
    write (525) xn0e
    write (525) y3e
    write (525) z3e

    close(525)
  end subroutine regtest_jpar0_insave

  ! reads in reference input variables to set up test
  subroutine regtest_jpar0_inload(datadir, tmpname)
    use gem_com
    use equil
    implicit none
    character(len=*) :: datadir
    character(len=*) :: tmpname
    character(len=4) :: myidstring
    character(len=1024) :: regfname

    ! determine filename
    write (myidstring, '(I0.4)') myid
    regfname = datadir//'/jpar0.'//tmpname//'.'//myidstring//'.in.regtest'

    open(unit=525, file=regfname, form='unformatted', action='read')
    ! read input vars
    read (525) amie
    read (525) apar
    read (525) bfld
    read (525) br0
    read (525) cn0e
    read (525) dbdr
    read (525) delz
    read (525) den0apa
    read (525) dene
    read (525) dr
    read (525) dth
    read (525) dx
    read (525) dy
    read (525) dz
    read (525) eldu
    read (525) emass
    read (525) f
    read (525) grcgt
    read (525) grid_comm
    read (525) im
    read (525) imx
    read (525) isg
    read (525) isuni
    read (525) jac
    read (525) jfn
    read (525) jm
    read (525) jmx
    read (525) lr0
    read (525) lx
    read (525) mme
    read (525) mue3
    read (525) mykm
    read (525) n0e
    read (525) nonline
    read (525) nr
    read (525) phi
    read (525) psip
    read (525) q0
    read (525) qel
    read (525) radius
    read (525) rin
    read (525) t0e
    read (525) tge
    read (525) thfnz
    read (525) tor
    read (525) u3e
    read (525) upa0
    read (525) upa00
    read (525) upa0t
    read (525) vwidthe
    read (525) w000
    read (525) w001
    read (525) w010
    read (525) w011
    read (525) w100
    read (525) w101
    read (525) w110
    read (525) w111
    read (525) x3e
    read (525) xn0e
    read (525) y3e
    read (525) z3e

    close(525)
  end subroutine regtest_jpar0_inload

  ! this function serializes the output for a reference run or test run of jpar0
  ! file name is <datadir>/jpar0.<tmpname>.<id>.out.regtest
  subroutine regtest_jpar0_outsave(datadir, tmpname)
    use gem_com
    use equil
    implicit none
    character(len=*) :: datadir
    character(len=*) :: tmpname
    character(len=4) :: myidstring
    character(len=1024) :: regfname

    ! determine filename
    write (myidstring, '(I0.4)') myid
    regfname = datadir//'/jpar0.'//tmpname//'.'//myidstring//'.out.regtest'

    open(unit=525, file=regfname, &
         form='unformatted', action='write', status='replace')

    ! write output vars
    write (525) den0apa
    write (525) upa0
    write (525) upa00

    close(525)
  end subroutine regtest_jpar0_outsave

  ! loads output variables for comparison
  subroutine regtest_jpar0_outload(datadir, tmpname)
    use gem_com
    use equil
    implicit none
    character(len=*) :: datadir
    character(len=*) :: tmpname
    character(len=4) :: myidstring
    character(len=1024) :: regfname

    ! determine filename
    write (myidstring, '(I0.4)') myid
    regfname = datadir//'/jpar0.'//tmpname//'.'//myidstring//'.out.regtest'

    open(unit=525, file=regfname, form='unformatted', action='read')

    ! read vars
    read (525) den0apa
    read (525) upa0
    read (525) upa00

    close(525)
  end subroutine regtest_jpar0_outload
end module regtest
