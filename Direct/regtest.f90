module regtest
contains
  ! prints the greatest discrepancy betweeen two arrays (on every task)
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


  !!!! REGRESSION TESTING FOR JPAR0 !!!!

  ! main regtest function
  subroutine regtest_jpar0(refrun, datadir, tmpname, ip, n, it, itp)
    use gem_com
    implicit none
    logical :: refrun ! whether this is a reference run
    integer :: ip, n, it, itp    !jpar0 params
    character(len=*) :: datadir
    character(len=*) :: tmpname

    real(8),dimension(:,:,:),allocatable :: upa0_new, upa00_new, den0apa_new

    integer :: i, j, k

    if (refrun) then
       call regtest_jpar0_insave(datadir, tmpname)
       call jpar0(ip, n, i, itp)
       call regtest_jpar0_outsave(datadir, tmpname)
    else
       call regtest_jpar0_inload(datadir, tmpname)
       call jpar0(ip, n, i, itp)

       ! copy output to new arrays, as outload will overwrite them
       allocate(upa0_new(0:nxpp,0:jmx,0:1))
       allocate(upa00_new(0:nxpp,0:jmx,0:1))
       allocate(den0apa_new(0:nxpp,0:jmx,0:1))
       upa0_new = upa0
       upa00_new = upa00
       den0apa_new = den0apa

       call regtest_jpar0_outload(datadir, tmpname)

       ! print largest differences between old and new arrays
       call print_arraydiff_3(.False., 'upa0', upa0, upa0_new)
       call print_arraydiff_3(.False., 'upa00', upa00, upa00_new)
       call print_arraydiff_3(.False., 'den0apa', den0apa, den0apa_new)

       deallocate(upa0_new, upa00_new, den0apa_new)
    end if
  end subroutine regtest_jpar0

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

  !!!! REGRESSION TESTING FOR JIE !!!!

  ! main regtest function
  subroutine regtest_jie(refrun, datadir, tmpname, ip, n)
    use gem_com
    implicit none
    logical :: refrun ! whether this is a reference run
    character(len=*) :: datadir
    character(len=*) :: tmpname
    integer :: ip, n ! jie arguments

    real(8),dimension(:,:,:),allocatable :: dnedt_new, upazd_new
    real(8),dimension(:,:,:),allocatable :: upex_new, upey_new
    real(8),dimension(:,:,:,:),allocatable :: dnidt_new, jpar_new
    real(8),dimension(:,:,:,:),allocatable :: jpex_new, jpey_new

    integer :: i
    character(len=8) :: istring

    if (refrun) then
       call regtest_jie_insave(datadir, tmpname)
       call jie(ip, n) ! jie params are ignoreable
       call regtest_jie_outsave(datadir, tmpname)
    else
       call regtest_jie_inload(datadir, tmpname)
       call jie(ip, n)

       ! copy output to new arrays, as outload will overwrite them
       allocate(dnedt_new(0:nxpp,0:jmx,0:1))
       allocate(upazd_new(0:nxpp,0:jmx,0:1))
       allocate(upex_new(0:nxpp,0:jmx,0:1))
       allocate(upey_new(0:nxpp,0:jmx,0:1))
       allocate(dnidt_new(nsmx,0:nxpp,0:jmx,0:1))
       allocate(jpar_new(nsmx,0:nxpp,0:jmx,0:1))
       allocate(jpex_new(nsmx,0:nxpp,0:jmx,0:1))
       allocate(jpey_new(nsmx,0:nxpp,0:jmx,0:1))

       dnedt_new = dnedt
       upazd_new = upazd
       upex_new  = upex
       upey_new  = upey
       dnidt_new = dnidt
       jpar_new  = jpar
       jpex_new  = jpex
       jpey_new  = jpey

       call regtest_jie_outload(datadir, tmpname)

       ! print largest differences between old and new arrays
       call print_arraydiff_3(.False., 'dnedt', dnedt, dnedt_new)
       call print_arraydiff_3(.False., 'upazd', upazd, upazd_new)
       call print_arraydiff_3(.False., 'upex', upex, upex_new)
       call print_arraydiff_3(.False., 'upey', upey, upey_new)

       do i = 1, nsmx ! for 4d arrays, we do this for each species
          write (istring, '(A,I0,A)'), '(', i, ')'
          call print_arraydiff_3(.False., 'dnidt'//trim(istring), &
               dnidt(i,:,:,:), dnidt_new(i,:,:,:))
          call print_arraydiff_3(.False., 'jpar'//trim(istring), &
               jpar(i,:,:,:), jpar_new(i,:,:,:))
          call print_arraydiff_3(.False., 'jpex'//trim(istring), &
               jpex(i,:,:,:), jpex_new(i,:,:,:))
          call print_arraydiff_3(.False., 'jpey'//trim(istring), &
               jpey(i,:,:,:), jpey_new(i,:,:,:))
       end do

       deallocate(dnedt_new, upazd_new, upex_new, upey_new)
       deallocate(dnidt_new, jpar_new, jpex_new, jpey_new)
    end if
  end subroutine regtest_jie

  ! this function serializes the input for some reference case of jie
  ! file name is <datadir>/jie.<tmpname>.<id>.in.regtest
  subroutine regtest_jie_insave(datadir, tmpname)
    use gem_com
    use equil
    implicit none
    character(len=*) :: datadir
    character(len=*) :: tmpname
    character(len=4) :: myidstring
    character(len=1024) :: regfname

    ! determine filename for input variables
    write (myidstring, '(I0.4)') myid
    regfname = datadir//'/jie.'//tmpname//'.'//myidstring//'.in.regtest'

    open(unit=525, file=regfname, &
         form='unformatted', action='write', status='replace')
    ! write input vars
    write (525) amie
    write (525) apar
    write (525) bdcrvb
    write (525) bfld
    write (525) br0
    write (525) capne
    write (525) capns
    write (525) capte
    write (525) capts
    write (525) cn0e
    write (525) cn0s
    write (525) curvbz
    write (525) dbdr
    write (525) dbdth
    write (525) delbx
    write (525) delby
    write (525) delz
    write (525) dipdr
    write (525) dnedt
    write (525) dnidt
    write (525) dr
    write (525) dth
    write (525) dx
    write (525) dy
    write (525) dydr
    write (525) dz
    write (525) eldu
    write (525) emass
    write (525) ex
    write (525) ey
    write (525) ez
    write (525) f
    write (525) gclr
    write (525) gr
    write (525) grcgt
    write (525) grdgt
    write (525) gxdgy
    write (525) iflr
    write (525) iflut
    write (525) ildu
    write (525) im
    write (525) iorb
    write (525) isg
    write (525) isuni
    write (525) jac
    write (525) jfn
    write (525) jm
    write (525) jpar
    write (525) jpex
    write (525) jpey
    write (525) kcnt
    write (525) lr
    write (525) lr0
    write (525) lx
    write (525) ly
    write (525) mims
    write (525) mm
    write (525) mme
    write (525) mu
    write (525) mue3
    write (525) mykm
    write (525) n0
    write (525) n0e
    write (525) nonlin
    write (525) nonline
    write (525) nr
    write (525) nsm
    write (525) phi
    write (525) phincp
    write (525) psip
    write (525) psip2
    write (525) q
    write (525) q0
    write (525) qel
    write (525) qhat
    write (525) r0
    write (525) radius
    write (525) rin
    write (525) t0e
    write (525) t0s
    write (525) tge
    write (525) tgis
    write (525) thfnz
    write (525) tor
    write (525) u3
    write (525) u3e
    write (525) upazd
    write (525) upex
    write (525) upey
    write (525) vexbsw
    write (525) vparsp
    write (525) vparsw
    write (525) vwidth
    write (525) vwidthe
    write (525) w000
    write (525) w001
    write (525) w010
    write (525) w011
    write (525) w100
    write (525) w101
    write (525) w110
    write (525) w111
    write (525) w3
    write (525) w3e
    write (525) x3
    write (525) x3e
    write (525) xn0e
    write (525) xn0s
    write (525) y3
    write (525) y3e
    write (525) z3
    write (525) z3e
    close(525)
  end subroutine regtest_jie_insave

  ! reads in reference input variables to set up test
  subroutine regtest_jie_inload(datadir, tmpname)
    use gem_com
    use equil
    implicit none
    character(len=*) :: datadir
    character(len=*) :: tmpname
    character(len=4) :: myidstring
    character(len=1024) :: regfname

    ! determine filename
    write (myidstring, '(I0.4)') myid
    regfname = datadir//'/jie.'//tmpname//'.'//myidstring//'.in.regtest'

    open(unit=525, file=regfname, form='unformatted', action='read')
    ! read input vars
    read (525) amie
    read (525) apar
    read (525) bdcrvb
    read (525) bfld
    read (525) br0
    read (525) capne
    read (525) capns
    read (525) capte
    read (525) capts
    read (525) cn0e
    read (525) cn0s
    read (525) curvbz
    read (525) dbdr
    read (525) dbdth
    read (525) delbx
    read (525) delby
    read (525) delz
    read (525) dipdr
    read (525) dnedt
    read (525) dnidt
    read (525) dr
    read (525) dth
    read (525) dx
    read (525) dy
    read (525) dydr
    read (525) dz
    read (525) eldu
    read (525) emass
    read (525) ex
    read (525) ey
    read (525) ez
    read (525) f
    read (525) gclr
    read (525) gr
    read (525) grcgt
    read (525) grdgt
    read (525) gxdgy
    read (525) iflr
    read (525) iflut
    read (525) ildu
    read (525) im
    read (525) iorb
    read (525) isg
    read (525) isuni
    read (525) jac
    read (525) jfn
    read (525) jm
    read (525) jpar
    read (525) jpex
    read (525) jpey
    read (525) kcnt
    read (525) lr
    read (525) lr0
    read (525) lx
    read (525) ly
    read (525) mims
    read (525) mm
    read (525) mme
    read (525) mu
    read (525) mue3
    read (525) mykm
    read (525) n0
    read (525) n0e
    read (525) nonlin
    read (525) nonline
    read (525) nr
    read (525) nsm
    read (525) phi
    read (525) phincp
    read (525) psip
    read (525) psip2
    read (525) q
    read (525) q0
    read (525) qel
    read (525) qhat
    read (525) r0
    read (525) radius
    read (525) rin
    read (525) t0e
    read (525) t0s
    read (525) tge
    read (525) tgis
    read (525) thfnz
    read (525) tor
    read (525) u3
    read (525) u3e
    read (525) upazd
    read (525) upex
    read (525) upey
    read (525) vexbsw
    read (525) vparsp
    read (525) vparsw
    read (525) vwidth
    read (525) vwidthe
    read (525) w000
    read (525) w001
    read (525) w010
    read (525) w011
    read (525) w100
    read (525) w101
    read (525) w110
    read (525) w111
    read (525) w3
    read (525) w3e
    read (525) x3
    read (525) x3e
    read (525) xn0e
    read (525) xn0s
    read (525) y3
    read (525) y3e
    read (525) z3
    read (525) z3e
    close(525)
  end subroutine regtest_jie_inload

  ! this function serializes the output for a reference run or test run of jie
  ! file name is <datadir>/jie.<tmpname>.<id>.out.regtest
  subroutine regtest_jie_outsave(datadir, tmpname)
    use gem_com
    use equil
    implicit none
    character(len=*) :: datadir
    character(len=*) :: tmpname
    character(len=4) :: myidstring
    character(len=1024) :: regfname

    ! determine filename
    write (myidstring, '(I0.4)') myid
    regfname = datadir//'/jie.'//tmpname//'.'//myidstring//'.out.regtest'

    open(unit=525, file=regfname, &
         form='unformatted', action='write', status='replace')

    ! write output vars
    write (525) dnedt
    write (525) dnidt
    write (525) jpar
    write (525) jpex
    write (525) jpey
    write (525) upazd
    write (525) upex
    write (525) upey
    write (525) den0apa

    close(525)
  end subroutine regtest_jie_outsave

  ! loads output variables for comparison
  subroutine regtest_jie_outload(datadir, tmpname)
    use gem_com
    use equil
    implicit none
    character(len=*) :: datadir
    character(len=*) :: tmpname
    character(len=4) :: myidstring
    character(len=1024) :: regfname

    ! determine filename
    write (myidstring, '(I0.4)') myid
    regfname = datadir//'/jie.'//tmpname//'.'//myidstring//'.out.regtest'

    open(unit=525, file=regfname, form='unformatted', action='read')

    ! read vars
    read (525) dnedt
    read (525) dnidt
    read (525) jpar
    read (525) jpex
    read (525) jpey
    read (525) upazd
    read (525) upex
    read (525) upey
    read (525) den0apa

    close(525)
  end subroutine regtest_jie_outload
end module regtest
