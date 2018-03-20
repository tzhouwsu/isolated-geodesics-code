!
! This is to get the statistic histogram for isolated geodesic lengths
!   this script is reading snap$i.geocard.isogd files
!

program statGDLength
     implicit none
     integer :: snapid, nodeid, nodejd, ilength
     integer :: snapi, snapf
     integer, dimension(500) :: Stat_length_of_edge
     character(100) :: filename, fid, tmp
     integer :: eri, erj, i, j, lower, upper

     do i = 1, 500
        Stat_length_of_edge(i) = 0
     enddo

     ! reading snapshot range
     eri = command_argument_count()
     if(eri /= 2 ) then
        write(6, '(a)') 'use this as: statGDLength.exe snapi snapf . Exiting.'
        stop
     endif

     call getarg(1,tmp)
     read(tmp, '(I10)') snapi
     call getarg(2,tmp)
     read(tmp, '(I10)') snapf
!     write(6,*) snapi, snapf

     ! reading the isolated gd path-length over all snapshots
     do snapid = snapi, snapf
        if(snapid .le. 9) then
           write(fid,"(I1)") snapid
        else if(snapid .ge. 10 .and. snapid .le. 99) then
           write(fid,"(I2)") snapid
        else if(snapid .ge. 100 .and. snapid .le. 999) then
           write(fid,"(I3)") snapid
        else if(snapid .ge. 1000 .and. snapid .le. 9999) then
           write(fid,"(I4)") snapid
        endif

        filename = "snap"//trim(fid)//".geocard.isogd"
        write(6,*) filename
        open(unit=11,file=filename,action='read',iostat=eri)
        if(eri /= 0) then
           write(6,*) "error, cannot find the input file: ", filename
           stop
        else
           erj = 0
           do while (erj .eq. 0)
              read(11, *, iostat = erj) nodeid, nodejd, ilength
              if(erj /= 0) then
                 exit
              else
                 if(ilength .ge. 500) then
                    write(6,*) 'overflow in Stat_length_of_edge', fid, nodeid,nodejd,ilength
                    stop
                 endif
                 Stat_length_of_edge(ilength) = Stat_length_of_edge(ilength) + 1
              endif
           enddo
        endif
        write(6,*) "  processed", snapid
        close(11)
     enddo

     ! print information
     filename = "result.statLength"
     open(unit=22,file=filename,action='write',iostat=eri)
     if(eri /= 0) then
         write(6,*) "error, cannot create output file: ", filename
         stop
     else
         lower = 500
         upper = 0
         do i = 1, 500
           if(Stat_length_of_edge(i) /= 0) then
             if(i .lt. lower) then
               lower = i
             endif
             if(i .gt. upper) then
               upper = i
             endif
           endif
         enddo
!         write(6,*) lower, upper

         do i = lower, upper
           write(22,*) i, Stat_length_of_edge(i)
         enddo
     endif
     close(22)

end program


