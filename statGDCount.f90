!
! This is to get the statistic histogram for isolated geodesic counts
!   this script is reading count.snap$i.geocard.isogd files
!

program statGDCount
     implicit none
     integer :: snapid, nodeid, edgek, sumk
     integer :: snapi, snapf
     integer, dimension(5000) :: Stat_edge_per_node
     integer, dimension(1000000) :: Stat_edge_per_snap
     character(100) :: filename, fid, tmp
     integer :: eri, erj, i, j, lower, upper

     do i = 1, 5000
        Stat_edge_per_node(i) = 0
     enddo

     do i = 1, 1000000
        Stat_edge_per_snap(i) = 0
     enddo

     ! reading snapshot range
     eri = command_argument_count()
     if(eri /= 2 ) then
        write(6, '(a)') 'use this as: statGDCount.exe snapi snapf . Exiting.'
        stop
     endif

     call getarg(1,tmp)
     read(tmp, '(I10)') snapi
     call getarg(2,tmp)
     read(tmp, '(I10)') snapf
!     write(6,*) snapi, snapf

     ! reading the isolated gd path number over all snapshots
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

        filename = "count.snap"//trim(fid)//".geocard.isogd"
        write(6,*) filename
        open(unit=11,file=filename,action='read',iostat=eri)
        if(eri /= 0) then
           write(6,*) "error, cannot find the input file: ", filename
           stop
        else
           erj = 0
           sumk = 0
           do while (erj .eq. 0)
              read(11, *, iostat = erj) nodeid, edgek
              if(erj /= 0) then
                 exit
              else
                 if(edgek .ge. 5000) then
                    write(6,*) 'overflow in Stat_edge_per_node', fid, nodeid
                    stop
                 endif
                 sumk = sumk + edgek
                 Stat_edge_per_node(edgek+1) = Stat_edge_per_node(edgek+1) + 1
              endif
!              write(6,*) "   ", snapid, nodeid, edgek, Stat_edge_per_node(edgek+1)
           enddo
           if(sumk .ge. 1000000) then
              write(6,*) 'overlow in Stat_edge_per_snap', fid
              stop
           endif
           Stat_edge_per_snap(sumk) = Stat_edge_per_snap(sumk) + 1
        endif
        write(6,*) snapid,sumk
        close(11)
     enddo

     ! print information
     filename = "result.statNode"
     open(unit=22,file=filename,action='write',iostat=eri)
     if(eri /= 0) then
         write(6,*) "error, cannot create output file: ", filename
         stop
     else
         lower = 5000
         upper = 0
         do i = 1, 5000
           if(Stat_edge_per_node(i) /= 0) then
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
           write(22,*) i-1, Stat_edge_per_node(i)
         enddo
     endif
     close(22)

     filename = "result.statSnap"
     open(unit=33,file=filename,action='write',iostat=eri)
     if(eri /= 0) then
         write(6,*) "error, cannot create output file: ", filename
         stop
     else
         lower = 1000000
         upper = 0
         do i = 1, 1000000
           if(Stat_edge_per_snap(i) /= 0) then
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
           write(33,*) i, Stat_edge_per_snap(i)
         enddo
     endif
     close(33)




end program statGDCount


