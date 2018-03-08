   PROGRAM isoGD
   implicit none
!*********************************************************************
!     
!      program to determined the actual count of geodesics
!      that omits all sub-geodesics contained within our normal
!      geodesic reporting code
!    
!         updated version by tiecheng
! 
!         compile this code: gfortran isoGD-new2.f90 -o isoGD-new2.exe
!         use this code as:  ./isoGD-new2.exe input-file-name
! 
!*********************************************************************
   integer, parameter :: maxline =1000000    ! maximum # of lines in the input file (.geocard)
   integer, parameter :: maxlength = 200   ! maximum path length of a geodesic path (in one line of .geocard)
   integer :: j,k,nlines,imatch,nklines,p,labk,lineid
   integer :: dum_i1,dum_i2,dum_i3,ilength,largestlength
   integer, dimension(1:maxlength) :: dum_chain
   character(LEN=5) :: dum_c1
   character(LEN=9) :: dum_c2
   real :: dum_d
   integer :: num_args,ia
   character(LEN=100),dimension(:),allocatable :: args
   character(LEN=100) :: fname1,fname2
   
   TYPE GD_path   ! the derived type of geodesic path
     integer :: firstnode
     integer :: lastnode
     integer :: pathlength
     real :: pathdist
     integer,dimension(1:maxlength) :: pathchain
     integer :: isolated  ! =zero if it is isolated, otherwise =line number of GD_path it belongs to
   end TYPE GD_path

   TYPE(GD_path), dimension(1:maxline) :: GD_ALL,GD_k,GD_p   ! keeps all the geodesics from the input file
   TYPE(GD_path) :: pathj,pathk

!****************************************
! get the input-file-name from the 1st argument

      num_args = command_argument_count()
      if(num_args .ne. 1) then
         write(6,*) 'error: need 1 argument as the input-file-name'
         stop
      endif
      allocate(args(num_args))
      do ia = 1, num_args
         call get_command_argument(1,args(1))
 !        write(6,*) args(1)
      enddo
     
      fname1 = trim(args(1))
      fname2 = trim(fname1) // '.isogd'
!      write(6,*) fname1,fname2

      open(1,file=fname1)     ! input file
      open(2,file=fname2)     ! output file

!
!***************************************************************
! Open input file and read the information

      j  = 1
      largestlength = 0
      do while(j .lt. maxline)
         read(1,*,end=200) dum_i1,dum_i2,dum_i3  !figure out how many lines in file

         if(dum_i3 .ge. maxlength) then
            write(6,*) 'error: maxlength is not enough',dum_i1,dum_i2,dum_i3
            exit
         else
            ilength=dum_i3
         endif

         backspace 1
         read(1,*,end=200) dum_i1,dum_i2,dum_i3,dum_c1,&      ! read a whole line
              & (dum_chain(k),k=1,ilength+1),dum_c2,dum_d
!            write(6,*) dum_i1,dum_i2,dum_i3,dum_c1,&
!               & (dum_chain(k),k=1,ilength+1),dum_c2,dum_d

         GD_ALL(j)%firstnode = dum_i1
         GD_ALL(j)%lastnode = dum_i2
         GD_ALL(j)%pathlength = ilength
         GD_ALL(j)%pathdist = dum_d
         do k=1,ilength+1
            GD_ALL(j)%pathchain(k) = dum_chain(k)
         enddo
         GD_ALL(j)%isolated = 0  ! initially, consider all of them are isolated

         if(largestlength .lt. ilength) then
            largestlength = ilength
         endif

         j = j+1
      enddo
200   nlines = j-1     ! total number of lines

      if((nlines .ge. maxline-1) .or. largestlength .ge. maxlength) then
         write(6,*) 'array overflow: nlines',nlines, 'maximum', maxline, '  ,  ', &
            & 'length',largestlength, 'maximum of', maxlength
         stop
      endif

!      write(6,*) nlines,largestlength
      close (1)    ! close the input file

! **********************
! below is the main loop for select the isolated geodesics
! starting from the longest path, remove all the short paths that are included in it

      ilength = largestlength

      nklines=nlines
      do k=1,nklines
        GD_k(k) = GD_ALL(k)     ! GD_k is the temporary array, for those paths that are temporary isolated (not belong to other longer paths)
        GD_p(k) = GD_ALL(k)
        GD_k(k)%isolated = k  ! record the line number of that path
        GD_p(k)%isolated = k 
      enddo
      labk = 1     ! 1 use GD_k as the temporary array, or 0 use GD_p as the temporary array

      do while(ilength .ge. 1)  ! loop over GD_ALL, selecting from longest path, discard all of small paths included in it
!         write(6,*) "processing length",ilength
         do j = 1,nlines
            pathj = GD_ALL(j)
!           write(6,*) j,pathj%firstnode,pathj%lastnode,pathj%pathlength,pathj%pathdist,pathj%pathchain(1),pathj%pathchain(2)
            if(pathj%pathlength .ne. ilength) then    ! skip shorter paths
               cycle       ! skip to the next path id
            else
               if(pathj%isolated .ne. 0) then  ! skip if this path is included in some longer paths
                  cycle
               else    ! if this is a path with length of "ilength", and it has not been included before
                  p=0
                  do k = 1,nklines
                     if(labk .eq. 1) then
                        pathk = GD_k(k)
                     else if (labk .eq. 0) then
                        pathk = GD_p(k)
                     endif 

                     lineid = pathk%isolated
                     if(lineid .eq. j) then  ! skip if k == j
                        cycle
                     endif
              !       if(pathk%isolated .ne. 0) then  ! skip if pathk is already belong to some longer path
              !          cycle                        ! updated this region, by using temporary array GD_k
              !       endif
                     imatch = GD_match(pathk,pathj)
                     if(imatch .eq. 1)  then     ! pathj here is include in pathi
                        GD_ALL(lineid)%isolated = j   ! set the 'isolated' label to the pathi
!  write(6,*) k,pathk%firstnode,pathk%lastnode,j,pathj%firstnode,pathj%lastnode
                     else
                        p = p+1
                        if(labk .eq. 1) then
                           GD_p(p) = pathk  ! copy to a new temporary array, record those paths that are not belong to pathj
                        else if(labk .eq. 0) then
                           GD_k(p) = pathk
                        endif
                     endif
                  enddo

                  nklines = p  ! update the temporary array 
   !                 write(6,*) ilength,j,nklines
                  if(labk .eq. 1) then
                     labk = 0
                  else
                     labk = 1
                  endif

               endif      ! end of "if(pathj%isolated .ne. 0)"
            endif        ! end of "if(pathj%pathlength .ne. ilength)"
         enddo

         ilength = ilength-1
      enddo

! ********************
! start output   
      do j = 1,nlines
         pathj = GD_ALL(j)
         if(pathj%isolated .eq. 0) then ! only print the isolated ones
            do k=1,pathj%pathlength+1
               dum_chain(k) = pathj%pathchain(k)
            enddo
            write(2,*) pathj%firstnode, pathj%lastnode, pathj%pathlength, pathj%pathdist, & 
                & pathj%isolated, (dum_chain(k),k=1,pathj%pathlength+1)
          endif    ! end of "if(pathj%isolated .eq. 0)" 
     enddo

      close (2)  ! close the output file


   contains
      integer function GD_match(path1,path2)    ! this function checks whether path1 is included in path2
         TYPE(GD_path),intent(in) :: path1
         TYPE(GD_path),intent(in) :: path2
         integer :: node1,node2,length1,length2,nodei,nodej,nodek,dum_mi,dum_mj,find

         node1 = path1%firstnode
         node2 = path1%lastnode
         length1 = path1%pathlength
         length2 = path2%pathlength

         GD_match = 0    ! initially assume path1 is not included in path2

         if(length1 .gt. length2) then  ! if path1 is included in path2, it has to be shorter than path2
            GD_match = 0
         else     
            find = 0
            do dum_mi = 1,length2-length1+1
                nodei = path2%pathchain(dum_mi)
                nodej = path2%pathchain(dum_mi+length1)
                if((nodei .ne. node1) .or. (nodej .ne. node2)) then
                   cycle     ! skip if the starting node or the ending node are not match
                else
                   find = 1
                   do dum_mj = 1,length1+1
                      nodek = path2%pathchain(dum_mi+dum_mj-1)
                      if(nodek .eq. path1%pathchain(dum_mj)) then
                         cycle
                      else
                         find = 0
                         exit
                      endif
                   enddo  ! end of "do dum_mj = 1,length1+1"
                endif

                if(find .eq. 1) then
                   exit
                endif
            enddo ! end of "do dum_mi = 1,length2-length1+1"

            if(find .ne. 1) then ! if it does not match in the ordinary order, check the reverse order
               do dum_mi = length2+1,length1+1,-1   ! considering the reverse order of the path
                  nodei = path2%pathchain(dum_mi)
                  nodej = path2%pathchain(dum_mi-length1)
                  if((nodei .ne. node1) .or. (nodej .ne. node2)) then
                     cycle     ! skip if the starting node or the ending node are not match
                  else
                     find = 1
                     do dum_mj = 1,length1+1
                        nodek = path2%pathchain(dum_mi-dum_mj+1)
                        if(nodek .eq. path1%pathchain(dum_mj)) then
                           cycle
                        else
                           find = 0
                           exit
                        endif
                     enddo
                  endif

                  if(find .eq. 1) then
                     exit
                  endif
               enddo   ! end of "do dum_mi = length2+1,length1+1"
            endif   ! end of "if(find .ne. 1) then" 

         endif   ! end of "if(length1 .lt. length2) then else"

         if(find .eq. 1) then
            GD_match = 1   ! return 1 if path1 is included in path2, otherwise return 0
         else 
            GD_match = 0
         endif
         return
      end function GD_match

   end PROGRAM isoGD


