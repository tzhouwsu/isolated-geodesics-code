      PROGRAM isoGDCount
    
!
!!!  WARNING: The filename nlines.dat will be replaced by this program!!
!             Input: name_of_input_file number_of_nodes
!              Output: count.total
!              *-> To get geodesic index, divide count.total numbers by
!                   number_of_nodes * number_of_nodes * snapshot_count
!   
!    last modified: Jan 2018 by L. Edens
!**********************************************************************
      IMPLICIT NONE
      integer, parameter :: filelen = 100
      integer :: nlines, er, skip, npath
      integer :: i,j,k,nnodes,sumEdge
      integer, dimension(100) :: hold
      integer, allocatable :: edge(:)
      real :: dist
      character*(filelen) :: inputfile
      character*(6) :: com, tmp
      character*(34) :: mand
      character*(filelen+40) :: command
!**********************************************************************
!check
      er = command_argument_count()
      if(er == 0 ) then
         write(6, '(a)')'An input file is required. Exiting.'
         stop
      endif
      nlines=0
!**********************************************************************
!linecount

      !inputs  name_of_input_file number_of_nodes

      CALL getarg(1,inputfile)
      CALL getarg(2,tmp)
      read(tmp, '(I10)')nnodes
      
      !combines the command line with the input filename
      !command: wc -l "input_file" | cut -f1 -d' ' >nlines.dat"
      com = "wc -l "
      mand = "| awk '{print $1'} > nlines.dat"
      command = com//trim(inputfile)//mand

      !remove previous nlines.dat file if it exists
      CALL execute_command_line("rm -f nlines.dat")

      !this command creates a word count file     
      CALL execute_command_line(command)

      !read line count number 
      open(unit=11,file="nlines.dat",action='read',iostat=er)
          if(er/=0) then
             write(6, '(a)')'Could not open input file. Exiting.'
             stop
          endif
       read(11,*,iostat=er) nlines
          if(er>0) then
             write(6,'(a)')'Error reading input file'
             stop
          endif 
      close(11)
      
      !clean up created files
      CALL execute_command_line("rm nlines.dat")

!**********************************************************************
!initialize

      allocate(edge(nnodes))
      do i=1,nnodes
         edge(i)= 0
      enddo
      sumEdge = 0
!**********************************************************************
!main

     !open input file
      open(unit=11,file=inputfile,action='read',iostat=er)
         if(er/=0) then
            write(6, '(a)')'Could not open input file. Exiting.'
            stop
         endif

      !read lines into array
      iloop: do i=1,nlines
         read(11,*,iostat=er)skip, skip, npath, &
                 dist, skip,(hold(j),j=1,npath+1) 
         if(er>0) then
            write(6,'(a)')'Error with input file'
            stop
         else if(er<0) then    !exit loop at end of file
            exit iloop
         else
!            continue 
            do j=1, npath+1
               edge(hold(j)) = edge(hold(j)) + 1
               hold(j)=0
!               write(6,*)k
            enddo
         endif 
      enddo iloop


      do i=1,nnodes
         sumEdge=sumEdge + edge(i)
      enddo

!      write(6,*)sumEdge

      !open output file

      mand = "count.total"
!      command = trim(inputfile)//mand
       open(unit=101,file=mand,position="append", action="write", &
        iostat=er)
         if(er/=0) then
            write(6, '(a)')'Could not open output file. Exiting.'
            stop
         endif
       write(101,'(I6)')sumEdge 
       !To get geodesic index, divide by N*N*Snaps


      deallocate(edge)
      close(11)
      close(101)
      END PROGRAM isoGDCount
