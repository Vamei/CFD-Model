	subroutine write_result(outputfile,imax,jmax,U,V,P,t,n_output)
        !----------------------------------------------------------------------
        ! write U,V,P to the output file
        ! The output file is ASCII file
        ! Future: possible write to NETCDF file
        !----------------------------------------------------------------------
        !variable statement
	character(len=20),intent(in) :: outputfile
	integer,intent(in) :: imax,jmax
	integer,intent(in) :: n_output
	real,intent(in) :: t
	real,dimension(0:imax+1,0:jmax+1),intent(in) :: U,V,P
	integer :: j
	integer :: istat
	character(len=2) :: no

        !------------------------------------------------------------------------
	write(no,'(I2)') n_output
        !transform integer n_output to string no
        !------------------------------------------------------------------------
	open(1,file=trim(outputfile)//'_'//trim(no)//'_t.dat',status='replace', &
                action='write',iostat=istat)
	if (istat /= 0) then
	  print *,'open t file error, error type:',istat
	  stop
	else
	  write (1,151) t
	  close(1)
	end if

       !-------------------------------------------------------------------------
	open(1,file=trim(outputfile)//'_'//trim(no)//'_U.dat',status='replace', &
                action='write',iostat=istat)
	if (istat /= 0) then
	  print *,'open U file error, error type:',istat
	  stop
	else
	  do j=0,jmax+1
	    write (1,*) U(0:imax+1,j)
	  end do 
	  close(1)
	end if

        !------------------------------------------------------------------------
	open(1,file=trim(outputfile)//'_'//trim(no)//'_V.dat',status='replace', &
                action='write',iostat=istat)
	if (istat /= 0) then
	  print *,'open V file error, error type:',istat
	  stop
	else
	  do j=0,jmax+1
	    write (1,*) V(0:imax+1,j)
	  end do 
	  close(1)
	end if

        !------------------------------------------------------------------------
	open(1,file=trim(outputfile)//'_'//trim(no)//'_P.dat',status='replace', &
                action='write',iostat=istat)
	if (istat /= 0) then
	  print *,'open P file error, error type:',istat
	  stop
	else
	  do j=0,jmax+1
	    write (1,*) P(0:imax+1,j)
	  end do 
	  close(1)
	end if

!----------------------------------------------------------------------------------
151	format('t:',F12.8)
	end subroutine write_result
