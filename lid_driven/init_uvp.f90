	subroutine init_uvp(U,V,P,t,imax,jmax,UI,VI,PI,init_UVP_type,initfile)
        !the arrays U,V,P are initialized to the constant values UI,VI,and PI
        !or to the values read from initfile
	integer,intent(in) :: imax,jmax
	integer,intent(in) :: init_UVP_type
	real,intent(in) :: UI,VI,PI
	real,dimension(0:imax+1,0:jmax+1),intent(out) :: U,V,P
	real,intent(out) :: t
	character(len=20),intent(in) :: initfile
	
	integer :: j
	integer :: istat

        !-----------------------------------------------------------------------------------
        !initialize U,V,P with UI,VI,PI; t = 0.0
	if ( init_UVP_type == 1 ) then
          U = UI
          V = VI
          P = PI
          t = 0.0
	end if

        !------------------------------------------------------------------------------
        !initialize U,V,P,t with previously recorded files
	if ( init_UVP_type == 2 ) then
	  open(1,file=trim(initfile)//'_t.dat',status='old', &
              action='read',iostat=istat)
	  if (istat /= 0) then
	    print *,'open t file error, error type:',istat
	    stop
	  else
	    read (1,151) t
	    close(1)
	  end if

          !-------------------------------------------------------------------------------
	  open(1,file=trim(initfile)//'_U.dat',status='old', &
              action='read',iostat=istat)
	  if (istat /= 0) then
	    print *,'open U file error, error type:',istat
	    stop
	  else
	    do j=0,jmax+1
	      read (1,*) U(0:imax+1,j)
	    end do 
	    close(1)
	  end if

          !-------------------------------------------------------------------------------
	  open(1,file=trim(initfile)//'_V.dat',status='old', &
              action='read',iostat=istat)
	  if (istat /= 0) then
	    print *,'open V file error, error type:',istat
	    stop
	  else
	    do j=0,jmax+1
	      read (1,*) V(0:imax+1,j)
	    end do 
	    close(1)
	  end if

          !--------------------------------------------------------------------------------
	  open(1,file=trim(initfile)//'_P.dat',status='old', &
              action='read',iostat=istat)
	  if (istat /= 0) then
	    print *,'open P file error, error type:',istat
	    stop
	  else
	    do j=0,jmax+1
	      read (1,*) P(0:imax+1,j)
	    end do 
	    close(1)
	  end if
	endif
!----------------------------------------------------------------------------------------------
151	format('t:',F12.8)
	end subroutine init_uvp
