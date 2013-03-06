	subroutine read_parameter(inputfile,problem,normtype,xlength,ylength,imax,jmax,&
		delx,dely,delt,tau,itermax,eps,omg,gammar,Re,GX,GY,UI,VI,PI,wW,wE,wN,wS,&
		init_UVP_type,delt_output)
        !----------------------------------------------------------------------------
        !read parameter from the inputfile
        !check whether the parameter is legal
        !----------------------------------------------------------------------------
        !variable statement
	character(len=20),intent(in) :: inputfile

	real,intent(out) :: xlength,ylength
	real,intent(out) :: delt
	real,intent(out) :: tau,eps,omg,gammar
	real,intent(out) :: Re,GX,GY,UI,VI,PI
	real,intent(out) :: delx,dely
	real,intent(out) :: delt_output

	integer,intent(out) :: problem,normtype,init_UVP_type
	integer,intent(out) :: wW,wE,wN,wS
	integer,intent(out) :: itermax
	integer,intent(out) :: imax,jmax

	integer :: istat ! variable used to restore the iostat when openning the parameter file
        !---------------------------------------------------------------------------
        !open parameter file and read parameter
	open(1,file=trim(inputfile)//'.dat',status='old',action='read',iostat= istat)
outer:	if (istat /= 0) then
		print *,inputfile//'.dat'
		print *,'open parameter file error, error type:',istat
		stop
	else
	read (1,101) imax,jmax
	read (1,102) itermax
	read (1,103) wW,wE,wN,wS
	read (1,104) delt
	read (1,105) xlength,ylength
	read (1,106) Re
	read (1,107) tau
	read (1,108) gammar
	read (1,109) omg
	read (1,110) eps
	read (1,111) UI,VI,PI
	read (1,112) problem
	read (1,113) normtype
	read (1,114) GX,GY
	read (1,115) init_UVP_type
	read (1,116) delt_output
	close(1)
	end if outer

!-----------------------------------------------------------------------------------
!fmT
101	format(16X,2I6)
102	format(16X,I6)
103	format(16X,4I1)
104	format(16X,F10.4)
105	format(16X,2F6.2)
106	format(16X,F10.0)
107	format(16X,F6.4)
108	format(16X,F6.4)
109	format(16X,F6.4)
110	format(16X,F10.7)
111	format(16X,3F10.4)
112	format(16X,I2)
113	format(16X,I1)
114	format(16X,2F10.4)
115	format(16X,I1)
116	format(16X,F6.2)
!-----------------------------------------------------------------------------------------------
!calculate delx,dely
	delx = xlength/imax
	dely = ylength/jmax
!-----------------------------------------------------------------------------------------------

	end subroutine read_parameter
