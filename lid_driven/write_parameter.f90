	program write_parameter
        !-------------------------------------------------------------------------------------
        !format-write parameter to inputfile
        !this is a dirty way in solving the problem
        !-------------------------------------------------------------------------------------
        !variable statement
	character(len=20) :: inputfile

	real :: xlength,ylength
	real :: delt,delt_output
	real :: tau,res,eps,omg,gammar
	real :: Re,GX,GY,UI,VI,PI
	real :: delx,dely

	integer :: problem,normtype,init_UVP_type
	integer :: wW,wE,wN,wS
	integer :: itermax
	integer :: imax,jmax
	integer :: istat
!----------------------------------------------------------------------------------------------
	write(*,*) 'parameter file name:'
	read (*,*) inputfile

!open parameter file and read parameter
	open (1,file=inputfile,status='replace',action='write',iostat=istat)
	if (istat /= 0) then
		print *,'open parameter file error, error type:',istat
		stop
	else
	print *,'input imax'
	read (*,*) imax
	print *,'input jmax'
	read (*,*) jmax
	print *,'input itermax'
	read (*,*) itermax
	print *,'input wW'
	read (*,*) wW
	print *,'input wE'
	read (*,*) wE
	print *,'input wN'
	read (*,*) wN
	print *,'input wS'
	read (*,*) wS
	print *,'input delt'
	read (*,*) delt
	print *,'input xlength'
	read (*,*) xlength
	print *,'input ylength'
	read (*,*) ylength
	print *,'input Re'
	read (*,*) Re
	print *,'input tau'
	read (*,*) tau
	print *,'input gammar'
	read (*,*) gammar
	print *,'input omg'
	read (*,*) omg
	print *,'input eps'
	read (*,*) eps
	print *,'input UI'
	read (*,*) UI
	print *,'input VI'
	read (*,*) VI
	print *,'input PI'
	read (*,*) PI
	print *,'input problem'
	read (*,*) problem
	print *,'input normtype'
	read (*,*) normtype
	print *,'input GX'
	read (*,*) GX
	print *,'input GY'
	read (*,*) GY
	print *,'input init_UVP_type'
	read (*,*) init_UVP_type

	write (1,101) imax,jmax
	write (1,102) itermax
	write (1,103) wW,wE,wN,wS
	write (1,104) delt
	write (1,105) xlength,ylength
	write (1,106) Re
	write (1,107) tau
	write (1,108) gammar
	write (1,109) omg
	write (1,110) eps
	write (1,111) UI,VI,PI
	write (1,112) problem
	write (1,113) normtype
	write (1,114) GX,GY
	write (1,115) init_UVP_type
	write (1,116) delt_output

	close(1)
	end if

!-----------------------------------------------------------------------------------
!fmT
101	format('imax,jmax      :',2I6)
102	format('itermax        :',I6)
103	format('wW,wE,wN,wS    :',4I1)
104	format('delt           :',F10.4)
105	format('xlength,ylength:',2F6.2)
106	format('Re             :',F10.0)
107	format('tau            :',F6.4)
108	format('gammar         :',F6.4)
109	format('omg            :',F6.4)
110	format('eps            :',F10.7)
111	format('UI,VI,PI       :',3F10.4)
112	format('problem        :',I2)
113	format('normtype       :',I1)
114	format('GX,GY          :',2F10.4)
115	format('init_UVP_type  :',I1)
116	format('delt_output    :',F6.2)
!-----------------------------------------------------------------------------------------------

	end program write_parameter
