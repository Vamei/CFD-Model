	program main
!------------------------------------------------------------------------------------------------------------------------------------
!main program
!purpose: this program is used for the numerical simulation of lid_driven cavity flow
!incompressible
!Reference the book <Numerical Simulation in Fluid Dynamics: a Practical Introduction> by Michael Griebel et.al. 
!------------------------------------------------------------------------------------------------------------------------------------
!variable dictionary
!n: n th iteration
!t: present time 
!n_output: Nth output of the result

!imax,jmax: domain size
!wW,wE,wN,wS: boundary style, [1,2,3,4] 1:free-slip conditions 2:no-slip condition 3:outflow condition 4:periodic boundary condition

!t_end: end time
!delt_output: delt between two outputs
!xlength,ylength: length of the domain in x/y direction
!delt: stepsize of time
!delx,dely: length of one cell in x/y direction; delx = xlength/imax; dely = ylength/jmax
!Re: Renolds number
!tau: safety factor for time step size control tau
!gammar: upwind differncing factor gamma
!omg: relaxation parameter omg for SOR iteration
!res: norm of pressure equation residual
!eps: stopping tolerance eps for pressure iteration
!UI,VI,PI: initial data for velocities and pressure
!GX,GY: body force gx and gy (e.g., gravity)
!itermax: maximal number of pressure iterations in one time step

!normtype: indicate the way adopted to calculate the norm of R 
!problem: this variable allows further flow-specific quantities,such as inflow velocity of internal obstacles, to be specified depending on the problem type

!init_UVP_type: this variable indicate the UVP init type; 
!----init_UVP_type == 1: initiate U,V,P according to UI,VI,PI, initiate t == 0
!----init_UVP_type == 2: initiate U,V,P,t according to previously recorded result

!U,V: velocity field
!P: pressure field
!RHS: right-hand side for pressure iteration
!F,G: F,G field

!-------------------------------------------------------------------------------------------------

!variable statement and initiation
	integer :: n             ! iteration recorder
	integer :: n_output 
!-------------------------------------------------------------------------------------------------
	integer :: imax,jmax
	integer :: itermax
	integer :: wW,wE,wN,wS
	integer :: normtype
	integer :: problem
	integer :: init_UVP_type
!-------------------------------------------------------------------------------------------------
	real :: t         ! time recorder
	real :: t_output  ! output time recorder
!-------------------------------------------------------------------------------------------------
	real :: t_end
	real :: delt,delx,dely
	real :: xlength,ylength
	real :: Re,tau,gammar,omg,res,eps
	real :: UI,VI,PI
	real :: GX,GY
	real :: delt_output
!-------------------------------------------------------------------------------------------------
	real :: norm      !declare the function norm() 'norm.f90'
!-------------------------------------------------------------------------------------------------
	real,allocatable :: U(:,:),V(:,:)
	real,allocatable :: P(:,:),RHS(:,:)
	real,allocatable :: F(:,:),G(:,:)
	
	real,allocatable :: UL(:,:),VL(:,:),PL(:,:) !record the result of last iteration
	
	character(len=20) :: inputfile
	character(len=20) :: outputfile
	character(len=20) :: initfile
!----------------------------------------------------------------------------------------------------
!specify the name of inputfile; initiate parameters
!--	print *,'pls input the name of the inputfile'
!--	read (*,*) inputfile
	inputfile = 'para_lid_driven_flow'

	call read_parameter(inputfile,problem,normtype,xlength,ylength, &
		imax,jmax,delx,dely,delt,tau,itermax,eps,omg,&
		gammar,Re,GX,GY,UI,VI,PI,wW,wE,wN,wS,init_UVP_type,delt_output)

	print *,'problem    :',problem
	print *,'normtype   :',normtype
	print *,'xlength,ylength:',xlength,ylength
	print *,'imax,jmax  :',imax,jmax
	print *,'delx,dely  :',delx,dely
	print *,'delt       :',delt
	print *,'tau,eps,omg:',tau,eps,omg
	print *,'gammar,Re  :',gammar,Re
	print *,'itermax    :',itermax
	print *,'GX,GY      :',GX,GY
	print *,'UI,VI,PI   :',UI,VI,PI
	print *,'wW,wE,wN,wS:',wW,wE,wN,wS
	print *,'init_UVP_type:',init_UVP_type
	print *,'delt_output:',delt_output
!------------------------------------------------------------------------------------------------
!specify outputfile
!---	print *,'pls input the name of the outputfile'
!---	read (*,*) outputfile
	outputfile = 'result'

!-------------------------------------------------------------------------------------------------
!initiate the recorders 
	n = 0
	n_output = 1
	t_output = delt_output
!-------------------------------------------------------------------------------------------------
!allocate memory to the field arrays
	allocate(U(0:imax+1,0:jmax+1))
	allocate(V(0:imax+1,0:jmax+1))
	allocate(P(0:imax+1,0:jmax+1))
	allocate(RHS(0:imax+1,0:jmax+1))
	allocate(F(0:imax+1,0:jmax+1))
	allocate(G(0:imax+1,0:jmax+1))

	allocate(UL(0:imax+1,0:jmax+1))
	allocate(VL(0:imax+1,0:jmax+1))
	allocate(PL(0:imax+1,0:jmax+1))
	
!------------------------------------------------------------------------
!initiate U,V,P,t

	if (init_UVP_type == 2) then
	print *,'input the name of the initfile:'
	read (*,*) initfile
	end if
	call init_uvp(U,V,P,t,imax,jmax,UI,VI,PI,init_UVP_type,initfile)
	print *,'present t:',t

!-------------------------------------------------------------------------
!t_end
	print *,'input t_end'
	read (*,*) t_end
!-------------------------------------------------------------------------

	do while (t < t_end)

		UL = U
		VL = V
		PL = P

		call comp_delt(U,V,delt,imax,jmax,delx,dely,Re,tau)
		call setbcond(U,V,P,imax,jmax,wW,wE,wN,wS)
	if (problem /= 0) then
		call setspecbcond(U,V,imax,jmax,problem)
	endif
		call comp_fg(U,V,F,G,imax,jmax,delt,delx,dely,GX,GY,gammar,Re)
		

		call comp_rhs(F,G,RHS,imax,jmax,delt,delx,dely)
	
		call poisson(P,RHS,imax,jmax,delx,dely,eps,&
		  itermax,omg,res,normtype)

		call adap_uv(U,V,F,G,P,imax,jmax,delt,delx,dely)

!************************************************************************************************
		
!improve plan: stable check
!purpose: want to insert a stable check subroutine here. if the norm of UL-U < threshold, then end the iteration
!problem: the value of threshold
!		print *,'norm_uchange',norm(UL(1:imax,1:jmax)-U(1:imax,1:jmax),imax,jmax,1)
!		print *,'norm_vchange',norm(VL(1:imax,1:jmax)-V(1:imax,1:jmax),imax,jmax,1)
!		print *,'norm_pchange',norm(PL(1:imax,1:jmax)-P(1:imax,1:jmax),imax,jmax,1)

!*************************************************************************************************
!monitor the process

		t = t + delt
		n = n + 1
		print *,'t,n:',t,n
!------------------------------------------------------------------------------------------
!output control
		if ( (t - delt) < t_output .and. t >= t_output) then
		call write_result(outputfile,imax,jmax,U,V,P,t,n_output)
		t_output = t + delt_output
		n_output = n_output + 1
		end if
!------------------------------------------------------------------------------------------
	end do


	print *,'final result:'
	print *,'t',t
	print *,'n',n
!record the results
!----------------------------------------------------------------------------------------------
!write U,V,P to outputfile
	call write_result(outputfile,imax,jmax,U,V,P,t,n_output)
!---------------------------------------------------------------------------------------------------
	end program main

include 'init_uvp.f90'
include 'comp_delt.f90'
include 'setbcond.f90'
include 'setspecbcond.f90'
include 'comp_fg.f90'
include 'comp_rhs.f90'
include 'poisson.f90'
include 'adap_uv.f90'
include 'write_result.f90'
include 'read_parameter.f90'
include 'norm.f90'
