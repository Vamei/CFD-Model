	subroutine setspecbcond(U,V,imax,jmax,problem)
        !specific boundary conditions(such as inflowe boundary conditions) 
        !of the problem specified by the parameter problem can be set.
        !-------------------------------------------------------------
        !variable declaration
	integer,intent(in) :: imax,jmax
	integer,intent(in) :: problem
	real,dimension(0:imax+1,0:jmax+1),intent(inout) :: U,V

	integer :: i  ! iteration control varialble

	real :: u_lid ! velocity of upper lid in Problem 1
        !-------------------------------------------------------------	
        !set special boundary conditions according to variable problem
	select case (problem)
	  case(1)
            !-----------------------------------------------------
            !Problem 1
            !Lid-Driven Cavity
	    u_lid = 1
	    do i=1,imax
	      U(i,jmax+1) = 2.0*u_lid - U(i,jmax) 
	    end do
          !-----------------------------------------------------
          ! Not implemented yet
	  case(2:)
	    print *, 'variable problem illegal'
            print *, 'no special condition set in setspecbcond'
	    stop
	end select	
	end subroutine setspecbcond
