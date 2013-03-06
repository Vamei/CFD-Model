	subroutine setbcond(U,V,P,imax,jmax,wW,wE,wN,wS)
!======================================================================================
!The boundary values for the arrays U and V are set 
!According to boundary parameters wW,wE,wN,wS 
!-------------------------------------------------------------------------------------
!wW,wE,wN,wS:
!1. no-slip
!2. free-slip
!3. outflow
!4. periodic: wW == wE == 4: x-direction periodic; wN == wS == 4: y-direction periodic
!--------------------------------------------------------------------------------------

!variable statement
	integer,intent(in) :: imax,jmax,wW,wE,wN,wS
	real,dimension(0:imax+1,0:jmax+1),intent(inout) :: U,V,P
	integer :: i,j !iteration control variables

!change U,V boundary values according to the values of wW,wE,wN,wS
!P will be changed only in periodic boundary condition
	
	select case (wW)
		case (1)
			do j=1,jmax
			U(0,j) = 0
			V(0,j) = -V(1,j)
			end do
		case (2)
			do j=1,jmax
			U(0,j) = 0
			V(0,j) = V(1,j)
			end do
		case (3)
			do j=1,jmax
			U(0,j) = U(1,j)
			V(0,j) = V(1,j)
			end do
		case (4)
			do j=1,jmax
			U(0,j) = U(imax-1,j)
			V(0,j) = V(imax-1,j)
			U(imax,j) = U(1,j)
			V(1,j) = V(imax,j)
			V(imax+1,j) = V(2,j)
			P(1,j) = P(imax,j)
			end do
	end select
	
	select case (wE)
		case (1)
			do j=1,jmax
			U(imax,j) = 0
			V(imax+1,j) = -V(imax,j)
			end do
		case (2)
			do j=1,jmax
			U(imax,j) = 0
			V(imax+1,j) = V(imax,j)
			end do
		case (3)
			do j=1,jmax
			U(imax,j) = U(imax-1,j)
			V(imax+1,j) = V(imax,j)
			end do
		case (4)
	end select

	select case (wS)
		case (1)
			do i=1,imax
			V(i,0) = 0
			U(i,0) = - U(i,1)
			end do
		case (2)
			do i=1,imax
			V(i,0) = 0
			U(i,0) = U(i,1)
			end do
		case (3)
			do i=1,imax
			U(i,0) = U(i,1)
			V(i,0) = V(i,1)
			end do
		case (4)
			do i=1,imax
			V(i,0) = V(i,jmax-1)
			V(i,jmax) = V(i,1)
			P(i,1) = P(i,jmax)
			U(i,0) = U(i,jmax -1)
			U(i,1) = U(i,jmax)
			U(i,jmax+1) = U(i,2)
			end do
	end select

	select case (wN)
		case (1)
			do i=1,imax
			V(i,jmax) = 0
			U(i,jmax+1) = - U(i,jmax)
			end do
		case (2)
			do i=1,imax
			V(i,jmax) = 0
			U(i,jmax+1) = U(i,jmax)
			end do
		case (3)
			do i=1,imax
			U(i,jmax+1) = U(i,jmax)
			V(i,jmax) = V(i,jmax-1)
			end do
		case (4)
	end select
	
	end subroutine setbcond
