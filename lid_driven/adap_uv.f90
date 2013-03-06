	subroutine adap_uv(U,V,F,G,P,imax,jmax,delt,delx,dely)
        !The new velocities are calculated according to (5),(6)
        !----------------------------------------------------------
        !variable declaration
	integer,intent(in) :: imax,jmax
	integer :: i,j !iteration control variables
	real,intent(in) :: delt,delx,dely
	real,dimension(0:imax+1,0:jmax+1),intent(out) :: U,V
	real,dimension(0:imax+1,0:jmax+1),intent(in) :: F,G,P
        !----------------------------------------------------------
        !executive part	
	do i=1,imax-1
	  do j=1,jmax
	    U(i,j) = F(i,j) - (delt/delx)*(P(i+1,j) - P(i,j))
	  end do
	end do

	do i=1,imax
	  do j=1,jmax-1
	    V(i,j) = G(i,j) - (delt/dely)*(P(i,j+1) - P(i,j)) 
	  end do
	end do
	
        end subroutine adap_uv
