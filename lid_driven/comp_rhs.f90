	subroutine comp_rhs(F,G,RHS,imax,jmax,delt,delx,dely)
        !Computation of the right-side of the pressure equation (4)

        integer,intent(in) :: imax,jmax
	real,intent(in) :: delt,delx,dely
	real,dimension(0:imax+1,0:jmax+1),intent(in) :: F,G
	real,dimension(0:imax+1,0:jmax+1),intent(out) :: RHS

outer:	do i=1,imax
inner:	  do j=1,jmax
            RHS(i,j) = (1./delt)*((F(i,j) - F(i-1,j))/delx  &
                        + (G(i,j) - G(i,j-1))/dely)
	  enddo inner
	enddo outer
	end subroutine comp_rhs
