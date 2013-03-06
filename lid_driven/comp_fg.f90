	subroutine comp_fg(U,V,F,G,imax,jmax,delt,delx,dely,GX,GY,gammar,Re)
        !Computation of F and G according to (2) and (3). 
        !Boundary Values could be set arbitrarily.
        !Boundary Values will only be used in SOR solution of Poisson Equation.
        !-----------------------------------------------------------------------------
        !variable declaration
	integer,intent(in) :: imax,jmax
	real,intent(in) :: delt,delx,dely,GX,GY,gammar,Re
	real,dimension(0:imax+1,0:jmax+1),intent(in) :: U,V
	real,dimension(0:imax+1,0:jmax+1),intent(out) :: F,G
	
	real :: uu_diffx,uv_diffy,u_2diffx,u_2diffy,v_2diffx,v_2diffy,uv_diffx,vv_diffy
	integer :: i,j   ! iteration control variables
!------------------------------------------------------------------------------------
!executive part

outer:	do i=1,imax-1
inner:	  do j = 1,jmax
            !2diffx: second-order derivative about x diff 
            !diffx: first-order derivative about x	
	    u_2diffx = (U(i+1,j) - 2*U(i,j) + U(i-1,j))/delx**2
	    u_2diffy = (U(i,j+1) - 2*U(i,j) + U(i,j-1))/dely**2

	    uu_diffx = (1./delx)*(((U(i,j) + U(i+1,j))/2.)**2-((U(i-1,j) + U(i,j))/2.)**2) &
	               + (gammar/delx)*(abs(U(i,j)                                         &
                                       + U(i+1,j))*(U(i,j) - U(i+1,j))/4.                  &
                                       - abs(U(i-1,j)                                      &
                                       + U(i,j))*(U(i-1,j) - U(i,j))/4.)

	    uv_diffy = (1./dely)*((V(i,j) + V(i+1,j))*(U(i,j) + U(i,j+1))/4.               &
                                 - (V(i,j-1) + V(i+1,j-1))*(U(i,j-1) + U(i,j))/4.)         &
	               + (gammar/dely)*(abs(V(i,j) + V(i+1,j))*(U(i,j) - U(i,j+1))/4.      & 
                                       - abs(V(i,j-1)                                      & 
                                            + V(i+1,j-1))*(U(i,j-1) - U(i,j))/4.)

	    F(i,j) = U(i,j) + delt*((1./Re)*(u_2diffx + u_2diffy) - uu_diffx - uv_diffy + GX)
	  end do inner
	end do outer

outer1:	do i=1,imax 
inner1:	  do j=1,jmax-1
	    v_2diffx = (V(i+1,j) - 2*V(i,j) + V(i-1,j))/delx**2
	    v_2diffy = (V(i,j+1) - 2*V(i,j) + V(i,j-1))/dely**2

	    vv_diffy = (1./dely)*(((V(i,j) + V(i,j+1))/2.)**2 - ((V(i,j-1) + V(i,j))/2.)**2) &
	               + (gammar/dely)*(abs(V(i,j)                                           &  
                                       + V(i,j+1))*(V(i,j) - V(i,j+1))/4.                    &
                                       - abs(V(i,j-1)                                        &
                                       + V(i,j))*(V(i,j-1) - V(i,j))/4.)
	    uv_diffx = (1./delx)*((U(i,j) + U(i,j+1))*(V(i,j) + V(i+1,j))/4.                 &
                                 - (U(i-1,j) + U(i-1,j+1))*(V(i-1,j) + V(i,j))/4.)           &
	               + (gammar/delx)*(abs(U(i,j) + U(i,j+1))*(V(i,j) - V(i+1,j))/4.        &
                                       - abs(U(i-1,j)                                        &
                                            + U(i-1,j+1))*(V(i-1,j) - V(i,j))/4.)
	    G(i,j) = V(i,j) + delt*((1./Re)*(v_2diffx + v_2diffy) - uv_diffx - vv_diffy + GY)
	  enddo inner1
	end do outer1

        !boundary F,G set
	do j=1,jmax
	  F(0,j) = U(0,j)
	  F(imax,j) = U(imax,j)
	end do

	do i=1,imax
	  G(i,0) =  V(i,0)
	  G(i,jmax) = V(i,jmax)
	end do

end subroutine comp_fg
