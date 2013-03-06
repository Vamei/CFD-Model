	subroutine comp_delt(U,V,delt,imax,jmax,delx,dely,Re,tau)
        !the stepsize delt for the next time step is calculated according to (1). 
        !In case of negative tau the stepsize read in read_parameter is to be used
	real,intent(in) :: delx,dely,Re,tau
	real,intent(inout) :: delt
	integer,intent(in) :: imax,jmax
	real,dimension(0:imax+1,0:jmax+1),intent(in) :: U,V

	if (tau >= 0) then  
        !calculate delt according to (3.50)
	  delt = tau*                                   &
                  min(Re/((1./delx**2 + 1./dely**2)*2), &
                      delx/maxval(abs(U)),              &
                      dely/maxval(abs(V)))
	end if

	end subroutine comp_delt

