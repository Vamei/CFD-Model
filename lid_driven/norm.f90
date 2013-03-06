        !function norm()
        !purpose: calculate the norm of a 2D array
        !-----------------------------------------------------------------
	real function norm(r,imax,jmax,normtype)

	integer,intent(in) :: imax,jmax
	integer,intent(in) :: normtype
	real,dimension(1:imax,1:jmax),intent(in) :: r
	real,dimension(1:imax,1:jmax) :: r1
	real :: sum_r1
	if (normtype == 1) then
	  r1 = r**2
	  sum_r1 = sum(r1)
	  norm = sqrt(sum_r1/(imax*jmax))
	end if
	

	if (normtype == 2) then
	  r1 = abs(r)
	  norm = maxval(r1)
	end if
	end function norm
