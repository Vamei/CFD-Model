	subroutine poisson(P,RHS,imax,jmax,delx,dely,eps,itermax,omg,res,normtype)
        !SOR iteration for the pressure Poisson equation
        !--------------------------------------------------------------------------------
        !variable declaration
	integer,intent(in) :: imax,jmax,itermax,normtype
	real,intent(in) :: delx,dely,res,eps,omg
	integer :: i,j,it !iteration control variables
	real :: norm  !result returned from norm
	real :: norm_r
	real,dimension(0:imax+1,0:jmax+1),intent(in) :: RHS
	real,dimension(0:imax+1,0:jmax+1),intent(inout) :: P
	real,dimension(1:imax,1:jmax) :: r !residual array
        !---------------------------------------------------------------------
        !executive part
outit:	do it = 1,itermax

          !reset boundary before each iteration
	  do j = 1,jmax
            P(0,j) = P(1,j)
            P(imax+1,j) = P(imax,j)
	  enddo
	  do i = 1,imax
            P(i,0) =  P(i,1)
            P(i,jmax+1) = P(i,jmax)
	  end do

          !iteration to calculate the P in the domain
middle:	  do i = 1,imax
inner:    do j = 1,jmax
            P(i,j) = (1 - omg)*P(i,j) +               &
                     (omg/(2./delx**2 + 2./dely**2))  &
                     *((P(i+1,j) + P(i-1,j))/delx**2  &
                      + (P(i,j+1) + P(i,j-1))/dely**2 - RHS(i,j))
          enddo inner
	  end do middle

          !calclate the norm of the residual
middle1:  do i = 1,imax
inner1:	    do j = 1,jmax
	      r(i,j) = ((P(i+1,j) - P(i,j)) - (P(i,j)-P(i-1,j)))/delx**2     &
	               + ((P(i,j+1) - P(i,j)) - (P(i,j) - P(i,j-1)))/dely**2 &
                       - RHS(i,j)
	    enddo inner1
	  end do middle1
	
	  norm_r = norm(r,imax,jmax,normtype)
	  if (norm_r < eps) exit outit

	end do outit
	end subroutine poisson

!============================================================
!function eb_para() wb_para() nb_para() sb_para()
!------------------------------------------------------------
	integer function eb_para(i)
	integer,intent(in) :: i
	if (i == 1) then 
	  eb_para = 0
	else
	  eb_para = 1
	end if
	end function eb_para

!--------------------------------------------------------------
	integer function wb_para(i)
	integer,intent(in) :: i
	if (i == imax) then
	  wb_para = 0
	else
	  wb_para = 1
	end if
	end function wb_para

!---------------------------------------------------------------
	integer function sb_para(j)
	integer,intent(in) :: j
	if (j == 1) then
	  sb_para = 0
	else
	  sb_para = 1
	end if
	end function sb_para

!----------------------------------------------------------------
	integer function nb_para(j)
	integer,intent(in) :: j
	if (j == jmax) then
	  nb_para = 0
	else
	  nb_para = 1
	end if
	end function nb_para
