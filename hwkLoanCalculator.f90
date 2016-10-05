!=======================================================================================
!         FILE: hwkLoanCalculator.f90
!       AUTHOR: Ashlin Harris
!         DATE: 4 October 2016
!  DESCRIPTION: This program calculates the payment plan for a loan.
!
!        INPUT: A filename is providede by the user. File may contain one loan per line,
!               and values must be separated by white space. Format:
!               <Loan Principal>  <Length in Years>  <Annual Interest Rate>
!       OUTPUT: A table of payments is printed to the screen, along with the total
!               interest and grand total of payments
!=======================================================================================

program hwkLoanCalculator

implicit none

!---------------------------------------------------------------------------------------
!  Declare variables
!---------------------------------------------------------------------------------------

integer, parameter  :: dp = selected_real_kind(15,307)
integer, parameter  :: WRITE_UNIT = 16
integer, parameter  :: READ_UNIT  = 15
character (len=200)   :: FILE_NAME
integer :: loan_counter = 0, period_counter=0
real     ( kind=dp) :: balance, years, rate, interest, payment
integer :: i

!---------------------------------------------------------------------------------------
!  Get the file
!---------------------------------------------------------------------------------------

! print prompt
write (*,*) "Please enter the name of a loan terms file: "
! read file name from standard input
read *,FILE_NAME
! echo file name
write (*,*) "You entered ", FILE_NAME
! attempt to open file
!open(unit=READ_UNIT, file=FILE_NAME)

!---------------------------------------------------------------------------------------
!  Check file for errors
!---------------------------------------------------------------------------------------

! if file cannot be opened, exit with error

!---------------------------------------------------------------------------------------
!  Loop though file by line
!---------------------------------------------------------------------------------------

! priming read
!read(READ_UNIT,*) balance, years, rate
balance = 100
years = 30
rate = 0.0001
! while there is a line
!do while ()

!---------------------------------------------------------------------------------------
!  Check line for errors
!---------------------------------------------------------------------------------------

	! if line is incorrectly formatted, continue
	! extract principal, years, and rate from line

!---------------------------------------------------------------------------------------
!  Print header
!---------------------------------------------------------------------------------------

	! increment and print counter value
	period_counter = period_counter + 1
	! echo principal, years, and rate

	! function call to calculate monthly payment
	i = monthly_payment()

!---------------------------------------------------------------------------------------
!  Loop through loan by month
!---------------------------------------------------------------------------------------

	! initialize period counter
	! print header
	! principal is the initial balance

	! while the balance is greater than 0

!---------------------------------------------------------------------------------------
!  Update and print
!---------------------------------------------------------------------------------------

		! increment period counter

		! function call to update loan
		call update_values()

		! if counter indicates 1st of last 12 periods
			! print periodi, payments towards principal and interest, balance
			!write (*,'(f15.7)') something

	! end loop through loan

	! read next line

! end loop though file
!enddo

close(READ_UNIT)
close(WRITE_UNIT)

contains

!===  SUBROUTINE  ======================================================================
!         NAME: update_values
!  DESCRIPTION: This subroutine updates values associated with the loan each month.
!
!        INPUT:     rate - the yearly interest rate
!       OUTPUT:  balance - the loan amount left to be paid
!               interest - the interest accrued by the balance this month
!=======================================================================================

subroutine update_values()!update_values(rate, balance, interest)

implicit none

! calculate interest on balance
interest = rate*balance
! add interest to balance
balance = balance + interest
! subtract payment from balance
balance = balance - payment

end subroutine update_values

!===  FUNCTION  ========================================================================
!         NAME: monthly_payment
!  DESCRIPTION: This function calculates the monthly payment for a loan.
!
!        INPUT: n - the number of periods
!               p - the principal of the loan
!               r - the annual interest rate
!               t - the time in years to pay the loan
!       OUTPUT: monthly_payment
!=======================================================================================

integer function monthly_payment()
!monthly_payment(n, p, r, t)

implicit none

! calculate monthly payment using formula
!monthly_payment=rp/(1-(1+r)**(-n))
monthly_payment = 4

return
end function monthly_payment

!===  END  =============================================================================
end program hwkLoanCalculator

