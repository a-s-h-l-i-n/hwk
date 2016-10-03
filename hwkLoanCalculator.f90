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

!---------------------------------------------------------------------------------------
!  Get the file
!---------------------------------------------------------------------------------------

! print prompt
! read file name from standard input
! echo file name
! attempt to open file

!---------------------------------------------------------------------------------------
!  Check file for errors
!---------------------------------------------------------------------------------------

! if file does not exist, exit with error
! if file cannot be opened, exit with error

!---------------------------------------------------------------------------------------
!  Loop though file by line
!---------------------------------------------------------------------------------------

! initialize counter

! priming read

! while there is a line

!---------------------------------------------------------------------------------------
!  Check line for errors
!---------------------------------------------------------------------------------------

	! if line is incorrectly formatted, continue
	! extract principal, years, and rate from line

!---------------------------------------------------------------------------------------
!  Print header
!---------------------------------------------------------------------------------------

	! increment and print counter value
	! echo principal, years, and rate

	! function call to calculate monthly payment

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

		! if counter indicates 1st of last 12 periods
			! print periodi, payments towards principal and interest, balance

	! end loop through loan

	! read next line

! end loop though file

! close file

! include statement

!===  FUNCTION  ========================================================================
!         NAME: monthly_payment
!  DESCRIPTION: This program calculates the monthly payment for a loan.
!
!        INPUT: p - the principal of the loan
!               t - the time in years to pay the loan
!               r - the annual interest rate
!       OUTPUT: monthly_payment
!=======================================================================================

! begin function
! calculate monthly payment using formula
! end function

!===  SUBROUTINE  ======================================================================
!         NAME: monthly_update
!  DESCRIPTION: This program calculates the payment plan for a loan.
!
!        INPUT: rate - the yearly interest rate
!       OUTPUT: balance - the loan amount left to be paid
! interest - the interest accrued by the balance this month
!=======================================================================================

! calculate interest on balance
! add interest to balance
! principal portion: subtract interest from payment, not its own variable
! subtract payment from balance

end program hwkLoanCalculator
