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
real     ( kind=dp) :: total_interest=0, total_payment=0
integer :: io

!---------------------------------------------------------------------------------------
!  Get the file
!---------------------------------------------------------------------------------------

! print prompt
write (*,*) "Please enter the name of a loan terms file: "
! read file name from standard input
read *,FILE_NAME
! echo file name
write (*,*) "You entered ", FILE_NAME
write (*,*)
! attempt to open file
open(READ_UNIT, file=FILE_NAME, status='OLD',iostat=io)

!---------------------------------------------------------------------------------------
!  Check file for errors
!---------------------------------------------------------------------------------------

! if file cannot be opened, exit with error
! priming read
read(READ_UNIT,*,iostat=io) balance, years, rate

!---------------------------------------------------------------------------------------
!  Loop though file by line
!---------------------------------------------------------------------------------------

do while ( io == 0 )

!---------------------------------------------------------------------------------------
!  Print header
!---------------------------------------------------------------------------------------

	! increment and print counter value
	loan_counter = loan_counter + 1
	write (*,'(A6,I2)') "Loan #", loan_counter

	! echo principal, years, and rate

	write (*,'(A18,F12.2)') "Initial Balance: $", balance
	write (*,'(A18,F12.1)') "   Years to Pay:  ", years
	write (*,'(A18,F12.5,A1)') "  Interest Rate:  ", rate, '%'
	write (*,*)

	! convert rate from percentage to number
	rate = rate/100

	! function call to calculate monthly payment
	payment = monthly_payment(years, balance, rate)

	write (*,'(A18,F12.2)') "Monthly Payment: $", payment
	write (*,*)


!---------------------------------------------------------------------------------------
!  Loop through loan by month
!---------------------------------------------------------------------------------------

	! print header
	write (*,'(A10,A20,A20,A20)') "Period","Principal","Interest","Balance"

	! initialize period counter and totals
	period_counter = 0
	total_payment = 0
	total_interest = 0

	! while it is not the last period
	do while (period_counter < years*12)

!---------------------------------------------------------------------------------------
!  Update and print
!---------------------------------------------------------------------------------------

		! increment period counter
		period_counter = period_counter+1

		! function call to update loan
		call update_values()

		! if counter indicates 1st or last 12 periods
		if ( period_counter <=12 .or. period_counter > (years-1)*12 ) then
			! print period, payments towards principal and interest, balance
			write (*,'(I10,F20.2,F20.2,F20.2)') period_counter, payment-interest, interest, balance
			! after period 12, print a newline
			if ( period_counter == 12 ) then
				write (*,*)
			end if
		end if

	! end loop through loan
	enddo

	! output total interest and payments
	write (*,*)
	write (*,'(A16,F12.2)') "Total Interest: ", total_interest
	write (*,'(A16,F12.2)') "Total Payments: ", total_payment
	write (*,*)

	! read next line
	read(READ_UNIT,*,iostat=io) balance, years, rate

! end loop though file
enddo

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
interest = (rate/12)*balance
! add interest to balance
balance = balance + interest
! subtract payment from balance
balance = balance - payment

!update totals
total_payment = total_payment + payment
total_interest = total_interest + interest

end subroutine update_values

!===  FUNCTION  ========================================================================
!         NAME: monthly_payment
!  DESCRIPTION: This function calculates the monthly payment for a loan.
!
!        INPUT: n - the number of years, not periods
!               p - the principal of the loan
!               r - the annual interest rate
!       OUTPUT: monthly_payment
!=======================================================================================

real (kind=dp) function monthly_payment(n, p, r)

implicit none

real (kind=dp) n,p,r 

! calculate monthly payment using formula
monthly_payment = (r/12)*p/(1-(1+(r/12))**(-n*12))

return
end function monthly_payment

!===  END  =============================================================================
end program hwkLoanCalculator

