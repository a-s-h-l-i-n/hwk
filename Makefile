hwkLoanCalculator: hwkLoanCalculator.o 
	gfortran -o hwkLoanCalculator hwkLoanCalculator.o

hwkLoanCalculator.o: hwkLoanCalculator.f90
	gfortran -c hwkLoanCalculator.f90


