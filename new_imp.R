mean.imp <- function(csv){
	library("mice")

	#missing.data <- read.table(csv, header = TRUE, sep= ",", na.strings = c(" "))
	missing.data <- read.csv(csv, na.strings = c(" ","  ","  \n"), colClasses=c('numeric'))

	# check if all values are the same save the NA values (this fixes a small bug)
	missing.data <- apply(missing.data, 2, .same.vals)

	# perform the mean imputation
	imp <- mice(missing.data, method = "mean", m = 1, maxit = 1, printFlag = FALSE)

	# fill in the imputed values
	filled.data <- complete(imp)

	# perform the emergency mean on the missing columns
	if( length( filled.data[is.na(filled.data)] ) > 0){
		filled.data <- apply(filled.data, 2, .impute.wrapper) 
	}

	# if the program still fails throw an error!
	if( length( filled.data[is.na(filled.data)] ) > 0) stop("imputation failed: missing data still present")

	write.csv(filled.data, file=paste("mean_imputation", csv, sep='_'), row.names = FALSE)
	file.remove(csv)
}

# perform multiple imputation on the missing data csv file
# This will output several frames!
mult.imp <- function(csv, num_imps){
	library("mice")
	
	if ( num_imps < 2) stop("Number of imputations is too low!")

	missing.data <- read.table(csv, header = TRUE, sep= ",", na.strings = c(" "))
	# perform the default multiple imputation as defined by mice
	imp <- mice(missing.data, m = num_imps, printFlag = FALSE)
	
	for (i in 1:num_imps){
		filled.data <- complete(imp,i)
		# perform the emergency mean on the missing columns
		if( length( filled.data[is.na(filled.data)] ) > 0){
			filled.data <- apply(filled.data, 2, .impute.wrapper) 
		}
		# if the program still fails throw an error!
		if( length( filled.data[is.na(filled.data)] ) > 0) stop("imputation failed: missing data still present")

		write.csv(filled.data, file = paste("mult", i, csv, sep='_'), row.names = FALSE)
	}

	file.remove(csv)
}

# perform regression imputation on the missing data csv file
#regress.imp <- function(csv){
#	library("mice")
#
#	missing.data <- read.table(csv, header = TRUE, sep= ",", na.strings = c(" "))
#	imp <- mice(missing.data , method = "norm.nob", m = 1, maxit = 1, seed = 1, printFlag = FALSE)
#	filled.data <- complete(imp,1)
	# perform the emergency mean on the missing columns
#	if( length( filled.data[is.na(filled.data)] ) > 0){
#		filled.data <- apply(filled.data, 2, .impute.wrapper) 
#	}
#	if( length( filled.data[is.na(filled.data)] ) > 0) stop("imputation failed: missing data still present")
#
#	write.csv(filled.data, file=paste("regression_imputation", csv, sep='_'), row.names = FALSE)
#	
#	file.remove(csv)
#}

.impute.wrapper <- function(col){

	m <- mean(col, na.rm = TRUE)
	col[is.na(col)] <- m
	return(col)
}

# check if all of the values in the column are identical and if so fill in all NAs with that value
# this prevents a bug
.same.vals <- function(column){

	# check for repeated values and sub in
	if ( abs( max(column[!is.na(column)]) - min(column[!is.na(column)]) ) < .Machine$double.eps ^ 0.5 ){
		column[is.na(column)] <- column[!is.na(column)][1]
	}
	return (column)
}






