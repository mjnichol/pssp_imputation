# perform mean imputation on the missing data csv file
mean.imp <- function(missing.data.csv){
	library("mice")
	missing.data <- read.table(missing.data.csv, header = TRUE, sep= ",", 
		na.strings = c(" "))
	imp <- mice(missing.data, method = "mean", m = 1, maxit = 1, printFlag = FALSE)
	.impute.and.write(missing.data, imp$imp, missing.data.csv, "mean")
}

# perform regression imputation on the missing data csv file
regress.imp <- function(missing.data.csv){
	library("mice")
	missing.data <- read.table(missing.data.csv, header = TRUE, sep= ",", 
		na.strings = c(" "))
	imp <- mice(missing.data , method = "norm.nob", m = 1, maxit = 1, seed = 1, printFlag = FALSE)
	.impute.and.write(missing.data, imp$imp, missing.data.csv, "regression")
}

# perform multiple imputation on the missing data csv file
# This will output several frames!
mult.imps <- function(missing.data.csv){
	library("mice")

	missing.data <- read.table(missing.data.csv, header = TRUE, sep= ",", 
		na.strings = c(" "))
	# perform the default multiple imputation as defined by mice
	imp <- mice(missing.data, printFlag = FALSE)
	.impute.and.write(missing.data, imp$imp, missing.data.csv, "mult")
}

# reinsert and write the imputation results to a suitably named output file
.impute.and.write <- function(missing.data, missing.vals, csv, type){

	missing.data <- .reconstruct.missing.vals(missing.data, missing.vals)
	if( length( missing.data[is.na(missing.data)] ) > 0) stop("imputation failed: missing data still present")
	write.csv(missing.data, file=paste(type, "imputation", csv, sep='_'))
}

# take the result with missing data values and then take result of the 
# imputation and re-insert the imputed values into the frame

# Note that multiple imputation will take in and outout multiple frames
# use a list to handle this possibility
.reconstruct.missing.vals <- function(missing.frame, imputation){

	complete.frame <- missing.frame 

	# form a vector containing the missing values
	imputation <- unlist(imputation)

	# insert this vector into the missing frame		
	complete.frame[is.na(complete.frame)] <- imputation

	return(complete.frame)
}
