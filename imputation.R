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
mult.imps <- function(missing.data.csv, num_imps){
	library("mice")
	
	if ( num_imps < 2) stop("Number of imputations is too low!")

	missing.data <- read.table(missing.data.csv, header = TRUE, sep= ",", 
		na.strings = c(" "))
	# perform the default multiple imputation as defined by mice
	imp <- mice(missing.data, m = num_imps, printFlag = FALSE)
	
	# prepare the multiple imputation list for passing to the csv output
	list.imp <- vector(mode="list", length = num_imps)	
	for (i in 1:num_imps){
		temp.imp = NULL		

		for (j in 1:length(list.imp) ){
			
			temp.imp = c( temp.imp, imp$imp[[j]][i] )
		}

		list.imp[[i]] <- as.list(temp.imp)
	}



	for (i in 1:num_imps){
					       
		.impute.and.write(missing.data, list.imp[[i]], paste(i, missing.data.csv, sep="_"), "mult")
	}
}

# reinsert and write the imputation results to a suitably named output file
.impute.and.write <- function(missing.data, missing.vals, csv, type){

	missing.data <- .reconstruct.missing.vals(missing.data, missing.vals, type)
	if( length( missing.data[is.na(missing.data)] ) > 0) stop("imputation failed: missing data still present")
	write.csv(missing.data, file=paste(type, "imputation", csv, sep='_'))
}

# handle the case where you have to do multiple imputation, so you will be 
# returning and writing a set of imputed values
.multiple.imputation.handler <- function(imputation, missing.data){


	return()
}

# take the result with missing data values and then take result of the 
# imputation and re-insert the imputed values into the frame

# Note that multiple imputation will take in and outout multiple frames
# use a list to handle this possibility
.reconstruct.missing.vals <- function(missing.frame, imputation, type){

	complete.frame <- missing.frame 

	# form a vector containing the missing values
	imputation <- unlist(imputation)

	# insert this vector into the missing frame		
	complete.frame[is.na(complete.frame)] <- imputation

	return(complete.frame)
}
