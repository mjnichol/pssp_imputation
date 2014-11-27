# perform mean imputation on the missing data csv file
mean.imp <- function(missing.data.csv){
	library("mice")
	missing.data <- .csv.to.frame(missing.data.csv)
	imp <- mice(missing.data, method = "mean", m = 1, maxit = 1)
	missing.vals <- imp$imp

}

# perform regression imputation on the missing data csv file
regress.imp <- function(missing.data.csv){
	library("mice")
	missing.data <- .csv.to.frame(missing.data.csv)
	imp <- mice(missing.data.csv , method = "norm.nob", m = 1, maxit = 1, seed = 1)
	missing.vals <- imp$imp

}

# perform multiple imputation on the missing data csv file
# This will output several frames!
mult.imps <- function(missing.data.csv){
	library("mice")
	missing.data <- .csv.to.frame(missing.data.csv)
	# perform the default multiple imputation as defined by mice
	imp <- mice(missing.data.csv)
	missing.vals <- imp$imp
}

# 'private' method to convert the csv file to a frame and handle the missing
# entries correctly for mice's functions. In mice missing values are coded as
# 'NA' entries 
.csv.to.frame <- function(csv.file){

	data.frame = read.table(csv.file, header = TRUE, sep= ",", 
		na.strings = c(" "))
	return(data.frame)
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
