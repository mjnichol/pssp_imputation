args<-commandArgs(TRUE)
source("new_imp.R")
# first argument is the method
# second argument is the csv
# third argument is the number of times to run
switch = toString(args[1])
file = toString(args[2])
iters = as.integer(args[3])

if (switch == "mean.imp"){
	mean.imp(file) 
} else if (switch == "regress.imp"){
	regress.imp(file)
} else if (switch == "mult.imp"){
	mult.imp(file, iters)
} else {
	stop("Imputation method does not exist!")
}
