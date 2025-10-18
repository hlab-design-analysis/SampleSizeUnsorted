#' Do initial checks on data
#'
#' This function performs a set of consistency checks on the input data (dat).
#'
#' @param x Input data
#'
#' @return message "data ok" or error
#'
#' @details some checks only .
#' @examples
#' \dontrun{
#' dat <- readRDS("data/HaVCtrl_strict.rds")
#' doInitialChecks(dat)
#' }


doInitialChecks<-function(x) {

if(all(
# test: dat is data.table
	is.data.table(dat) & 
# test: expected columns exist, are at the left of the data.table and in right order
	all(colnames(dat)[1:5]==c("lanID","bucID","sp", "sppWeight_obs","totWeight_obs")) &
# test: all species should exist in all buckets
	length(unique(dat[,paste(sort(unique(sp)), collapse=","),
		by=.(lanID, bucID)]$V1))==1 &
# test: only one row per lan, buc, and species
	all(dat[,.N, .(lanID,bucID,sp)]$N==1) &
# test: only 1 totWeight_obs per landing
	all(dat[,.N, .(lanID,totWeight_obs)][,.N, lanID]$N==1) == TRUE
	)) print ("dat ok") else stop("check consistency of dat object")	
}