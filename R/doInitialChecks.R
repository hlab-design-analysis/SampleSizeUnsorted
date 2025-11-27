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
	is.data.table(x) & 
# test: expected columns exist, are at the left of the data.table and in right order
	all(colnames(x)[1:10]==c("ctry","lanID","bucID","sp", "sppWeight_obs","totWeight_obs","year","fishery","area","fisheryArea")) &
# test: all species should exist in all buckets
	#length(unique(x[,paste(sort(unique(sp)), collapse=","),
		#by=.(lanID, bucID, fisheryArea)]$V1))==1 &
	# new implementation
	all(apply(table(x$sp, x$fisheryArea),2, function(x)length(unique(x[x>0]))==1)) &
# test: only one row per lan, buc, and species
	all(x[,.N, .(lanID,bucID,sp)]$N==1) &
# test: only 1 totWeight_obs per landing
	all(x[,.N, .(lanID,totWeight_obs)][,.N, lanID]$N==1) == TRUE &
# test: no NAs
	all(apply(x, 2, function(x) sum(is.na(x))))==0

	)) cat ("dat ok \n\n") else stop("check consistency of dat object")	

# warning: data contain a mix of declared and mixed totWeight_obs
	# present algorithm cannot fully handle these two cases at the same time
		# see #
	if(nrow(x[,.N,is.na(totWeight_obs)])>1) 
cat("===Attention=== \n
The data contains some totWeight_obs missing and others declared: \n
The analyses should be split in two: \n
   dat<-dat[!is.na(totWeight_obs),] # where both % and absolute weights can be calculated \n
   dat<-dat[is.na(totWeight_obs),] # where only % can be calculated\n
with results 'rbind-ed' in the end \n \n")

}