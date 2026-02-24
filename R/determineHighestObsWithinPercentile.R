#' Determine highest observation within percentile
#'
#' This function calculates the highest observation within a certain percentile (e.g., 95%), circunventing the estimates of quantile type=7 when sample size is low.
#'

determineHighestObsWithinPercentile<-function(y, target_prob){	
	pos <- which(sort(y, decreasing=T)==round(quantile(y, type=7, prob=c(target_prob)))); pos
if(length(pos)==0) pos <- max(which(sort(y, decreasing=T)>round(quantile(y, type=7, prob=c(target_prob))))); pos<-pos+1
if(length(pos)>1)return(round(quantile(y, type=7, prob=c(target_prob)))) else {
return(sort(y, decreasing=T)[pos])
}
}