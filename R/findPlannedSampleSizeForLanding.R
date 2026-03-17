#' function that assigns the sample size based on reference values
#' @param x is a landings weight in ton
#' @param plan is a data.frame with columns size_upp, n and freq. 
#' 
#' @return the value of sample size planned for x
#' 
#' @details in plan, size_upp is the upper limit (in tonnes) of the weight categories [need to be ordered], 
#' n is the sample size to apply on it and 
#' freq is the interval (in tonnes) fo data collection above the last size_upp
#' E.g., data.frame(size_upp=c(20,29,39, 49, 100, 300), n=c(7,9,12,15,18,21), freq_above=25)
#'
#' @examples
#' \dontrun{
#' findPlannedSampleSizeForLanding(21, data.frame(size_upp=c(20,29,39, 49, 100, 300), n=c(7,9,12,15,18,21), freq_above=25))
#' }

findPlannedSampleSizeForLanding <- function(x, plan){

index <- which(x<=plan$size_upp)[1]
if(any(is.na(index))) {
		lastCategWeight<-max(plan$size_upp)
		freq<-plan$freq[1]
		bucketsInlastCategWeight<-plan$n[nrow(plan)]
		out<- floor((x-lastCategWeight)/freq+bucketsInlastCategWeight)
		} else {
		out<-plan[max(index),"n"]
		}
out
}