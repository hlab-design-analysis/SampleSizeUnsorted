#' Median of sample size estimates
#'
#' This function calculates the median of sample size estimates obtained for a set of landings.
#'
#' @param x dataset containing estimated sample size columns ("n_*" columns).
#' @param group a vector of column names defining how to group results. Default "sp" provides 
#' median sample size by error margin and species)
#' @param min_n integer defining the minimum sample size accepted. Default is 2. See details. 
#'
#' @return data.table with means by group
#'
#' @details A minimum of 2 samples is needed to calculate a variance. If the dataset contains
#' extra variable, e.g., a fishery column, setting group=c("sp","fishery") will provide the 
#' median sample size needed to attain each error margin by sp and fishery.
#'
#' @examples
#' \dontrun{
#' dat <- readRDS("data/SPE.rds")
#' doInitialChecks(dat)
#' dat <- estimateWeightComp(dat)
#' doSampleSizeGivenError(x=dat, e=c(0.03,0.05,0.07,0.10), error_type="Percent")
#' summariseMedian(x = dat, group="sp")
#' # compare with
#' summariseMedian(x = dat, group="sp", min_n=0)
#' }

summariseMedian<-function(x, group=c("sp"), min_n=2){

#excludes from calculations landings with only 1 bucket
if(any(x$nbuc_obs==1)) 
{
cat("\n")
print(paste0("ATT: excluding ",nrow(x[nbuc_obs==1,.N,lanID])," landings with only n=1 buckets sampled: variance is not defined in those cases"))
x<-x[nbuc_obs>1,]
cat("\n")
}
# filters columns
target_cols <- colnames(x)[grepl(colnames(x), pat="^n_")]
if (!is.null(group)) cols<-c("lanID",group,target_cols) else cols<-c("lanID",target_cols)
res<-unique(x[nbuc_obs>1,..cols])
# sets the minimum sample size
if(!min_n==0) {
aux<-colnames(res)[grepl(colnames(res), pat="^n_")]
res<-cbind(res[,..group],res[, lapply(.SD, function(x) ifelse(x %in% c(0:min_n-1),min_n,x)),.SDcols=aux])
}
# calculates the median
out<-res[, lapply(.SD, median), .SDcols=target_cols, by=group]
cols2<-c(group, "lanID")
merge(x[nbuc_obs>1,.N,by=cols2][,.N,by=group],out)
}

