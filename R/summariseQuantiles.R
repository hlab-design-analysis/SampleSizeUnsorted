#' Quantiles of sample size estimates
#'
#' This function calculates quantiles of sample size estimates obtained for a set of landings.
#'
#' @param x dataset containing estimated sample size columns ("n_*" columns).
#' @param group a vector of column names defining how to group results. Default "sp" provides 
#' quantiles of sample size by error margin and species)
#' @param probs numeric vector of probabilities with values in [0,1].
#' @param min_n integer defining the minimum sample size accepted. Default is 2. See details. 
#'
#' @return data.table with means by group
#'
#' @details A minimum of 2 samples is needed to calculate a variance. If the dataset contains
#' extra variable, e.g., a fishery column, setting group=c("sp","fishery") will provide the 
#' quantiles of sample size needed to attain each error margin by sp and fishery.
#'
#' @examples
#' \dontrun{
#' dat <- readRDS("data/SPE.rds")
#' doInitialChecks(dat)
#' dat <- estimateWeightComp(dat)
#' doSampleSizeGivenError(x=dat, e=c(0.03,0.05,0.07,0.10), error_type="Percent")
#' summariseQuantiles(x = dat, group="sp", probs=c(0.025,0.975), min_n=2)
#' # compare with
#' summariseQuantiles(x = dat, group="sp", probs=c(0.025,0.975), min_n=0)
#' }


summariseQuantiles<-function(x, group=c("sp"), probs=c(0.025,0.975), min_n=2){

#excludes from calculations landings with only 1 bucket
if(any(x$nbuc_obs==1)) 
{
cat("\n")
print(paste0("ATT: excluding ",nrow(x[nbuc_obs==1,.N,lanID])," landings with only n=1 buckets sampled: variance is not defined in those cases"))
x<-x[nbuc_obs>1,]
cat("\n")
}

# credits to https://rpubs.com/josemz/SDbf for the following function
apply_func_and_get_names <- function(DT, func, ...) {
   res <- lapply(DT, func, ...)
   c(list(op = names(res[[1]])), res)
}

# filters out n cols with NAs (note: they appear when totWeight_obs has NAs)
tmp<-apply(data.frame(x)[,colnames(x)[grepl(colnames(x), pat="^n_")]],2,sum)
n_cols<-names(tmp)[!is.na(tmp)]
if (!is.null(group)) cols<-c("lanID","sp",group,n_cols) else cols<-c("lanID","sp",n_cols)
res<-unique(x[nbuc_obs>1,..cols])

# sets the minimum sample size
if(!min_n==0) {
aux<-colnames(res)[grepl(colnames(res), pat="^n_")]
res<-cbind(res[,..group],res[, lapply(.SD, function(x) ifelse(x %in% c(0:min_n-1),min_n,x)),.SDcols=aux])
}
# calculates the quantiles
out<-res[, apply_func_and_get_names(.SD, quantile, probs), .SDcols=cols[grepl(cols, pat="^n_")], by=group]
if (!is.null(group)) {
cols2<-c(group, "lanID")
out<-merge(x[nbuc_obs>1,.N,by=cols2][,.N,by=group],out)
} else {
out
}
out
}