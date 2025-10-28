summariseQuantiles<-function(x, group=c(lanID,sp), probs=c(0.025,0.975)){

#excludes from calculations landings with only 1 bucket
if(any(x$nbuc_obs==1)) 
{
cat("\n")
print(paste0("ATT: excluding ",nrow(x[nbuc_obs==1,.N,lanID])," landings with only n=1 buckets sampled: variance is not defined in those cases"))
x<-x[nbuc_obs>1,]
cat("\n")
}

# thanks to httpsrpubs.comjosemzSDbf for the function
apply_func_and_get_names <- function(DT, func, ...) {
   res <- lapply(DT, func, ...)
   c(list(op = names(res[[1]])), res)
}
# filters out n cols with NAs (note: they appear when totWeight_obs has NAs)
tmp<-apply(data.frame(x)[,colnames(x)[grepl(colnames(x), pat="^n_")]],2,sum)
n_cols<-names(tmp)[!is.na(tmp)]

if (!is.null(group)) cols<-c(group,n_cols) else cols<-n_cols
res<-unique(x[sppWeight_obs>0,..cols])
out<-res[, apply_func_and_get_names(.SD, quantile, probs), .SDcols=cols[grepl(cols, pat="^n_")], by=group]
merge(x[sppWeight_obs>0,..group][,.N,group],out)
}