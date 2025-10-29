
summariseMedian<-function(x, group=c("lanID","sp")){

#excludes from calculations landings with only 1 bucket
if(any(x$nbuc_obs==1)) 
{
cat("\n")
print(paste0("ATT: excluding ",nrow(x[nbuc_obs==1,.N,lanID])," landings with only n=1 buckets sampled: variance is not defined in those cases"))
x<-x[nbuc_obs>1,]
cat("\n")
}
target_cols <- colnames(x)[grepl(colnames(x), pat="^n_")]
if (!is.null(group)) cols<-c(group,target_cols) else cols<-target_cols
res<-unique(x[nbuc_obs>1,..cols])
out<-res[, lapply(.SD, median), .SDcols=target_cols, by=group]
cols2<-c(group, "lanID")
merge(x[nbuc_obs>1,.N,by=cols2][,.N,by=group],out)
}

