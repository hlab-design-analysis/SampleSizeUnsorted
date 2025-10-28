summariseMean<-function(x, group=c("lanID","sp")){

#excludes from calculations landings with only 1 bucket
if(any(x$nbuc_obs==1)) 
{
cat("\n")
print(paste0("ATT: excluding ",nrow(x[nbuc_obs==1,.N,lanID])," landings with only 1 bucket: variance is not defined in those cases"))
x<-x[nbuc_obs>1,]
cat("\n")
}
if (!is.null(group)) cols<-c(group,colnames(x)[grepl(colnames(x), pat="^n_")]) else cols<-c(colnames(x)[grepl(colnames(x), pat="^n_")])
res<-unique(x[sppWeight_obs>0,..cols])
res[, lapply(.SD, mean), .SDcols=colnames(x)[grepl(colnames(x), pat="^n_")], by=group]

}