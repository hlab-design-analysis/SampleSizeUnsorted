summariseMean<-function(x, group=c("lanID","sp")){

if (group!="") cols<-c(group,colnames(dat)[grepl(colnames(dat), pat="^n_")]) else cols<-c(colnames(dat)[grepl(colnames(dat), pat="^n_")])
res<-unique(dat[sppWeight_obs>0,..cols])
res[, lapply(.SD, mean), .SDcols=colnames(dat)[grepl(colnames(dat), pat="^n_")], by=group]

}