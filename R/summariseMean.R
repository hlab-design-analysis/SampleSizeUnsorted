summariseMean<-function(x, group=c("lanID","sp")){

if (group!="") cols<-c(group,colnames(x)[grepl(colnames(x), pat="^n_")]) else cols<-c(colnames(x)[grepl(colnames(x), pat="^n_")])
res<-unique(x[sppWeight_obs>0,..cols])
res[, lapply(.SD, mean), .SDcols=colnames(x)[grepl(colnames(x), pat="^n_")], by=group]

}