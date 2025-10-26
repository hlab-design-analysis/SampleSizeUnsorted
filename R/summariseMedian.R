
summariseMedian<-function(x, group=c("lanID","sp")){

target_cols <- colnames(x)[grepl(colnames(x), pat="^n_")]
if (!is.null(group)) cols<-c(group,target_cols) else cols<-target_cols
res<-unique(x[sppWeight_obs>0,..cols])
res[, lapply(.SD, median), .SDcols=target_cols, by=group]

}

