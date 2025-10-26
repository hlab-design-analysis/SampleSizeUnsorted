summariseQuantiles<-function(x, group=c(lanID,sp), probs=c(0.025,0.975)){

# thanks to httpsrpubs.comjosemzSDbf for the function
apply_func_and_get_names <- function(DT, func, ...) {
   res <- lapply(DT, func, ...)
   c(list(op = names(res[[1]])), res)
}

if (group!="") cols<-c(group,colnames(x)[grepl(colnames(x), pat="^n_")]) else cols<-c(colnames(x)[grepl(colnames(x), pat="^n_")])
res<-unique(dat[sppWeight_obs>0,..cols])
res[, apply_func_and_get_names(.SD, quantile, probs), .SDcols=colnames(x)[grepl(colnames(x), pat="^n_")], by=group]

}