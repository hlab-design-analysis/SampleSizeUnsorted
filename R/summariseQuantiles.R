summariseQuantiles<-function(x, group=c(lanID,sp), probs=c(0.025,0.975)){

# thanks to httpsrpubs.comjosemzSDbf for the function
apply_func_and_get_names <- function(DT, func, ...) {
   res <- lapply(DT, func, ...)
   c(list(op = names(res[[1]])), res)
}
# filters out n cols with NAs (note: they appear when totWeight_obs has NAs)
tmp<-apply(data.frame(x)[,colnames(x)[grepl(colnames(x), pat="^n_")]],2,sum)
n_cols<-names(tmp)[!is.na(tmp)]

if (!is.null(group)) cols<-c(group,n_cols) else cols<-n_cols
res<-unique(x[,..cols])
res[, apply_func_and_get_names(.SD, quantile, probs), .SDcols=cols[grepl(cols, pat="^n_")], by=group]

}