
# bootstrap routine (kind of sensitivity analysis)
# question: individual bootstraps are trips sampled with replacement (same sample size), from the original) within a fishery

library(parallel) # detectCores
library(foreach) # parallel computing
library(doMC) # parallel computing
library(doRNG) # Generic Reproducible Parallel Backend for 'foreach' Loops

# initiates parallel
registerDoMC(ncpus)
print(getDoParWorkers())
ncpus<-2

# bootstrap [trip level only]
dat_fishery<-dat[nbuc_obs>=5 & fisheryArea=="Baltic_HERSPR_IND",]

# starts the seeds
set.seed(123)

# runs simulations 
n_boots<-5000
options(warn=0) # from ?options: If warn is zero (the default) warnings are stored until the top–level function returns. 
system.time({
out <- foreach (i=1:n_boots) %dorng% {

trip_ids <- unique(dat_fishery$lanID)
n_trips <- length(trip_ids)
bootstrap_trip_ids<-sample(trip_ids, size=n_trips, replace=T)
bootstrap_data<-data.table(merge(data.frame(repl_id=1:length(bootstrap_trip_ids),lanID=bootstrap_trip_ids),dat_fishery, by="lanID",all.x=T))
bootstrap_data[,lanID:=paste(repl_id, lanID)]
rbindlist(lapply(split(summariseMax(bootstrap_data, group=c("lanID","fisheryArea"), min_n=5),by="fisheryArea"), function(x)cbind(x[1,2],nLanIDs=nrow(x),round(t(apply(x[,4:ncol(x)],2,quantile, type=7, prob=c(0.95)))))))
}
})

bootstrap_res <- rbindlist(out, idcol=TRUE)

# estimates an approx CI (note: approx because implemented based on the quantile of bootstraps, not based on their difference towards the original value)
hist(bootstrap_res$n_0.050)
mean(bootstrap_res$n_0.050); quantile(bootstrap_res$n_0.050, probs=c(0.025,0.975))
# [1] 170.0476
 # 2.5% 97.5% 
  # 143   192 
