# =========================
# SampleSizeUnsorted
# Determination of sample size needed to attain specific margins of error 
# 	in the species composition of unsorted landings.
#
# Nuno Prista & Annica de Groote
# Swedish University of Agricultural Sciences
# =========================

rm(list=ls())

# read packages
library(data.table)
# load funs
source("R/sourceAllFunctions.R")

# select country/data: use 3-letter acronym or "all_countries_*USOscenario*" for entire dataset
target_country<-"SWE"
target_country<-"all_countries_USO_as_species"
target_country<-"all_countries_USO_expert_judgement"

# if(target_country=="SPE") dat <- readRDS("data/SPE.rds")
# if(target_country=="SLU") dat <- readRDS("data/SLU.rds")
#if(target_country=="DNK") dat <- readRDS("data/DNK_3rd_party_USO_expert_judgement.rds")
# if(target_country=="FIN") dat <- readRDS("data/FIN.rds")
# if(target_country=="LVA") dat <- readRDS("data/LVA.rds")
# if(target_country=="EST") dat <- readRDS("data/EST.rds")
# if(target_country=="IRL") dat <- readRDS("data/IRL_SFPA.rds")
# if(target_country=="SWE") dat <- rbind(readRDS("data/SWE_other.rds"),readRDS("data/SWE_Baltic_HERSPR_HUC.rds"))

# all countries scenarios [differing on the way DNK USO is handled]
if(target_country=="all_countries_USO_expert_judgement")  dat <- readRDS("data/all_countries_USO_expert_judgement.rds")
if(target_country=="all_countries_USO_deleted")  dat <- readRDS("data/all_countries_USO_deleted.rds")
if(target_country=="all_countries_USO_allocated")  dat <- readRDS("data/all_countries_USO_allocated.rds")
if(target_country=="all_countries_USO_as_species")  dat <- readRDS("data/all_countries_USO_as_species.rds")

#lanIDtoRemove<-scan("data/lanID_DNK_NSea_SPR_withUSOabove5perc.txt", what="raw")
#dat<-dat[!lanID %in% lanIDtoRemove,]

# do a set of initial data checks on input data
doInitialChecks(dat)

# standardize
	# dat[, bucWeight:=sum(sppWeight_obs), .(lanID,bucID)][, new_sppWeight_obs:=sppWeight_obs/bucWeight*10]
	# dat$sppWeight_obs<-dat$new_sppWeight_obs
	# dat$bucWeight<-NULL
	# dat$new_sppWeight_obs<-NULL
	# dat$totWeight_obs<-NA

# species selection
	# dat <- dat[fisheryArea %in% c("Baltic_HERSPR_IND","Baltic_HERSPR_HUC") & sp %in% c("HER","SPR","SKB","GTA","SME"),]



# estimates weight Composition in Percentage and Absolute Weight
	# note: absolute estimates are only calculated if totWeight_obs is provided 
dat <- estimateWeightComp (x = dat,round_Nbuc_estim=FALSE, finitePopCorr=FALSE)


#===============================================
# calculate sample size for diferent margins of error in proportion
#===============================================

doSampleSizeGivenError(x=dat, e=c(0.02,0.025,0.05,0.07,0.10), error_type="Percent")
		
# example
unique(dat[nbuc_obs>1,c("fisheryArea","lanID","sp","totWeight_obs", "nbuc_obs","bucWeightmean_obs","sppPercWeight_s2","sppPercWeight_estim","n_0.020","n_0.025","n_0.050","n_0.070","n_0.100")])

#===============================================
# calculate sample size for diferent margins of error in absolute weight
#===============================================

doSampleSizeGivenError(x=dat, e=c(10000, 5000, 1000, 500, 100), error_type="Absolute")
		
# example
unique(dat[nbuc_obs>1,c("lanID","sp","totWeight_obs","nbuc_obs","sppPercWeight_estim", "n_10000", "n_5000", "n_1000","n_500","n_100")])

#===============================================
# examples of result summaries
#===============================================

# example: different types of summaries [with adjustment of n<2 to 2]
summariseMean(x = dat, group="sp", min_n=2) # mean by sp
summariseMean(x = dat, group=c("sp","lanID"), min_n=2) # mean by sp and lanID
summariseMedian(x = dat, group="sp", min_n=2)
summariseMax(x = dat, group="lanID", min_n=2) # sample size needed to characterize all species in each lanID
summariseQuantiles(x = dat, group="sp", probs=c(0.025,0.975), min_n=2)
summariseQuantiles(x = dat, group=NULL, probs=c(0.025,0.975), min_n=2)

# example: sample size needed to characterize all species in 95% of the landings [with adjustment of n<2 to 2]
	# note on min_n: adjustment of min_n to a different values (e.g., min_n=5) means that for each landing, the minimum number of buckets to be sampled would be set to 5
summariseMax(x = dat, group=c("fisheryArea"), min_n=5)

# example: 95% percentile of the worst case scenarios [all fisheryAreas]
apply(summariseMax(x = dat, group="lanID", min_n=5)[,3:5],2, quantile, prob=c(0.95))

#===============================================
# main results (agreed scenarios)
#===============================================

# per fisheryArea
	# 90% among the worst case scenarios
	summary_090<-rbindlist(lapply(split(summariseMax(x = dat, group=c("lanID","fisheryArea"), min_n=5),by="fisheryArea"), function(x) cbind(x[1,2],nLanIDs=nrow(x),round(t(apply(x[,4:ncol(x)],2,quantile, type=7, prob=c(0.90)))))))[order(fisheryArea),]
	# 95% among the worst case scenarios
	summary_095<-rbindlist(lapply(split(summariseMax(x = dat, group=c("lanID","fisheryArea"), min_n=5),by="fisheryArea"), function(x) {cbind(x[1,2],nLanIDs=nrow(x), round(t(apply(x[,4:ncol(x)],2, quantile, type=7, prob=c(0.95)))))}))[order(fisheryArea),]
	
	min_bucs_obs<-5
	# 50% among the worst case scenarios [restricted to nbuc_obs>min_bucs_obs
	summary_050_min_bucs_obs_5<-rbindlist(lapply(split(summariseMax(x = dat[nbuc_obs>=min_bucs_obs,], group=c("lanID","fisheryArea"), min_n=5),by="fisheryArea"), function(x) cbind(x[1,2],nLanIDs=nrow(x),round(t(apply(x[,4:ncol(x)],2,quantile, type=7, prob=c(0.50)))))))[order(fisheryArea),]


	# 70% among the worst case scenarios [restricted to nbuc_obs>min_bucs_obs
	summary_070_min_bucs_obs_5<-rbindlist(lapply(split(summariseMax(x = dat[nbuc_obs>=min_bucs_obs,], group=c("lanID","fisheryArea"), min_n=5),by="fisheryArea"), function(x) cbind(x[1,2],nLanIDs=nrow(x),round(t(apply(x[,4:ncol(x)],2,quantile, type=7, prob=c(0.70)))))))[order(fisheryArea),]
	# 75% among the worst case scenarios [restricted to nbuc_obs>min_bucs_obs
	summary_075_min_bucs_obs_5<-rbindlist(lapply(split(summariseMax(x = dat[nbuc_obs>=min_bucs_obs,], group=c("lanID","fisheryArea"), min_n=5),by="fisheryArea"), function(x) cbind(x[1,2],nLanIDs=nrow(x),round(t(apply(x[,4:ncol(x)],2,quantile, type=7, prob=c(0.75)))))))[order(fisheryArea),]
	# 90% among the worst case scenarios [restricted to nbuc_obs>min_bucs_obs
	summary_090_min_bucs_obs_5<-rbindlist(lapply(split(summariseMax(x = dat[nbuc_obs>=min_bucs_obs,], group=c("lanID","fisheryArea"), min_n=5),by="fisheryArea"), function(x) cbind(x[1,2],nLanIDs=nrow(x),round(t(apply(x[,4:ncol(x)],2,quantile, type=7, prob=c(0.90)))))))[order(fisheryArea),]
	# 95% among the worst case scenarios [restricted to nbuc_obs>min_bucs_obs
	summary_095_min_bucs_obs_5<-rbindlist(lapply(split(summariseMax(x = dat[nbuc_obs>=min_bucs_obs,], group=c("lanID","fisheryArea"), min_n=5),by="fisheryArea"), function(x) cbind(x[1,2],nLanIDs=nrow(x),round(t(apply(x[,4:ncol(x)],2,quantile, type=7, prob=c(0.95)))))))[order(fisheryArea),]
	# 100% among the worst case scenarios [restricted to nbuc_obs>min_bucs_obs
	summary_1_min_bucs_obs_5<-rbindlist(lapply(split(summariseMax(x = dat[nbuc_obs>=min_bucs_obs,], group=c("lanID","fisheryArea"), min_n=5),by="fisheryArea"), function(x) cbind(x[1,2],nLanIDs=nrow(x),round(t(apply(x[,4:ncol(x)],2,quantile, type=7, prob=c(1)))))))[order(fisheryArea),]

	# adds indicator on results being based on a minimum amount of landings
	min_n_landings<-14
	summary_050_min_bucs_obs_5[nLanIDs>=14,]
	summary_070_min_bucs_obs_5[nLanIDs>=14,]
	summary_075_min_bucs_obs_5[nLanIDs>=14,]
	summary_090_min_bucs_obs_5[nLanIDs>=14,]
	summary_095_min_bucs_obs_5[nLanIDs>=14,]
	summary_1_min_bucs_obs_5[nLanIDs>=14,]
	

#===============================================
# main results (agreed scenarios)
#===============================================

dir_results<-paste0("results/",target_country,"/main_results/")
dir.create(dir_results, showWarnings=FALSE, recursive=T)

# saves all observed data and core lanID results
fwrite(dat[nbuc_obs>1,], file=paste0(dir_results,target_country,"_lanID_results.csv"))

# saves results
fwrite(summary_050_min_bucs_obs_5[,1:7], file=paste0(dir_results, target_country,"_summary_070wcs_min_bucs_obs_",min_bucs_obs,"_",format(Sys.Date(), format="%Y%m%d"),".csv"))
fwrite(summary_070_min_bucs_obs_5[,1:7], file=paste0(dir_results, target_country,"_summary_070wcs_min_bucs_obs_",min_bucs_obs,"_",format(Sys.Date(), format="%Y%m%d"),".csv"))
fwrite(summary_090_min_bucs_obs_5[,1:7], file=paste0(dir_results, target_country,"_summary_090wcs_min_bucs_obs_",min_bucs_obs,"_",format(Sys.Date(), format="%Y%m%d"),".csv"))
fwrite(summary_095_min_bucs_obs_5[,1:7], file=paste0(dir_results, target_country,"_summary_095wcs_min_bucs_obs_",min_bucs_obs,"_",format(Sys.Date(), format="%Y%m%d"),".csv"))


#===============================================
# main results (agreed scenarios - by landing size category)
#===============================================

for (i in unique(dat$fisheryArea))
{
windows()
hist(dat[fisheryArea==i,.N, .(lanID,totWeight_obs)]$totWeight_obs/1000, main=i)
}

	library(xlsx)
	target_prob<-0.95
	if("lanSizeCateg" %in% colnames(dat)) dat$lanSizeCateg<-NULL
	dat[order(totWeight_obs),lanSizeCateg:=cut(unique(totWeight_obs)/1000, breaks=c(0, 10, 300, 500, 1000, 4000), right = FALSE, ordered_result=T), by=.(lanID)]

for (i in summary_095_min_bucs_obs_5[nLanIDs>=14,]$fisheryArea)
	{
	print(i)
	x<-rbindlist(lapply(split(summariseMax(x = dat[nbuc_obs>=min_bucs_obs & fisheryArea ==i,], group=c("lanID","lanSizeCateg"), min_n=5),by="lanSizeCateg"), function(x) {cbind(x[1,2],nLanIDs=nrow(x), round(t(apply(x[,4:ncol(x)],2, quantile, type=7, prob=c(target_prob)))))}))[order(lanSizeCateg),][,.(lanSizeCateg, nLanIDs, n_0.050,n_0.100)][order(lanSizeCateg),]
	if(i=="Baltic_HERSPR_HUC") write.xlsx(x[nLanIDs>=15,], file=paste0("results_size_categ",target_prob,"_min_",min_bucs_obs,".xlsx"),sheetName=i) else write.xlsx(x[nLanIDs>14,], file=paste0("results_size_categ",target_prob,"_min_",min_bucs_obs,".xlsx"),sheetName=i, append=T)
	print(x)
}

	if("lanSizeCateg" %in% colnames(dat)) dat$lanSizeCateg<-NULL
	dat[order(totWeight_obs),lanSizeCateg:=cut(unique(totWeight_obs)/1000, breaks=c(0, 10, 300, 4000), right = FALSE, ordered_result=T), by=.(lanID)]
	rbindlist(lapply(split(summariseMax(x = dat[nbuc_obs>=min_bucs_obs & fisheryArea == c("Baltic_HERSPR_HUC"),], group=c("lanID","lanSizeCateg"), min_n=5),by="lanSizeCateg"), function(x) {cbind(x[1,2],nLanIDs=nrow(x), round(t(apply(x[,4:ncol(x)],2, quantile, type=7, prob=c(0.95)))))}))[order(lanSizeCateg),][,.(lanSizeCateg, nLanIDs, n_0.050,n_0.100)]
	dat[order(totWeight_obs),lanSizeCateg:=cut(unique(totWeight_obs)/1000, breaks=c(0, 10, 300, 1000, 4000), right = FALSE, ordered_result=T), by=.(lanID)]
	rbindlist(lapply(split(summariseMax(x = dat[nbuc_obs>=min_bucs_obs & fisheryArea == c("Baltic_HERSPR_IND"),], group=c("lanID","lanSizeCateg"), min_n=5),by="lanSizeCateg"), function(x) {cbind(x[1,2],nLanIDs=nrow(x), round(t(apply(x[,4:ncol(x)],2, quantile, type=7, prob=c(0.95)))))}))[order(lanSizeCateg),][,.(lanSizeCateg, nLanIDs, n_0.050,n_0.100)]
	dat[order(totWeight_obs),lanSizeCateg:=cut(unique(totWeight_obs)/1000, breaks=c(0, 100, 300, 1000, 4000), right = FALSE, ordered_result=T), by=.(lanID)]
	rbindlist(lapply(split(summariseMax(x = dat[nbuc_obs>=min_bucs_obs & fisheryArea == c("NSea_HER"),], group=c("lanID","lanSizeCateg"), min_n=5),by="lanSizeCateg"), function(x) {cbind(x[1,2],nLanIDs=nrow(x), round(t(apply(x[,4:ncol(x)],2, quantile, type=7, prob=c(0.90)))))}))[order(lanSizeCateg),][c(4,1,2,3),.(lanSizeCateg, nLanIDs, n_0.050,n_0.100)]
	dat[order(totWeight_obs),lanSizeCateg:=cut(unique(totWeight_obs)/1000, breaks=c(0, 100, 500, 1000, 4000), right = FALSE, ordered_result=T), by=.(lanID)]
	rbindlist(lapply(split(summariseMax(x = dat[nbuc_obs>=min_bucs_obs & fisheryArea == c("NSea_SPR"),], group=c("lanID","lanSizeCateg"), min_n=5),by="lanSizeCateg"), function(x) {cbind(x[1,2],nLanIDs=nrow(x), round(t(apply(x[,4:ncol(x)],2, quantile, type=7, prob=c(0.90)))))}))[order(lanSizeCateg),][c(4,1,2,3),.(lanSizeCateg, nLanIDs, n_0.050,n_0.100)]
	dat[order(totWeight_obs),lanSizeCateg:=cut(unique(totWeight_obs)/1000, breaks=c(0, 100, 500, 1000, 4000), right = FALSE, ordered_result=T), by=.(lanID)]
	rbindlist(lapply(split(summariseMax(x = dat[nbuc_obs>=min_bucs_obs & fisheryArea == c("NSea_SAN"),], group=c("lanID","lanSizeCateg"), min_n=5),by="lanSizeCateg"), function(x) {cbind(x[1,2],nLanIDs=nrow(x), round(t(apply(x[,4:ncol(x)],2, quantile, type=7, prob=c(0.90)))))}))[order(lanSizeCateg),][c(4,1,2,3),.(lanSizeCateg, nLanIDs, n_0.050,n_0.100)]


	#standardize to x kg and see what is left from bycatch

	targetW<-25
	targetW<-5

	dat[, bucWeight:=sum(sppWeight_obs), .(lanID,bucID)][, prop:=sppWeight_obs/bucWeight][, new_sppWeight_obs:=round(sppWeight_obs/bucWeight*targetW,1)][, meanBucWeight:=mean(bucWeight), by="lanID"]
	
	dat[, meanBucWeightFisheryArea:=mean(meanBucWeight),.(fisheryArea)][bucWeight>targetW & prop<.1 & nbuc_obs>=min_bucs_obs,list(ini=sum(sppWeight_obs>0), fin=sum(new_sppWeight_obs>0)),.(fisheryArea,meanBucWeightFisheryArea)][, dif:=round((fin-ini)/ini*100,0)][]
	
	
	# dat$sppWeight_obs<-dat$new_sppWeight_obs
	# dat$bucWeight<-NULL
	# dat$new_sppWeight_obs<-NULL
	# dat$totWeight_obs<-NA









# bycatch is:
bycatch_limit<- 0.3

	rbindlist(lapply(split(
			summariseMax(x = dat[nbuc_obs>=min_bucs_obs & sppPercWeight_estim>bycatch_limit,], group=c("lanID","fisheryArea"), min_n=5),by="fisheryArea"), function(x) {cbind(x[1,2],nLanIDs=nrow(x), round(t(apply(x[,4:ncol(x)],2, quantile, type=7, prob=c(0.95)))))}))[order(fisheryArea),]


targetFisheryArea<-c("NAtlantic_MAC")

tmp<-dat[nbuc_obs>=min_bucs_obs,(V1=mean(bucWeight_obs)),.(fisheryArea, lanID, totWeight_obs)][,mean(V1),.(fisheryArea)]
tmp<-dat[nbuc_obs>=min_bucs_obs & fisheryArea == targetFisheryArea,(V1=mean(bucWeight_obs)),.(lanID, totWeight_obs, sppPercWeight_estim_cv, nbuc_obs, sp)]





plot(sppPercWeight_estim_cv~V1, data=tmp)
plot(sppPercWeight_estim_cv~nbuc_obs, data=tmp[sp=="HER",])
plot(sppPercWeight_estim_cv~V1, data=tmp[sp=="SPR",])

summary(lm(sppPercWeight_estim_cv~V1+nbuc_obs+totWeight_obs, data=tmp[sp=="HER",]))

a<-lm(atan(sppPercWeight_estim_cv)~V1+nbuc_obs+log(totWeight_obs), data=tmp)
a<-lm(sppPercWeight_estim_cv~V1+nbuc_obs, data=tmp[sp=="SPR",])
summary(a)
plot(a)


# bootstrap
# to look at variability (had a different set of trips (sampled with replacement) been sampled)

library(parallel) # detectCores
library(foreach) # parallel computing
library(doMC) # parallel computing
library(doRNG) # Generic Reproducible Parallel Backend for 'foreach' Loops


# bootstrap [trip level only]
dat_fishery<-dat[nbuc_obs>=5 & fisheryArea=="Bothnia_HER",]
dat_fishery<-dat[nbuc_obs>=5 & fisheryArea=="NSea_SAN",]
dat_fishery<-dat[nbuc_obs>=5 & fisheryArea=="Baltic_HERSPR_IND",]

ncpus<-2

# initiates parallel
registerDoMC(ncpus)
print(getDoParWorkers())

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

# estimates the sample size
mean(bootstrap_res$n_0.050); quantile(bootstrap_res$n_0.050, probs=c(0.025,0.975))
hist(bootstrap_res$n_0.050)

> mean(bootstrap_res$n_0.050); quantile(bootstrap_res$n_0.050, probs=c(0.025,0.975))
[1] 170.0476
 2.5% 97.5% 
  143   192 











# bootstrap [trip level only]
dat_fishery<-dat[nbuc_obs>=5 & fisheryArea=="Bothnia_HER",]

ncpus<-2

# initiates parallel
registerDoMC(ncpus)
print(getDoParWorkers())

# starts the seeds
set.seed(123)

# runs simulations 
n_boots<-1000
options(warn=0) # from ?options: If warn is zero (the default) warnings are stored until the top–level function returns. 
system.time({
out <- foreach (i=1:n_boots) %dorng% {

trip_ids <- unique(dat_fishery$lanID)
n_trips <- length(trip_ids)
bootstrap_trip_ids<-sample(trip_ids, size=n_trips, replace=T)
bootstrap_data<-merge(data.frame(repl_id=1:length(bootstrap_trip_ids),lanID=bootstrap_trip_ids),dat_fishery, by="lanID",all.x=T)
bootstrap_data$lanID<-
}
})

bootstrap_data <- rbindlist(out, idcol=TRUE)

# estimates the sample size
bootstrap_data[,lanID:=paste(.id, repl_id, lanID)]
bootstrap_data <- bootstrap_data[order(.id, repl_id, lanID, bucID),]

summary_095_min_bucs_obs_5<-rbindlist(lapply(split(summariseMax(x = bootstrap_data, group=c("lanID","fisheryArea"), min_n=2),by="fisheryArea"), function(x) {browser(); cbind(x[1,2],nLanIDs=nrow(x),round(t(apply(x[,4:ncol(x)],2,quantile, type=7, prob=c(0.95)))))}))[order(fisheryArea),]



