# =========================
# Estimation of species composition & determination of sample size needed to 
# 	attain pre-specified margins of error in the species composition of unsorted landings
#
# Nuno Prista and Annica de Groote
# Swedish University of Agricultural Sciences, 2025-2026
# =========================

rm(list=ls())

# read packages
library(data.table)
# load funs
source("R/sourceAllFunctions.R")

# select country/data: use 3-letter acronym or "all_countries_*USOscenario*" for entire dataset
target_country<-"all_countries_USO_expert_judgement"

# load data
if(target_country=="all_countries_USO_expert_judgement")  dat <- readRDS("data/all_countries_USO_expert_judgement.rds"); file.info("data/all_countries_USO_expert_judgement.rds")

# do a set of initial data checks on input data
doInitialChecks(dat)

#===============================================
# estimate weight Composition in Percentage and Absolute Weight
	# note: absolute estimates are only calculated if totWeight_obs is provided 
#===============================================

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
# main results (agreed scenarios)
#===============================================

# set the minimum number of buckets required for landings to enter analysis
	min_bucs_obs<-5
# set min_n: minimum number of buckets accepted as sample size result
	# note on min_n: 
		# adjustment of min_n to a different values (e.g., min_n=5) means that for each landing, the minimum number of buckets to be sampled would be set to 5 [i.e, only values of 5 or higher will be in output]
		# if one restricts calculations to landings with nbuc_obs>=x it makes sense to set min_n = x
	min_n<-5

# results per fisheryArea [all data with nbuc_obs>1, min_n set to 5]
	# 90% among the worst case scenarios
	summary_090<-rbindlist(lapply(split(summariseMax(x = dat, group=c("lanID","fisheryArea"), min_n=5),by="fisheryArea"), function(x) cbind(x[1,2],nLanIDs=nrow(x),round(t(apply(x[,4:ncol(x)],2,quantile, type=7, prob=c(0.90)))))))[order(fisheryArea),]
	# 95% among the worst case scenarios
	summary_095<-rbindlist(lapply(split(summariseMax(x = dat, group=c("lanID","fisheryArea"), min_n=5),by="fisheryArea"), function(x) {cbind(x[1,2],nLanIDs=nrow(x), round(t(apply(x[,4:ncol(x)],2, quantile, type=7, prob=c(0.95)))))}))[order(fisheryArea),]
	
# results per fisheryArea [all data with nbuc_obs>min_bucs_obs; min_n set to min_bucs_obs]	

	# 50% among the worst case scenarios [restricted to nbuc_obs>min_bucs_obs
	summary_050_min_bucs_obs_5<-rbindlist(lapply(split(summariseMax(x = dat[nbuc_obs>=min_bucs_obs,], group=c("lanID","fisheryArea"), min_n=min_n),by="fisheryArea"), function(x) cbind(x[1,2],nLanIDs=nrow(x),round(t(apply(x[,4:ncol(x)],2,quantile, type=7, prob=c(0.50)))))))[order(fisheryArea),]
	# 70% among the worst case scenarios [restricted to nbuc_obs>min_bucs_obs
	summary_070_min_bucs_obs_5<-rbindlist(lapply(split(summariseMax(x = dat[nbuc_obs>=min_bucs_obs,], group=c("lanID","fisheryArea"), min_n=min_n),by="fisheryArea"), function(x) cbind(x[1,2],nLanIDs=nrow(x),round(t(apply(x[,4:ncol(x)],2,quantile, type=7, prob=c(0.70)))))))[order(fisheryArea),]
	# 75% among the worst case scenarios [restricted to nbuc_obs>min_bucs_obs
	summary_075_min_bucs_obs_5<-rbindlist(lapply(split(summariseMax(x = dat[nbuc_obs>=min_bucs_obs,], group=c("lanID","fisheryArea"), min_n=min_n),by="fisheryArea"), function(x) cbind(x[1,2],nLanIDs=nrow(x),round(t(apply(x[,4:ncol(x)],2,quantile, type=7, prob=c(0.75)))))))[order(fisheryArea),]
	# 90% among the worst case scenarios [restricted to nbuc_obs>min_bucs_obs
	summary_090_min_bucs_obs_5<-rbindlist(lapply(split(summariseMax(x = dat[nbuc_obs>=min_bucs_obs,], group=c("lanID","fisheryArea"), min_n=min_n),by="fisheryArea"), function(x) cbind(x[1,2],nLanIDs=nrow(x),round(t(apply(x[,4:ncol(x)],2,quantile, type=7, prob=c(0.90)))))))[order(fisheryArea),]
	# 95% among the worst case scenarios [restricted to nbuc_obs>min_bucs_obs
	summary_095_min_bucs_obs_5<-rbindlist(lapply(split(summariseMax(x = dat[nbuc_obs>=min_bucs_obs,], group=c("lanID","fisheryArea"), min_n=min_n),by="fisheryArea"), function(x) cbind(x[1,2],nLanIDs=nrow(x),round(t(apply(x[,4:ncol(x)],2,quantile, type=7, prob=c(0.95)))))))[order(fisheryArea),]
	# 100% among the worst case scenarios [restricted to nbuc_obs>min_bucs_obs
	summary_1_min_bucs_obs_5<-rbindlist(lapply(split(summariseMax(x = dat[nbuc_obs>=min_bucs_obs,], group=c("lanID","fisheryArea"), min_n=min_n),by="fisheryArea"), function(x) cbind(x[1,2],nLanIDs=nrow(x),round(t(apply(x[,4:ncol(x)],2,quantile, type=7, prob=c(1)))))))[order(fisheryArea),]

	# 90% percentile (adjusted) to last observation within target prob
	summary_090_min_bucs_obs_5_adj<-rbindlist(lapply(split(summariseMax(x = dat[nbuc_obs>=min_bucs_obs,], group=c("lanID","fisheryArea"), min_n=min_n),by="fisheryArea"), function(x) {cbind(x[1,2],nLanIDs=nrow(x),round(t(apply(x[,4:ncol(x)],2,determineHighestObsWithinPercentile, target_prob=0.90))))}))[order(fisheryArea),]
	# 95% percentile (adjusted) to last observation within target prob
	summary_095_min_bucs_obs_5_adj<-rbindlist(lapply(split(summariseMax(x = dat[nbuc_obs>=min_bucs_obs,], group=c("lanID","fisheryArea"), min_n=min_n),by="fisheryArea"), function(x) {cbind(x[1,2],nLanIDs=nrow(x),round(t(apply(x[,4:ncol(x)],2,determineHighestObsWithinPercentile, target_prob=0.95))))}))[order(fisheryArea),]
	
	# prints results based on a minimum amount of landings
	min_n_landings<-14
	summary_050_min_bucs_obs_5[nLanIDs>=14,]
	summary_070_min_bucs_obs_5[nLanIDs>=14,]
	summary_075_min_bucs_obs_5[nLanIDs>=14,]
	summary_090_min_bucs_obs_5[nLanIDs>=14,]
	summary_095_min_bucs_obs_5[nLanIDs>=14,]
	summary_1_min_bucs_obs_5[nLanIDs>=14,]
	summary_090_min_bucs_obs_5_adj[nLanIDs>=14,]
	summary_095_min_bucs_obs_5_adj[nLanIDs>=14,]
	

#===============================================
# main results (agreed scenarios)
#===============================================

dir_results<-paste0("results_SLU/",target_country,"/main_results/")
dir.create(dir_results, showWarnings=FALSE, recursive=T)

# saves all observed data and core lanID results
fwrite(dat[nbuc_obs>1,], file=paste0(dir_results,target_country,"_lanID_results_",format(Sys.Date(), format="%Y%m%d"),".csv"))

# saves results
fwrite(summary_050_min_bucs_obs_5[,1:7], file=paste0(dir_results, target_country,"_summary_070wcs_min_bucs_obs_",min_bucs_obs,"_min_n_",min_n,"_",format(Sys.Date(), format="%Y%m%d"),".csv"))
fwrite(summary_070_min_bucs_obs_5[,1:7], file=paste0(dir_results, target_country,"_summary_070wcs_min_bucs_obs_",min_bucs_obs,"_min_n_",min_n,"_",format(Sys.Date(), format="%Y%m%d"),".csv"))
fwrite(summary_090_min_bucs_obs_5[,1:7], file=paste0(dir_results, target_country,"_summary_090wcs_min_bucs_obs_",min_bucs_obs,"_min_n_",min_n,"_",format(Sys.Date(), format="%Y%m%d"),".csv"))
fwrite(summary_095_min_bucs_obs_5[,1:7], file=paste0(dir_results, target_country,"_summary_095wcs_min_bucs_obs_",min_bucs_obs,"_min_n_",min_n,"_",format(Sys.Date(), format="%Y%m%d"),".csv"))
fwrite(summary_090_min_bucs_obs_5_adj[,1:7], file=paste0(dir_results, target_country,"_summary_090wcs_min_bucs_obs_",min_bucs_obs,"_min_n_",min_n,"_adj_",format(Sys.Date(), format="%Y%m%d"),".csv"))
fwrite(summary_095_min_bucs_obs_5_adj[,1:7], file=paste0(dir_results, target_country,"_summary_095wcs_min_bucs_obs_",min_bucs_obs,"_min_n_",min_n,"_adj_",format(Sys.Date(), format="%Y%m%d"),".csv"))

