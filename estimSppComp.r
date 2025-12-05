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

# select country/data: use 3-letter acronym or "all_countries" for entire dataset
target_country<-"SWE"
target_country<-"all_countries"

if(target_country=="SPE") dat <- readRDS("data/SPE.rds")
if(target_country=="SLU") dat <- readRDS("data/SLU.rds")
if(target_country=="DNK") dat <- readRDS("data/DNK_3rd_party.rds")
if(target_country=="FIN") dat <- readRDS("data/FIN.rds")
if(target_country=="LVA") dat <- readRDS("data/LVA.rds")
if(target_country=="EST") dat <- readRDS("data/EST.rds")
if(target_country=="IRL") dat <- readRDS("data/IRL_SFPA.rds")
if(target_country=="SWE") dat <- rbind(readRDS("data/SWE_other.rds"),readRDS("data/SWE_Baltic_HERSPR_HUC.rds"))
if(target_country=="all_countries")  dat <- readRDS("data/all_countries.rds")

	
# do a set of initial data checks on input data
doInitialChecks(dat)

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

# example: 95% percentile of the worst case scenarios
apply(summariseMax(x = dat, group="lanID", min_n=2)[,3:ncol(x)],2, quantile, prob=c(0.95))


#===============================================
# main results (agreed scenarios)
#===============================================

# per fisheryArea
	# 90% among the worst case scenarios
	summary_090<-rbindlist(lapply(split(summariseMax(x = dat, group=c("lanID","fisheryArea"), min_n=2),by="fisheryArea"), function(x) cbind(x[1,2],nLanIDs=nrow(x),round(t(apply(x[,4:ncol(x)],2,quantile, type=7, prob=c(0.90)))))))[order(fisheryArea),]
	# 95% among the worst case scenarios
	summary_095<-rbindlist(lapply(split(summariseMax(x = dat, group=c("lanID","fisheryArea"), min_n=2),by="fisheryArea"), function(x) {cbind(x[1,2],nLanIDs=nrow(x), round(t(apply(x[,4:ncol(x)],2, quantile, type=7, prob=c(0.95)))))}))[order(fisheryArea),]
	
	# 95% among the worst case scenarios [restricted to nbuc_obs>min_bucs_obs
	min_bucs_obs<-5
	summary_090_min_bucs_obs_5<-rbindlist(lapply(split(summariseMax(x = dat[nbuc_obs>min_bucs_obs,], group=c("lanID","fisheryArea"), min_n=2),by="fisheryArea"), function(x) cbind(x[1,2],nLanIDs=nrow(x),round(t(apply(x[,4:ncol(x)],2,quantile, type=7, prob=c(0.90)))))))[order(fisheryArea),]
	summary_095_min_bucs_obs_5<-rbindlist(lapply(split(summariseMax(x = dat[nbuc_obs>min_bucs_obs,], group=c("lanID","fisheryArea"), min_n=2),by="fisheryArea"), function(x) {cbind(x[1,2],nLanIDs=nrow(x), round(t(apply(x[,4:ncol(x)],2, quantile, type=7, prob=c(0.95)))))}))[order(fisheryArea),]

	# adds indicator on results being based on a minimum amount of landings
	min_n_landings<-15
	summary_090_min_bucs_obs_5[,min_n_landings:=min_n_landings]; summary_090_min_bucs_obs_5[,meets_min_n_landings:=nLanIDs>=min_n_landings]
	summary_095_min_bucs_obs_5[,min_n_landings:=min_n_landings]; summary_095_min_bucs_obs_5[,meets_min_n_landings:=nLanIDs>=min_n_landings]
	

#===============================================
# main results (agreed scenarios)
#===============================================

dir_results<-paste0("results/",target_country,"/main_results/")
dir.create(dir_results, showWarnings=FALSE, recursive=T)

# saves all observed data and core lanID results
fwrite(dat[nbuc_obs>1,], file=paste0(dir_results,target_country,"_lanID_results.csv"))

# saves results
fwrite(summary_095_min_bucs_obs_5, file=paste0(dir_results, target_country,"_summary_095wcs_min_bucs_obs_5.csv"))
fwrite(summary_090_min_bucs_obs_5, file=paste0(dir_results, target_country,"_summary_090wcs_min_bucs_obs_5.csv"))







