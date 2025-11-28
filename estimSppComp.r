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

# comment/uncomment to select data to load
# dat <- readRDS("data/SPE.rds")
# dat <- readRDS("data/SLU.rds")
# dat <- readRDS("data/DNK_3rd_party.rds")
# dat <- readRDS("data/FIN.rds")
# dat <- readRDS("data/LVA.rds")
# dat <- readRDS("data/EST.rds")
# dat <- readRDS("data/IRL_SFPA.rds")
# dat <- readRDS("data/SWE_Baltic_HERSPR_HUC.rds")
# dat <- readRDS("data/SWE_other.rds")
# dat <- readRDS("data/all_countries.rds")
dat <- rbind(readRDS("data/SWE_other.rds"),readRDS("data/SWE_Baltic_HERSPR_HUC.rds"))
	
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
# summarise results
#===============================================

summariseMean(x = dat, group="sp", min_n=2) # mean by sp
summariseMedian(x = dat, group="sp", min_n=2)
summariseMax(x = dat, group="lanID", min_n=2)
summariseQuantiles(x = dat, group="sp", probs=c(0.025,0.975), min_n=2)
summariseQuantiles(x = dat, group=NULL, probs=c(0.025,0.975), min_n=2)

# worst case scenario
summariseMax(x = dat, group=c("fisheryArea"), min_n=2)

# 95% among the worst case scenarios
apply(summariseMax(x = dat, group="lanID", min_n=2)[,3:ncol(x)],2, quantile, prob=c(0.95))

# per fishery
	# 95% among the worst case scenarios
	summary_095<-rbindlist(lapply(split(summariseMax(x = dat, group=c("lanID","fisheryArea"), min_n=2),by="fisheryArea"), function(x) cbind(x[1,2],t(apply(x[,4:ncol(x)],2,quantile, prob=c(0.95))))))[order(fisheryArea),]
	# 90% among the worst case scenarios
	summary_090<-rbindlist(lapply(split(summariseMax(x = dat, group=c("lanID","fisheryArea"), min_n=2),by="fisheryArea"), function(x) cbind(x[1,2],t(apply(x[,4:ncol(x)],2,quantile, prob=c(0.90))))))[order(fisheryArea),]

# 95% of species landings [here 
apply(summariseMax(x = dat, group=c("lanID","sp"),min_n=2)[,4:10],2, quantile, prob=c(0.95))

#===============================================
# save results
#===============================================

dir.create("results", showWarnings=FALSE)
fwrite(dat[nbuc_obs>1,], file="results/all_countries.csv")
fwrite(summary_095, file="results/all_countries_summary_095wcs.csv")
fwrite(summary_090, file="results/all_countries_summary_090wcs.csv")
fwrite(dat[nbuc_obs>1,], file="results/SWE.csv")
fwrite(summary_095, file="results/SWE_summary_095wcs.csv")
fwrite(summary_090, file="results/SWE_summary_090wcs.csv")
write.xlsx(dat, files, append=T)

# uncomment and tune the next line of code below to save results
# write.csv(summariseMean(x = dat, group="sp"), file="results/results.csv", row.names=FALSE)






