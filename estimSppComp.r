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
dat <- readRDS("data/SPE.rds")
#dat <- readRDS("data/SLU.rds")
	
# do a set of initial data checks on input data
doInitialChecks(dat)

# estimates weight Composition in Percentage and Absolute Weight
	# note: aboslute estimates are only calculated if totWeight_obs is provided 
dat <- estimateWeightComp (x = dat,round_Nbuc_estim=FALSE, finitePopCorr=FALSE)

#===============================================
# calculate sample size for diferent margins of error in proportion
#===============================================


		doSampleSizeGivenError(x=dat, e=c(0.03,0.05,0.07,0.10), error_type="Percent")
		
		# example
		unique(dat[nbuc_obs>1,c("lanID","sp","totWeight_obs", "nbuc_obs","bucWeightmean_obs","sppPercWeight_s2","sppPercWeight_estim","n_003","n_005","n_007","n_01")])

#===============================================
# calculate sample size for diferent margins of error in absolute weight
#===============================================

	doSampleSizeGivenError(x=dat, e=c(1000,500,100), error_type="Absolute")
		
		
		# example
		unique(dat[nbuc_obs>1,c("lanID","sp","totWeight_obs","nbuc_obs","sppPercWeight_estim","n_01","n_007","n_005","n_003", "n_1000","n_500","n_100")])


#===============================================
# summarise results
#===============================================

summariseMean(x = dat, group="sp")
summariseMedian(x = dat, group="sp")
summariseQuantiles(x = dat, group="sp", probs=c(0.025,0.975))

#===============================================
# save results
#===============================================

dir.create("results", showWarnings=FALSE)
# uncomment and tune the next line of code below to save results
# write.csv(summariseMean(x = dat, group="sp"), file="results/results.csv", row.names=FALSE)





