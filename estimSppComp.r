
library(data.table)
rm(list=ls())
# load funs
	source("R/sourceAllFunctions.R")

# select data to load
	 dat <- readRDS("data/SPE.rds")
	#dat <- readRDS("data/SLU.rds")
	
	
# do a set of initial data checks on input data
doInitialChecks(dat)

# estimates weight Composition in Percentage and Absolute Weight
	# note: aboslute estimates are only calculated if totWeight_obs is provided 
dat <- estimateWeightComp (x = dat,round_Nbuc_estim=FALSE, finitePopCorr=FALSE)

#===============================================
# calculates sample size for diferent margins of error in proportion
#===============================================


		doSampleSizeGivenError(x=dat, e=c(0.03,0.05,0.07,0.10), error_type="Percent")
		
		# example
		unique(dat[nbuc_obs>1,c("lanID","sp","totWeight_obs", "nbuc_obs","bucWeightmean_obs","sppPercWeight_s2","sppPercWeight_estim","n_003","n_005","n_007","n_01")])

#===============================================
# calculates sample size for diferent margins of error in absolute weight
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



# final checks:
	if(all(!is.na(dat$totWeight_obs))){
		#sum of estimates spp weights == estimate total weight
		test1<-dat[,.N,.(lanID,sp,totWeight_obs,sppWeight_estim)][, sum(sppWeight_estim),.(lanID,totWeight_obs)][,all.equal(V1,totWeight_obs)]
		#sum of estimates spp PercWeight ==  1
		test2<-dat[,.N,.(lanID,sp,sppPercWeight_estim)][, sum(sppPercWeight_estim),.(lanID)][,all.equal(V1,rep(1,.N))] 
		if(!all(c(test1,test2))){stop()
		} else {
		test2<-dat[,.N,.(lanID,sp,sppPercWeight_estim)][, sum(sppPercWeight_estim),.(lanID)][,all.equal(V1,rep(1,.N))] 
		if(!test2) stop()}
	}

