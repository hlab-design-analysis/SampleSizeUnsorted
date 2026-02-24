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
if(target_country=="all_countries_USO_expert_judgement")  dat <- readRDS("data/all_countries_USO_expert_judgement.rds"); file.info("data/all_countries_USO_expert_judgement.rds")
if(target_country=="all_countries_USO_deleted")  dat <- readRDS("data/all_countries_USO_deleted.rds")
if(target_country=="all_countries_USO_allocated")  dat <- readRDS("data/all_countries_USO_allocated.rds")
if(target_country=="all_countries_USO_as_species")  dat <- readRDS("data/all_countries_USO_as_species.rds"); file.info("data/all_countries_USO_as_species.rds")

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
# demo of result summaries
#===============================================

# example: different types of summaries [with adjustment of n<2 to 2]
summariseMean(x = dat, group="sp", min_n=0) # mean sample size by sp (across landings)
summariseMedian(x = dat, group="sp", min_n=0) # mean sample size by sp  (across landings)
summariseMax(x = dat, group="lanID", min_n=2) # sample size needed to characterize worst species (aka all species) in each lanID [with adjustment of n<2 to 2]
summariseQuantiles(x = dat, group="sp", probs=c(0.025,0.975), min_n=2) # quantiles of sample size by sp (determined across landings)[with adjustment of n<2 to 2]

# example: sample size needed to characterize worst species (aka all species) in all landings of a fishery [with adjustment of n<5 to 5]
	# first determines sample size needed for worst species within each landing, then computes the percentile 95% of those values
summariseMax(x = dat, group=c("fisheryArea"), min_n=5)

# example: 95% percentile of the worst case scenarios [all fisheryAreas]
	# first determines sample size needed for worst species within each landing, then computes the percentile 95% of those values
apply(summariseMax(x = dat, group="lanID", min_n=5)[,3:5],2, quantile, prob=c(0.95))

# note on min_n: 
	# adjustment of min_n to a different values (e.g., min_n=5) means that for each landing, the minimum number of buckets to be sampled would be set to 5
	# if one only uses the landings with nbuc_obs>=x in calculations it makes sense to set min_n to x

#===============================================
# main results (agreed scenarios)
#===============================================

# results per fisheryArea [all data with nbuc_obs>1, min_n set to 5]
	# 90% among the worst case scenarios
	summary_090<-rbindlist(lapply(split(summariseMax(x = dat, group=c("lanID","fisheryArea"), min_n=5),by="fisheryArea"), function(x) cbind(x[1,2],nLanIDs=nrow(x),round(t(apply(x[,4:ncol(x)],2,quantile, type=7, prob=c(0.90)))))))[order(fisheryArea),]
	# 95% among the worst case scenarios
	summary_095<-rbindlist(lapply(split(summariseMax(x = dat, group=c("lanID","fisheryArea"), min_n=5),by="fisheryArea"), function(x) {cbind(x[1,2],nLanIDs=nrow(x), round(t(apply(x[,4:ncol(x)],2, quantile, type=7, prob=c(0.95)))))}))[order(fisheryArea),]
	
# results per fisheryArea [all data with nbuc_obs>min_bucs_obs; min_n set to min_bucs_obs]	
	min_bucs_obs<-5
	min_n<-5
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

	# 90% percentile adjusted to last observation within target prob
	summary_090_min_bucs_obs_5_adj<-rbindlist(lapply(split(summariseMax(x = dat[nbuc_obs>=min_bucs_obs,], group=c("lanID","fisheryArea"), min_n=min_n),by="fisheryArea"), function(x) {cbind(x[1,2],nLanIDs=nrow(x),round(t(apply(x[,4:ncol(x)],2,determineHighestObsWithinPercentile, target_prob=0.90))))}))[order(fisheryArea),]
	# 95% percentile adjusted to last observation within target prob
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

dir_results<-paste0("results/",target_country,"/main_results/")
dir.create(dir_results, showWarnings=FALSE, recursive=T)

# saves all observed data and core lanID results
fwrite(dat[nbuc_obs>1,], file=paste0(dir_results,target_country,"_lanID_results_",format(Sys.Date(), format="%Y%m%d"),".csv"))

# saves results
fwrite(summary_050_min_bucs_obs_5[,1:7], file=paste0(dir_results, target_country,"_summary_070wcs_min_bucs_obs_",min_bucs_obs,"_",format(Sys.Date(), format="%Y%m%d"),".csv"))
fwrite(summary_070_min_bucs_obs_5[,1:7], file=paste0(dir_results, target_country,"_summary_070wcs_min_bucs_obs_",min_bucs_obs,"_",format(Sys.Date(), format="%Y%m%d"),".csv"))
fwrite(summary_090_min_bucs_obs_5[,1:7], file=paste0(dir_results, target_country,"_summary_090wcs_min_bucs_obs_",min_bucs_obs,"_",format(Sys.Date(), format="%Y%m%d"),".csv"))
fwrite(summary_095_min_bucs_obs_5[,1:7], file=paste0(dir_results, target_country,"_summary_095wcs_min_bucs_obs_",min_bucs_obs,"_",format(Sys.Date(), format="%Y%m%d"),".csv"))
fwrite(summary_090_min_bucs_obs_5_adj[,1:7], file=paste0(dir_results, target_country,"_summary_090wcs_min_bucs_obs_",min_bucs_obs,"_adj_",format(Sys.Date(), format="%Y%m%d"),".csv"))
fwrite(summary_095_min_bucs_obs_5_adj[,1:7], file=paste0(dir_results, target_country,"_summary_095wcs_min_bucs_obs_",min_bucs_obs,"_adj_",format(Sys.Date(), format="%Y%m%d"),".csv"))


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
	if(i=="Baltic_HERSPR_HUC") write.xlsx(x[nLanIDs>=15,], file=paste0("results_size_categ",target_prob,"_min_",min_bucs_obs,"_",format(Sys.Date(), format="%Y%m%d"),".xlsx"),sheetName=i) else write.xlsx(x[nLanIDs>14,], file=paste0("results_size_categ",target_prob,"_min_",min_bucs_obs,"_",format(Sys.Date(), format="%Y%m%d"),".xlsx"),sheetName=i, append=T)
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


	