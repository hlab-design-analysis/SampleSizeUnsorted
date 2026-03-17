#===============================================	
# evaluation of proposed sample sizes [0.05]
#===============================================
# estimates the cv that would have been achieved in each species under a proposed sample size

#source("estimSppComp.r")

# reads function
source("R/findPlannedSampleSizeForLanding.R")


# dev: results in terms of target species; comparison to present; evaluate efca original proposal

#=====================
# Baltic_HERSPR_HUC
#=====================
plan <- data.frame(size_upp=c(20,29,39, 49, 100, 300), n=c(7,9,12,15,18,21), freq_above=25)
	# test goal 
	findPlannedSampleSizeForLanding(1000, plan)==49
	# 
	testDat<-dat[fisheryArea %in% c("Baltic_HERSPR_HUC"),]
	out<-estimate_precision_from_sample_size(testDat, plan, target_species = c("HER"))
	out

#=====================
# Baltic_HERSPR_IND
#=====================
plan <- data.frame(size_upp=c(20,29,39, 49, 100, 300), n=c(7,9,12,15,18,21), freq_above=25)
	# test goal 
	findPlannedSampleSizeForLanding(1000, plan)==49
	# 
	testDat<-dat[fisheryArea %in% c("Baltic_HERSPR_IND"),]
	out<-estimate_precision_from_sample_size(testDat, plan, target_species = c("HER","SPR"))
	out
	
#=====================
# NSea_SPR
#=====================
plan <- data.frame(size_upp=c(50,100), n=c(5,8), freq_above=25)	
	# test goal 
	findPlannedSampleSizeForLanding(250, plan)==14
	# 
	testDat<-dat[fisheryArea %in% c("NSea_SPR"),]
	out<-estimate_precision_from_sample_size(testDat, plan)
	out

#=====================
# Med_SPF
#=====================
plan <- data.frame(size_upp=c(1,2,3,4), n=c(5,6,7,8), freq_above=1)	
	# test goal 
	findPlannedSampleSizeForLanding(4, plan)==9
	# 
	testDat<-dat[fisheryArea %in% c("Med_SPF"),]
	out<-estimate_precision_from_sample_size(testDat, plan)
	out
	

estimate_precision_from_sample_size<-function(dat, plan, target_species){

#browser()

dat1<-dat[nbuc_obs>1,]

# associates planned sample size based on totWeight_obs
dat1$proposed_sample_size <- sapply(dat1$totWeight_obs/1000, function(y) findPlannedSampleSizeForLanding(y, plan))

# estimates em of spp [perc/prop and sppWeight] and cv [only sppWeight]
dat1[,pred_em_perc_totWeight:=1.96*sqrt((1/(bucWeightmean_obs^2 ))*sppPercWeight_s2/proposed_sample_size)]
dat1[,pred_em_weight:=totWeight_obs*1.96*sqrt((1/(bucWeightmean_obs^2 ))*sppPercWeight_s2/proposed_sample_size)]
dat1[,pred_em_perc_sppWeight:=pred_em_weight/sppWeight_estim]
dat1[,pred_cv_sppWeight:=round(pred_em_weight/1.96/sppWeight_estim,2)]

# all data nbuc_obs>1

	# proportions: how many spp <0.05 in pred_em_perc_totWeight [per fishery]
		dat1[sppWeight_estim>0, max(pred_em_perc_totWeight), .(lanID, fisheryArea)]
		res_perc_totWeight_all <- dat1[sppWeight_estim>0, list(past_wcs=max(sppPercWeight_estim_errMargin)<0.05, wcs=max(pred_em_perc_totWeight)<0.05), .(lanID, fisheryArea)][,list(Past_Perc_less_0.05=sum(past_wcs)/.N, Plan_Perc_less_0.05=sum(wcs)/.N),.(fisheryArea)]
		targetSpp_res_perc_totWeight_all <- dat1[sp %in% target_species,][sppWeight_estim>0, list(past_wcs=max(sppPercWeight_estim_errMargin)<0.05, wcs=max(pred_em_perc_totWeight)<0.05), .(lanID, fisheryArea)][,list(Past_Perc_less_0.05=sum(past_wcs)/.N, Plan_Perc_less_0.05=sum(wcs)/.N),.(fisheryArea)]
		bycatch_res_perc_totWeight_all <- dat1[!sp %in% target_species,][sppWeight_estim>0, list(past_wcs=max(sppPercWeight_estim_errMargin)<0.05, wcs=max(pred_em_perc_totWeight)<0.05), .(lanID, fisheryArea)][,list(Past_Perc_less_0.05=sum(past_wcs)/.N, Plan_Perc_less_0.05=sum(wcs)/.N),.(fisheryArea)]

	# absolute values: how many spp <0.05 in pred_em_perc_sppWeight [per fishery]
		dat1[sppWeight_estim>0, max(pred_em_perc_sppWeight), .(lanID, fisheryArea)]
		res_abs_sppWeight_all <- dat1[sppWeight_estim>0, list(past_wcs=max(sppWeight_estim_errMargin/sppWeight_estim)<0.05, wcs=max(pred_em_perc_sppWeight)<0.05), .(lanID, fisheryArea)][,list(Past_Perc_less_0.05=sum(past_wcs)/.N, Plan_Perc_less_0.05=sum(wcs)/.N),.(fisheryArea)]
		targetSpp_res_abs_sppWeight_all <- dat1[sp %in% target_species,][sppWeight_estim>0, list(past_wcs=max(sppWeight_estim_errMargin/sppWeight_estim)<0.05, wcs=max(pred_em_perc_sppWeight)<0.05), .(lanID, fisheryArea)][,list(Past_Perc_less_0.05=sum(past_wcs)/.N, Plan_Perc_less_0.05=sum(wcs)/.N),.(fisheryArea)]
		#[,list(Past_Perc_less_0.05=sum(past_wcs)/.N, Plan_Perc_less_0.05=sum(wcs)/.N),.(fisheryArea)]
		bycatchSpp_res_abs_sppWeight_all <- dat1[!sp %in% target_species,][sppWeight_estim>0, list(past_wcs=max(sppWeight_estim_errMargin/sppWeight_estim)<0.05, wcs=max(pred_em_perc_sppWeight)<0.05), .(lanID, fisheryArea)][,list(Past_Perc_less_0.05=sum(past_wcs)/.N, Plan_Perc_less_0.05=sum(wcs)/.N),.(fisheryArea)]

# all data nbuc_obs>=5

	# proportions: how many spp <0.05 in pred_em_perc_totWeight [per fishery]
		res_perc_totWeight_nbucs_obs_5 <- dat1[sppWeight_estim>0 & nbuc_obs>=5, list(past_wcs=max(sppPercWeight_estim_errMargin)<0.05, wcs=max(pred_em_perc_totWeight)<0.05), .(lanID, fisheryArea)][,list(Past_Perc_less_0.05=sum(past_wcs)/.N, Plan_Perc_less_0.05=sum(wcs)/.N),.(fisheryArea)]
		targetSpp_res_perc_totWeight_nbucs_obs_5 <- dat1[sp %in% target_species,][sppWeight_estim>0 & nbuc_obs>=5, list(past_wcs=max(sppPercWeight_estim_errMargin)<0.05, wcs=max(pred_em_perc_totWeight)<0.05), .(lanID, fisheryArea)][,list(Past_Perc_less_0.05=sum(past_wcs)/.N, Plan_Perc_less_0.05=sum(wcs)/.N),.(fisheryArea)]
		bycatchSpp_res_perc_totWeight_nbucs_obs_5 <- dat1[!sp %in% target_species,][sppWeight_estim>0 & nbuc_obs>=5, list(past_wcs=max(sppPercWeight_estim_errMargin)<0.05, wcs=max(pred_em_perc_totWeight)<0.05), .(lanID, fisheryArea)][,list(Past_Perc_less_0.05=sum(past_wcs)/.N, Plan_Perc_less_0.05=sum(wcs)/.N),.(fisheryArea)]

	# absolute values: how many spp <0.05 in pred_em_perc_sppWeight [per fishery]
		res_abs_sppWeight_nbucs_obs_5 <- dat1[sppWeight_estim>0 & nbuc_obs>=5, list(past_wcs=max(sppWeight_estim_errMargin/sppWeight_estim)<0.05, wcs=max(pred_em_perc_sppWeight)<0.05), .(lanID, fisheryArea)][,list(Past_Perc_less_0.05=sum(past_wcs)/.N, Plan_Perc_less_0.05=sum(wcs)/.N),.(fisheryArea)]
		targetSpp_res_abs_sppWeight_nbucs_obs_5 <- dat1[sp %in% target_species,][sppWeight_estim>0 & nbuc_obs>=5, list(past_wcs=max(sppWeight_estim_errMargin/sppWeight_estim)<0.05, wcs=max(pred_em_perc_sppWeight)<0.05), .(lanID, fisheryArea)][,list(Past_Perc_less_0.05=sum(past_wcs)/.N, Plan_Perc_less_0.05=sum(wcs)/.N),.(fisheryArea)]
		bycatchSpp_res_perc_sppWeight_nbucs_obs_5 <- dat1[!sp %in% target_species,][sppWeight_estim>0 & nbuc_obs>=5, list(past_wcs=max(sppWeight_estim_errMargin/sppWeight_estim)<0.05, wcs=max(pred_em_perc_sppWeight)<0.05), .(lanID, fisheryArea)][,list(Past_Perc_less_0.05=sum(past_wcs)/.N, Plan_Perc_less_0.05=sum(wcs)/.N),.(fisheryArea)]

list(dat=dat1, summary =  
rbind(
cbind(data = "all", type="proportion", spp = c("all","target","bycatch"), rbind(res_perc_totWeight_all, targetSpp_res_perc_totWeight_all, bycatch_res_perc_totWeight_all)
),
cbind(data = "all", type="absolute", spp = c("all","target","bycatch"), rbind(res_abs_sppWeight_all, targetSpp_res_abs_sppWeight_all, bycatchSpp_res_abs_sppWeight_all)
),
cbind(data = "min_n", type="proportion", spp = c("all","target","bycatch"), rbind(res_perc_totWeight_nbucs_obs_5, targetSpp_res_perc_totWeight_nbucs_obs_5, bycatchSpp_res_perc_totWeight_nbucs_obs_5)
),
cbind(data = "min_n", type="absolute", spp = c("all","target","bycatch"), rbind(res_abs_sppWeight_nbucs_obs_5, targetSpp_res_abs_sppWeight_nbucs_obs_5, bycatchSpp_res_perc_sppWeight_nbucs_obs_5)
)
,fill=T)
)							

}
	
# ref Baltic_HERSPR_IND
ref <- data.frame(size_upp=c(20,29,39, 49, 100, 300), n=c(7,9,12,15,18,21), freq_above=25)


head(out$dat)

















proposed_sample_size0<-summary_095_min_bucs_obs_5_adj[nLanIDs>10,.(fisheryArea,n_0.050)]

proposed_sample_size1<-data.table(fisheryArea=c('Baltic_HERSPR_HUC','Baltic_HERSPR_IND','Bothnia_FVE','Bothnia_HER','GoR_HER_HUC','Med_SPF','NAtlantic_MAC','NAtlantic_WHB','NSea_HER','NSea_NOP','NSea_SAN','NSea_SPR'),
								 n_0.050=c(Baltic_HERSPR_HUC =  24,
								 Baltic_HERSPR_IND = 53 ,
								 Bothnia_FVE =  20,
								 Bothnia_HER =  4,
								 GoR_HER_HUC = 3,
								 Med_SPF = 6,
								 NAtlantic_MAC =  9 ,
								 NAtlantic_WHB =  1,
								 NSea_HER =  3,
								 NSea_NOP =  18,
								 NSea_SAN =  2,
								 NSea_SPR =  11) #21 alloc; 33 as species; 21 deleted; 12 expert
								)





























estimate_precision_from_sample_size<-function(dat, proposed_sample_size){

dat1<-dat[nbuc_obs>1,]
dat1$proposed_sample_size <- proposed_sample_size$n_0.050[match(dat1$fisheryArea,proposed_sample_size$fisheryArea)]

dat1[,pred_em_perc_totWeight:=1.96*sqrt((1/(bucWeightmean_obs^2 ))*sppPercWeight_s2/proposed_sample_size)]
dat1[,pred_em_weight:=totWeight_obs*1.96*sqrt((1/(bucWeightmean_obs^2 ))*sppPercWeight_s2/proposed_sample_size)]
dat1[,pred_em_perc_sppWeight:=pred_em_weight/sppWeight_estim]
dat1[,pred_cv:=round(pred_em_weight/1.96/sppWeight_estim,2)]

# per fishery how many <0.05 in pred_em_perc_totWeight
dat1[sppWeight_estim>0, max(pred_em_perc_totWeight), .(lanID, fisheryArea)]
res_perc_totWeight_all <- dat1[sppWeight_estim>0, list(wcs=max(pred_em_perc_totWeight)<0.05), .(lanID, fisheryArea)][,list(Perc_less_0.05=sum(wcs)/.N),.(fisheryArea)]
res_perc_totWeight_nbucs_obs_5 <- dat1[sppWeight_estim>0 & nbuc_obs>=5, list(wcs=max(pred_em_perc_totWeight)<0.05), .(lanID, fisheryArea)][,list(Perc_less_0.050=sum(wcs)/.N),.(fisheryArea)]

# per fishery how many <0.05 in pred_em_perc_sppWeight
dat1[sppWeight_estim>0, max(pred_em_perc_sppWeight), .(lanID, fisheryArea)]
res_perc_sppWeight_all <- dat1[sppWeight_estim>0, list(wcs=max(pred_em_perc_sppWeight)<0.05), .(lanID, fisheryArea)][,list(Perc_less_0.05=sum(wcs)/.N),.(fisheryArea)]
res_perc_sppWeight_nbucs_obs_5 <- dat1[sppWeight_estim>0 & nbuc_obs>=5, list(wcs=max(pred_em_perc_sppWeight)<0.05), .(lanID, fisheryArea)][,list(Perc_less_0.050=sum(wcs)/.N),.(fisheryArea)]

list(dat=dat1, res_perc_totWeight_all = res_perc_totWeight_all, res_perc_totWeight_nbucs_obs_5 =res_perc_totWeight_nbucs_obs_5,
				res_perc_sppWeight_all = res_perc_sppWeight_all, res_perc_sppWeight_nbucs_obs_5 = res_perc_sppWeight_nbucs_obs_5)

}

res_0<-estimate_precision_from_sample_size(dat, proposed_sample_size0)$res_perc_totWeight_nbucs_obs_5
res_1<-estimate_precision_from_sample_size(dat, proposed_sample_size1)$res_perc_totWeight_nbucs_obs_5

res_combined<-merge(merge(res_0, proposed_sample_size0, by="fisheryArea"), 
	merge(res_1, proposed_sample_size1, by="fisheryArea"), by="fisheryArea")
colnames(res_combined)<-gsub("50.x","50.0",colnames(res_combined))
colnames(res_combined)<-gsub("50.y","50.1",colnames(res_combined))
res_combined$nLanIDs<-summary_095_min_bucs_obs_5$nLanIDs[match(res_combined$fisheryArea,summary_095_min_bucs_obs_5$fisheryArea)]
res_combined