# estimates the error margin under for a planned sample size
#'
#' @param dat is dataset produced by estimaSppComp.r
#' @param plan is a data.frame with columns size_upp, n and freq 
#' @param target_species is a vector of one or more target species
#' @param round_res should the results be rounded? defaults to TRUE.
#'
#' @return a lost containing the data analysed
#' 
#' @details plan describes the planned sampling in terms of landing size categories. In plan, 
#' size_upp is the upper limit (in tonnes) of the weight categories [need to be ordered], 
#' n is the sample size to apply on it and 
#' freq is the interval (in tonnes) fo data collection above the last size_upp
#'
#' note: if a species has 0 present in the samples of a landing it is assumed to be estimated precisely (em=0)
#' E.g., data.frame(size_upp=c(20,29,39, 49, 100, 300), n=c(7,9,12,15,18,21), freq_above=25)
#'
#' requires findPlannedSampleSizeForLanding
#' 
#' @examples
#' \dontrun{
#' findPlannedSampleSizeForLanding(21, data.frame(size_upp=c(20,29,39, 49, 100, 300), n=c(7,9,12,15,18,21), freq_above=25))
#' }

estimateErrorMarginUnderSampleSize<-function(dat, plan, target_species, min_n=5, round_res=TRUE){

#browser()

# subsets landings with n>1 buckets sampled
dat1<-dat[nbuc_obs>1,]

# associates planned sample size based on totWeight_obs
dat1$proposed_sample_size <- sapply(dat1$totWeight_obs/1000, function(y) findPlannedSampleSizeForLanding(y, plan))

# estimates em of spp [perc/prop and sppWeight] and cv [only sppWeight]
dat1[,pred_em_perc_totWeight:=1.96*sqrt((1/(bucWeightmean_obs^2 ))*sppPercWeight_s2/proposed_sample_size)]
dat1[,pred_em_weight:=totWeight_obs*1.96*sqrt((1/(bucWeightmean_obs^2 ))*sppPercWeight_s2/proposed_sample_size)]
dat1[,pred_em_perc_sppWeight:=pred_em_weight/sppWeight_estim]
dat1[,pred_cv_sppWeight:=round(pred_em_weight/1.96/sppWeight_estim,2)]
dat1[,obs_em_perc_sppWeight:=sppWeight_estim_errMargin/sppWeight_estim]
#dat2<-dat1[sppWeight_estim>0,.N, .(fisheryArea, lanID, nbuc_obs, sp, sppPercWeight_estim, sppPercWeight_estim_errMargin, pred_em_perc_totWeight, sppWeight_estim, sppWeight_estim_errMargin, pred_em_perc_sppWeight)]
dat2<-dat1[,.N, .(fisheryArea, lanID, nbuc_obs, sp, sppPercWeight_estim, sppPercWeight_estim_errMargin, pred_em_perc_totWeight, sppWeight_estim, sppWeight_estim_errMargin, pred_em_perc_sppWeight, obs_em_perc_sppWeight)]

dat2$pred_em_perc_sppWeight[dat2$sppWeight_estim==0]<-0
dat2$sppWeight_estim_errMargin[dat2$sppWeight_estim==0]<-0
dat2$obs_em_perc_sppWeight[dat2$sppWeight_estim==0]<-0

# data with nbuc_obs>1
	# proportions: how many spp <0.05 in pred_em_perc_totWeight [per fishery]
		#dat1[sppWeight_estim>0, max(pred_em_perc_totWeight), .(lanID, fisheryArea)]
		res_perc_totWeight_all <- dat2[, list(past_wcs=max(sppPercWeight_estim_errMargin)<0.05, wcs=max(pred_em_perc_totWeight)<0.05), .(lanID, fisheryArea)][,list(Past_Perc_less_0.05=sum(past_wcs)/.N, Plan_Perc_less_0.05=sum(wcs)/.N),.(fisheryArea)]
		targetSpp_res_perc_totWeight_all <- dat2[sp %in% target_species,][, list(past_wcs=max(sppPercWeight_estim_errMargin)<0.05, wcs=max(pred_em_perc_totWeight)<0.05), .(lanID, fisheryArea)][,list(Past_Perc_less_0.05=sum(past_wcs)/.N, Plan_Perc_less_0.05=sum(wcs)/.N),.(fisheryArea)]
		bycatch_res_perc_totWeight_all <- dat2[!sp %in% target_species,][, list(past_wcs=max(sppPercWeight_estim_errMargin)<0.05, wcs=max(pred_em_perc_totWeight)<0.05), .(lanID, fisheryArea)][,list(Past_Perc_less_0.05=sum(past_wcs)/.N, Plan_Perc_less_0.05=sum(wcs)/.N),.(fisheryArea)]

	# absolute values: how many spp <0.05 in pred_em_perc_sppWeight [per fishery]
		#dat1[sppWeight_estim>0, max(pred_em_perc_sppWeight), .(lanID, fisheryArea)]
		res_abs_sppWeight_all <- dat2[, list(past_wcs=max(obs_em_perc_sppWeight)<0.05, wcs=max(pred_em_perc_sppWeight)<0.05), .(lanID, fisheryArea)][,list(Past_Perc_less_0.05=sum(past_wcs)/.N, Plan_Perc_less_0.05=sum(wcs)/.N),.(fisheryArea)]
		targetSpp_res_abs_sppWeight_all <- dat2[sp %in% target_species,][, list(past_wcs=max(obs_em_perc_sppWeight)<0.05, wcs=max(pred_em_perc_sppWeight)<0.05), .(lanID, fisheryArea)][,list(Past_Perc_less_0.05=sum(past_wcs)/.N, Plan_Perc_less_0.05=sum(wcs)/.N),.(fisheryArea)]
		#[,list(Past_Perc_less_0.05=sum(past_wcs)/.N, Plan_Perc_less_0.05=sum(wcs)/.N),.(fisheryArea)]
		bycatchSpp_res_abs_sppWeight_all <- dat2[!sp %in% target_species,][, list(past_wcs=max(obs_em_perc_sppWeight)<0.05, wcs=max(pred_em_perc_sppWeight)<0.05), .(lanID, fisheryArea)][,list(Past_Perc_less_0.05=sum(past_wcs)/.N, Plan_Perc_less_0.05=sum(wcs)/.N),.(fisheryArea)]

# data with nbuc_obs>= min_n

	# proportions: how many spp <0.05 in pred_em_perc_totWeight [per fishery]
		res_perc_totWeight_nbucs_obs_5 <- dat2[nbuc_obs>= min_n, list(past_wcs=max(sppPercWeight_estim_errMargin)<0.05, wcs=max(pred_em_perc_totWeight)<0.05), .(lanID, fisheryArea)][,list(Past_Perc_less_0.05=sum(past_wcs)/.N, Plan_Perc_less_0.05=sum(wcs)/.N),.(fisheryArea)]
		targetSpp_res_perc_totWeight_nbucs_obs_5 <- dat2[nbuc_obs>= min_n & sp %in% target_species,][, list(past_wcs=max(sppPercWeight_estim_errMargin)<0.05, wcs=max(pred_em_perc_totWeight)<0.05), .(lanID, fisheryArea)][,list(Past_Perc_less_0.05=sum(past_wcs)/.N, Plan_Perc_less_0.05=sum(wcs)/.N),.(fisheryArea)]
		bycatchSpp_res_perc_totWeight_nbucs_obs_5 <- dat2[nbuc_obs>= min_n & !sp %in% target_species,][, list(past_wcs=max(sppPercWeight_estim_errMargin)<0.05, wcs=max(pred_em_perc_totWeight)<0.05), .(lanID, fisheryArea)][,list(Past_Perc_less_0.05=sum(past_wcs)/.N, Plan_Perc_less_0.05=sum(wcs)/.N),.(fisheryArea)]

	# absolute values: how many spp <0.05 in pred_em_perc_sppWeight [per fishery]
		res_abs_sppWeight_nbucs_obs_5 <- dat2[nbuc_obs>= min_n, list(past_wcs=max(obs_em_perc_sppWeight)<0.05, wcs=max(pred_em_perc_sppWeight)<0.05), .(lanID, fisheryArea)][,list(Past_Perc_less_0.05=sum(past_wcs)/.N, Plan_Perc_less_0.05=sum(wcs)/.N),.(fisheryArea)]
		targetSpp_res_abs_sppWeight_nbucs_obs_5 <- dat2[nbuc_obs>= min_n & sp %in% target_species,][, list(past_wcs=max(obs_em_perc_sppWeight)<0.05, wcs=max(pred_em_perc_sppWeight)<0.05), .(lanID, fisheryArea)][,list(Past_Perc_less_0.05=sum(past_wcs)/.N, Plan_Perc_less_0.05=sum(wcs)/.N),.(fisheryArea)]
		bycatchSpp_res_perc_sppWeight_nbucs_obs_5 <- dat2[nbuc_obs>= min_n & !sp %in% target_species,][, list(past_wcs=max(obs_em_perc_sppWeight)<0.05, wcs=max(pred_em_perc_sppWeight)<0.05), .(lanID, fisheryArea)][,list(Past_Perc_less_0.05=sum(past_wcs)/.N, Plan_Perc_less_0.05=sum(wcs)/.N),.(fisheryArea)]

out <- list(dat1=dat1, dat2=dat2, summary =  
rbind(
cbind(data = "all", type="proportion", spp = c("all","target","bycatch"), rbind(res_perc_totWeight_all, targetSpp_res_perc_totWeight_all, bycatch_res_perc_totWeight_all)
),
cbind(data = "all", type="absolute", spp = c("all","target","bycatch"), rbind(res_abs_sppWeight_all, targetSpp_res_abs_sppWeight_all, bycatchSpp_res_abs_sppWeight_all)
),
cbind(data = paste0("min_n_", min_n), type="proportion", spp = c("all","target","bycatch"), rbind(res_perc_totWeight_nbucs_obs_5, targetSpp_res_perc_totWeight_nbucs_obs_5, bycatchSpp_res_perc_totWeight_nbucs_obs_5)
),
cbind(data = paste0("min_n_", min_n), type="absolute", spp = c("all","target","bycatch"), rbind(res_abs_sppWeight_nbucs_obs_5, targetSpp_res_abs_sppWeight_nbucs_obs_5, bycatchSpp_res_perc_sppWeight_nbucs_obs_5)
)
,fill=T)
)							

if(round_res) { out$summary$Past_Perc_less_0.05<-round(out$summary$Past_Perc_less_0.05,3); out$summary$Plan_Perc_less_0.05<-round(out$summary$Plan_Perc_less_0.05,3) }

out

}
