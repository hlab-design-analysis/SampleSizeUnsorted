#source("estimSppComp.r")

#===============================================
# displays the more variable cases within each fishery
#===============================================

	min_number_lanIDs<-10
	target_fisheryAreas<-summary_095_min_bucs_obs_5[nLanIDs>=min_number_lanIDs]$fisheryArea
	ls1<-split(dat[fisheryArea %in% target_fisheryAreas,], dat[fisheryArea %in% target_fisheryAreas,]$fisheryArea)
	a<-lapply(ls1,function(x){print(head(unique(x[nbuc_obs>=5,.(fisheryArea, lanID, sp, n_0.050)])[order(-n_0.050),],8))})


#===============================================
# displays boundary cases (p95_n_0.05) within each fishery
#===============================================

# To be done

#===============================================
# displays the more variable cases in each country
#===============================================

	ls1<-split(dat, dat$ctry)
	a<-lapply(ls1,function(x){print(head(unique(x[nbuc_obs>=5,.(ctry, fisheryArea, lanID, sp, n_0.050)])[order(-n_0.050),]))})

#===============================================
# graphs of individual landings
#===============================================

	graph_dir_country<-paste0("results/",target_country,"/plots_individual_landings/")
	if(!dir.exists(graph_dir_country)) dir.create(graph_dir_country, recursive=T)

	ls1<-split(dat, dat$fisheryArea)
	for (i in names(ls1)[7])
		{
		print(i)
		tmp_fisheryArea <- ls1[[i]]
		tmp_fisheryArea$colour<-as.integer(factor(tmp_fisheryArea$sp))
		for (j in unique(tmp_fisheryArea$lanID))
		{
		#print(j)
		tmp_trip<-tmp_fisheryArea[lanID==j,]
		dat_graph<-tmp_trip[,sppWeight_obs/bucWeight_obs, by=.(lanID, bucID,sp, totWeight_obs, sppPercWeight_estim, n_0.050, colour)]
		dat_graph<-dat_graph[order(bucID),]
		dat_graph$bucOrder<-as.integer(factor(dat_graph$bucID))
		png(filename = paste0(graph_dir_country,i,"_",j,".png"),width = 560, height = 480,)
		plot(V1~bucOrder, data=dat_graph, type="n", ylim=c(0,1), xlim=c(0,50), pch=19, main=paste0(target_country,":", i,": ",j,": ",dat_graph$totWeight_obs[1], " ton\n","n_0.050: ", max(tmp_trip[sppWeight_obs>0,]$n_0.050)), xlab="bucOrder")
		for (w in unique(dat_graph$sp))
		{	
		points(V1~bucOrder, data=dat_graph[sp==w,], type="o", lty=2, pch=19, col=unique(dat_graph[sp==w,]$colour))
		text(x=max(dat_graph[sp==w,]$bucOrder), y=mean(dat_graph[sp==w,]$sppPercWeight_estim), labels=paste(w,round(mean(dat_graph[sp==w,]$sppPercWeight_estim),2)), col=unique(dat_graph[sp==w,]$colour), cex=0.7, pos=4)
		}
		dev.off()
		#savePlot(paste0(graph_dir_country,i,"_",j,".png"), type="png")
}
}

# ===================================
# histogram: sample sizes required for 95% of trips
# ===================================

graph_dir_country<-paste0("results/",target_country,"/plots_histogram_sample_sizes/")
if(!dir.exists(graph_dir_country)) dir.create(graph_dir_country, recursive=T)

length(subset_fisheryArea<-summary_095_min_bucs_obs_5[meets_min_n_landings==T,]$fisheryArea)

for (i in unique(subset_fisheryArea))
{
hist(summariseMax(x = dat, group=c("lanID","fisheryArea"), min_n=5)[fisheryArea==i,]$n_0.050, breaks=100, main=paste0(i,": samp size for 0.05 e: ",summary_095_min_bucs_obs_5[fisheryArea==i,]$nLanIDs," lanIDs"), xlab="sample size")
abline(v=summary_095_min_bucs_obs_5[fisheryArea==i,]$n_0.05, lty=2, col="orange")
abline(v=summary_090_min_bucs_obs_5[fisheryArea==i,]$n_0.05, lty=2, col="red")
legend("topright", legend=c(paste0("95% of landings (",summary_095_min_bucs_obs_5[fisheryArea==i,]$n_0.05,")"),paste0("90% of landings (",summary_090_min_bucs_obs_5[fisheryArea==i,]$n_0.05,")")), lty=2, col=c("orange","red"))
savePlot(paste0(graph_dir_country,i), type="png")
}

# ===================================
# sensitivity analysis: number of minimum buckets in landings allowed for analysis
# ===================================

tab_max_nbuc_obs<-dat[fisheryArea %in% subset_fisheryArea, list(max_nbuc_obs=max(nbuc_obs)), fisheryArea]

graph_dir_country<-paste0("results/",target_country,"/plots_sensitivity_nbuc_obs/"); dir.create(graph_dir_country, showWarnings=F)

out<-data.table()
for(j in subset_fisheryArea)
{
res<-c()
res_n<-c()
for (i in c(2,5,10,15))
{
min_bucs_obs<-i
tabela<-rbindlist(lapply(split(summariseMax(x = dat[nbuc_obs>=min_bucs_obs,], group=c("lanID","fisheryArea"), min_n=5),by="fisheryArea"), function(x) {cbind(x[1,2],nLanIDs=nrow(x), round(t(apply(x[,4:ncol(x)],2, quantile, type=7, prob=c(0.95)))))}))[order(fisheryArea),][fisheryArea==j,]
res<-c(res,tabela$n_0.050)
res_n<-c(res_n,tabela$nLanIDs)
}
windows()
a<-barplot(res, names.arg=c(2,5,10,15), xlab="min_n_bucs admitted for analysis", ylab="sample size needed for 95% at 0.05e", main=j)
text(x=a[,1], y=2, label=res_n)
savePlot(paste0(graph_dir_country,j), type="png")
out<-rbind(out, data.table(fisheryArea=j, min_nbuc_in_analysis=c(2,5,10,15), n_lanIDs_in_analysis=res_n, p95_n_0.05=res))
}

table(dat[fisheryArea %in% subset_fisheryArea,.N,.(lanID, nbuc_obs, fisheryArea)][,2:3])

#===============================================
# Relationship between number of samples and variance [per fishery]
#===============================================
target_fisheryArea <- "Baltic_HERSPR_HUC"
min_number_lanIDs<-15
windows(15,15); par(mfrow=c(3,3), oma=c(2,2,4,2))
target_fisheryAreas<-summary_095_min_bucs_obs_5[nLanIDs>=min_number_lanIDs]$fisheryArea
for(target_fisheryArea in target_fisheryAreas)
{
windows(10,10)
tmp<-unique(dat[fisheryArea==target_fisheryArea, .(lanID, nbuc_obs, n_0.050)])
# expands on missing nbuc_obs
tmp<-rbind(tmp, data.frame(lanID=NA,nbuc_obs=1:max(tmp$nbuc_obs), n_0.050=NA))
plot(n_0.050~nbuc_obs, data=rbind(tmp, data.frame(lanID=NA,nbuc_obs=1:max(tmp$nbuc_obs), n_0.050=NA)), main=target_fisheryArea)
mtext(tmp[!n_0.050=="NA",.N,.(nbuc_obs)]$N, side=3, at=tmp[!n_0.050=="NA",.N,.(nbuc_obs)]$nbuc_obs, cex=.8)
abline(h=summary_095_min_bucs_obs_5[fisheryArea==target_fisheryArea,]$n_0.050, lty=2, col="green")
abline(h=summary_090_min_bucs_obs_5[fisheryArea==target_fisheryArea,]$n_0.050, lty=2, col="orange")
title(main=paste0("n_0.050 vs n_bucs_obs (fisheries with >=",min_number_lanIDs,"lanIDs"), outer=T, line=1, cex.main=1.5)
}



#===============================================
# Relationship between number of samples and variance [per country]
#===============================================

# To be saved?

windows(15,15); par(mfrow=c(3,3), oma=c(2,2,4,2))
for(target_country in unique(dat$ctry))
{
windows(10,10)
tmp<-unique(dat[ctry==target_country & nbuc_obs>=5, .(lanID, nbuc_obs, n_0.050)])
# expands on missing nbuc_obs
tmp<-rbind(tmp, data.frame(lanID=NA,nbuc_obs=1:max(tmp$nbuc_obs), n_0.050=NA))
plot(n_0.050~nbuc_obs, data=rbind(tmp, data.frame(lanID=NA,nbuc_obs=1:max(tmp$nbuc_obs), n_0.050=NA)), main=target_country)
mtext(tmp[!n_0.050=="NA",.N,.(nbuc_obs)]$N, side=3, at=tmp[!n_0.050=="NA",.N,.(nbuc_obs)]$nbuc_obs, cex=.8)
abline(h=summary_095_min_bucs_obs_5[fisheryArea==target_country,]$n_0.050, lty=2, col="green")
abline(h=summary_090_min_bucs_obs_5[fisheryArea==target_country,]$n_0.050, lty=2, col="orange")
title(main=paste0(target_country,": n_0.050 vs n_bucs_obs (landings with >=",5,"bucIDs"), outer=T, line=1, cex.main=1.5)
}


#===============================================
# Is there evidence that larger buckets / more buckets reduce variance or have more species?
#===============================================
for (i in subset_fisheryArea)
{
		
windows(30,15); par(mfrow=c(2,2))
plot(number_spp_lanID~meanBucketWeight_lanID, dat=tmp3[fisheryArea==i,], main="number_spp vs meanBucketWeight (lanID)")
if(nrow(tmp3[fisheryArea==i,])>1)abline(lm(number_spp_lanID~meanBucketWeight_lanID, dat=tmp3[fisheryArea==i,])$coef)
plot(number_spp_lanID~nbuc_obs, dat=tmp3[fisheryArea==i,], main="number_spp vs nbuc_obs (lanID)")
if(nrow(tmp3[fisheryArea==i,])>1)abline(lm(number_spp_lanID~nbuc_obs, dat=tmp3[fisheryArea==i,])$coef)
anova(lm(number_spp_lanID~meanBucketWeight_lanID+nbuc_obs, dat=tmp3[fisheryArea==i,]))

plot(meanCV~meanBucketWeight_lanID, dat=tmp3[fisheryArea==i,], main="sppMeanCV vs meanBucketWeight (lanID)")
if(nrow(tmp3[fisheryArea==i,])>1)abline(lm(meanCV~meanBucketWeight_lanID, dat=tmp3[fisheryArea==i,])$coef)
plot(meanCV~nbuc_obs, dat=tmp3[fisheryArea==i,], main="sppMeanCV vs n_buc_obs (lanID)")
if(nrow(tmp3[fisheryArea==i,])>1)abline(lm(meanCV~nbuc_obs, dat=tmp3[fisheryArea==i,])$coef)
anova(lm(meanCV~meanBucketWeight_lanID+nbuc_obs, dat=tmp3[fisheryArea==i,]))

title(main=i, outer=T, line=-1)
}

#===============================================
# Relationship between mean bucket size and sample size?
#===============================================

tmp<-dat[sppWeight_obs>0 & nbuc_obs>=5,.N,.(lanID,totWeight_obs, nbuc_obs,sp,sppWeight_obs,fisheryArea, n_0.050)][,list(sampleWeight_landing=sum(sppWeight_obs), meanBucketWeight=sum(sppWeight_obs)/.N, number_spp=length(unique(sp)), n_0.050=max(n_0.050)),.(lanID,totWeight_obs, nbuc_obs, fisheryArea)]
tmp2<-dat[sppWeight_obs>0,.N,.(fisheryArea,lanID,bucID, bucWeight_obs, sp)][, list(number_spp=.N),.(fisheryArea,lanID,bucID, bucWeight_obs)]

	graph_dir_country<-paste0("results/",target_country,"/plots_bucketSizeNumber_eval/")
	if(!dir.exists(graph_dir_country)) dir.create(graph_dir_country, recursive=T)

	for (i in unique(tmp$fisheryArea))
		{
		#par(mfrow=c(2,3))
		plot(number_spp~nbuc_obs, dat=tmp[fisheryArea==i,], main=i)
		if(nrow(tmp[fisheryArea==i,])>1)abline(lm(number_spp~nbuc_obs, dat=tmp[fisheryArea==i,])$coef)		

		plot(n_0.050~nbuc_obs, dat=tmp[fisheryArea==i,], main=i)
		
		plot(n_0.050~meanBucketWeight, dat=tmp[fisheryArea==i,], main=i)
		
		
		plot(nbuc_obs~totWeight_obs, dat=tmp[fisheryArea==i,], main=i)
		plot(sampleWeight_landing~totWeight_obs, dat=tmp[fisheryArea==i,], main=i)
		plot(number_spp~totWeight_obs, dat=tmp[fisheryArea==i,], main=i)
		if(nrow(tmp[fisheryArea==i,])>1)abline(lm(number_spp~totWeight_obs, dat=tmp[fisheryArea==i,])$coef)
		plot(number_spp~sampleWeight_landing, dat=tmp[fisheryArea==i,], main=i)
		if(nrow(tmp[fisheryArea==i,])>1)abline(lm(number_spp~sampleWeight_landing, dat=tmp[fisheryArea==i,])$coef)
		plot(number_spp~nbuc_obs, dat=tmp[fisheryArea==i,], main=i)
		if(nrow(tmp[fisheryArea==i,])>1)abline(lm(number_spp~nbuc_obs, dat=tmp[fisheryArea==i,])$coef)
		plot(number_spp~bucWeight_obs, dat=tmp2[fisheryArea==i,], main=i)
		abline(lm(number_spp~bucWeight_obs, dat=tmp2[fisheryArea==i,])$coef)
		savePlot(paste0(graph_dir_country,i,"_bucketSizeNumber_eval.png"), type="png")
		}
		
		
#===============================================
# Proportion of species landings with poor estimates (cv>25) of individual landings per fisheries
#===============================================
# analysis sppWeight_estim<5000
dat[sppWeight_estim<5000 & sppWeight_estim>0 & !is.na(sppWeight_estim_cv), .(lanID,sp,sppWeight_estim_cv, fisheryArea)][,list(sum(sppWeight_estim_cv>25)/.N), by=fisheryArea]
# analysis sppWeight_estim>10000
dat[sppWeight_estim>10000  & !is.na(sppWeight_estim_cv), .(lanID,sp,sppWeight_estim_cv, fisheryArea)][,list(sum(sppWeight_estim_cv>25)/.N), by=fisheryArea]

dat[sppWeight_estim>0  & !is.na(sppWeight_estim_cv), .(lanID,sp,sppWeight_estim_cv, fisheryArea)][,list(sum(sppWeight_estim_cv>5)/.N), by=fisheryArea]

plot(sppWeight_estim_cv~sppWeight_estim, data=dat)
abline(0,0.25, col=2)
plot(sppWeight_estim_errMargin~sppWeight_estim, data=dat)
abline(0,1)
plot(sppWeight_estim_errMargin~sppWeight_estim, data=dat[sppWeight_estim<5000,])
abline(0,0.25)
plot(sppWeight_estim_errMargin~sppWeight_estim, data=dat[sppWeight_estim>10000,])
abline(0,0.25)


#===============================================	
# evaluation of present sample sizes
#===============================================

# analysis sppPercWeight_estim_cv | sppWeight_estim_cv>5
dat[sppWeight_estim>0  & !is.na(sppPercWeight_estim_cv) & nbuc_obs>=5, .(lanID,sp, fisheryArea, sppPercWeight_estim_cv)][,list(sum(sppPercWeight_estim_cv>0.05)/.N), by=fisheryArea]
dat[sppWeight_estim>0  & !is.na(sppWeight_estim_cv) & nbuc_obs>=5, .(lanID,sp, fisheryArea, sppWeight_estim_cv)][,list(sum(sppWeight_estim_cv>0.05)/.N), by=fisheryArea]


dat1[sppWeight_estim>0  & !is.na(sppWeight_estim_cv), .(lanID,sp,pred_em_perc, fisheryArea)][,list(sum(pred_em_perc>10)/.N), by=fisheryArea]



	
#===============================================	
# evaluation of proposed sample sizes [0.05]
#===============================================
	# estimates the cv that would have been achieved in each species under a proposed sample size

proposed_sample_size0<-summary_095_min_bucs_obs_5[nLanIDs>10,.(fisheryArea,n_0.050)]

proposed_sample_size1<-data.table(fisheryArea=c('Baltic_HERSPR_HUC','Baltic_HERSPR_IND','Bothnia_FVE','Bothnia_HER','NAtlantic_MAC','NAtlantic_WHB','NSea_HER','NSea_NOP','NSea_SAN','NSea_SPR'),
								 n_0.050=c(Baltic_HERSPR_HUC =  24,
								 Baltic_HERSPR_IND = 25 ,
								 Bothnia_FVE =  20,
								 Bothnia_HER =  6,
								 NAtlantic_MAC =  55 ,
								 NAtlantic_WHB =  1,
								 NSea_HER =  5,
								 NSea_NOP =  22,
								 NSea_SAN =  2,
								 NSea_SPR =  33) #21 alloc; 33 as species; 21 deleted; 12 expert
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

res_0<-estimate_precision_from_sample_size(dat, proposed_sample_size0)$res_perc_sppWeight_nbucs_obs_5
res_1<-estimate_precision_from_sample_size(dat, proposed_sample_size1)$res_perc_sppWeight_nbucs_obs_5

res_combined<-merge(merge(res_0, proposed_sample_size0, by="fisheryArea"), 
	merge(res_1, proposed_sample_size1, by="fisheryArea"), by="fisheryArea")
colnames(res_combined)<-gsub("50.x","50.0",colnames(res_combined))
colnames(res_combined)<-gsub("50.y","50.1",colnames(res_combined))
res_combined$nLanIDs<-summary_095_min_bucs_obs_5$nLanIDs[match(res_combined$fisheryArea,summary_095_min_bucs_obs_5$fisheryArea)]
res_combined
















dat1[lanID=="DNK_676",][sppWeight_obs>0,.N,.(proposed_sample_size,sp, pred_em_perc_totWeight=round(pred_em_perc_totWeight*100,1), pred_em_perc_sppWeight=c(round(pred_em_perc_sppWeight,1)))]





# analysis sppWeight_estim_cv>5
dat1[sppWeight_estim>0  & !is.na(sppWeight_estim_cv), .(lanID,sp,pred_cv, fisheryArea)][,list(sum(pred_cv>5)/.N), by=fisheryArea]
dat1[sppWeight_estim>0  & !is.na(sppWeight_estim_cv), .(lanID,sp,pred_em_perc, fisheryArea)][,list(sum(pred_em_perc>10)/.N), by=fisheryArea]


# analysis sppWeight_estim<5000
dat1[sppWeight_estim<5000 & sppWeight_estim>0 & !is.na(pred_cv), .(lanID,sp,pred_cv, fisheryArea)][,list(sum(pred_cv>25)/.N), by=fisheryArea]
# analysis sppWeight_estim>10000
dat1[sppWeight_estim>10000  & !is.na(sppWeight_estim_cv), .(lanID,sp,pred_cv, fisheryArea)][,list(sum(pred_cv>25)/.N), by=fisheryArea]

