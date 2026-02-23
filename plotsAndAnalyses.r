#source("estimSppComp.r")

#===============================================
# create directories
#===============================================

for (i in unique(dat$ctry))
{
for (j in unique(dat$fisheryArea))
{
	graph_dir<-paste0("results/",target_country,"/plots_individual_landings/",format(Sys.Date(), format="%Y%m%d"),"/country/",i,"/",j,"/")
	dir.create(graph_dir, recursive=T, showWarnings=F)
}
}
for (i in unique(dat$fisheryArea))
{
	graph_dir<-paste0("results/",target_country,"/plots_individual_landings/",format(Sys.Date(), format="%Y%m%d"),"/fishery/",i,"/")
	dir.create(graph_dir, recursive=T, showWarnings=F)
}

#===============================================
# graphs of individual landings
#===============================================
	# spp composition in each landing bucket by bucket
	# saves files organized by country/fishery AND fishery (files repeated)

	base_graph_dir <- paste0("results/",target_country,"/plots_individual_landings/",format(Sys.Date(), format="%Y%m%d"))

	ls1<-split(dat, dat$fisheryArea)
	for (i in names(ls1))
		{
		print(i)
		tmp_fisheryArea <- ls1[[i]]
		tmp_fisheryArea$colour<-as.integer(factor(tmp_fisheryArea$sp))
		for (j in unique(tmp_fisheryArea$lanID))
		{
		print(paste0(".",j,": ",which(unique(tmp_fisheryArea$lanID)==j)," out of ",length(unique(tmp_fisheryArea$lanID))))
		tmp_trip<-tmp_fisheryArea[lanID==j,]
		dat_graph<-tmp_trip[sppPercWeight_estim>0,sppWeight_obs/bucWeight_obs, by=.(lanID, bucID,sp, totWeight_obs, sppPercWeight_estim, n_0.050, colour)]
		dat_graph2<-tmp_trip[sppPercWeight_estim>0,.N,.(lanID, sp, totWeight_obs, sppPercWeight_estim, sppPercWeight_estim_CIlow, sppPercWeight_estim_CIupp, sppWeight_estim, sppWeight_estim_CIlow, sppWeight_estim_CIupp,colour)]
		dat_graph<-dat_graph[order(bucID),]
		dat_graph$bucOrder<-as.integer(factor(dat_graph$bucID))
		png(filename = paste0(base_graph_dir,"/fishery/",i,"/",i,"_",j,".png"),width = 960, height = 480)
		par(oma=c(1,1,1,5))
		if(max(dat_graph$bucOrder)<50) plot(V1~bucOrder, data=dat_graph, type="n", ylim=c(0,1), xlim=c(0,50), pch=19, main="", xlab="bucOrder", ylab="proportion in weight")
		if(max(dat_graph$bucOrder)>=50) plot(V1~bucOrder, data=dat_graph, type="n", ylim=c(0,1), xlim=c(0,100), pch=19, main="", xlab="bucOrder", ylab="proportion in weight")
		title(main=paste0(i,": ",j,":\n total weight:",round(dat_graph$totWeight_obs[1]/1000), " ton\n","n_0.050: ", max(tmp_trip[sppWeight_obs>0,]$n_0.050),"; n_10000: ", max(tmp_trip[sppWeight_obs>0,]$n_10000)), cex.main=0.8)
		for (w in unique(dat_graph$sp))
		{	
		points(V1~bucOrder, data=dat_graph[sp==w,], type="o", lty=2, pch=19, col=unique(dat_graph[sp==w,]$colour))
		text_to_print<-paste0(w,": ",round(mean(dat_graph2[sp==w,]$sppPercWeight_estim),2),"(",round(mean(dat_graph2[sp==w,]$sppPercWeight_estim_CIlow),2),"-",round(mean(dat_graph2[sp==w,]$sppPercWeight_estim_CIupp),2),")")
		text(x=max(dat_graph[sp==w,]$bucOrder), y=mean(dat_graph[sp==w,]$sppPercWeight_estim), labels=text_to_print, col=unique(dat_graph[sp==w,]$colour), cex=0.7, pos=4)
		# margin 4 text: total weight estimates
			mtext("Westim (t); IC 95%", side=4, at=1.05, line=0.5, las=2, cex=0.8)
			text_to_print<-paste0(w,": ",round(dat_graph2[sp==w,]$sppWeight_estim/1000), "t: ", round(dat_graph2[sp==w,]$sppWeight_estim_CIlow/1000),"-",round(dat_graph2[sp==w,]$sppWeight_estim_CIupp/1000))
			mtext(text_to_print, side=4, at=dat_graph2[sp==w,]$sppPercWeight_estim, las=1, col=dat_graph2[sp==w,]$colour, line=0.5, cex=0.7)
		}
		# copies also to country directory
		file.copy(from=paste0(base_graph_dir,"/fishery/",i,"/",i,"_",j,".png"), to = paste0(base_graph_dir,"/country/", substr(j,1,3),"/",i,"/",i,"_",j,".png"))
		dev.off()
}
}


#===============================================
# displays the more variable cases within each fishery
#===============================================
	
	number_of_cases_to_display<-30
	min_number_lanIDs<-15
	target_fisheryAreas<-summary_095_min_bucs_obs_5[nLanIDs>=min_number_lanIDs]$fisheryArea
	ls1<-split(dat[fisheryArea %in% target_fisheryAreas,], dat[fisheryArea %in% target_fisheryAreas,]$fisheryArea)
	a<-lapply(ls1,function(x){print(head(unique(x[nbuc_obs>=5,.(fisheryArea, lanID, nbuc_obs, sp, n_0.050, sppWeight_estim, sppWeight_estim_CIlow, sppWeight_estim_CIupp)])[order(-n_0.050),],number_of_cases_to_display))})
	a<-lapply(ls1,function(x){print(head(unique(x[nbuc_obs>=5,.(fisheryArea, lanID, nbuc_obs, n_0.050, totWeight_obs)])[, list(n_0.050=max(n_0.050)),.(fisheryArea, lanID, nbuc_obs, totWeight_obs)][order(-n_0.050),],number_of_cases_to_display))})


#===============================================
# displays boundary cases (p95_n_0.05) within each fishery
#===============================================

# To be done

# position of the max in the ordered vector
lapply(split(summariseMax(x = dat[nbuc_obs>=min_bucs_obs,], group=c("lanID","fisheryArea"), min_n=5),by="fisheryArea"), function(x) {
pos <- which(unname(unlist(x[order(-x[,"n_0.050"]),][,"n_0.050"]))==round(apply(x[order(x[,"n_0.050"])][,"n_0.050"],2,quantile, type=7, prob=c(0.95)))); pos
if(length(pos)==0) pos <- max(which(unname(unlist(x[order(-x[,"n_0.050"]),][,"n_0.050"]))>round(apply(x[order(x[,"n_0.050"])][,"n_0.050"],2,quantile, type=7, prob=c(0.95))))); pos<-pos+1
print(x$fisheryArea[1])
if(length(pos)>1) print("many cases: ambiguous") else {
#print(x[order(-x[,"n_0.050"]),][pos,]$lanID)
print(x[order(-x[,"n_0.050"]),][pos,])
}
})


target_prob=0.95
	summary_095_min_bucs_obs_5_adj<-rbindlist(
								lapply(split(summariseMax(x = dat[nbuc_obs>=min_bucs_obs,], group=c("lanID","fisheryArea"), min_n=5),by="fisheryArea"), function(x) {cbind(x[1,2],nLanIDs=nrow(x),round(t(apply(x[,4:ncol(x)],2,do_determine_closest))))}))[order(fisheryArea),]

target_prob=0.90
	summary_090_min_bucs_obs_5_adj<-rbindlist(
								lapply(split(summariseMax(x = dat[nbuc_obs>=min_bucs_obs,], group=c("lanID","fisheryArea"), min_n=5),by="fisheryArea"), function(x) {cbind(x[1,2],nLanIDs=nrow(x),round(t(apply(x[,4:ncol(x)],2,do_determine_closest))))}))[order(fisheryArea),]
	
	
do_determine_closest<-function(y){	
	pos <- which(sort(y, decreasing=T)==round(quantile(y, type=7, prob=c(target_prob)))); pos
if(length(pos)==0) pos <- max(which(sort(y, decreasing=T)>round(quantile(y, type=7, prob=c(target_prob))))); pos<-pos+1
if(length(pos)>1)return(round(quantile(y, type=7, prob=c(target_prob)))) else {
return(sort(y, decreasing=T)[pos])
}
}	



#===============================================
# displays the more variable cases in each country
#===============================================

	ls1<-split(dat, dat$ctry)
	a<-lapply(ls1,function(x){print(head(unique(x[nbuc_obs>=5,.(ctry, fisheryArea, lanID, sp, n_0.050)])[order(-n_0.050),]))})



# ===================================
# histogram: sample sizes required for 95% of trips
# ===================================

graph_dir_country<-paste0("results/",target_country,"/plots_histogram_sample_sizes/")
if(!dir.exists(graph_dir_country)) dir.create(graph_dir_country, recursive=T)

length(subset_fisheryArea<-summary_095_min_bucs_obs_5_adj[nLanIDs>=14,]$fisheryArea)

for (i in unique(subset_fisheryArea))
{
windows()
hist(summariseMax(x = dat, group=c("lanID","fisheryArea"), min_n=5)[fisheryArea==i,]$n_0.050, breaks=100, main=paste0(i,": samp size for 0.05 e: ",summary_095_min_bucs_obs_5_adj[fisheryArea==i,]$nLanIDs," lanIDs"), xlab="sample size")
abline(v=summary_095_min_bucs_obs_5[fisheryArea==i,]$n_0.05, lty=2, col="orange")
abline(v=summary_095_min_bucs_obs_5_adj[fisheryArea==i,]$n_0.05, lty=1, col="orange")
abline(v=summary_090_min_bucs_obs_5[fisheryArea==i,]$n_0.05, lty=2, col="red")
abline(v=summary_090_min_bucs_obs_5_adj[fisheryArea==i,]$n_0.05, lty=1, col="red")
legend("topright", legend=c(paste0("95% of landings (",summary_095_min_bucs_obs_5[fisheryArea==i,]$n_0.05,")"),paste0("95% of landings - adj (",summary_095_min_bucs_obs_5_adj[fisheryArea==i,]$n_0.05,")"),
								paste0("90% of landings (",summary_090_min_bucs_obs_5[fisheryArea==i,]$n_0.05,")"),paste0("90% of landings - adj (",summary_090_min_bucs_obs_5_adj[fisheryArea==i,]$n_0.05,")")), lty=c(2,1,2,1), col=c("orange","red","orange","red"))
savePlot(paste0(graph_dir_country,i), type="png")
}
graphics.off()
# ===================================
# sensitivity analysis: number of minimum buckets in landings allowed for analysis
# ===================================

tab_max_nbuc_obs<-dat[fisheryArea %in% subset_fisheryArea, list(max_nbuc_obs=max(nbuc_obs)), fisheryArea]

graph_dir_country<-paste0("results/",target_country,"/plots_sensitivity_nbuc_obs/"); dir.create(graph_dir_country, showWarnings=F)

out<-data.table()
for(j in subset_fisheryArea)
{
print(j)
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
if(length(res)==4){
a<-barplot(res, names.arg=c(2,5,10,15), xlab="min_n_bucs admitted for analysis", ylab="sample size needed for 95% at 0.05e", main=j)
text(x=a[,1], y=2, label=res_n)
savePlot(paste0(graph_dir_country,j), type="png")
out<-rbind(out, data.table(fisheryArea=j, min_nbuc_in_analysis=c(2,5,10,15), n_lanIDs_in_analysis=res_n, p95_n_0.05=res))
} else print("not all classes")
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
		windows()
		par(mfrow=c(2,3))
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
dat[sppWeight_estim>0  & !is.na(sppPercWeight_estim_cv) & nbuc_obs>=5, .(lanID,sp, fisheryArea, sppPercWeight_estim_cv)][,list(sum(sppPercWeight_estim_cv>5)/.N), by=fisheryArea]
dat[sppWeight_estim>0  & !is.na(sppWeight_estim_cv) & nbuc_obs>=5, .(lanID,sp, fisheryArea, sppWeight_estim_cv)][,list(sum(sppWeight_estim_cv>5)/.N), by=fisheryArea]



dat[sppWeight_estim>0  & !is.na(sppWeight_estim_cv) & nbuc_obs>=5 & sp=="HER", .N, .(lanID,sp, fisheryArea, sppWeight_estim_cv)][,list(sum(sppWeight_estim_cv>5)/.N), by=fisheryArea]


	
#===============================================	
# evaluation of proposed sample sizes [0.05]
#===============================================
	# estimates the cv that would have been achieved in each species under a proposed sample size

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

# to be done: think better about interpetation in terms of sppWeight 



#===============================================	
# evaluation of proposed sample sizes (% of spp weight)
#===============================================
	# evaluates number of spp above 5% CV in present samples and expected under new sample size
	
# present situation
dat[sppWeight_estim>0  & !is.na(sppWeight_estim_cv) & nbuc_obs>=5 & sp %in% c("SAN"), .N, .(lanID,sp, fisheryArea, sppWeight_estim_cv)][,list(sum(sppWeight_estim_cv>5)/.N), by=fisheryArea]
dat[sppWeight_estim>0  & !is.na(sppWeight_estim_cv) & nbuc_obs>=5 & sp %in% c("SPR","HER"), .N, .(lanID,sp, fisheryArea, sppWeight_estim_cv)][,list(sum(sppWeight_estim_cv>5)/.N), by=fisheryArea]
# proposed sampled size
proposed_sample_size0<-summary_095_min_bucs_obs_5[nLanIDs>10,.(fisheryArea,n_0.050)]	
estimate_precision_from_sample_size(dat, proposed_sample_size0)$dat[sppWeight_estim>0  & !is.na(pred_cv) & nbuc_obs>=5 & sp %in% c("SPR","HER"), .N, .(lanID,sp, fisheryArea, pred_cv)][,list(sum(pred_cv>.05)/.N), by=fisheryArea]


#===============================================	
# a look into the sand-eel
#===============================================

dat[sppWeight_estim>0 & sp %in% c("SAN") & fisheryArea=="NSea_SAN",.N,.(lanID, sp, nbuc_obs, totWeight_obs, sppPercWeight_estim=round(sppPercWeight_estim,2), sppWeight_estim=round(sppWeight_estim),sppWeight_estim_CIlow=round(sppWeight_estim_CIlow), sppWeight_estim_CIupp=round(sppWeight_estim_CIupp), sppWeight_estim_cv)][order(-sppWeight_estim),]


dat[sppWeight_estim>0 & !sp %in% c("SAN") & fisheryArea=="NSea_SAN",.N,.(lanID, sp, nbuc_obs, totWeight_obs, sppPercWeight_estim=round(sppPercWeight_estim,2), sppWeight_estim=round(sppWeight_estim),sppWeight_estim_CIlow=round(sppWeight_estim_CIlow), sppWeight_estim_CIupp=round(sppWeight_estim_CIupp), sppWeight_estim_cv)][order(-sppWeight_estim),]
head(dat[sppWeight_estim>0 & !sp %in% c("SAN") & fisheryArea=="NSea_SAN",.N,.(lanID, sp, nbuc_obs, totWeight_obs, sppPercWeight_estim=round(sppPercWeight_estim,2), sppWeight_estim=round(sppWeight_estim),sppWeight_estim_CIlow=round(sppWeight_estim_CIlow), sppWeight_estim_CIupp=round(sppWeight_estim_CIupp), sppWeight_estim_cv)][order(-sppWeight_estim),],20)
tail(dat[sppWeight_estim>0 & !sp %in% c("SAN") & fisheryArea=="NSea_SAN",.N,.(lanID, sp, nbuc_obs, totWeight_obs, sppPercWeight_estim=round(sppPercWeight_estim,2), sppWeight_estim=round(sppWeight_estim),sppWeight_estim_CIlow=round(sppWeight_estim_CIlow), sppWeight_estim_CIupp=round(sppWeight_estim_CIupp), sppWeight_estim_cv)][order(-sppWeight_estim),],20)



dat[sppWeight_estim>0 & sp %in% c("COD") & fisheryArea=="NSea_SAN",.N,.(lanID, sp, nbuc_obs, totWeight_obs, sppPercWeight_estim=round(sppPercWeight_estim,2), sppWeight_estim=round(sppWeight_estim),sppWeight_estim_CIlow=round(sppWeight_estim_CIlow), sppWeight_estim_CIupp=round(sppWeight_estim_CIupp), sppWeight_estim_cv)][order(-sppWeight_estim),]
dat[sppWeight_estim>0 & sp %in% c("WHG") & fisheryArea=="NSea_SAN",.N,.(lanID, sp, nbuc_obs, totWeight_obs, sppPercWeight_estim=round(sppPercWeight_estim,2),sppWeight_estim=round(sppWeight_estim),sppWeight_estim_CIlow=round(sppWeight_estim_CIlow), sppWeight_estim_CIupp=round(sppWeight_estim_CIupp), sppWeight_estim_cv)][order(-sppWeight_estim),]
dat[sppWeight_estim>0 & sp %in% c("HER") & fisheryArea=="NSea_SAN",.N,.(lanID, sp, nbuc_obs, totWeight_obs, sppPercWeight_estim=round(sppPercWeight_estim,2),sppWeight_estim=round(sppWeight_estim),sppWeight_estim_CIlow=round(sppWeight_estim_CIlow), sppWeight_estim_CIupp=round(sppWeight_estim_CIupp), sppWeight_estim_cv)][order(-sppWeight_estim),]


#===============================================	
# current evaluation of programmes
#===============================================
	# margin of error [% around total weight] vs sample size
		targetFisheryArea<-"NSea_SAN"
		data_graph<-dat[fisheryArea== targetFisheryArea & sppWeight_obs>0,.N,.(lanID,sp,sppPercWeight_estim_cv,sppWeight_estim_cv,percErrorMarginPercWeight=round(sppPercWeight_estim_errMargin/sppPercWeight_estim*100,1),percErrorMarginTotalWeight=round(sppWeight_estim_errMargin/sppWeight_estim*100,1))]
		hist(data_graph$percErrorMarginTotalWeight, breaks=seq(0,300,by=1))
		hist(data_graph[sp=="SPR",]$percErrorMarginTotalWeight, breaks=seq(0,300,by=1))
		hist(data_graph[!sp=="SPR",]$percErrorMarginTotalWeight, breaks=seq(0,300,by=1))

		targetFisheryArea<-"Bothnia_FVE"
		targetFisheryArea<-"Baltic_HERSPR_HUC"
		targetFisheryArea<-"Baltic_HERSPR_IND"
		targetFisheryArea<-"Bothnia_HER"
		targetFisheryArea<-"NAtlantic_MAC"
		targetFisheryArea<-"NAtlantic_WHB"
		targetFisheryArea<-"NSea_NOP"
		targetFisheryArea<-"NSea_SAN"
		targetFisheryArea<-"NSea_SPR"
		targetFisheryArea<-"NSea_HER"
		targetFisheryArea<-"Med_SPF"
		targetFisheryArea<-"GoR_HER_HUC"
		if(targetFisheryArea %in% c("Baltic_HERSPR_IND","Baltic_HERSPR_HUC")) targetSpp<-c("SPR","HER")
		if(targetFisheryArea=="NSea_SPR") targetSpp<-c("SPR")
		if(targetFisheryArea %in% c("Bothnia_HER", "NSea_HER")) targetSpp<-c("HER")
		if(targetFisheryArea=="NSea_NOP") targetSpp<-c("NOP")
		if(targetFisheryArea=="NSea_SAN") targetSpp<-c("SAN")
		if(targetFisheryArea=="Bothnia_FVE") targetSpp<-c("FVE")
		if(targetFisheryArea=="NAtlantic_MAC") targetSpp<-c("MAC")
		if(targetFisheryArea=="NAtlantic_WHB") targetSpp<-c("WHB")
		if(targetFisheryArea=="MED_SPF") targetSpp<-c("PIL","ANE")
		if(targetFisheryArea=="GoR_HER_HUC") targetSpp<-c("HER")
		data_graph<-dat[fisheryArea== targetFisheryArea & sppWeight_obs>0 & nbuc_obs>=5,.N,.(lanID,sp,nbuc_obs,sppPercWeight_estim_cv,sppWeight_estim_cv,percErrorMarginPercWeight=round(sppPercWeight_estim_errMargin/sppPercWeight_estim*100,1),percErrorMarginTotalWeight=round(sppWeight_estim_errMargin/sppWeight_estim*100,1))]
		windows(15,15); par(mfrow=c(3,1), oma=c(1,1,3,1))
		ylimite=c(0, max(table(data_graph[,.N,.(lanID,nbuc_obs)]$nbuc_obs)))
		if(!targetFisheryArea %in% c("NAtlantic_MAC","NAtlantic_WHB")) hist(data_graph[,.N,.(lanID,nbuc_obs)]$nbuc_obs, breaks=seq(0,60,by=1), main="No. Buckets Obs", xlab="n", ylim=ylimite)
		if(targetFisheryArea %in% c("NAtlantic_MAC","NAtlantic_WHB")) hist(data_graph[,.N,.(lanID,nbuc_obs)]$nbuc_obs, breaks=seq(0,100,by=1), main="No. Buckets Obs", xlab="n", ylim=ylimite)
		ylimite=c(0, max(table(cut(data_graph$percErrorMarginTotalWeight, breaks=seq(0,300,by=1), right = FALSE, ordered_result=T))))
		hist(data_graph[sp %in% targetSpp,]$percErrorMarginTotalWeight, breaks=seq(0,300,by=1), main="error margin target spp", xlab="Percent Error Margin", ylim=ylimite, col="blue")
		abline(v=5, lty=2, col="red")
		hist(data_graph[!sp%in% targetSpp,]$percErrorMarginTotalWeight, breaks=seq(0,300,by=1), main=paste0("error margin bycatch\n",length(unique(data_graph[!sp%in% targetSpp,]$sp))," spp; mean ",round(data_graph[,.N,.(lanID,sp)][,!sp%in% targetSpp, .(lanID,sp)][,sum(V1),lanID][, mean(V1)],1),": spp per LanID"), xlab="Percent Error Margin", ylim=ylimite, col="brown")
		abline(v=5, lty=2, col="red")
		title(main=paste0(targetFisheryArea,": lanIDs (n>=5) = ",nrow(data_graph[,.N,.(lanID)])), outer=T, line=1, cex.main=1.5) 


		#most sampled landings
			# DNK_5019 - 91 buckets
			dat[lanID=="DNK_5019",.N,.(lanID, sp, sppPercWeight_estim, sppPercWeight_estim_errMargin, sppWeight_estim, sppWeight_estim_errMargin)]
			
		
#===============================================	
# summing up
#===============================================		
	
		targetFisheryArea<-"Baltic_HERSPR_IND"
		targetFisheryArea<-"NSea_SPR"
		targetFisheryArea<-"Baltic_HERSPR_HUC"
		targetFisheryArea<-"NAtlantic_MAC"
		targetFisheryArea<-"NSea_NOP"
		if(targetFisheryArea %in% c("Baltic_HERSPR_IND","Baltic_HERSPR_HUC")) targetSpp<-c("SPR","HER")
		if(targetFisheryArea=="NSea_SPR") targetSpp<-c("SPR")
		if(targetFisheryArea %in% c("Bothnia_HER", "NSea_HER")) targetSpp<-c("HER")
		if(targetFisheryArea=="NSea_NOP") targetSpp<-c("NOP")
		if(targetFisheryArea=="NSea_SAN") targetSpp<-c("SAN")
		if(targetFisheryArea=="Bothnia_FVE") targetSpp<-c("FVE")
		if(targetFisheryArea=="NAtlantic_MAC") targetSpp<-c("MAC")		
		data_graph<-dat[fisheryArea== targetFisheryArea & sppWeight_obs>0 & nbuc_obs>=2 & !sp %in% targetSpp,.N,.(year, lanID,sp,nbuc_obs,sppWeight_estim,sppWeight_estim_var)]
		data_graph[, list(total=round(sum(sppWeight_estim)/1000,1), 
						CIlow=round((sum(sppWeight_estim)-1.96*sqrt(sum(sppWeight_estim_var)))/1000,1),
						CIupp=round((sum(sppWeight_estim)+1.96*sqrt(sum(sppWeight_estim_var)))/1000,1)
						),.(year,sp)][order(-abs(total),sp, year),]

		
		data_graph<-dat[fisheryArea== targetFisheryArea & sppWeight_obs>0 & nbuc_obs>=2 & sp %in% targetSpp,.N,.(year, lanID,sp,nbuc_obs,sppWeight_estim,sppWeight_estim_var)]
		data_graph[, list(total=round(sum(sppWeight_estim)/1000,1), 
						CIlow=round((sum(sppWeight_estim)-1.96*sqrt(sum(sppWeight_estim_var)))/1000,1),
						CIupp=round((sum(sppWeight_estim)+1.96*sqrt(sum(sppWeight_estim_var)))/1000,1)
						),.(year,sp)][order(-abs(total),sp, year),]



dat1[lanID=="DNK_676",][sppWeight_obs>0,.N,.(proposed_sample_size,sp, pred_em_perc_totWeight=round(pred_em_perc_totWeight*100,1), pred_em_perc_sppWeight=c(round(pred_em_perc_sppWeight,1)))]





# analysis sppWeight_estim_cv>5
dat1[sppWeight_estim>0  & !is.na(sppWeight_estim_cv), .(lanID,sp,pred_cv, fisheryArea)][,list(sum(pred_cv>5)/.N), by=fisheryArea]
dat1[sppWeight_estim>0  & !is.na(sppWeight_estim_cv), .(lanID,sp,pred_em_perc, fisheryArea)][,list(sum(pred_em_perc>10)/.N), by=fisheryArea]


# analysis sppWeight_estim<5000
dat1[sppWeight_estim<5000 & sppWeight_estim>0 & !is.na(pred_cv), .(lanID,sp,pred_cv, fisheryArea)][,list(sum(pred_cv>25)/.N), by=fisheryArea]
# analysis sppWeight_estim>10000
dat1[sppWeight_estim>10000  & !is.na(sppWeight_estim_cv), .(lanID,sp,pred_cv, fisheryArea)][,list(sum(pred_cv>25)/.N), by=fisheryArea]

