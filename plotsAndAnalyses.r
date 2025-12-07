#source("estimSppComp.r")



#===============================================
# graphs of individual landings
#===============================================

	graph_dir_country<-paste0("results/",target_country,"/plots_individual_landings/")
	if(!dir.exists(graph_dir_country)) dir.create(graph_dir_country, recursive=T)

	ls1<-split(dat, dat$fisheryArea)
	for (i in names(ls1))
		{
		print(i)
		tmp_fisheryArea <- ls1[[i]]
		for (j in unique(tmp_fisheryArea$lanID))
		{
		print(j)
		tmp_trip<-tmp_fisheryArea[lanID==j,]
		dat_graph<-tmp_trip[,sppWeight_obs/bucWeight_obs, by=.(lanID, bucID,sp, totWeight_obs, sppPercWeight_estim, n_0.050)]
		dat_graph$bucOrder<-as.integer(factor(dat_graph$bucID))
		plot(V1~bucOrder, data=dat_graph, type="n", ylim=c(0,1), xlim=c(0,38), pch=19, main=paste0(target_country,":", i,": ",j,": ",dat_graph$totWeight_obs[1], " ton\n","n_0.050: ", max(tmp_trip[sppWeight_obs>0,]$n_0.050)), xlab="bucOrder")
		for (w in unique(dat_graph$sp))
		{	
		points(V1~bucOrder, data=dat_graph[sp==w,], type="o", lty=2, pch=19, col=2)
		text(x=max(dat_graph[sp==w,]$bucOrder), y=mean(dat_graph[sp==w,]$sppPercWeight_estim), labels=paste(w,round(mean(dat_graph[sp==w,]$sppPercWeight_estim),2)), col=2, cex=0.7, pos=4)
		}
		savePlot(paste0(graph_dir_country,i,"_",j,".png"), type="png")
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

graph_dir_country<-paste0("results/",target_country,"/plots_sensitivity_nbuc_obs/")

out<-data.table()
for(j in subset_fisheryArea)
{
res<-c()
res_n<-c()
for (i in c(2,5,10))
{
min_bucs_obs<-i
tabela<-rbindlist(lapply(split(summariseMax(x = dat[nbuc_obs>min_bucs_obs,], group=c("lanID","fisheryArea"), min_n=5),by="fisheryArea"), function(x) {cbind(x[1,2],nLanIDs=nrow(x), round(t(apply(x[,4:ncol(x)],2, quantile, type=7, prob=c(0.95)))))}))[order(fisheryArea),][fisheryArea==j,]
res<-c(res,tabela$n_0.050)
res_n<-c(res_n,tabela$nLanIDs)
}
windows()
a<-barplot(res, names.arg=c(2,5,10), xlab="min_n_bucs admitted for analysis", ylab="sample size needed for 95% at 0.05e", main=j)
text(x=a[,1], y=2, label=res_n)
savePlot(paste0(graph_dir_country,j), type="png")
out<-rbind(out, data.table(fisheryArea=j, min_nbuc_in_analysis=c(2,5,10), n_lanIDs_in_analysis=res_n, p95_n_0.05=res))
}

table(dat[fisheryArea %in% subset_fisheryArea,.N,.(lanID, nbuc_obs, fisheryArea)][,2:3])

#===============================================
# Relationship between mean bucket size and sample size?
#===============================================

tmp<-dat[sppWeight_obs>0 & nbuc_obs>5,.N,.(lanID,totWeight_obs, nbuc_obs,sp,sppWeight_obs,fisheryArea)][,list(sampleWeight_landing=sum(sppWeight_obs), number_spp=length(unique(sp))),.(lanID,totWeight_obs, nbuc_obs, fisheryArea)]
tmp2<-dat[sppWeight_obs>0,.N,.(fisheryArea,lanID,bucID, bucWeight_obs, sp)][, list(number_spp=.N),.(fisheryArea,lanID,bucID, bucWeight_obs)]

	graph_dir_country<-paste0("results/",target_country,"/plots_bucketSizeNumber_eval/")
	if(!dir.exists(graph_dir_country)) dir.create(graph_dir_country, recursive=T)

	for (i in unique(tmp$fisheryArea))
		{
		par(mfrow=c(2,3))
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
# evaluation of proposed sample sizes
#===============================================
	# estimates the cv that would have been achieved in each species under a proposed sample size

proposed_sample_size<-summary_095_min_bucs_obs_5[,.(fisheryArea,n_0.050)]

dat1<-dat
dat1$proposed_sample_size <- proposed_sample_size$n_0.050[match(dat1$fisheryArea,proposed_sample_size$fisheryArea)]

dat1[,pred_em:=totWeight_obs*1.96*sqrt((1/(bucWeightmean_obs^2 ))*sppPercWeight_s2/proposed_sample_size)]
dat1[,pred_em_perc:=pred_em/sppWeight_estim*100]
dat1[,pred_cv:=round(pred_em/1.96/sppWeight_estim*100,2)]

dat1[lanID=="DNK_676",][sppWeight_obs>0,.N,.(proposed_sample_size,sp, round(pred_em_perc,1))]


# analysis sppWeight_estim_cv>5
dat1[sppWeight_estim>0  & !is.na(sppWeight_estim_cv), .(lanID,sp,pred_cv, fisheryArea)][,list(sum(pred_cv>5)/.N), by=fisheryArea]
dat1[sppWeight_estim>0  & !is.na(sppWeight_estim_cv), .(lanID,sp,pred_em_perc, fisheryArea)][,list(sum(pred_em_perc>10)/.N), by=fisheryArea]


# analysis sppWeight_estim<5000
dat1[sppWeight_estim<5000 & sppWeight_estim>0 & !is.na(pred_cv), .(lanID,sp,pred_cv, fisheryArea)][,list(sum(pred_cv>25)/.N), by=fisheryArea]
# analysis sppWeight_estim>10000
dat1[sppWeight_estim>10000  & !is.na(sppWeight_estim_cv), .(lanID,sp,pred_cv, fisheryArea)][,list(sum(pred_cv>25)/.N), by=fisheryArea]

