
library(data.table)
rm(list=ls())
# load data
	# dat <- readRDS("data/SPE.rds")
	# dat <- readRDS("data/Sofia.rds")
	dat <- readRDS("data/HaVCtrl_strict.rds")
	



# prep data
dat[,bucWeight_obs:=sum(sppWeight_obs), by=.(lanID, bucID)][,nbuc_obs:=length(unique(bucID)), by=.(lanID)]

# adds estimate of mean bucket weight per landing (bucWmean_estim) and number of buckets per landing
dat[unique(dat[, .SD, .SDcols=c("lanID","bucID","bucWeight_obs")]), bucWeightmean_estim:=mean(bucWeight_obs), by=.(lanID), on=.(lanID, bucID)]
dat[,Nbuc_estim:=round(totWeight_obs/bucWeightmean_estim)]

# estimates lanID Weight total [lan_Weight_estim]
	# point estimate [just rounding effects]
	dat[, totWeight_estim:=bucWeightmean_estim*Nbuc_estim, by=.(lanID)]

	# variance of totWeight_estim
	sum((dat[lanID=="FIN_2025_11" & sp=="HER",]$bucWeight_obs-dat[lanID=="FIN_2025_11" & sp=="HER",]$bucWeightmean_estim)^2)/13
	#dat[lanID=="FIN_2025_11" & sp=="HER", list(s2=var(bucWeight_obs))]
	#dat[lanID=="FIN_2025_11", list(s2=var(bucWeight_obs))]
	dat<-merge(dat, dat[, unique(.SD),.SDcols=c("lanID","bucID","bucWeight_obs")][,list(var_totWeight_s2=var(bucWeight_obs)), by=.(lanID)], by="lanID", all.x=T)
 
	dat[, totWeight_estim_var:=Nbuc_estim^2*(1-nbuc_obs/Nbuc_estim)*var_totWeight_s2/nbuc_obs, by=.(lanID)]
	dat[, totWeight_estim_se:=sqrt(totWeight_estim_var), by=.(lanID)]
	dat[, totWeight_estim_cv:=round(totWeight_estim_se/totWeight_estim*100,2), by=.(lanID)]
	
	# CI with t approximation, df = nBuckets-1
	dat[, totWeight_estim_tvalue:=ifelse(nbuc_obs>1,qt(p=0.975, df=nbuc_obs-1),NA), by=.(lanID)]
	dat[, totWeight_estim_errMargin:=totWeight_estim_tvalue*totWeight_estim_se, by=.(lanID)]
	dat[, totWeight_estim_errMargin_CIlow:= totWeight_estim-totWeight_estim_errMargin, by=.(lanID)]
	dat[, totWeight_estim_errMargin_CIupp:= totWeight_estim+totWeight_estim_errMargin, by=.(lanID)]
	
#estimates lanID Weight spp [sppWeight_estim]
	#  point estimate
	dat[, sppWeight_estim:=sum(sppWeight_obs)/nbuc_obs*Nbuc_estim, by=.(lanID, sp)]
	
	# variance
	dat<-merge(dat, dat[, unique(.SD),.SDcols=c("lanID","bucID","sp","sppWeight_obs")][,list(var_sppWeight_s2=var(sppWeight_obs)), by=.(lanID, sp)], by=c("lanID","sp"), all.x=T)
	dat[, sppWeight_estim_var:=Nbuc_estim^2*(1-nbuc_obs/Nbuc_estim)*var_sppWeight_s2/nbuc_obs, by=.(lanID, sp)] # depends on N
	dat[, sppWeight_estim_se:=sqrt(sppWeight_estim_var), by=.(lanID, sp)]
	dat[, sppWeight_estim_cv:=round(sppWeight_estim_se/sppWeight_estim*100,2), by=.(lanID, sp)]
	
	# CI with t approximation, df = nBuckets-1
	dat[, sppWeight_estim_tvalue:=ifelse(nbuc_obs>1,qt(p=0.975, df=nbuc_obs-1),NA), by=.(lanID, sp)]
	dat[, sppWeight_estim_errMargin:=sppWeight_estim_tvalue*sppWeight_estim_se, by=.(lanID, sp)]
	dat[, sppWeight_estim_CIlow:= sppWeight_estim-sppWeight_estim_errMargin, by=.(lanID, sp)]
	dat[, sppWeight_estim_CIupp:= sppWeight_estim+sppWeight_estim_errMargin, by=.(lanID, sp)]
	
#estimates lanID perc spp in weight[sppPerWeight_estim]	
	#  point estimate
	dat[, sppPerWeight_estim:=sppWeight_estim/totWeight_estim, by=.(lanID, sp)]
	if(all(is.na(dat$totWeight_obs))) dat[, sppPerWeight_estim:=sum(sppWeight_obs)/bucWeight_obs, by=.(lanID, sp)]
	
	# variance
		# building S2spp
			tmp <- dat[, unique(.SD),.SDcols=c("lanID","bucID","bucWeight_obs","sp","sppPerWeight_estim","sppWeight_obs")][
				,list(teih_spp=sppWeight_obs-sppPerWeight_estim*bucWeight_obs), by=.(lanID, bucID, sp)][, list(sppPerWeight_s2=var(teih_spp)),.(lanID, sp)]

			
			
			dat<-merge(dat, tmp, by=c("lanID","sp"), all.x=T)
			
			dat[, sppPerWeight_estim_var:=(1/(sum(bucWeight_obs)/nbuc_obs)^2)*(1-nbuc_obs/Nbuc_estim)*sppPerWeight_s2/nbuc_obs,by=.(lanID,sp)] # depends on N [how much?]
			if(all(is.na(dat$totWeight_obs))) dat[, sppPerWeight_estim_var:=(1/(sum(bucWeight_obs)/nbuc_obs)^2)*sppPerWeight_s2/nbuc_obs,by=.(lanID,sp)] # depends on N [how much?]
		
				# the above is equiavalent to
				#dat[, sppPerWeight_estim_var1:=1/totWeight_estim^2*Nbuc_estim^2*(1-nbuc_obs/Nbuc_estim)*sppPerWeight_s2/nbuc_obs,by=.(lanID,sp)] # depends on N [how much?]
				# all.equal(dat$sppPerWeight_estim_var, dat$sppPerWeight_estim_var1)
			dat[, sppPerWeight_estim_se:=sqrt(sppPerWeight_estim_var), by=.(lanID, sp)]
			dat[, sppPerWeight_estim_cv:=round(sppPerWeight_estim_se/sppPerWeight_estim*100,2), by=.(lanID, sp)]
		# CI with t approximation, df = nBuckets-1
		dat[, sppPerWeight_estim_tvalue:=ifelse(nbuc_obs>1,qt(p=0.975, df=nbuc_obs-1),NA), by=.(lanID, sp)]
		dat[, sppPerWeight_estim_errMargin:=sppPerWeight_estim_tvalue*sppPerWeight_estim_se, by=.(lanID, sp)]
		dat[, sppPerWeight_estim_CIlow:= sppPerWeight_estim-sppPerWeight_estim_errMargin, by=.(lanID, sp)]
		dat[, sppPerWeight_estim_CIupp:= sppPerWeight_estim+sppPerWeight_estim_errMargin, by=.(lanID, sp)]

			# demo
			dat[,unique(.SD),.SDcols=c("lanID","sp","sppPerWeight_estim","sppPerWeight_estim_se","sppPerWeight_estim_errMargin")]

		# sampleSize
		dat[, e005:=ceiling(1/bucWeightmean_estim^2*(1.96*sqrt(sppPerWeight_s2)/0.05)^2), by=.(lanID, sp)]
		dat[, e003:=ceiling(1/bucWeightmean_estim^2*(1.96*sqrt(sppPerWeight_s2)/0.03)^2), by=.(lanID, sp)]
		dat[, e001:=ceiling(1/bucWeightmean_estim^2*(1.96*sqrt(sppPerWeight_s2)/0.01)^2), by=.(lanID, sp)]
		#dat[, e0005:=ceiling(1/bucWeightmean_estim^2*(1.96*sqrt(sppPerWeight_s2)/0.005)^2), by=.(lanID, sp)]
		
		unique(dat[sppPerWeight_estim>0,c("lanID","sp","nbuc_obs","sppPerWeight_estim","e005","e003","e001")])


# final checks:
	all(
	#sum of estimates spp weights == estimate total weight
	dat[,.N,.(lanID,sp,totWeight_estim,sppWeight_estim)][, sum(sppWeight_estim),.(lanID,totWeight_estim)][,all.equal(V1,totWeight_estim)],
	
	#sum of estimates spp perWeight ==  1
	dat[,.N,.(lanID,sp,sppPerWeight_estim)][, sum(sppPerWeight_estim),.(lanID)][,all.equal(V1,rep(1,.N))]
)

		unique(dat[,c("lanID","sp","nbuc_obs","sppPerWeight_estim","e005","e003","e001")])

	res_sppPerWeight<-unique(dat[,.(year, month, sampleType, lanID,sp, nbuc_obs,Nbuc_estim,totWeight_estim, sppPerWeight_estim, sppPerWeight_estim_var, sppWeight_estim,sppPerWeight_estim_se,sppPerWeight_estim_tvalue,
		sppPerWeight_estim,sppPerWeight_estim_tvalue,sppPerWeight_estim_cv,sppPerWeight_estim_errMargin,sppPerWeight_estim_CIlow,sppPerWeight_estim_CIupp, sppPerWeight_s2,
			e005, e003, e001)])[order(lanID,sp),]

plot(sppPerWeight_estim_cv~sppPerWeight_estim, data=res_sppPerWeight)
plot(sppPerWeight_estim_var~sppPerWeight_estim, data=res_sppPerWeight[sppPerWeight_estim_var<0.15,])
plot(sppPerWeight_estim_errMargin~sppPerWeight_estim, data=res_sppPerWeight[sppPerWeight_estim_var<0.15,])

	# complements
		res_sppPerWeight[order(totWeight_estim),sizeCateg:=cut(unique(totWeight_estim)/1000, breaks=c(0, 10, 20, 30, 40, 50, 75, 100, 200, 300, 400, 500, 1000, 1500), right = FALSE, ordered_result=T), by=.(lanID)]
		res_sppPerWeight[order(totWeight_estim),sizeCateg:=cut(unique(totWeight_estim)/1000, breaks=c(0, 20, 40, 75, 100, 200, 300, 400, 500, 1000, 1500), right = FALSE, ordered_result=T), by=.(lanID)]

	boxplot(V1~sizeCateg,
		data=res_sppPerWeight[sp %in% c("HER","SPR"),.N, .(sizeCateg, lanID, sp, e003)][, mean(e003), by=.(sizeCateg, sp)][order(sizeCateg),]
		,ylim=c(0,100))

# save estimates
	# totWeight
	res_totWeight<-unique(dat[,.(lanID,totWeight_obs,nbuc_obs,bucWeightmean_estim,Nbuc_estim,totWeight_estim,totWeight_estim_var,totWeight_estim_se,totWeight_estim_cv,
			totWeight_estim_tvalue,totWeight_estim_errMargin,totWeight_estim_errMargin_CIlow,totWeight_estim_errMargin_CIupp)])

	res_sppWeight<-unique(dat[,.(lanID,sp, nbuc_obs,Nbuc_estim, sppWeight_estim_tvalue,sppWeight_estim,sppWeight_estim_tvalue,sppWeight_estim_errMargin,sppWeight_estim_CIlow,sppWeight_estim_CIupp)])[order(lanID,sp),]

res_sppWeight
	res_sppPerWeight<-unique(dat[,.(lanID,sp, nbuc_obs,Nbuc_estim,sppPerWeight_estim, sppPerWeight_estim_var, sppWeight_estim,sppPerWeight_estim_se,sppPerWeight_estim_tvalue,
		sppPerWeight_estim,sppPerWeight_estim_tvalue,sppPerWeight_estim_errMargin,sppPerWeight_estim_CIlow,sppPerWeight_estim_CIupp, sppPerWeight_s2,
			e005, e003, e001)])[order(lanID,sp),]

# sample size
	



# completes data with auxiliary info
	# size category of landings
	
	# dominance of species

	# something on b ycatch?