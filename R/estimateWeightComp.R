#' Estimate Weight Composition in Percentage and Absolute Weight
#'
#' This function calculates species composition in weight for a set of landings.
#'
#' @param x Input data (previously checked)
#' @param round_Nbuc_estim should Nbuc_estim be rounded? if FALSE (default) Nbuc_estim is calculated as 
#' totWeight_obs/bucWeightmean_obs. If TRUE rounds calculation of Nbuc_estim default to nearest integer
#' @param finitePopCorr should finite population correction be used? defaults to FALSE
#'
#' @return same x object with estimates of species composition as columns
#'
#' @details rounding to nearest integer is more consistent with definition of sampling unit, sampling fraction, etc, 
#' but will introduces a minor error in landing level estimates.
#'
#' @examples
#' \dontrun{
#' dat <- readRDS("data/HaVCtrl_strict.rds")
#' doInitialChecks(dat)
#' dat<-estimateWeightComp(dat)
#' dat
#' }


	

estimateWeightComp<-function(x, round_Nbuc_estim=FALSE, finitePopCorr=FALSE){


# add bucWeight_obs and nbuc_obs
x[,bucWeight_obs:=sum(sppWeight_obs), by=.(lanID, bucID)][,nbuc_obs:=length(unique(bucID)), by=.(lanID)]

# adds mean bucket weight (bucWeightmean_obs)
x[unique(x[, .SD, .SDcols=c("lanID","bucID","bucWeight_obs")]), bucWeightmean_obs:=mean(bucWeight_obs), by=.(lanID), on=.(lanID, bucID)]

# adds an estimate of the number of buckets per landing (Nbuc_estim)
x[,Nbuc_estim:=ifelse(round_Nbuc_estim, round(totWeight_obs/bucWeightmean_obs), totWeight_obs/bucWeightmean_obs), by=.(lanID)]

# checks
	all(x[,length(unique(bucWeightmean_obs)), .(lanID)]$V1==1)
	all(x[,length(unique(Nbuc_estim)), .(lanID)]$V1==1)
	

#part1: estimates lanID perc spp in weight[sppPercWeight_estim]
	
	#  point estimate
	x[, sppPercWeight_estim:=sum(sppWeight_obs)/sum(bucWeight_obs), by=.(lanID, sp)]
	
	# variance
		# building sppPercWeight_s2
			tmp <- x[, unique(.SD),.SDcols=c("lanID","bucID","bucWeight_obs","sp","sppPercWeight_estim","sppWeight_obs")][
				,list(teih_spp=sppWeight_obs-sppPercWeight_estim*bucWeight_obs), by=.(lanID, bucID, sp)][, sppPercWeight_s2:=var(teih_spp),.(lanID, sp)]
			x$sppPercWeight_s2<-tmp$sppPercWeight_s2[match(paste0(x$lanID, x$sp),paste0(tmp$lanID, tmp$sp))]
		
		# building sppPercWeight_estim_var (with finite correction factor)
			if(all(!is.na(x$totWeight_obs))){
			if(finitePopCorr){
			x[, sppPercWeight_var_estim:=1/(bucWeightmean_obs^2)*(1-nbuc_obs/Nbuc_estim)*sppPercWeight_s2/nbuc_obs,by=.(lanID,sp)]
			} else{
			x[, sppPercWeight_var_estim:=1/(bucWeightmean_obs^2)*sppPercWeight_s2/nbuc_obs,by=.(lanID,sp)]
			}	
			} else {
			print("warning: some totWeight_obs not available: ignoring finite population correction")
			x[, sppPercWeight_var_estim:=1/(bucWeightmean_obs^2)*sppPercWeight_s2/nbuc_obs,by=.(lanID,sp)]
			}
			
	# se and cv		
		x[, sppPercWeight_estim_se:=sqrt(sppPercWeight_var_estim), by=.(lanID, sp)]
		x[, sppPercWeight_estim_cv:=round(sppPercWeight_estim_se/sppPercWeight_estim*100,2), by=.(lanID, sp)]
	
	# CI with normal approximation
		x[, sppPercWeight_estim_zvalue:=ifelse(nbuc_obs>1,qnorm(0.975),NA), by=.(lanID, sp)]
		x[, sppPercWeight_estim_errMargin:=sppPercWeight_estim_zvalue*sppPercWeight_estim_se, by=.(lanID, sp)]
		x[, sppPercWeight_estim_CIlow:= sppPercWeight_estim-sppPercWeight_estim_errMargin, by=.(lanID, sp)]
		x[, sppPercWeight_estim_CIupp:= sppPercWeight_estim+sppPercWeight_estim_errMargin, by=.(lanID, sp)]

			# demo
			# x[sppPercWeight_estim_errMargin>0,unique(.SD),.SDcols=c("lanID","Nbuc_estim","sp","nbuc_obs","sppPercWeight_estim","sppPercWeight_estim_zvalue","sppPercWeight_estim_se","sppPercWeight_estim_errMargin","sppPercWeight_estim_CIlow","sppPercWeight_estim_CIupp")]

#part2: estimates Weight spp per lanID via species composition

	if(all(!is.na(x$totWeight_obs))){

	x[, sppWeight_estim := totWeight_obs*sppPercWeight_estim, by=.(lanID, sp)]
	
	x[, sppWeight_estim_var := totWeight_obs^2*sppPercWeight_var_estim, by=.(lanID, sp)]
	#x[, sppWeight_estim_var := Nbuc_estim^2*sppPercWeight_s2/nbuc_obs*(1-nbuc_obs/Nbuc_estim), by=.(lanID, sp)]
	
	x[, sppWeight_estim_se := sqrt(sppWeight_estim_var), by=.(lanID, sp)]
	x[, sppWeight_estim_cv := round(sppPercWeight_estim_se/sppPercWeight_estim*100,2), by=.(lanID, sp)]

	# CI with t approximation, df = nBuckets-1
		x[, sppWeight_estim_zvalue:=ifelse(nbuc_obs>1,qnorm(0.975),NA), by=.(lanID, sp)]
		x[, sppWeight_estim_errMargin:=sppWeight_estim_zvalue*sppWeight_estim_se, by=.(lanID, sp)]
		x[, sppWeight_estim_CIlow:= sppWeight_estim-sppWeight_estim_errMargin, by=.(lanID, sp)]
		x[, sppWeight_estim_CIupp:= sppWeight_estim+sppWeight_estim_errMargin, by=.(lanID, sp)]

		# demo
		# unique(x[sppPercWeight_estim>0,c("lanID","sp", "nbuc_obs","totWeight_obs","bucWeightmean_obs","sppPercWeight_estim","sppWeight_estim","sppWeight_estim_se","sppWeight_estim_CIlow","sppWeight_estim_CIupp")])


} else {
print("some totWeight_obs are NA: sppWeight_estim cannot be calculated")
	
	x[, sppWeight_estim := NA]
	x[, sppWeight_estim_var := NA]
	x[, sppWeight_estim_zvalue:=NA]
	x[, sppWeight_estim_errMargin:=NA]
	x[, sppWeight_estim_CIlow:= NA]
	x[, sppWeight_estim_CIupp:= NA]

}

# final checks:
	if(all(!is.na(x$totWeight_obs))){
		#sum of estimates spp weights == estimate total weight
		test1<-x[,.N,.(lanID,sp,totWeight_obs,sppWeight_estim)][, sum(sppWeight_estim),.(lanID,totWeight_obs)][,all.equal(V1,totWeight_obs)]
		#sum of estimates spp PercWeight ==  1
		test2<-x[,.N,.(lanID,sp,sppPercWeight_estim)][, sum(sppPercWeight_estim),.(lanID)][,all.equal(V1,rep(1,.N))] 
		if(!all(c(test1,test2))){stop()
		} else {
		test2<-x[,.N,.(lanID,sp,sppPercWeight_estim)][, sum(sppPercWeight_estim),.(lanID)][,all.equal(V1,rep(1,.N))] 
		if(!test2) stop()}
	}
x
}
