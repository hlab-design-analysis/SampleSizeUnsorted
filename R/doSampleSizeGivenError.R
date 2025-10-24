	doSampleSizeGivenError<-function(x, e, error_type)
		{
		# checks
		if(!error_type %in% c("Percent","Absolute")) stop ("error_type value must be either 'Percent' or 'Absolute'")
		if(!is.numeric(e)) stop ("e value must be numeric")
		if(! all(c("lanID", "sp", "totWeight_obs","bucWeightmean_obs", "sppPercWeight_s2") %in% colnames(x))) stop ("check colnames in x")


		if (error_type=="Percent")
			{
			for (i in 1:length(e)) x[, paste0("n_", gsub("\\.","",as.character(e[i]))):=ceiling(1/bucWeightmean_obs^2*(1.96*sqrt(sppPercWeight_s2)/e[i])^2), by=.(lanID, sp)]
			}
		if (error_type=="Absolute")
			{
			for (i in 1:length(e)) x[, paste0("n_", gsub("\\.","",as.character(e[i]))):= ceiling((totWeight_obs/bucWeightmean_obs)^2*(1.96*sqrt(sppPercWeight_s2)/e[i])^2), by=.(lanID, sp)]
		}
		x
		}