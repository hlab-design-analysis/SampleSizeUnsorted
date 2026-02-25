# demo of adjustment

	min_bucs_obs<-2
	min_n<-0
		# implementation in estimaSppComp (via determineHighestObsWithinPercentile)
		rbindlist(lapply(split(summariseMax(x = dat[nbuc_obs>=min_bucs_obs,], group=c("lanID","fisheryArea"), min_n=min_n),by="fisheryArea"), function(x) {cbind(x[1,2],nLanIDs=nrow(x),round(t(apply(x[,4:ncol(x)],2,determineHighestObsWithinPercentile, target_prob=0.95))))}))[order(fisheryArea),]$n_0.050
		# alternative implementation
		do.call("rbind",lapply(split(dat, dat$fisheryArea), function(x){x[!is.na(n_0.050) & nbuc_obs>1,][,n_0.050:=ifelse(n_0.050<min_n,min_n,n_0.050)][,max(n_0.050), .(lanID,tmp)][order(V1),][,list(V2=V1<=tmp), .(lanID,V1, tmp)][V2==TRUE,][, tail(V1,1)]}))

	min_bucs_obs<-5
	min_n<-5
		# implementation in estimaSppComp (via determineHighestObsWithinPercentile)
		rbindlist(lapply(split(summariseMax(x = dat[nbuc_obs>=min_bucs_obs,], group=c("lanID","fisheryArea"), min_n=min_n),by="fisheryArea"), function(x) {cbind(x[1,2],nLanIDs=nrow(x),round(t(apply(x[,4:ncol(x)],2,determineHighestObsWithinPercentile, target_prob=0.95))))}))[order(fisheryArea),]$n_0.050
		# alternative implementation
		dat$tmp<-summary_095_min_bucs_obs_5$n_0.050[match(dat$fisheryArea,summary_095_min_bucs_obs_5$fisheryArea)]
		do.call("rbind",lapply(split(dat, dat$fisheryArea), function(x){x[!is.na(n_0.050) & nbuc_obs>=min_bucs_obs,][,n_0.050:=ifelse(n_0.050<min_n,min_n,n_0.050)][,max(n_0.050), .(lanID,tmp)][order(V1),][,list(V2=V1<=tmp), .(lanID,V1, tmp)][V2==TRUE,][, tail(V1,1)]}))

	min_bucs_obs<-5
	min_n<-0
		# implementation in estimaSppComp (via determineHighestObsWithinPercentile)
		dat$tmp<-summary_095_min_bucs_obs_5$n_0.050[match(dat$fisheryArea,summary_095_min_bucs_obs_5$fisheryArea)]
		# alternative implementation
		rbindlist(lapply(split(summariseMax(x = dat[nbuc_obs>=min_bucs_obs,], group=c("lanID","fisheryArea"), min_n=min_n),by="fisheryArea"), function(x) {cbind(x[1,2],nLanIDs=nrow(x),round(t(apply(x[,4:ncol(x)],2,determineHighestObsWithinPercentile, target_prob=0.95))))}))[order(fisheryArea),]$n_0.050
		do.call("rbind",lapply(split(dat, dat$fisheryArea), function(x){x[!is.na(n_0.050) & nbuc_obs>=min_bucs_obs,][,n_0.050:=ifelse(n_0.050<min_n,min_n,n_0.050)][,max(n_0.050), .(lanID,tmp)][order(V1),][,list(V2=V1<=tmp), .(lanID,V1, tmp)][V2==TRUE,][, tail(V1,1)]}))
