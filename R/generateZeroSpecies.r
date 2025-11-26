# aux function to generate zeros on a prepared object x
# when data i

generateZeroSpecies<-function(x)
	{
	all_combinations <- x[, CJ(id = unique(paste(lanID, bucID)), sp = unique(sp))]
	x[,id:=paste(lanID, bucID)]
	
	# to be used later
	missing_combinations <- paste(all_combinations$id,all_combinations$sp)[
			!paste(all_combinations$id,all_combinations$sp) %in% paste(x$id, x$sp)]
	
	x1 <- merge(all_combinations, x, by =c("id","sp"), all.x = TRUE)

	# completes variables
	aux<-do.call("rbind",strsplit(x1$id," "))
	target_v<-is.na(x1$lanID)
	x1[target_v,"lanID"]<-as.integer(aux[target_v,][,1])
	x1[target_v,"bucID"]<-as.integer(aux[target_v,][,2])
	for (i in colnames(x1)[!colnames(x1) %in% c("lanID","bucID","sppWeight_obs","id")])
	{
	x1[,(i):=unique(get(i)[!is.na(get(i))]) , by=.(lanID, bucID)]
	}
	# fills in zeros
	x1$sppWeight_obs[paste(x1$id, x1$sp) %in% missing_combinations]<-0
	x1
	}