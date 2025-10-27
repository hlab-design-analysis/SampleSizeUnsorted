# loads and prepares SLU data (extracted on 20251025 using script 002b_EstimationLandings.r 

rm(list=ls())

library(reshape2)
library(data.table)

# load SLU data
dat <- data.table(read.csv("Original/BucketData2021Q1_AandB.csv"))

# moves vikt.oidentifierbart..kg. to spp
dat<-rbind(dat[,.(Resa,BucketName,BucketNameNew=SAparentId,BucketWeight,BucketTon,Spp,SppInBucketWeight,CatchFishersWeight)],
	unique(dat[,.(Resa,BucketName,BucketNameNew=SAparentId,BucketWeight,BucketTon,Spp="vikt.oidentifierbart..kg.",SppInBucketWeight=vikt.oidentifierbart..kg.,CatchFishersWeight)]))

# expands spp
	all_combinations <- dat[, CJ(id = unique(paste(Resa, BucketNameNew)), Spp = unique(Spp))]
	dat[,id:=paste(Resa, BucketNameNew)]
	dat <- merge(all_combinations, dat, by =c("id","Spp"), all.x = TRUE)

	aux<-do.call("rbind",strsplit(dat$id," "))
	target_v<-is.na(dat$Resa)
	dat[target_v,"Resa"]<-as.character(aux[target_v,][,1])
	dat[target_v,"BucketNameNew"]<-as.integer(aux[target_v,][,2])
	dat[,BucketName:=unique(BucketName[!is.na(BucketName)]) , by=.(Resa, BucketNameNew)]
	dat[,BucketWeight:=unique(BucketWeight[!is.na(BucketWeight)]) , by=.(Resa, BucketNameNew)]
	dat[,BucketTon:=unique(BucketTon[!is.na(BucketTon)]) , by=.(Resa, BucketNameNew)]
	dat[,CatchFishersWeight:=unique(CatchFishersWeight[!is.na(CatchFishersWeight)]) , by=.(Resa, BucketNameNew)]
	dat[is.na(SppInBucketWeight)]$SppInBucketWeight<-0

dat$sppWeight_obs<-dat$sppWeight_obs/1000

colnames(dat)[colnames(dat)=="Resa"]<-"lanID"
colnames(dat)[colnames(dat)=="CatchFishersWeight"]<-"totWeight_obs"
colnames(dat)[colnames(dat)=="BucketNameNew"]<-"bucID"
colnames(dat)[colnames(dat)=="Spp"]<-"sp"
colnames(dat)[colnames(dat)=="SppInBucketWeight"]<-"sppWeight_obs"

# totWeight_obs set to kg
dat$totWeight_obs<-dat$totWeight_obs*1000

# sp set to "FAO-ish" codes
dat$sp[dat$sp=="Sill"]<-"HER"
dat$sp[dat$sp=="Skarpsill"]<-"SPR"
dat$sp[dat$sp=="Spigg"]<-"GTA"
dat$sp[dat$sp=="Skrubba"]<-"FLE"
dat$sp[dat$sp=="blåvitling"]<-"WHB"
dat$sp[dat$sp=="vikt.oidentifierbart..kg."]<-"UNK"
unique(dat$sp)

# masks lanID
dat$lanID<-as.integer(factor(dat$lanID))+1000


# re-orders and selects columns
dat<-dat[,.(lanID,bucID,sp, sppWeight_obs, totWeight_obs)]

saveRDS(dat, file="../data/SLU.rds")
