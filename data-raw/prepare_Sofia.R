# loads and prepares FIN data sent by Sofia Pettersen

rm(list=ls())

library(reshape2)
library(data.table)

# load data
dat <- data.table(read.csv2("Original/Fin_2025_data_Sofia - compiled.csv"))

dat <- data.table::melt(data = dat, id.vars=c("Resa","LandingWeight","BucketName","BucketWeight","BucketTon"), measure.vars=c("HER","SPR","GTA","SCU","SME","ELP"), value.name="SppInBucketWeight", variable.name="Spp")

dat$sppWeight_obs<-dat$sppWeight_obs/1000

colnames(dat)[colnames(dat)=="Resa"]<-"lanID"
colnames(dat)[colnames(dat)=="LandingWeight"]<-"totWeight_obs"
colnames(dat)[colnames(dat)=="BucketName"]<-"bucID"
colnames(dat)[colnames(dat)=="Spp"]<-"sp"
colnames(dat)[colnames(dat)=="Spp"]<-"sp"
colnames(dat)[colnames(dat)=="SppInBucketWeight"]<-"sppWeight_obs"

# re-orders and selects columns
dat<-dat[,.(lanID,bucID,sp, sppWeight_obs, totWeight_obs)]

saveRDS(dat, file="../data/Sofia.rds")
