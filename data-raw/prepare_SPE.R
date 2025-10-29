# loads and prepares SPE.dat

rm(list=ls())
library(data.table)


dat <- read.table("Original/SPE.dat", head=TRUE)
dat$totWeight_obs<-NA # from control
dat<-data.table(dat)
dat$sppWeight_obs<-dat$w; dat$w<-NULL
dat$lanID<-dat$lan
dat$bucID<-dat$buc

# re-orders and selects columns
dat<-dat[,.(lanID,bucID,sp, sppWeight_obs, totWeight_obs)]

saveRDS(dat, file="../data/SPE.rds")