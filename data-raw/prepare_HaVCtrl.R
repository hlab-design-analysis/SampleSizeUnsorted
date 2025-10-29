# loads and prepares HaV Ctrl data sent to SLU

rm(list=ls())

library(data.table)

# load data
dat<-readRDS("Original/HaVCtrl_strict.rds")

# anonymises lanID
dat$lanID<-as.integer(factor(dat$lanID))

# re-orders and selects columns
dat<-dat[order(lanID,bucID,sp),.(lanID,bucID,sp, sppWeight_obs, totWeight_obs)]

saveRDS(dat, file="../data/HaVCtrl_strict.rds")
