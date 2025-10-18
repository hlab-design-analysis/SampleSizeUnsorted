# loads and prepares HaV Ctrl data sent to SLU

rm(list=ls())

library(reshape2)
library(data.table)

# load data
	dat<-readRDS("Original/HaVCtrl_strict.rds")

# re-orders and selects columns
dat<-dat[,.(lanID,bucID,sp, sppWeight_obs, totWeight_obs)]

saveRDS(dat, file="../data/HaVCtrl_strict.rds")
