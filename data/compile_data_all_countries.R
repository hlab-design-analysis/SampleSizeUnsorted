# run after having run individual preparations of each country

rm(list=ls())
library(data.table)

source("../R/doInitialChecks.R")
source("../R/generateZeroSpecies.r")

dat<-data.table(rbind(readRDS("DNK_3rd_party.rds"), 
readRDS("FIN.rds"),
readRDS("LVA.rds"),
readRDS("EST.rds"),
readRDS("IRL_SFPA.rds"),
readRDS("SWE_Baltic_HERSPR_HUC.rds"),
readRDS("SWE_other.rds")))

# remove SWE & DNK SPR 3a from NorthSea_SPR
dat<-dat[!(lanID %in% c("SWE_3020","SWE_3090", "SWE_3119", "SWE_3136","DNK_1","DNK_9")),]

# generatesZeroSpecies
nrow(dat)
ls1<-split(dat, dat$fisheryArea)
dat<-rbindlist(lapply(ls1,generateZeroSpecies)); nrow(dat)

# checks data
doInitialChecks(dat)

#saves
saveRDS(dat, file="all_countries.rds")


#=====================
# some exploratory analysis below
#=====================

# No landings per fishery
dat[,.N, .(lanID,ctry,fisheryArea)][,.N,.(ctry,fisheryArea)]

# No landings per fishery and country
{
a<-dat[,.N, .(lanID,ctry,fisheryArea)][,.N,.(ctry,fisheryArea)]
tapply(a$N, a[,2:1], sum)
}

# mean number buckets per fishery
{
a<-dat[,.N, .(lanID,ctry,fisheryArea)][,.N,.(fisheryArea)]
a1<-tapply(a$N, a[,1], sum)
b<-dat[,.N, .(bucID,lanID,ctry,fisheryArea)][,.N,.(fisheryArea)]
b1<-tapply(b$N, b[,1], sum)
round(b1/a1,1)
}

# No landings per fishery and country
{
a<-dat[,.N, .(lanID,ctry,fisheryArea)][,.N,.(ctry,fisheryArea)]
a1<-tapply(a$N, a[,2:1], sum)
b<-dat[,.N, .(bucID,lanID,ctry,fisheryArea)][,.N,.(ctry,fisheryArea)]
b1<-tapply(b$N, b[,2:1], sum)
round(b1/a1,1)
}

# landings with few buckets
dat[,.N, .(bucID,lanID,ctry,fisheryArea)][,list(nbuckets=.N),.(lanID,ctry,fisheryArea)][,list(less2=sum(nbuckets<2),less4=sum(nbuckets<4)),.(fisheryArea,ctry)]
 