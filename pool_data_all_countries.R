
rm(list=ls())
library(data.table)

source("R/doInitialChecks.R")
source("R/generateZeroSpecies.r")
dat<-data.table(rbind(readRDS("data/DNK_3rd_party.rds"), 
readRDS("data/FIN.rds"),
readRDS("data/LVA.rds"),
readRDS("data/EST.rds"),
readRDS("data/IRL_SFPA.rds"),
readRDS("data/SWE_Baltic_HERSPR_HUC.rds"),
readRDS("data/SWE_other.rds")))

# generatesZeroSpecies
nrow(dat)
ls1<-split(dat, dat$fisheryArea)
dat<-rbindlist(lapply(ls1,generateZeroSpecies)); nrow(dat)

# checks data
doInitialChecks(dat)

#saves
saveRDS(dat, file="./data/all_countries.rds")


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
 