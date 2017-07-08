library(parallel)
library(mgcv)
library(kali)
source("aux_functions.R")

nodes = 4
cl = makeCluster(nodes)

pbp = read.csv("input/base_sizeRecruitment_pbp.csv", row.names = 1)
pbp$time = with(pbp, year + (month-1)/12)

# Prediction database -----------------------------------------------------

dx = 1/2
dy = 1/2
xlat  = range(pretty(pbp$lat))
xlon  = range(pretty(pbp$lon))
xsize = range(pbp$length)
xdc   = range(pretty(pbp$dc))
  
predBaseLat        = expand.grid(lat=seq(from=xlat[1], to=xlat[2], by=dy), 
                                 month = 1:12, year = seq(1995, 2017, by = 1), 
                                 length = seq(from=xsize[1], to=xsize[2], by = 0.5))

predBaseLat        = expand.grid(lat=seq(from=xlat[1], to=xlat[2], by=dy), 
                                 length = seq(from=xsize[1], to=xsize[2], by = 0.5))

predBaseDC         = expand.grid(dc=seq(from=xdc[1], to=xdc[2], by=dx), 
                                 month = 1:12, year = seq(1995, 2017, by = 1), 
                                 length = seq(from=xsize[1], to=xsize[2], by = 0.5))

# predBaseLat$time   = with(predBaseLat, year + (month-1)/12)
# predBaseDC$time    = with(predBaseDC, year + (month-1)/12)
# predBaseLat        = predBaseLat[sort(predBaseLat$time, index=TRUE)$ix, ]
# predBaseDC         = predBaseDC[sort(predBaseDC$time, index=TRUE)$ix, ]
# predBaseLat$monthF = as.factor(predBaseLat$month)
# predBaseDC$monthF  = as.factor(predBaseDC$month)
y                  = unique(predBaseLat$lat)
x                  = unique(predBaseLat$length)



# Fitting models ----------------------------------------------------------

DateStamp()
recsp1 = gam(rec ~ s(length, k=20, bs="cr"), data=pbp, family=binomial(link="logit"), cluster=cl)
DateStamp()
recsp2 = bam(rec ~ te(length, lat, k=c(10, 5)), data=pbp, family=binomial(link="logit"), 
             method = "REML", cluster=cl)
DateStamp()


recsp3 = bam(rec ~ te(length, lat), data=pbp, family=binomial(link="logit"), cluster=cl)
DateStamp()

r0lat1 = predict(recsp1, newdata = predBaseLat, type="response", cluster=cl)

selectivityPlot(x, y, r0lat1)

r0lat2 = predict(recsp2, newdata = predBaseLat, type="response", cluster=cl)
r0lat3 = predict(recsp3, newdata = predBaseLat, type="response", cluster=cl)


selectivityPlot(x, y, r0lat1, levels = c(0.5, 0.95))
