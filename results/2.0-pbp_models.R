library(parallel)
library(mgcv)
library(kali)

nodes = 4
cl = makeCluster(nodes)

pbp = read.csv("input/base_sizeRecruitment_pbp.csv", row.names = 1)
pbp = with(pbp, year + (month-1)/12)

# Prediction database -----------------------------------------------------

dx = 1/2
dy = 1/2
xlat = range(pretty(pbp$lat))
xlon = range(pretty(pbp$lon))
xsize = range(pretty(pbp$length))

predBase        = expand.grid(lon=seq(from=xlon[1], to=xlon[2], by=dx),
                              lat=seq(from=xlat[1], to=xlat[2], by=dy),
                              month = 1:12, year = seq(1995, 2017, by = 1), 
                              length = seq(from=xsize[1], to=xsize[2], by = 0.5))

predBase$time   = with(predBase, year + (month-1)/12)
predBase        = predBase[sort(predBase$time, index=TRUE)$ix, ]
predBase$monthF = as.factor(predBase$month)
y               = unique(predBase$length)
x               = unique(predBase$time)


# Fitting models ----------------------------------------------------------

DateStamp()
recsp1 = bam(rec ~ s(length, bs="cr"), data=pbp, family=binomial(link="logit"), cluster=cl)
DateStamp()
recsp2 = bam(rec ~ te(time, length), data=pbp, family=binomial(link="logit"), cluster=cl)
DateStamp()
recsp3 = bam(rec ~ te(length, lat), data=pbp, family=binomial(link="logit"), cluster=cl)
DateStamp()


r0 = predict(recsp1, newdata = predBase, type="response", cluster=cl)
