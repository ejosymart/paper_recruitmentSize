library(reshape2)
library(kali)
library(sp)
library(maptools)

source("aux_functions.R")

raw <- read.csv("raw/Base_PBP.csv", stringsAsFactors = FALSE, na.strings = c("NA", "", "SIN DATO"))

pbp = data.frame(year=raw$AÑO, month=raw$MES, port=raw$PUERTO_SALIDA, 
                 yield=raw$CAPTURA_ANCHOVETA, fishing_area_method=raw$ELECCION_ZONA_PESCA,
                 type1=raw$TIPOLOGIA_1, type2=raw$TIPOLOGIA_2, trip_type=raw$TIPO_VIAJE,
                 set=raw$NUMERO_CALA, total_set=raw$TOTAL_CALAS, sst=raw$TEMP_SUPE_MAR,
                 lat = -raw$LATITUD_INICIAL, lon = -raw$LONGITUD_INICIAL)

measure.vars = grep(names(raw), patt="ANCHOVETA_")
ancLEN = as.matrix(raw[, measure.vars])
ancLEN[is.na(ancLEN)] = 0
ancLEN = t(apply(ancLEN, 1, cumsum))
pbp = cbind(pbp, ancLEN)
pbp$ncatch = rowSums(ancLEN)

# filters
coast = read.csv("input/costa_peps.csv")
shelf = read.csv("input/shelfBreak_peps.csv")
xdist     = getSignedDistance(data=pbp, ref=shelf, abs=coast)
pbp$shelf = round(xdist$dist, 3)
pbp$dc    = round(xdist$abs, 3)

ind1 = validPoints(lon=pbp$lon, lat=pbp$lat) # removing point from land
ind2 = which(isInside(pbp$dc, range = c(5, 100)*111/60)) # removing industrial sets
ind3 = which(isInside(pbp$lat, range = c(-16, -3.33))) # northern-central area
ind4 = which(pbp$ncatch > 0)

ind = intersect(intersect(intersect(ind1, ind2), ind3), ind4)

plot.map(pbp)
pbp = pbp[ind, ]
plot.map(pbp)

measure.vars = grep(names(pbp), patt="ANCHOVETA_")
pbp = melt(pbp, measure.vars = measure.vars)
pbp$variable = as.character(pbp$variable)
pbp$variable = as.numeric(gsub(gsub(x=pbp$variable, patt="ANCHOVETA_", rep=""), patt="_", rep="."))
pbp$rec = ifelse(pbp$value>0, 1, 0)
names(pbp)[grep(x=names(pbp), patt="variable")] = "length"

write.csv(pbp, file="input/base_sizeRecruitment_pbp.csv")

