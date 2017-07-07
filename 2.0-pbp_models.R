library(reshape2)

nodes = 4
cl = makeCluster(nodes)

pbp <- read.csv("data/Base_PBP.csv")
names(pbp) = c("year", "month", "puerto_salida", "day", "day2", "yield", "est_yield", "fishing_area_method",
               "fleet_type_1", "fleet_type_2", "set", "total_set", "")

 [1] "AÑO"                     "MES"                     "PUERTO_SALIDA"           "DIA_SALIDA"             
 [5] "DIA_ARRIBO"              "CAPTURA_TOTAL"           "DESEMBARQUE_ESTIMADO"    "ELECCION_ZONA_PESCA"    
 [9] "TIPOLOGIA_1"             "TIPOLOGIA_2"             "NUMERO_CALA"             "TOTAL_CALAS"            
[13] "LON_GRAD_INI"            "LON_MIN_INI"             "LAT_GRAD_INI"            "LAT_MIN_INI"            
[17] "LON_GRAD_FIN"            "LON_MIN_FIN"             "LAT_GRAD_FIN"            "LAT_MIN_FIN"            
[21] "LONGITUD_INICIAL"        "LATITUD_INICIAL"         "LONGITUD_FINAL"          "LATITUD_FINAL"          
[25] "TEMP_SUPE_MAR"           "CAPTURA_ENVASADA_CALA"   "CAPTURA_CALA"            "EMBARCACIONES_AVISTADAS"
[29] "CAPTURA_ANCHOVETA"       "CAPTURA_CALA_1"          "TIPO_VIAJE"              "total"                  
[33] "length"                  "value"                   "rec"   



pbp <- pbp[pbp$total > 0, ]
pbp <- pbp[pbp$TIPO_VIAJE == "ANCHOVETERO", ]

measure.vars = grep(names(pbp), patt="ANCHOVETA_")
ancLEN = as.matrix(pbp[, measure.vars])
ancLEN[is.na(ancLEN)] = 0
ancLEN = t(apply(ancLEN, 1, cumsum))
pbp[, measure.vars] = ancLEN

pbp = melt(pbp, measure.vars = measure.vars)
pbp$variable = as.character(pbp$variable)
pbp$variable = as.numeric(gsub(gsub(x=pbp$variable, patt="ANCHOVETA_", rep=""), patt="_", rep="."))
pbp$rec = ifelse(pbp$value>0, 1, 0)

names(pbp)[grep(x=names(pbp), patt="variable")] = "length"


recsp01 = bam(rec ~ s(length), data=pbp, family=binomial(link="logit"), cluster=cl)

predBase        = expand.grid(length = seq(2,20, by = 0.1), year = seq(1995, 2017, by = 1), month = 1:12)
predBase$time   = with(predBase, year + (month-1)/12)
predBase        = predBase[sort(predBase$time, index=TRUE)$ix, ]
predBase$reg    = as.factor((predBase$year>1972) + (predBase$year>1992))
predBase$monthF = as.factor(predBase$month)
y               = unique(predBase$length)
x               = unique(predBase$time)



