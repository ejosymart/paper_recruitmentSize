library(reshape2)
library(mgcv)
library(fields)
library(parallel)
library(kali)

source("code/selectivityFunctions.R")

nodes = 4
meses = c("ene", "feb", "mar", "abr", "may", "jun", "jul", "ago", "sep", "oct", "nov", "dic")

cl = makeCluster(nodes)

# Read LC matrix ----------------------------------------------------------
datos        = read.csv("raw/base_frq_mensual.csv", row.names=1)
index        = rownames(datos)
catch        = rowSums(datos, na.rm = TRUE)
catch        = ifelse(catch==0, NA, catch)
datos        = datos/catch
datos        = melt(ifelse(t(apply(datos, 1, cumsum)) > 0, 1, 0))
names(datos) = c("time", "length", "cum")
datos$length = as.numeric(gsub(x=datos$length, patt="X", rep=""))
datos$time   = as.character(datos$time)
datos$month  = substr(datos$time, 1, 3) 
datos$year   = as.numeric(substr(datos$time, 5, 6))
datos$year   = with(datos, ifelse(year > 60, year + 1900, year + 2000))
datos$month  = match(datos$month, meses)                  
datos$time   = datos$year + (datos$month-1)/12
datos$monthF = as.factor(datos$month)
datos        = datos[sort(datos$time, index.return = TRUE)$ix, ]
datos$reg    = as.factor((datos$year > 1972) + (datos$year > 1992))


# Base prediction ---------------------------------------------------------
predBase        = expand.grid(length = seq(2,20, by = 0.1), year = seq(1961, 2017, by = 1), month = 1:12)
predBase$time   = with(predBase, year + (month-1)/12)
predBase        = predBase[sort(predBase$time, index=TRUE)$ix, ]
predBase$reg    = as.factor((predBase$year>1972) + (predBase$year>1992))
predBase$monthF = as.factor(predBase$month)
y               = unique(predBase$length)
x               = unique(predBase$time)


# Recruitment size --------------------------------------------------------
rec0a = gam(cum ~ s(length), family = binomial(link = "logit"), data = datos, cluster=cl)
rec1a = gam(cum ~ te(time, length), family = binomial(link = "logit"), data = datos, cluster=cl)
rec2a = gam(cum ~ te(time, length, month), family = binomial(link = "logit"), data = datos, cluster=cl)
rec3a = gam(cum ~ te(length, by=reg), family = binomial(link = "logit"), data = datos, cluster=cl)
rec4a = gam(cum ~ te(length, by=monthF), family = binomial(link = "logit"), data = datos, cluster=cl)

R0a  = predict(rec0a, newdata = predBase, type="response")
R1a  = predict(rec1a, newdata = predBase, type="response")
R2a  = predict(rec2a, newdata = predBase, type="response")
R3a  = predict(rec3a, newdata = predBase, type="response")
R4a  = predict(rec4a, newdata = predBase, type="response")

lr.0a = L50(x, y, R0a)
lr.1a = L50(x, y, R1a)
lr.2a = L50(x, y, R2a)
lr.3a = L50(x, y, R3a)
lr.4a = L50(x, y, R4a)

la.0a = L50(x, y, R0a, level=0.95)
la.1a = L50(x, y, R1a, level=0.95)
la.2a = L50(x, y, R2a, level=0.95)
la.3a = L50(x, y, R3a, level=0.95)
la.4a = L50(x, y, R4a, level=0.95)

par(mfrow=c(2,3), mar=c(3,3,3,3), oma=c(1,1,1,1))
selectivityPlot(x, y, R0a, levels = c(0.5, 0.95))
selectivityPlot(x, y, R1a, levels = c(0.5, 0.95))
selectivityPlot(x, y, R2a, levels = c(0.5, 0.95))
selectivityPlot(x, y, R3a, levels = c(0.5, 0.95))
selectivityPlot(x, y, R4a, levels = c(0.5, 0.95))
