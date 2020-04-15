# --------------------------- 0. Setup --------------------------------------

# Importing libraries
library(glmulti)
library(tidyverse)
library(rgdal)
library(mapview)
library(sf)
library(tmap)
library(gridExtra)
library(rgeos)
library(mapview)
library(leaflet)
library(mgcv)
library(rgdal)
library(rstudioapi)
library(gridExtra)
library(ggplot2)
library(dplyr)

# Setting the directory
dir = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir)

#---------------------------- 1. Data Exploration ------------------------------

# Importing the two DataBase
DB1 = read.csv("./DB1.csv")
DB1 = as_tibble(DB1)
DB2 = read.csv("./inspost.csv")
DB2 = as_tibble(DB2)

# Merging the two DB by Postal Code
DB = left_join(DB1, DB2, by = c("CODPOSS" = "CODPOSS"))

# Re-arranging the Columns to reflect: infos about the city, ph, car, policy
col_order <- c("CODPOSS", "COMMUNE", "LAT", "LONG", "INS", "AGEPH", "sexp", 
               "agecar", "fuelc", "split", "usec", "duree", "lnexpo", "nbrtotc",
               "nbrtotan", "chargtot")
DB = DB[, col_order]

# Rename the columns in lowercase and spaces with underscores
DB = DB %>% rename_all(function(.name) {
  .name %>% tolower 
})
DB = rename(DB, expo = duree)


DB %>% slice(1:3) 

# Some analysis
mean(DB$nbrtotc)

mean = sum(DB$nbrtotc)/sum(DB$expo)
mean
variance = sum((DB$nbrtotc - mean * DB$expo)^2)/sum(DB$expo)
variance

DB %>% summarize(emp_freq = sum(nbrtotc) / sum(expo)) 
freq_by_sex = DB %>% group_by(sexp) %>% summarize(emp_freq = sum(nbrtotc) / sum(expo))
freq_by_age = DB %>% group_by(ageph) %>% summarize(emp_freq = sum(nbrtotc) / sum(expo))

# Some graphs
g1 = ggplot(freq_by_age, aes(x = ageph, y = emp_freq)) + theme_bw() + 
  geom_bar(stat = "identity", color = "red",
           fill = "orange", alpha = .5) + 
  ggtitle("MTPL - empirical claim freq per age policyholder")
g1

g2 = ggplot(freq_by_sex, aes(x = sexp, y = emp_freq)) + theme_bw() +
  geom_bar(stat = "identity", color = "red", 
           fill = "orange", alpha = .5) + 
  ggtitle("MTPL - empirical claim freq per sex policyholder")
g2

col = "red"
fill = "orange"
ylab = "Relative frequency"
ggplot.bar = function(DT, variable, xlab){
  ggplot(data = DT, aes(as.factor(variable))) + theme_bw() + 
    geom_bar(aes(y = (..count..)/sum(..count..)), col = col, fill = fill, alpha = 0.5) + labs(x = xlab, y = ylab)
}

ggplot.hist = function(DT, variable, xlab, binwidth){
  ggplot(data = DT, aes(variable)) + theme_bw() + 
    geom_histogram(aes(y = (..count..)/sum(..count..)), binwidth = binwidth, col = col, fill = fill, alpha = 0.5) + 
    labs(x = xlab, y = ylab)
}

#Frequency
plot.eda.nclaims = ggplot.bar(DB, variable = DB$nbrtotc, "nclaims")
#Exposure
plot.eda.exp = ggplot.hist(DB, DB$expo, "expo", 0.05)
#Severity
DB.sev = DB %>% filter(chargtot > 0 & nbrtotan <= 81000)
plot.eda.amount = ggplot(data = DB.sev, aes(chargtot)) + 
  geom_density(adjust = 3, col = col, fill = fill, alpha = 0.5) + 
  xlim(0, 1e4) + ylab(ylab) + xlab("severity") + theme_bw()

g3 = grid.arrange(plot.eda.nclaims, plot.eda.exp, plot.eda.amount)
g3

plot.eda.fuel = ggplot.bar(DB, DB$fuelc, "fuel")
plot.eda.sex = ggplot.bar(DB, DB$sexp, "sex")
plot.eda.use = ggplot.bar(DB, DB$usec, "use")
DB$split = factor(DB$split, ordered = TRUE, levels = c("Once", "Twice", "Thrice", "Monthly"))
plot.eda.split = ggplot.bar(DB, DB$split, "split")
DB$agecar = factor(DB$agecar, ordered = TRUE, levels = c("0-1", "2-5", "6-10", ">10"))
plot.eda.agecar = ggplot.bar(DB, DB$agecar, "agec")
plot.eda.ageph = ggplot.hist(DB, DB$ageph, "ageph", 1)

g4 = grid.arrange(plot.eda.fuel, plot.eda.sex, plot.eda.use, plot.eda.split, plot.eda.ageph, plot.eda.agecar)
g4

#---------------------------- 2. Spatial Data ------------------------------

readShapefile = function(){
  belgium_shape <- readOGR(dsn = path.expand("./shape file Belgie postcodes"), 
                           layer = "npc96_region_Project1")
  belgium_shape <- spTransform(belgium_shape, CRS("+proj=longlat +datum=WGS84"))
  belgium_shape$id <- row.names(belgium_shape)
  return(belgium_shape)
}
belgium_shape = readShapefile()
plot.eda.map = ggplot(belgium_shape, aes(long, lat, group = group)) + 
  geom_polygon(fill = NA, colour = "black", size = 0.1) + 
  theme_bw()
plot.eda.map
mapview(belgium_shape)
fortify(belgium_shape)%>% slice(1:3)

post_expo = DB %>% group_by(codposs) %>% summarize(num = n(), total_expo = sum(expo)) 
post_expo %>% slice(1:5)

belgium_shape@data = left_join(belgium_shape@data, post_expo, by = c("POSTCODE" = "codposs"))
belgium_shape@data %>% slice(1:3) 

belgium_shape@data$freq = belgium_shape@data$total_expo/belgium_shape@data$Shape_Area
belgium_shape@data$freq_class = cut(belgium_shape@data$freq, breaks = quantile(belgium_shape@data$freq, c(0,0.2,0.8,1), na.rm = TRUE), right = FALSE, include.lowest = TRUE, labels = c("low","average","high"))
belgium_shape@data %>% slice(1:3) 

belgium_shape_f = fortify(belgium_shape)
belgium_shape_f = left_join(belgium_shape_f, belgium_shape@data)

plot.eda.map = ggplot(belgium_shape_f, aes(long, lat, group = group)) + 
  geom_polygon(aes(fill = belgium_shape_f$freq_class), colour = "black", size = 0.1)
plot.eda.map = plot.eda.map + theme_bw() + labs(fill = "Relative\nfrequency") + 
  scale_fill_brewer(palette = "Blues", na.value = "white")
plot.eda.map

#---------------------------- 3. GLM ------------------------------
# plotting the frequency and severity
plot.eda.nclaims
plot.eda.amount

# plotting the frequency by gender (*)
freq_by_sex
g2

# creating a GLM for frequency by sex
#freq_glm_sex = glm(nbrtotc ~ sexp, offset = log(expo),family = poisson(link = "log"), data = DB)
#freq_glm_sex %>% broom::tidy()
#summary(freq_glm_sex)

# the predicted values are equal to *
#exp(coef(freq_glm_sex)[1])
#exp(coef(freq_glm_sex)[1] + coef(freq_glm_sex)[2])

# GLM model comparison
freq_GLM = glmulti(nbrtotc ~ commune+ageph+sexp+agecar+fuelc+split+usec+expo+offset(lnexpo), family = poisson(link = "log"), confsetsize = 100, crit = aic, data = DB, intercept=FALSE, level=1, plotty=TRUE)
plot(freq_GLM, type = "p", highlight = c("lnexpo:ins"))
plot(freq_GLM, type = "w", highlight = c("lnexpo:ins"))
plot(freq_GLM, type = "s")


