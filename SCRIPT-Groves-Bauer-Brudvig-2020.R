### R Script for Groves, Bauer, Brudvig 2020 (Scientific Reports) ####
### First created by Groves, March 2018 #### 
### Last update: 18 Sep 2019 ####

### SETUP ############################
### Clear workspace ----
#rm(list=ls())

### Load packages ----
# for function as.data.table() when uploading data:
library(data.table)
# for function ddply() when summing seeding densities:
library(plyr)
# for function ggbiplot() and geom_text_repel() to visualize soils PCA:
library(ggbiplot)
library(ggrepel)
# for histograms of variables:
library(ggplot2); theme_set(theme_bw())
# for function case_when() to setup bins for interaction plot:
library(dplyr)
# for Anova() function, to get F values for linear model results 
library(car)
# for r2beta() function, to get R2 values for linear model results
library(r2glmm)
# for effect_plot() function, for Figure 1
library(jtools)

### Set working directory ----
setwd("C:/Users/Anna/Documents/Prairie-Project-SHARE/")

### Load data file ----
data <- as.data.table(read.csv("Groves-Bauer-Brudvig-2020-MASTER-DATA.csv", header=T))
colnames(data)

## Site information used: 
# "Site"
# "Landscape"                
# "Age_2016"
# "Last.Burn"  
# "Mix.Richness"

## Weather metrics used:
# "Jun1.dd.accum"             
# "Jun1.precip.accum"        
# "summer.dd.accum"           
# "summer.precip.accum"      
# "min.month.precip"         
# "Sep1.dd.accum"             
# "Sep1.precip.accum"        
# "avg.low.temp"     
# "max.month.dd.accum"
# "max.tot.month.precip"
# "max.days.no.precip"  
# "avg.mon.rain.days"         
     
## Response variables used:
# "Site.Richness.Total"
# "Sown.Richness.Total"
# "Nonsown.Richness.Total" 
# "Mean.Sown.Cover"
# "Mean.Nonsown.Cover"
# "Mix.Veg.BCdissim"  

## Soil metrics used:
# "pH"                       
# "Organic.Matter.percent"    
# "S.ppm"                    
# "P.mg.per.kg"               
# "Ca.mg.per.kg"             
# "Mg.mg.per.kg"              
# "K.mg.per.kg"              
# "Na.mg.per.kg"              
# "B.half.detection"         
# "Fe.mg.per.kg"              
# "Mn.mg.per.kg"             
# "Cu.half.detection"         
# "Zn.mg.per.kg"             
# "Al.mg.kg"                  
# "Clay.percent"             
# "Water.Holding.Capacity"

## Not used in this manuscript:
data[,c("Biomass", "Apr1.dd.accum", 
    "Apr1.precip.accum", "May1.dd.accum", "May1.precip.accum", 
    "Jul1.dd.accum", "Jul1.precip.accum", "Aug1.dd.accum", 
    "Aug1.precip.accum", "Oct1.dd.accum", "Oct1.precip.accum", 
    "max.mean.month.precip", "high.temp", "avg.high.temp", 
    "low.temp", "avg.temp", "spring.temp", "max.tot.month.precip",
    "summer.temp", "Mean.ANDGER.Cover", "Mean.SOLCAN.Cover",
    "Mean.SCHSCO.Cover", "Mean.SORNUT.Cover", "Mean.POAPRA.Cover",
    "Mean.Rubus.Cover", "Mean.RATPIN.Cover", "Mean.DESCAN.Cover",
    "Mean.ELYCAN.Cover", "Mean.MONFIS.Cover", "Num.Plots", 
    "Percent.of.Mix.Sp.Present", "Site.Richness.Walkthru",
    "Site.Richness.Plots", "Sown.Richness.Plots",
    "Nonsown.Richness.Plots", "Mean.Plot.Richness"):=NULL] 
colnames(data)

## Drop sites 0 years old, not planted, or missing data:
# Which sites?
data[data$Age_2016=="0", "Site"] # None
data[data$Age_2016=="1", "Site"] # ELF_GenchAgSE, ELF_GenchAgSW, these o.k.
data[is.na(data$Age_2016), "Site"] # K2015, ConradN, FN, Gilchrist Phase 1, Gilchrist Phase 2
# Drop 'em:
data <- data[data$Site!="FN",] 
data <- data[data$Site!="ConradN",] 
data <- data[data$Site!="GilchristPhase1",] 
data <- data[data$Site!="GilchristPhase2",] 
data <- data[data$Site!="K2015",] 
data$Site <- droplevels(data$Site)

### Get seeding density ----

# Load seed mix data:
seedmixinfo <- as.data.table(read.csv("Groves-Bauer-Brudvig-2020-Seeds-Density.csv", header=T))
head(seedmixinfo)
# This dataset is per site, per species

# Pull out Lbs per acre (which is the total per site):
seedmixinfo <- seedmixinfo[,c("Site","Total.Lbs.Acre")]
head(seedmixinfo)

# Really we just need one value per site
# But because the total is the same per site, we can take the mean
# (Probably a better way to do this)
mix.density <- ddply(seedmixinfo, .(Site), numcolwise(mean), na.rm=T)
head(mix.density)

# Convert pounds/acre to kg/hectare:
mix.density$Seed.Density <- mix.density$Total.Lbs.Acre*1.12085
mix.density$Total.Lbs.Acre <- NULL

# Merge with main dataset:
data <- as.data.table(merge(data, mix.density, by=c("Site"), all.x=TRUE))
nrow(data)
rownames(data) <- data$Site

# Clean up extra dataframes:
rm(seedmixinfo, mix.density)

### Info for manuscript ----
# How many sites?
nrow(data) #83
# How many per state?
nrow(data[data$Landscape=="MCCD",]) #18
nrow(data[data$Landscape=="KSands",]) #32
nrow(data[data$Landscape=="MI",]) #33
18+32+33 #83
# Soil characteristics?
min(data$Water.Holding.Capacity) #0.325
max(data$Water.Holding.Capacity) #0.799
mean(data$Water.Holding.Capacity) #0.503
# Planting year?
min(data$First_Growing_Season) #1998
max(data$First_Growing_Season) #2015
# How old?
min(data$Age_2016) #1
max(data$Age_2016) #19
# Last burn?
min(data$Last.Burn) #0
max(data$Last.Burn) #16
nrow(data[data$Last.Burn<5,])/83*100 #75.0%
nrow(data[data$Last.Burn==0,])/83*100 #13.25%
# Species sown?
min(data$Mix.Richness) #5
max(data$Mix.Richness) #221
mean(data$Mix.Richness) #55.87
median(data$Mix.Richness) #38
# Sowing density (kg/ha)?
min(data$Seed.Density, na.rm=TRUE) #1.62
max(data$Seed.Density, na.rm=TRUE) #54.09
mean(data$Seed.Density, na.rm=TRUE) #9.68
median(data$Seed.Density, na.rm=TRUE) #8.48
# Average cover?
data$Total.Cover <- data$Mean.Sown.Cover + data$Mean.Nonsown.Cover
min(data$Total.Cover) #55.763
max(data$Total.Cover) # 122.7833
mean(data$Total.Cover) #84.195
sd(data$Total.Cover) #14.68
median(data$Total.Cover) #81.619
data$Total.Cover <- NULL

### Rescale variables ----
data$Mix.Richness.scaled <- as.numeric(scale(data$Mix.Richness))
data$Age_2016.scaled <- as.numeric(scale(data$Age_2016))
data$Last.Burn.scaled <- as.numeric(scale(data$Last.Burn))
data$Seed.Density.scaled <- as.numeric(scale(data$Seed.Density))

nrow(data)
rownames(data) <- data$Site
rownames(data)

### Soils PCA ----

soils.pca <- prcomp(~ scale(pH) + scale(Organic.Matter.percent) + scale(S.ppm) + 
                      scale(P.mg.per.kg) + scale(Ca.mg.per.kg) + scale(Mg.mg.per.kg) + 
                      scale(K.mg.per.kg) + scale(Na.mg.per.kg) + scale(B.half.detection) + 
                      scale(Fe.mg.per.kg) + scale(Mn.mg.per.kg) + scale(Cu.half.detection) +
                      scale(Zn.mg.per.kg) + scale(Al.mg.kg) + scale(Water.Holding.Capacity) + 
                      scale(Clay.percent) + scale(Silt.percent) + scale(Sand.percent), data=data)

# Soils PCA visualization:
ggbiplot(soils.pca, scale=0.5, 
  xlim=c(-5, 5), labels.size=5, 
  varname.size=3, varname.adjust=1.5,
  ellipse = FALSE, circle = FALSE) +
  scale_color_discrete(name = '') +
  geom_point(aes(colour=data$Landscape), size = 3) +
  geom_text_repel(size=3, aes(label=rownames(data)))+
  theme_bw(base_size = 11, base_family = "")

# Soils PCA summary:
summary(soils.pca) 
  #PC1 accounts for 40.5% of the variation
  #PC2 only adds 15.4% more
  #use PC1 for soils

### Table S1: Soil variable rotations for PC axes.
round(soils.pca$rotation[,1], digits = 3)
  #PC1 has high organic matter, clay, silt, nutrients (most but not all)
  #PC1 has low sand, iron, phosphorus, zinc
round(soils.pca$rotation[,2], digits = 3)
round(soils.pca$rotation[,3], digits = 3)

# Pull out results:
soils.results <- as.data.frame(soils.pca$x[,1])
colnames(soils.results) <- c("Soils")
soils.results$Site <- rownames(soils.results)
head(soils.results)

# Merge with main dataset:
data <- as.data.table(merge(data, soils.results, by=c("Site"), all.x=TRUE))
head(data)
nrow(data)
rownames(data) <- data$Site

# Clear soils data from main dataset:
data[,c("pH", "Organic.Matter.percent", "S.ppm", "P.mg.per.kg", 
        "Ca.mg.per.kg", "Mg.mg.per.kg", "K.mg.per.kg", 
        "Na.mg.per.kg", "B.half.detection", "Fe.mg.per.kg", 
        "Mn.mg.per.kg", "Cu.half.detection", "Zn.mg.per.kg",
        "Al.mg.kg", "Clay.percent", "Silt.percent",
        "Sand.percent", "Water.Holding.Capacity"):=NULL] 

# Clear extra data frames:
rm(soils.results, soils.pca)

### Weather PCA ----

# Create a weather PCA:
weather.pca <- prcomp(~ scale(Jun1.dd.accum) + scale(Jun1.precip.accum) + 
                        scale(Sep1.dd.accum) + scale(Sep1.precip.accum) + 
                        scale(summer.dd.accum) + scale(summer.precip.accum) + 
                        scale(max.month.dd.accum) + scale(min.month.precip) + 
                        scale(avg.low.temp) + scale(max.days.no.precip) + 
                        scale(avg.mon.rain.days), data=data)

# Look at it:
ggbiplot(weather.pca, scale=0.5, 
  xlim=c(-5, 5), labels.size=4, 
  varname.size=4, varname.adjust=2,
  ellipse = FALSE, circle = FALSE) +
  scale_color_discrete(name = '') +
  geom_point(aes(colour=data$Landscape), size = 3) +
  geom_text_repel(size=3, aes(label=rownames(data)))+
 theme_bw(base_size = 11, base_family = "")

# Same, but dots labeled with year planted:
ggbiplot(weather.pca, scale=0.5, 
  xlim=c(-5, 5), labels.size=4, 
  varname.size=3, varname.adjust=1.5,
  ellipse = FALSE, circle = FALSE) +
  scale_color_discrete(name = '') +
  geom_point(aes(colour=data$Landscape), size = 3) +
  geom_text_repel(size=3, aes(label=data$First_Growing_Season))+
 theme_bw(base_size = 11, base_family = "")

# Look at results:
summary(weather.pca)
# PC1=37.6%; PC2=22.0%; PC3=14.7% of the variance explained

# Make a dataset of PC1, PC2, PC3 results:
weather.results <- as.data.frame(weather.pca$x[,1:3])
head(weather.results)
colnames(weather.results) <- c("Weather.PC1", "Weather.PC2", "Weather.PC3")
weather.results$Site <- rownames(weather.results)

# Table S2: Weather variable rotations for PC axes 1, 2, and 3. 
round(weather.pca$rotation[,1], digits = 3)
round(weather.pca$rotation[,2], digits = 3)
round(weather.pca$rotation[,3], digits = 3)

# Merge weather PCs with main dataset:
data <- as.data.table(merge(data, weather.results, by=c("Site"), all.x=TRUE))
head(data)
rownames(data) <- data$Site

# Clean up:
rm(weather.results, weather.pca)
  # don't clear weather variables yet

### Check colinearity of predictors ----

# Write a function to plot colinearity:
my_line <- function(x,y,...){
    points(x,y,...)
    abline(y~x,...)
}
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...){
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x, y))
    fontsize <- if(abs(cor(x, y))>0.5){abs(cor(x, y))}else{0.5}
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste0(prefix, txt)
    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex.cor * fontsize )
}

# Pull out model predictors (not scaled):
names(data)
names(data[,c(4:5,17,24,29:32)])
predictors <- data[complete.cases(data[,]),c(4:5,17,24,29:32)]
nrow(predictors) #should be 81, drops two missing seed.density cases

# Graph predictors, pairwise:
pairs(predictors, lower.panel = my_line, upper.panel = panel.cor)

# Check the r values in the boxes:
  # highest r=0.47 is Soils x Mix Richness
  0.47*0.47 #r2=0.22

# Clean up:
rm(predictors)

### LINEAR MODELS ####################
### Site.Richness.Total ----
# 1000 m2 plots, total species richness

siterich.mod <- lm(Site.Richness.Total ~ 
                  Weather.PC1*Age_2016.scaled + Weather.PC2*Age_2016.scaled + Weather.PC3*Age_2016.scaled + 
                  Weather.PC1*Soils + Weather.PC2*Soils + Weather.PC3*Soils +
                  Mix.Richness.scaled + Seed.Density.scaled + Last.Burn.scaled, data=data)
#plot(siterich.mod)

# Get estimate (beta) values for Table S3:
summary(siterich.mod)

# Get F values for Table S3:
Anova(siterich.mod, type="III") 
  # no effects on total site richness

# Get R2 values for Table S3:
siterich.r2 <- r2beta(siterich.mod, partial=TRUE, method="sgv", data=data)
siterich.r2[siterich.r2$Effect=="Weather.PC1","Rsq"]
siterich.r2[siterich.r2$Effect=="Weather.PC2","Rsq"]
siterich.r2[siterich.r2$Effect=="Weather.PC3","Rsq"]
siterich.r2[siterich.r2$Effect=="Mix.Richness.scaled","Rsq"]
siterich.r2[siterich.r2$Effect=="Seed.Density.scaled","Rsq"]
siterich.r2[siterich.r2$Effect=="Last.Burn.scaled","Rsq"]
siterich.r2[siterich.r2$Effect=="Soils","Rsq"]
siterich.r2[siterich.r2$Effect=="Age_2016.scaled","Rsq"]
siterich.r2[siterich.r2$Effect=="Weather.PC1:Age_2016.scaled","Rsq"]
siterich.r2[siterich.r2$Effect=="Age_2016.scaled:Weather.PC2","Rsq"]
siterich.r2[siterich.r2$Effect=="Age_2016.scaled:Weather.PC3","Rsq"]
siterich.r2[siterich.r2$Effect=="Weather.PC1:Soils","Rsq"]
siterich.r2[siterich.r2$Effect=="Weather.PC2:Soils","Rsq"]
siterich.r2[siterich.r2$Effect=="Weather.PC3:Soils","Rsq"]

### Sown.Richness.Total ----
# 1000 m2 plots, sown species richness

sownrich.mod <- lm(Sown.Richness.Total ~ 
                  Weather.PC1*Age_2016.scaled + Weather.PC2*Age_2016.scaled + Weather.PC3*Age_2016.scaled +
                  Weather.PC1*Soils + Weather.PC2*Soils + Weather.PC3*Soils +
                  Mix.Richness.scaled + Seed.Density.scaled + Last.Burn.scaled, data=data)
# plot(sownrich.mod)

# Get estimate (beta) values for Table S3:
summary(sownrich.mod)

# get F values for Table S3:
Anova(sownrich.mod,type="III") 
  # effect of mix richness (p=0.0041)
  # marginal effect of PC2 (p=0.0692)

# get R2 values for Table S3:
sownrich.r2 <- r2beta(sownrich.mod, partial=TRUE, method="sgv", data=data)
sownrich.r2[sownrich.r2$Effect=="Weather.PC1","Rsq"]
sownrich.r2[sownrich.r2$Effect=="Weather.PC2","Rsq"]
sownrich.r2[sownrich.r2$Effect=="Weather.PC3","Rsq"]
sownrich.r2[sownrich.r2$Effect=="Mix.Richness.scaled","Rsq"]
sownrich.r2[sownrich.r2$Effect=="Seed.Density.scaled","Rsq"]
sownrich.r2[sownrich.r2$Effect=="Last.Burn.scaled","Rsq"]
sownrich.r2[sownrich.r2$Effect=="Soils","Rsq"]
sownrich.r2[sownrich.r2$Effect=="Age_2016.scaled","Rsq"]
sownrich.r2[sownrich.r2$Effect=="Weather.PC1:Age_2016.scaled","Rsq"]
sownrich.r2[sownrich.r2$Effect=="Age_2016.scaled:Weather.PC2","Rsq"]
sownrich.r2[sownrich.r2$Effect=="Age_2016.scaled:Weather.PC3","Rsq"]
sownrich.r2[sownrich.r2$Effect=="Weather.PC1:Soils","Rsq"]
sownrich.r2[sownrich.r2$Effect=="Weather.PC2:Soils","Rsq"]
sownrich.r2[sownrich.r2$Effect=="Weather.PC3:Soils","Rsq"]

### Nonsown.Richness.Total ----
# 1000 m2 plots, non-sown species richness

nonsownrich.mod <- lm(Nonsown.Richness.Total ~ 
                  Weather.PC1*Age_2016.scaled + Weather.PC2*Age_2016.scaled + Weather.PC3*Age_2016.scaled + 
                  Weather.PC1*Soils + Weather.PC2*Soils + Weather.PC3*Soils +
                  Mix.Richness.scaled + Seed.Density.scaled + Last.Burn.scaled, data=data)
# plot(nonsownrich.mod)

# Get estimate (beta) values for Table S3:
summary(nonsownrich.mod)

# get F values for Table S3:
Anova(nonsownrich.mod, type="III") 
  # effect of PC2 (p=0.006986)
  # marginal effect of seed density (p=0.064973)
  # marginal effect of age*PC2 (p=0.092803)

# get R2 values for Table S3:
nonsownrich.r2 <- r2beta(nonsownrich.mod, partial=TRUE, method="sgv", data=data)
nonsownrich.r2[nonsownrich.r2$Effect=="Weather.PC1","Rsq"]
nonsownrich.r2[nonsownrich.r2$Effect=="Weather.PC2","Rsq"]
nonsownrich.r2[nonsownrich.r2$Effect=="Weather.PC3","Rsq"]
nonsownrich.r2[nonsownrich.r2$Effect=="Mix.Richness.scaled","Rsq"]
nonsownrich.r2[nonsownrich.r2$Effect=="Seed.Density.scaled","Rsq"]
nonsownrich.r2[nonsownrich.r2$Effect=="Last.Burn.scaled","Rsq"]
nonsownrich.r2[nonsownrich.r2$Effect=="Soils","Rsq"]
nonsownrich.r2[nonsownrich.r2$Effect=="Age_2016.scaled","Rsq"]
nonsownrich.r2[nonsownrich.r2$Effect=="Weather.PC1:Age_2016.scaled","Rsq"]
nonsownrich.r2[nonsownrich.r2$Effect=="Age_2016.scaled:Weather.PC2","Rsq"]
nonsownrich.r2[nonsownrich.r2$Effect=="Age_2016.scaled:Weather.PC3","Rsq"]
nonsownrich.r2[nonsownrich.r2$Effect=="Weather.PC1:Soils","Rsq"]
nonsownrich.r2[nonsownrich.r2$Effect=="Weather.PC2:Soils","Rsq"]
nonsownrich.r2[nonsownrich.r2$Effect=="Weather.PC3:Soils","Rsq"]

### Mean.Sown.Cover ----
# 1m2 plots, mean sown species percent cover

sowncover.mod <- lm(Mean.Sown.Cover ~
                  Weather.PC1*Age_2016.scaled + Weather.PC2*Age_2016.scaled + Weather.PC3*Age_2016.scaled + 
                  Weather.PC1*Soils + Weather.PC2*Soils + Weather.PC3*Soils +
                  Mix.Richness.scaled + Seed.Density.scaled + Last.Burn.scaled, data=data)
#plot(sowncover.mod)

# Get estimate (beta) values for Table S3:
summary(sowncover.mod)

# get F values for Table S3:
Anova(sowncover.mod, type="III")
  # effect of soils (p=0.0240)
  # marginal effect of PC1*age (p=0.0983)

# get R2 values for Table S3:
sowncover.r2 <- r2beta(sowncover.mod, partial=TRUE, method="sgv", data=data)
sowncover.r2[sowncover.r2$Effect=="Weather.PC1","Rsq"]
sowncover.r2[sowncover.r2$Effect=="Weather.PC2","Rsq"]
sowncover.r2[sowncover.r2$Effect=="Weather.PC3","Rsq"]
sowncover.r2[sowncover.r2$Effect=="Mix.Richness.scaled","Rsq"]
sowncover.r2[sowncover.r2$Effect=="Seed.Density.scaled","Rsq"]
sowncover.r2[sowncover.r2$Effect=="Last.Burn.scaled","Rsq"]
sowncover.r2[sowncover.r2$Effect=="Soils","Rsq"]
sowncover.r2[sowncover.r2$Effect=="Age_2016.scaled","Rsq"]
sowncover.r2[sowncover.r2$Effect=="Weather.PC1:Age_2016.scaled","Rsq"]
sowncover.r2[sowncover.r2$Effect=="Age_2016.scaled:Weather.PC2","Rsq"]
sowncover.r2[sowncover.r2$Effect=="Age_2016.scaled:Weather.PC3","Rsq"]
sowncover.r2[sowncover.r2$Effect=="Weather.PC1:Soils","Rsq"]
sowncover.r2[sowncover.r2$Effect=="Weather.PC2:Soils","Rsq"]
sowncover.r2[sowncover.r2$Effect=="Weather.PC3:Soils","Rsq"]

### Mean.Nonsown.Cover ----
# 1m2 plots, mean non-sown species percent cover

# log transform non-sown cover
data$log.Mean.Nonsown.Cover <- log(data$Mean.Nonsown.Cover)

nonsowncover.mod <- lm(log.Mean.Nonsown.Cover ~  
                  Weather.PC1*Age_2016.scaled + Weather.PC2*Age_2016.scaled + Weather.PC3*Age_2016.scaled + 
                  Weather.PC1*Soils + Weather.PC2*Soils + Weather.PC3*Soils +
                  Mix.Richness.scaled + Seed.Density.scaled + Last.Burn.scaled, data=data)
#plot(nonsowncover.mod)

# Get estimate (beta) values for Table S3:
summary(nonsowncover.mod)

# get F values for Table S3:
Anova(nonsowncover.mod, type="III")
  # effect of Age (p=0.0156236)
  # effect of PC2 (p=0.0156236)
  # effect of soils (p-0.0003891)
  # effect of mix richness (p=0.0319749)
  # effect of PC1*age (p=0.0026734)

# get R2 values for Table S3:
nonsowncover.r2 <- r2beta(nonsowncover.mod, partial=TRUE, method="sgv", data=data)
nonsowncover.r2[nonsowncover.r2$Effect=="Weather.PC1","Rsq"]
nonsowncover.r2[nonsowncover.r2$Effect=="Weather.PC2","Rsq"]
nonsowncover.r2[nonsowncover.r2$Effect=="Weather.PC3","Rsq"]
nonsowncover.r2[nonsowncover.r2$Effect=="Mix.Richness.scaled","Rsq"]
nonsowncover.r2[nonsowncover.r2$Effect=="Seed.Density.scaled","Rsq"]
nonsowncover.r2[nonsowncover.r2$Effect=="Last.Burn.scaled","Rsq"]
nonsowncover.r2[nonsowncover.r2$Effect=="Soils","Rsq"]
nonsowncover.r2[nonsowncover.r2$Effect=="Age_2016.scaled","Rsq"]
nonsowncover.r2[nonsowncover.r2$Effect=="Weather.PC1:Age_2016.scaled","Rsq"]
nonsowncover.r2[nonsowncover.r2$Effect=="Age_2016.scaled:Weather.PC2","Rsq"]
nonsowncover.r2[nonsowncover.r2$Effect=="Age_2016.scaled:Weather.PC3","Rsq"]
nonsowncover.r2[nonsowncover.r2$Effect=="Weather.PC1:Soils","Rsq"]
nonsowncover.r2[nonsowncover.r2$Effect=="Weather.PC2:Soils","Rsq"]
nonsowncover.r2[nonsowncover.r2$Effect=="Weather.PC3:Soils","Rsq"]

### Mix.Veg.BCdissim ----
# Bray-Curtis dissimilarity between seed mix composition
# and plant community composition in 2016

bc.mod <- lm(Mix.Veg.BCdissim ~ 
                  Weather.PC1*Age_2016.scaled + Weather.PC2*Age_2016.scaled + Weather.PC3*Age_2016.scaled + 
                  Weather.PC1*Soils + Weather.PC2*Soils + Weather.PC3*Soils +
                  Mix.Richness.scaled + Seed.Density.scaled + Last.Burn.scaled, data=data)
#plot(bc.mod)

# Get estimate (beta) values for Table S3:
summary(bc.mod)

# get F values for Table S3:
Anova(bc.mod, type="III")
  # effect of age (p=0.001834)
  # effect of PC2 (p=0.010365)
  # effect of mix richness (p=0.000103)

# get R2 values for Table S3:
bc.r2 <- r2beta(bc.mod, partial=TRUE, method="sgv", data=data)
bc.r2[bc.r2$Effect=="Weather.PC1","Rsq"]
bc.r2[bc.r2$Effect=="Weather.PC2","Rsq"]
bc.r2[bc.r2$Effect=="Weather.PC3","Rsq"]
bc.r2[bc.r2$Effect=="Mix.Richness.scaled","Rsq"]
bc.r2[bc.r2$Effect=="Seed.Density.scaled","Rsq"]
bc.r2[bc.r2$Effect=="Last.Burn.scaled","Rsq"]
bc.r2[bc.r2$Effect=="Soils","Rsq"]
bc.r2[bc.r2$Effect=="Age_2016.scaled","Rsq"]
bc.r2[bc.r2$Effect=="Weather.PC1:Age_2016.scaled","Rsq"]
bc.r2[bc.r2$Effect=="Age_2016.scaled:Weather.PC2","Rsq"]
bc.r2[bc.r2$Effect=="Age_2016.scaled:Weather.PC3","Rsq"]
bc.r2[bc.r2$Effect=="Weather.PC1:Soils","Rsq"]
bc.r2[bc.r2$Effect=="Weather.PC2:Soils","Rsq"]
bc.r2[bc.r2$Effect=="Weather.PC3:Soils","Rsq"]

### FIGURES ##########################
### FIGURE 1A: Effect of PC2 on Nonsown Richness ----
effect_plot(nonsownrich.mod, pred = Weather.PC2, 
            interval = TRUE, plot.points = TRUE,
            x.label="Planting Year Weather (PC2: Rainfall)",
            y.label=expression(paste("Non-sown species per 1000 ",m^2)))+
    theme_bw(base_size = 11, base_family = "") +
    theme(plot.title=element_text(size=15),
          axis.title=element_text(size=14),
          axis.text = element_text(size=12)) +
    ggtitle("(A)                                                                     p=0.007")
Anova(nonsownrich.mod, type="III") 

### FIGURE 1B: Effect of PC2 on Nonsown Cover ----
effect_plot(nonsowncover.mod, pred = Weather.PC2, 
            interval = TRUE, plot.points = TRUE,
            x.label="Planting Year Weather (PC2: Rainfall)",
            y.label=expression(paste("Mean cover of non-sown species per 1 ", m^2)))+
    theme_bw(base_size = 11, base_family = "") +
    theme(plot.title=element_text(size=15),
        axis.title=element_text(size=14),
        axis.text = element_text(size=12)) +
    ggtitle("(B)                                                                      p=0.047")
Anova(nonsowncover.mod, type="III") 

### FIGURE 1C: Effect of PC2 on Sown Richness ----
effect_plot(sownrich.mod, pred = Weather.PC2, 
            interval = TRUE, plot.points = TRUE,
            x.label="Planting Year Weather (PC2: Rainfall)",
            y.label=expression(paste("Sown species per 1000 ", m^2)))+
    theme_bw(base_size = 11, base_family = "") +
    theme(plot.title=element_text(size=15),
        axis.title=element_text(size=14),
        axis.text = element_text(size=12)) +
    ggtitle("(C)                                                                      p=0.070")
Anova(sownrich.mod, type="III") 

### FIGURE 1D: Effect of PC2 on Bray-Curtis Mix-Comm Dissimiliarity ----
effect_plot(bc.mod, pred = Weather.PC2, 
            interval = TRUE, plot.points = TRUE,
            x.label="Planting Year Weather (PC2: Rainfall)",
            y.label="Bray-Curtis Dissimilarity between seed \n mix and 2016 vegetation composition")+
    theme_bw(base_size = 11, base_family = "") +
    theme(plot.title=element_text(size=15),
        axis.title=element_text(size=14),
        axis.text = element_text(size=12)) +
    ggtitle("(D)                                                                   p=0.010")
Anova(bc.mod, type="III")

### FIGURE 1X(Unused): Effect of Seed Mix Richness on Sown Richness ----
effect_plot(sownrich.mod, pred = Mix.Richness.scaled, 
            interval = TRUE, plot.points = TRUE,
            x.label="Seed Mix Richness (scaled)",
            y.label="Richness of Sown Species")+
    theme_bw(base_size = 11, base_family = "") +
    theme(plot.title=element_text(size=15,face="bold"),
        axis.title=element_text(size=14),
        axis.text = element_text(size=12)) +
    ggtitle(expression(paste("Richness of sown species, 1000", m^2, " sites")))
    # unscaled x-axis for overlay:
    ggplot(data, aes(x=Mix.Richness, y=Sown.Richness.Total)) + 
      geom_point(size=2) +
      theme_bw(base_size = 11, base_family = "") +
      theme(plot.title=element_text(size=15,face="bold"),
            axis.title=element_text(size=14),
            axis.text = element_text(size=12)) +  
      labs(x="Seed Mix Richness")
    
### FIGURE 1X(Unused): Effect of Soil Productivity on Sown Cover ----
ggplot(data, aes(x=Soils, y=Mean.Sown.Cover)) + 
  geom_point(size=2) +
  geom_smooth(method = "lm", data = data, color="black")  +
  theme_bw(base_size = 11, base_family = "") +
  theme(plot.title=element_text(size=15,face="bold"),
        axis.title=element_text(size=14),
        axis.text = element_text(size=12)) +  
  ggtitle(expression(paste("Cover of sown species, 1", m^2, " plots"))) +
  labs(x="Soil Productivity (PCA)",
       y="Cover of Sown Species")
    
### FIGURE 1X(Unused): Effect of Age on Non-Sown Cover ----
effect_plot(nonsowncover.mod, pred = Age_2016.scaled, 
            interval = TRUE, plot.points = TRUE,
            x.label="Site Age (2016)",
            y.label="(Log) Cover of Non-sown Species")+
    theme_bw(base_size = 11, base_family = "") +
    theme(plot.title=element_text(size=15,face="bold"),
        axis.title=element_text(size=14),
        axis.text = element_text(size=12)) +
    ggtitle(expression(paste("Cover of non-sown species, 1", m^2, " plots")))

### FIGURE 1X(Unused): Effect of Soil Productivity on Non-Sown Cover ----
effect_plot(nonsowncover.mod, pred = Soils, 
            interval = TRUE, plot.points = TRUE,
            x.label="Soil Productivity (PCA)",
            y.label="(Log) Cover of Non-sown Species")+
    theme_bw(base_size = 11, base_family = "") +
    theme(plot.title=element_text(size=15,face="bold"),
        axis.title=element_text(size=14),
        axis.text = element_text(size=12)) +
    ggtitle(expression(paste("Cover of non-sown species, 1", m^2, " plots")))

### FIGURE 1X(Unused): Effect of Seed Mix Richness on Non-Sown Cover ----
effect_plot(nonsowncover.mod, pred = Mix.Richness.scaled, 
            interval = TRUE, plot.points = TRUE,
            x.label="Seed mix richness",
            y.label="(Log) Cover of Non-sown Species")+
    theme_bw(base_size = 11, base_family = "") +
    theme(plot.title=element_text(size=15,face="bold"),
        axis.title=element_text(size=14),
        axis.text = element_text(size=12)) +
    ggtitle(expression(paste("Cover of non-sown species, 1", m^2, " plots")))
    
### FIGURE 1X(Unused): Effect of Site Age on BC Dissim ----
effect_plot(bc.mod, pred = Age_2016.scaled, 
            interval = TRUE, plot.points = TRUE,
            x.label="Site Age",
            y.label="Bray-Curtis Dissimilarity")+
    theme_bw(base_size = 11, base_family = "") +
    theme(plot.title=element_text(size=15,face="bold"),
        axis.title=element_text(size=14),
        axis.text = element_text(size=12)) +
    ggtitle(expression(paste("Dissimilarity between seed mix and 2016 vegetation composition")))
    # unscaled x axis:
    ggplot(data, aes(x=Age_2016, y=Mix.Veg.BCdissim)) + 
      geom_point(size=2) +
      theme_bw(base_size = 11, base_family = "") +
      theme(plot.title=element_text(size=15,face="bold"),
            axis.title=element_text(size=14),
            axis.text = element_text(size=12)) +  
      labs(x="Site Age")

### FIGURE 1X(Unused): Effect of Seed Mix Richness on BC Dissim ----
effect_plot(bc.mod, pred = Mix.Richness.scaled, 
            interval = TRUE, plot.points = TRUE,
            x.label="Seed Mix Richness",
            y.label="Bray-Curtis Dissimilarity")+
    theme_bw(base_size = 11, base_family = "") +
    theme(plot.title=element_text(size=15,face="bold"),
        axis.title=element_text(size=14),
        axis.text = element_text(size=12)) +
    ggtitle(expression(paste("Dissimilarity between seed mix and 2016 vegetation composition")))

### FIGURE 2: Interaction Effect of Age*PC1 on Non-Sown Cover ----
x <- data$Weather.PC1
data$temp_3group <- case_when(x > mean(x)+0.75*sd(x) ~ "Warmest",
                      x < mean(x)+0.75*sd(x) & x > mean(x)-0.75*sd(x) ~ "Average",
                      x < mean(x)-0.75*sd(x) ~ "Coolest")
data$temp_3group <- factor(data$temp_3group, levels = c("Coolest", "Average", "Warmest"))
levels(as.factor(data$temp_3group))
count(data, temp_3group)
  #don't forget to use the unscaled version of age for the figure
  #else the axis labels won't make sense

data %>% ggplot() +
  aes(x = Age_2016, y = Mean.Nonsown.Cover, color=temp_3group, group=temp_3group) +
  geom_point(size=2, alpha = .7) + #color = "black",
  geom_smooth(method = "lm") +
  labs(color = "Planting Year \nWeather (PC1)") +
  scale_color_manual(values=c("slateblue3","turquoise4","darkorange1"))+
  #scale_color_gradient(low="blue",high="orange")+
  theme_bw(base_size = 11, base_family = "") +
  theme(plot.title=element_text(size=15),
        axis.title=element_text(size=14),
        axis.text = element_text(size=12)) +
  theme(plot.margin=unit(c(1,0,0.5,1.2),"cm"))+
  labs(x="Site Age",
       y=expression(paste("Cover of non-sown\n species, mean per 1 ", m^2)))

### FIGURE 3: Regression Coefficients for All Models ----

mod.list<-list(sownrich.mod,nonsownrich.mod,
               siterich.mod,sowncover.mod,
               nonsowncover.mod,bc.mod)

effect.table<-matrix()
effect.table2<-NULL
for(i in mod.list){
  response<-rep(all.vars(formula(i))[1],length(coef(i))-1)
  variable<-names(coef(i)[2:length(coef(i))])
  effect<-as.vector(coef(i)[2:length(coef(i))])
  lower<-as.vector(confint(i)[2:(length(coef(i))),1])
  upper<-as.vector(confint(i)[2:(length(coef(i))),2])

  effect.table<-data.frame(response,variable,effect,upper,lower)
  effect.table2<-rbind(effect.table2,effect.table)
}

effect.table2 <- droplevels(effect.table2[effect.table2$variable%in%c(
                 "Soils", "Last.Burn.scaled", "Age_2016.scaled", "Seed.Density.scaled",
                  "Mix.Richness.scaled","Weather.PC3","Weather.PC2","Weather.PC1"),])

effect.table2$variable <- factor(effect.table2$variable,
                                 levels = c("Soils", 
                                              "Last.Burn.scaled", "Age_2016.scaled", "Seed.Density.scaled",
                                              "Mix.Richness.scaled","Weather.PC3","Weather.PC2","Weather.PC1"))
labels2<- c(Sown.Richness.Total="Sown richness",
            Nonsown.Richness.Total="Non-sown richness",
            Site.Richness.Total="Total richness",
            Mean.Sown.Cover="Sown cover",
            log.Mean.Nonsown.Cover="Non-sown cover",
            Mix.Veg.BCdissim="Bray-Curtis dissim.")
var.label<-c("Weather.PC1"="Weather (PC1:Temp)",
             "Weather.PC2"="Weather (PC2:Rain)",
             "Weather.PC3"="Weather (PC3)",
             "Seed.Density.scaled"="Seeding density",
             "Mix.Richness.scaled"="Mix richness", 
             "Age_2016.scaled"="Site age", 
             "Last.Burn.scaled"="Years since burn",
             "Soils"="Soil productivity")

#pdf("effects.pdf",width=8.5,height=11) #export figure
ggplot(effect.table2, aes(variable,effect))+
  geom_point(size=1.5)+
  facet_wrap(~response,ncol=2,labeller=labeller(response = labels2),scales="free")+
  geom_hline(yintercept=0, linetype="dotted")+
  coord_flip()+
  geom_errorbar(width=0, aes(ymin=lower, ymax=upper)) +
  xlab(label="")+
  ylab(label="Standardized regression coefficients")+
  scale_x_discrete(labels = var.label)+
  theme(text = element_text(size=14),axis.text=element_text(colour="black"),strip.background =element_rect(fill="white"),
        panel.background=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        axis.line = element_line(size=1, color="black"),legend.position="none")
#dev.off() #export figure

### FIGURE S3: Pairwise Weather Scatterplots ----
# Scatter plots and pairwise correlations (r) between weather variables.

# Pull out weather predictors:
names(data)
names(data[,c(6:16)])
weather.only <- data[complete.cases(data[,]),c(6:16)]

# Plot:
pairs(weather.only, lower.panel = my_line, upper.panel = panel.cor)

# Clean up:
rm(weather.only)

### FIGURE S4: Histograms of Variables ----
# Distributions of weather and site condition variables across sites.
# Exported as 600 x 400 pixels; resized to 2.1in tall in word doc

#(A) Spring temperatures
ggplot(data, aes(x=Jun1.dd.accum, fill=Landscape)) +
  geom_histogram(color="black", binwidth=25, position="dodge", alpha=0.5) +
  ggtitle("(A)")+
  scale_x_continuous("Degree days accumulated Mar 1 - Jun 1 (spring temperatures)", 
                     expand=c(0,0), limits=c(166,459), breaks=c(200,250,300,350,400,450))+
  scale_y_continuous("Number of Sites", expand=c(0,0), limits=c(0,11), breaks=c(0,2,4,6,8,10))+
  theme(axis.text=element_text(size=12), legend.title=element_blank())+
  scale_fill_brewer(type="qual", palette=2, labels=c("Indiana", "Illinois", "Michigan"))

#(B) Spring rains
ggplot(data, aes(x=Jun1.precip.accum, fill=Landscape)) +
  geom_histogram(color="black", binwidth=35, position="dodge", alpha=0.5) +
  ggtitle("(B)")+
  scale_x_continuous("Precipitation accumulated Mar 1 - Jun 1 (mm) (spring rain)", 
                     expand=c(0,0), limits=c(100,460), breaks=c(100,200,300,400))+
  scale_y_continuous("Number of Sites", expand=c(0,0), limits=c(0,10), breaks=c(0,2,4,6,8,10,12))+
  theme(axis.text=element_text(size=12), legend.title=element_blank())+
  scale_fill_brewer(type="qual", palette=2, labels=c("Indiana", "Illinois", "Michigan"))

#(C) Overall temperature
ggplot(data, aes(x=Sep1.dd.accum, fill=Landscape)) +
  geom_histogram(color="black", binwidth=60, position="dodge", alpha=0.5) +
  ggtitle("(C)")+
  scale_x_continuous("Degree days accumulated Mar 1 - Sep 1 (growing season temperature)", 
                     expand=c(0,0), limits=c(990,1620), breaks=c(1000,1100,1200,1300,1400,1500,1600))+
  scale_y_continuous("Number of Sites", expand=c(0,0), limits=c(0,12), breaks=c(0,2,4,6,8,10,12))+
  theme(axis.text=element_text(size=12), legend.title=element_blank())+
  scale_fill_brewer(type="qual", palette=2, labels=c("Indiana", "Illinois", "Michigan"))

# (D) Overall rain
ggplot(data, aes(x=Sep1.precip.accum, fill=Landscape)) +
  geom_histogram(color="black", binwidth=60, position="dodge", alpha=0.5) +
  ggtitle("(D)")+
  scale_x_continuous("Precipitation accumulated Mar 1 - Sep 1 (mm) (full growing season)", 
                     expand=c(0,0), limits=c(300,860), breaks=c(300,400,500,600,700,800))+
  scale_y_continuous("Number of Sites", expand=c(0,0), limits=c(0,10), breaks=c(0,2,4,6,8,10,12))+
  theme(axis.text=element_text(size=12), legend.title=element_blank())+
  scale_fill_brewer(type="qual", palette=2, labels=c("Indiana", "Illinois", "Michigan"))

#(E) Summer temperature
ggplot(data, aes(x=summer.dd.accum, fill=Landscape)) +
  geom_histogram(color="black", binwidth=50, position="dodge", alpha=0.5) +
  ggtitle("(E)")+
  scale_x_continuous("Degree days accumulated Jun 1 - Sep 1 (summer temperature)", 
                     expand=c(0,0), limits=c(800,1260), breaks=c(800,900,1000,1100,1200))+
  scale_y_continuous("Number of Sites", expand=c(0,0), limits=c(0,10), breaks=c(0,2,4,6,8,10,12))+
  theme(axis.text=element_text(size=12), legend.title=element_blank())+
  scale_fill_brewer(type="qual", palette=2, labels=c("Indiana", "Illinois", "Michigan"))

#(F) Summer rain
ggplot(data, aes(x=summer.precip.accum, fill=Landscape)) +
  geom_histogram(color="black", binwidth=50, position="dodge", alpha=0.5) +
  ggtitle("(F)")+
  scale_x_continuous("Precipitation accumulated Jun 1 - Sep 1 (mm) (summer rain)", 
                     expand=c(0,0), limits=c(100,650), breaks=c(100,200,300,400,500,600))+
  scale_y_continuous("Number of Sites", expand=c(0,0), limits=c(0,11), breaks=c(0,2,4,6,8,10,12))+
  theme(axis.text=element_text(size=12), legend.title=element_blank())+
  scale_fill_brewer(type="qual", palette=2, labels=c("Indiana", "Illinois", "Michigan"))

#(G) Hottest month
ggplot(data, aes(x=max.month.dd.accum, fill=Landscape)) +
  geom_histogram(color="black", binwidth=0.6, position="dodge", alpha=0.5) +
  ggtitle("(G)")+
  scale_x_continuous("Maximum degree days accumulated in 30 days (hottest month)", 
                    expand=c(0,0), limits=c(10.5,17), breaks=c(11,12,13,14,15,16))+
  scale_y_continuous("Number of Sites", expand=c(0,0), limits=c(0,9), breaks=c(0,2,4,6,8,10,12))+
  theme(axis.text=element_text(size=12), legend.title=element_blank())+
  scale_fill_brewer(type="qual", palette=2, labels=c("Indiana", "Illinois", "Michigan"))

#(H) Driest month
ggplot(data, aes(x=min.month.precip, fill=Landscape)) +
  geom_histogram(color="black", binwidth=4, position="dodge", alpha=0.5) +
  ggtitle("(H)")+
  scale_x_continuous("Least amount of precipitation in a 30-day period (mm) (driest month)", 
                    expand=c(0,0), limits=c(0,40), breaks=c(0,10,20,30,40))+
  scale_y_continuous("Number of Sites", expand=c(0,0), limits=c(0,9), breaks=c(0,2,4,6,8,10,12))+
  theme(axis.text=element_text(size=12), legend.title=element_blank())+
  scale_fill_brewer(type="qual", palette=2, labels=c("Indiana", "Illinois", "Michigan"))

#(I) Average low temperature
ggplot(data, aes(x=avg.low.temp, fill=Landscape)) +
  geom_histogram(color="black", binwidth=0.4, position="dodge", alpha=0.5) +
  ggtitle("(I)")+
  scale_x_continuous("Average low temperature (degrees Celsius)", 
                    expand=c(0,0), limits=c(8,12), breaks=c(8,9,10,11,12))+
  scale_y_continuous("Number of Sites", expand=c(0,0), limits=c(0,10), breaks=c(0,2,4,6,8,10,12))+
  theme(axis.text=element_text(size=12), legend.title=element_blank())+
  scale_fill_brewer(type="qual", palette=2, labels=c("Indiana", "Illinois", "Michigan"))

#(J) Longest drought
ggplot(data, aes(x=max.days.no.precip, fill=Landscape)) +
  geom_histogram(color="black", binwidth=2, position="dodge", alpha=0.5) +
  ggtitle("(J)")+
  scale_x_continuous("Maximum number of days without a precipitation event (longest drought)", 
                    expand=c(0,0), limits=c(7,25), breaks=c(8,10,12,14,16,18,20,22,24,26))+
  scale_y_continuous("Number of Sites", expand=c(0,0), limits=c(0,13), breaks=c(0,2,4,6,8,10,12))+
  theme(axis.text=element_text(size=12), legend.title=element_blank())+
  scale_fill_brewer(type="qual", palette=2, labels=c("Indiana", "Illinois", "Michigan"))

#(K) Average number of rainy days
ggplot(data, aes(x=avg.mon.rain.days, fill=Landscape)) +
  geom_histogram(color="black", binwidth=0.6, position="dodge", alpha=0.5) +
  ggtitle("(K)")+
  scale_x_continuous("Average number of rainy days per 30-day period", 
                    expand=c(0,0), limits=c(9,15), breaks=c(9,10,11,12,13,14,15))+
  scale_y_continuous("Number of Sites", expand=c(0,0), limits=c(0,9), breaks=c(0,2,4,6,8,10,12))+
  theme(axis.text=element_text(size=12), legend.title=element_blank())+
  scale_fill_brewer(type="qual", palette=2, labels=c("Indiana", "Illinois", "Michigan"))

#(L) First growing season
ggplot(data, aes(x=First_Growing_Season, fill=Landscape)) +
  geom_histogram(color="black", binwidth=2, position="dodge", alpha=0.5) +
  ggtitle("(L)")+
  scale_x_continuous("First growing season", 
                    expand=c(0,0), limits=c(1996,2016), breaks=c(1998,2000,2002,2004,2006,2008,2010,2012,2014,2016))+
  scale_y_continuous("Number of Sites", expand=c(0,0), limits=c(0,9), breaks=c(0,2,4,6,8,10,12))+
  theme(axis.text=element_text(size=12), legend.title=element_blank())+
  scale_fill_brewer(type="qual", palette=2, labels=c("Indiana", "Illinois", "Michigan"))

#(M) Soil productivity
ggplot(data, aes(x=Soils, fill=Landscape)) +
  geom_histogram(color="black", binwidth=1, position="dodge", alpha=0.5) +
  ggtitle("(M)")+
  scale_x_continuous("Soil productivity (PCA axis 1 of multiple soil variables)", 
                    expand=c(0,0), limits=c(-4,7.5), breaks=c(-4,-2,0,2,4,6))+
  scale_y_continuous("Number of Sites", expand=c(0,0), limits=c(0,13), breaks=c(0,2,4,6,8,10))+
  theme(axis.text=element_text(size=12), legend.title=element_blank())+
  scale_fill_brewer(type="qual", palette=2, labels=c("Indiana", "Illinois", "Michigan"))

#(N) Years since last prescribed fire
ggplot(data, aes(x=Last.Burn, fill=Landscape)) +
  geom_histogram(color="black", binwidth=2, position="dodge", alpha=0.5) +
  ggtitle("(N)")+
  scale_x_continuous("Years since last prescribed fire", 
                    expand=c(0,0), limits=c(-1.5,16), breaks=c(0,3,6,9,12,15))+
  scale_y_continuous("Number of Sites", expand=c(0,0), limits=c(0,12), breaks=c(0,2,4,6,8,10))+
  theme(axis.text=element_text(size=12), legend.title=element_blank())+
  scale_fill_brewer(type="qual", palette=2, labels=c("Indiana", "Illinois", "Michigan"))

#(O) Seed mix richness
ggplot(data, aes(x=Mix.Richness, fill=Landscape)) +
  geom_histogram(color="black", binwidth=25, position="dodge", alpha=0.5) +
  ggtitle("(O)")+
  scale_x_continuous("Seed mix richness (number of species sown)", 
                    expand=c(0,0), limits=c(-18,230), breaks=c(0,20,40,60,80,100,120,140,160,180,200,220,240))+
  scale_y_continuous("Number of Sites", expand=c(0,0), limits=c(0,19), breaks=c(0,2,4,6,8,10,12,14,16,18,20))+
  theme(axis.text=element_text(size=12), legend.title=element_blank())+
  scale_fill_brewer(type="qual", palette=2, labels=c("Indiana", "Illinois", "Michigan"))

#(P) Seeding rate
ggplot(data, aes(x=Seed.Density, fill=Landscape)) +
  geom_histogram(color="black", binwidth=6, position="dodge", alpha=0.5) +
  ggtitle("(P)")+
  scale_x_continuous("Seeding rate (kg/ha)", 
                    expand=c(0,0), limits=c(0,55), breaks=c(0,5,10,15,20,25,30,35,40,45,50,55,60))+
  scale_y_continuous("Number of Sites", expand=c(0,0), limits=c(0,27), breaks=c(0,5,10,15,20,25,30))+
  theme(axis.text=element_text(size=12), legend.title=element_blank())+
  scale_fill_brewer(type="qual", palette=2, labels=c("Indiana", "Illinois", "Michigan"))

### FIGURE S5: Weather x Predictor Scatterplots ----

# (A) Site Age * Weather PC1 (Temperature)
ggplot(data, aes(x=Age_2016, y=Weather.PC1)) + 
  geom_point(size=2) +
  geom_smooth(method = "lm", data = data, color="black")  +
  theme_bw(base_size = 11, base_family = "") +
  theme(plot.title=element_text(size=15,face="bold"),
        axis.title=element_text(size=14),
        axis.text = element_text(size=12)) +  
  ggtitle(expression(paste("(A)"))) +
  labs(y="Weather PC1 (Temperature)",
       x="Site Age (Years)")

# (B) Seed Mix Richness * Weather PC1 (Temperature)
ggplot(data, aes(y=Weather.PC1, x=Mix.Richness)) + 
  geom_point(size=2) +
  geom_smooth(method = "lm", data = data, color="black")  +
  theme_bw(base_size = 11, base_family = "") +
  theme(plot.title=element_text(size=15,face="bold"),
        axis.title=element_text(size=14),
        axis.text = element_text(size=12)) +  
  ggtitle(expression(paste("(B)"))) +
  labs(y="Weather PC1 (Temperature)",
       x="Seed Mix Richness")

# (C) State * Weather PC1 (Temperature)
ggplot(data, aes(y=Weather.PC1, x=Landscape)) + 
  geom_point(size=2) +
  geom_boxplot()  +
  geom_jitter(position=position_jitter(0.1))+
  theme_bw(base_size = 11, base_family = "") +
  theme(plot.title=element_text(size=15,face="bold"),
        axis.title=element_text(size=14),
        axis.text = element_text(size=12)) +  
  ggtitle(expression(paste("(C)"))) +
  labs(y="Weather PC1 (Temperature)",
       x="State")+
  scale_x_discrete(labels = c('Indiana','Illinois','Michigan'))

# (D) Soil productivity * Weather PC1 (Temperature)
ggplot(data, aes(y=Weather.PC1, x=Soils)) + 
  geom_point(size=2) +
  geom_smooth(method = "lm", data = data, color="black")  +
  theme_bw(base_size = 11, base_family = "") +
  theme(plot.title=element_text(size=15,face="bold"),
        axis.title=element_text(size=14),
        axis.text = element_text(size=12)) +  
  ggtitle(expression(paste("(D)"))) +
  labs(y="Weather PC1 (Temperature)",
       x="Soil Productivity (PC1)")

# (E) Seeding Density * Weather PC1 (Temperature)
ggplot(data, aes(y=Weather.PC1, x=Seed.Density)) + 
  geom_point(size=2) +
  geom_smooth(method = "lm", data = data, color="black")  +
  theme_bw(base_size = 11, base_family = "") +
  theme(plot.title=element_text(size=15,face="bold"),
        axis.title=element_text(size=14),
        axis.text = element_text(size=12)) +  
  ggtitle(expression(paste("(E)"))) +
  labs(y="Weather PC1 (Temperature)",
       x="Seeding Density (kg/ha)")

# (F) Years Since Rx Fire & Weather PC1 (Temperature)
ggplot(data, aes(y=Weather.PC1, x=Last.Burn)) + 
  geom_point(size=2) +
  geom_smooth(method = "lm", data = data, color="black")  +
  theme_bw(base_size = 11, base_family = "") +
  theme(plot.title=element_text(size=15,face="bold"),
        axis.title=element_text(size=14),
        axis.text = element_text(size=12)) +  
  ggtitle(expression(paste("(F) "))) +
  labs(y="Weather PC1 (Temperature)",
       x="Years Since Prescribed Fire")

# (G) Site Age * Weather PC2 (Rainfall)
ggplot(data, aes(x=Age_2016, y=Weather.PC2)) + 
  geom_point(size=2) +
  geom_smooth(method = "lm", data = data, color="black")  +
  theme_bw(base_size = 11, base_family = "") +
  theme(plot.title=element_text(size=15,face="bold"),
        axis.title=element_text(size=14),
        axis.text = element_text(size=12)) +  
  ggtitle(expression(paste("(G) "))) +
  labs(y="Weather PC2 (Rainfall)",
       x="Site Age (Years)")

# (H) Seed Mix Richness * Weather PC2 (Rainfall)
ggplot(data, aes(y=Weather.PC2, x=Mix.Richness)) + 
  geom_point(size=2) +
  geom_smooth(method = "lm", data = data, color="black")  +
  theme_bw(base_size = 11, base_family = "") +
  theme(plot.title=element_text(size=15,face="bold"),
        axis.title=element_text(size=14),
        axis.text = element_text(size=12)) +  
  ggtitle(expression(paste("(H)"))) +
  labs(y="Weather PC2 (Rainfall)",
       x="Seed Mix Richness")

# (I) State * Weather PC2 (Rainfall)
ggplot(data, aes(y=Weather.PC2, x=Landscape)) + 
  geom_point(size=2) +
  geom_boxplot()  +
  geom_jitter(position=position_jitter(0.1))+
  theme_bw(base_size = 11, base_family = "") +
  theme(plot.title=element_text(size=15,face="bold"),
        axis.title=element_text(size=14),
        axis.text = element_text(size=12)) +  
  ggtitle(expression(paste("(I)"))) +
  labs(y="Weather PC2 (Rainfall)",
       x="State")+
  scale_x_discrete(labels = c('Indiana','Illinois','Michigan'))

# (J) Soil Productivity & Weather PC2 (Rainfall)
ggplot(data, aes(y=Weather.PC2, x=Soils)) + 
  geom_point(size=2) +
  geom_smooth(method = "lm", data = data, color="black")  +
  theme_bw(base_size = 11, base_family = "") +
  theme(plot.title=element_text(size=15,face="bold"),
        axis.title=element_text(size=14),
        axis.text = element_text(size=12)) +  
  ggtitle(expression(paste("(J)"))) +
  labs(y="Weather PC2 (Rainfall)",
       x="Soil Productivity (PC1)")

# (K) Seeding rate * Weather PC2 (Rainfall)
ggplot(data, aes(y=Weather.PC2, x=Seed.Density)) + 
  geom_point(size=2) +
  geom_smooth(method = "lm", data = data, color="black")  +
  theme_bw(base_size = 11, base_family = "") +
  theme(plot.title=element_text(size=15,face="bold"),
        axis.title=element_text(size=14),
        axis.text = element_text(size=12)) +  
  ggtitle(expression(paste("(K)"))) +
  labs(y="Weather PC2 (Rainfall)",
       x="Seeding Density (kg/ha)")

# (L) Years Since Fire * Weather PC2 (Rainfall)
ggplot(data, aes(y=Weather.PC2, x=Last.Burn)) + 
  geom_point(size=2) +
  geom_smooth(method = "lm", data = data, color="black")  +
  theme_bw(base_size = 11, base_family = "") +
  theme(plot.title=element_text(size=15,face="bold"),
        axis.title=element_text(size=14),
        axis.text = element_text(size=12)) +  
  ggtitle(expression(paste("(L)"))) +
  labs(y="Weather PC2 (Rainfall)",
       x="Years Since Prescribed Fire")

# (M) Site age & Weather PC3 (Mild Droughts)
ggplot(data, aes(x=Age_2016, y=Weather.PC3)) + 
  geom_point(size=2) +
  geom_smooth(method = "lm", data = data, color="black")  +
  theme_bw(base_size = 11, base_family = "") +
  theme(plot.title=element_text(size=15,face="bold"),
        axis.title=element_text(size=14),
        axis.text = element_text(size=12)) +  
  ggtitle(expression(paste("(M)"))) +
  labs(y="Weather PC3 (Mild Droughts)",
       x="Site Age (Years)")

# (N) Seed Mix Richness * Weather PC3 (Mild Droughts)
ggplot(data, aes(y=Weather.PC3, x=Mix.Richness)) + 
  geom_point(size=2) +
  geom_smooth(method = "lm", data = data, color="black")  +
  theme_bw(base_size = 11, base_family = "") +
  theme(plot.title=element_text(size=15,face="bold"),
        axis.title=element_text(size=14),
        axis.text = element_text(size=12)) +  
  ggtitle(expression(paste("(N)"))) +
  labs(y="Weather PC3 (Mild Droughts)",
       x="Seed Mix Richness")

# (O) State * Weather PC3 (Mild Droughts)
ggplot(data, aes(y=Weather.PC3, x=Landscape)) + 
  geom_point(size=2) +
  geom_boxplot()  +
  geom_jitter(position=position_jitter(0.1))+
  theme_bw(base_size = 11, base_family = "") +
  theme(plot.title=element_text(size=15,face="bold"),
        axis.title=element_text(size=14),
        axis.text = element_text(size=12)) +  
  ggtitle(expression(paste("(O)"))) +
  labs(y="Weather PC3 (Mild Droughts)",
       x="State")+
  scale_x_discrete(labels = c('Indiana','Illinois','Michigan'))

# (P) Soil Productivity * Weather PC3 (Mild Droughts)
ggplot(data, aes(y=Weather.PC3, x=Soils)) + 
  geom_point(size=2) +
  geom_smooth(method = "lm", data = data, color="black")  +
  theme_bw(base_size = 11, base_family = "") +
  theme(plot.title=element_text(size=15,face="bold"),
        axis.title=element_text(size=14),
        axis.text = element_text(size=12)) +  
  ggtitle(expression(paste("(P)"))) +
  labs(y="Weather PC3 (Mild Droughts)",
       x="Soil Productivity (PC1)")

# (Q) Seeding Rate * Weather PC3 (Mild Droughts)
ggplot(data, aes(y=Weather.PC3, x=Seed.Density)) + 
  geom_point(size=2) +
  geom_smooth(method = "lm", data = data, color="black")  +
  theme_bw(base_size = 11, base_family = "") +
  theme(plot.title=element_text(size=15,face="bold"),
        axis.title=element_text(size=14),
        axis.text = element_text(size=12)) +  
  ggtitle(expression(paste("(Q)"))) +
  labs(y="Weather PC3 (Mild Droughts)",
       x="Seeding Density (kg/ha)")

# (R) Years Since Fire * Weather PC3 (Mild Droughts)
ggplot(data, aes(y=Weather.PC3, x=Last.Burn)) + 
  geom_point(size=2) +
  geom_smooth(method = "lm", data = data, color="black")  +
  theme_bw(base_size = 11, base_family = "") +
  theme(plot.title=element_text(size=15,face="bold"),
        axis.title=element_text(size=14),
        axis.text = element_text(size=12)) +  
  ggtitle(expression(paste("(R)"))) +
  labs(y="Weather PC3 (Mild Droughts)",
       x="Years Since Prescribed Fire")
