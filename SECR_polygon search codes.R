
library (secr, quietly = TRUE)
library(maptools)
library(parallel)
###total 4 models
getwd()
# load package secr
library (secr, quietly = TRUE)
library(maptools)

##########################################
# 
#import data from text files to capthist object CH
CH<-read.capthist("Ele_Capturefile.txt","Ele_Trapfile.txt",fmt ='XY',detector="polygon", covname=c("Adult_age", "Group_type", "Group_size"))
plot(CH, tracks=T)


mask=read.mask("Mask.txt",spacing=1000)
# summarise capthist object
summary(CH)

plot(mask)
plot(CH,tracks=T,add=T)	


#Null Model
mod1_null<-secr.fit(CH,mask=mask,model=list(lambda0~1,sigma~1), detectfn = "HHN", CL=TRUE, ncores=4)

## scale parameter
mod1_Sh2<-secr.fit(CH,mask=mask,model=list(lambda0~Group_type,sigma~1), detectfn = "HHN", CL=TRUE, ncores=4)

## detection probability
mod1_Lh2<-secr.fit(CH,mask=mask,model=list(lambda0~1,sigma~Group_type), detectfn = "HHN", ncores=4)

# both on detection probability and scale parameter
mod1_Sh2Lh2<-secr.fit(CH,mask=mask,model=list(lambda0~h2,sigma~h2), detectfn = "HHN", ncores=4)




##### To derive Densities from CL=T models

##Overall Density
OverallDensity<-derived(Model,sessnum = NULL, groups = NULL, alpha = 0.05, se.esa = T, se.D = TRUE, ncores = 4)



#Covariate wise density
CovDensity<-derived(Model,sessnum = NULL, groups = "COVARIATE", alpha = 0.05, se.esa = T, se.D = TRUE, ncores = 4)
