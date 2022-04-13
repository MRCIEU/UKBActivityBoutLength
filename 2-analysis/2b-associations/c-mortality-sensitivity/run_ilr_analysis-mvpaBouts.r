## Adapted from code provided by Duncan McGregor https://doi.org/10.1177/0962280219864125


resDir = Sys.getenv('RES_DIR')



########
########
######## load data and put in right format

source('../../loadData.r')
#dataCD = loadData('CD', TRUE)
source('../../loadDataFromSaved.r')
dataCD = loadDataFromSaved('cd', TRUE)


## laplace correction

dataCD$total = dataCD$overall_classSleep + dataCD$overall_classLight + dataCD$overall_classSed + dataCD$overall_100mg + 7
dataCD$overall_classSleep = 1440*(dataCD$overall_classSleep+1)/(dataCD$total)
dataCD$overall_classSed = 1440*(dataCD$overall_classSed+1) / (dataCD$total)
dataCD$overall_classLight = 1440*(dataCD$overall_classLight+1) / (dataCD$total)

dataCD$dur1mod100 = 1440*(dataCD$dur1mod100+1) / (dataCD$total)
dataCD$dur2mod100 = 1440*(dataCD$dur2mod100+1) / (dataCD$total)
dataCD$dur3mod100 = 1440*(dataCD$dur3mod100+1) / (dataCD$total)
dataCD$dur4mod100 = 1440*(dataCD$dur4mod100+1) / (dataCD$total)

data = dataCD[,c('overall_classSed', 'overall_classLight', 'overall_classSleep', 'dur1mod100','dur2mod100','dur3mod100','dur4mod100', 'age', 'sex', 'survivalStatus', 'survivalTime')]
colnames(data) = c('SB', 'LIPA', 'SL', 'MV1', 'MV2', 'MV3', 'MV4','age', 'sex', 'd', 't')



## get confounders

source('../generic-functions/getConfounders.r')
source('../generic-functions/getConfounderVariables.r')
source('../generic-functions/getConfounderVariablesAll.r')

confsAll = getConfounderVariablesAll(dataCD)
nConfs = ncol(confsAll)

data = cbind(data, confsAll)



#######
#######

library(survival)
library(survminer)
library(robCompositions)
library(ggplot2)
library(ggtern)
library(gtools)
library(zCompositions)
library(Hmisc)


nCats = 7

dat_CoDA = data[,1:nCats]

print(head(dat_CoDA))
print(summary(dat_CoDA))


# Generate ilr coordinates
z1          <- pivotCoord(dat_CoDA,1)
z2          <- pivotCoord(dat_CoDA,2)
z3          <- pivotCoord(dat_CoDA,3)
z4          <- pivotCoord(dat_CoDA,4)
z5          <- pivotCoord(dat_CoDA,5)
z6          <- pivotCoord(dat_CoDA,6)
z7          <- pivotCoord(dat_CoDA,7)


print('z')
print(head(z1))
print(head(z2))
print(head(z3))
print(head(z4))
print(head(z5))
print(head(z6))
print(head(z7))



# Merge onto dataframe
data       <- data.frame(data,z1,z2,z3,z4,z5,z6,z7)


print(head(data))
print(summary(data))



## FOR SOME REASON MY VERSION HAS - NOT . SO I REPLACE THESE
colnames(data) = gsub('-', '.', colnames(data))



## each cox regression is a binary partition tree with a different starting category
# each one uses a set of pivot coordinates, rotating the categories as the most left one of the binary partitions
cox1 <- coxph(Surv(t,d) ~ sex + survivalStartAge + townsend + income + smokestatus + prev_firstocc_circ + prev_firstocc_resp + prev_firstocc_canc + ethblack + ethasian + ethother + ed1college + ed2alevels + ed3gcse + ed4cse + ed5nvq + ed6other_profes + seasonCos + seasonSin + bmi + SB_LI.SL.MV.MV.MV.MV + LIPA_SL.MV.MV.MV.MV + SL_MV.MV.MV.MV + MV1_MV.MV.MV + MV2_MV.MV + MV2_MV, data=data)
cox2 <- coxph(Surv(t,d) ~ sex + survivalStartAge + townsend + income + smokestatus + prev_firstocc_circ + prev_firstocc_resp + prev_firstocc_canc + ethblack + ethasian + ethother + ed1college + ed2alevels + ed3gcse + ed4cse + ed5nvq + ed6other_profes + seasonCos + seasonSin + bmi + LIPA_SB.SL.MV.MV.MV.MV + SB_SL.MV.MV.MV.MV + SL_MV.MV.MV.MV.1 + MV1_MV.MV.MV.1 + MV2_MV.MV.1 + MV3_MV.1, data=data)
cox3 <- coxph(Surv(t,d) ~ sex + survivalStartAge + townsend + income + smokestatus + prev_firstocc_circ + prev_firstocc_resp + prev_firstocc_canc + ethblack + ethasian + ethother + ed1college + ed2alevels + ed3gcse + ed4cse + ed5nvq + ed6other_profes + seasonCos + seasonSin + bmi + SL_SB.LI.MV.MV.MV.MV + SB_LI.MV.MV.MV.MV + LIPA_MV.MV.MV.MV + MV1_MV.MV.MV.2 + MV2_MV.MV.2 + MV3_MV.2, data=data)
cox4 <- coxph(Surv(t,d) ~ sex + survivalStartAge + townsend + income + smokestatus + prev_firstocc_circ + prev_firstocc_resp + prev_firstocc_canc + ethblack + ethasian + ethother + ed1college + ed2alevels + ed3gcse + ed4cse + ed5nvq + ed6other_profes + seasonCos + seasonSin + bmi + MV1_SB.LI.SL.MV.MV.MV + SB_LI.SL.MV.MV.MV + LIPA_SL.MV.MV.MV + SL_MV.MV.MV + MV2_MV.MV.3 + MV3_MV.3, data=data)
cox5 <- coxph(Surv(t,d) ~ sex + survivalStartAge + townsend + income + smokestatus + prev_firstocc_circ + prev_firstocc_resp + prev_firstocc_canc + ethblack + ethasian + ethother + ed1college + ed2alevels + ed3gcse + ed4cse + ed5nvq + ed6other_profes + seasonCos + seasonSin + bmi + MV2_SB.LI.SL.MV.MV.MV + SB_LI.SL.MV.MV.MV.1 + LIPA_SL.MV.MV.MV.1 + SL_MV.MV.MV.1 + MV1_MV.MV + MV3_MV.4, data=data)
cox6 <- coxph(Surv(t,d) ~ sex + survivalStartAge + townsend + income + smokestatus + prev_firstocc_circ + prev_firstocc_resp + prev_firstocc_canc + ethblack + ethasian + ethother + ed1college + ed2alevels + ed3gcse + ed4cse + ed5nvq + ed6other_profes + seasonCos + seasonSin + bmi + MV3_SB.LI.SL.MV.MV.MV + SB_LI.SL.MV.MV.MV.2 + LIPA_SL.MV.MV.MV.2 + SL_MV.MV.MV.2 + MV1_MV.MV.1 + MV2_MV, data=data)
cox7 <- coxph(Surv(t,d) ~ sex + survivalStartAge + townsend + income + smokestatus + prev_firstocc_circ + prev_firstocc_resp + prev_firstocc_canc + ethblack + ethasian + ethother + ed1college + ed2alevels + ed3gcse + ed4cse + ed5nvq + ed6other_profes + seasonCos + seasonSin + bmi + MV4_SB.LI.SL.MV.MV.MV + SB_LI.SL.MV.MV.MV.3 + LIPA_SL.MV.MV.MV.3 + SL_MV.MV.MV.3 + MV1_MV.MV.2 + MV2_MV.1, data=data)

summary(cox1)
summary(cox2)
summary(cox3)
summary(cox4)
summary(cox5)
summary(cox6)
summary(cox7)


# Setup necessary for calculations based on cox1

# variance-covariance matrix for our activity variables
vmat <- vcov(cox1)[(nConfs+1):(nConfs+6),(nConfs+1):(nConfs+6)]
# 3:9

# this is the geometric mean or the number of categories in the numerator and denominator
# also +ve for numerator and -ve for denominator 
#matA <- matrix(c(sqrt(2/3),-sqrt(2/3),-sqrt(2/3),0,sqrt(1/2),-sqrt(1/2)),nrow=2,byrow=T)

matA <- matrix(c(	sqrt(6/7), -sqrt(6/7)/6, -sqrt(6/7)/6, -sqrt(6/7)/6, -sqrt(6/7)/6, -sqrt(6/7)/6, -sqrt(6/7)/6,
			0, sqrt(5/6), -sqrt(5/6)/5, -sqrt(5/6)/5, -sqrt(5/6)/5, -sqrt(5/6)/5, -sqrt(5/6)/5,
			0, 0, sqrt(4/5), -sqrt(4/5)/4, -sqrt(4/5)/4, -sqrt(4/5)/4, -sqrt(4/5)/4,
			0, 0, 0, sqrt(3/4),-sqrt(3/4)/3,-sqrt(3/4)/3,-sqrt(3/4)/3,
                        0, 0, 0, 0,sqrt(2/3),-sqrt(2/3)/2, -sqrt(2/3)/2,
                        0, 0, 0, 0, 0, sqrt(1/2),-sqrt(1/2)),
			nrow=6,byrow=T)




# this is a as in equation 25 on page 1454 of Duncan's paper
vecA <- coef(cox1)[(nConfs+1):(nConfs+6)] %*% matA

colnames(vecA) <- c("SB","LIPA", "SL", "MV1", "MV2", "MV3", "MV4")

Wake1 <- 1440




# Calculate geometric average as reference point
# Geometric mean function
gm <- function(x) {
  exp(mean(log(x)))
}

#Close to Wake1
Cavg <- apply(dat_CoDA,2,gm)*Wake1/sum(apply(dat_CoDA,2,gm))   # close to "Wake1" hours

# Replace Cavg with something other than the average
names(Cavg) <- c("SB","LIPA", "SL", "MV1", "MV2", "MV3", "MV4")








# Setup isotemporal plots / calculations

# Setup reference point
Cref        <- Cavg
names(Cref) <- NULL
MV4_0       <-  Cref[7]
MV3_0       <-  Cref[6]
MV2_0       <-  Cref[5]
MV1_0       <-  Cref[4]
SL0         <-  Cref[3]
LIPA0       <-  Cref[2]
SB0         <-  Cref[1]


SB_max=LIPA_max=SL_max=MV1_max=MV2_max=MV3_max=MV4_max=1440
SB_min=LIPA_min=SL_min=SL_min=MV1_min=MV2_min=MV3_min=MV4_min=0

# Setup vectors of variable on x-axis - these are the values used when each category is the comparison (i.e. x axis on plot)
SB_x   <-  seq(SB_min   ,SB_max   ,length.out=2000)
LIPA_x <-  seq(LIPA_min ,LIPA_max ,length.out=2000)
SL_x  <-  seq(SL_min   ,SL_max   ,length.out=2000)
MV1_x   <-  seq(MV1_min   ,MV1_max   ,length.out=2000)
MV2_x   <-  seq(MV2_min   ,MV2_max   ,length.out=2000)
MV3_x   <-  seq(MV3_min   ,MV3_max   ,length.out=2000)
MV4_x   <-  seq(MV4_min   ,MV4_max   ,length.out=2000)


#Setup vector of variable in legend
## this step calculates, for each category, the values of the other categories respectively when the remaining are the average (baseline) value
## e.g. calculating SB for each value of MV0_x, where the others are LIPA0, SL0 etc.

# other category for MV1 comparison
(SBvsMV1        <- Wake1 - LIPA0 - SL0 - MV1_x - MV2_0 - MV3_0 - MV4_0)
(LIPAvsMV1      <- Wake1 - SB0 - SL0 - MV1_x - MV2_0 - MV3_0 - MV4_0)
(SLvsMV1        <- Wake1 - SB0 - LIPA0 - MV1_x - MV2_0 - MV3_0 - MV4_0)
(MV2vsMV1       <- Wake1 - SB0 - LIPA0 - SL0 - MV1_x - MV3_0 - MV4_0)
(MV3vsMV1       <- Wake1 - SB0 - LIPA0 - SL0  - MV1_x - MV2_0 - MV4_0)
(MV4vsMV1       <- Wake1 - SB0 - LIPA0 - SL0 - MV1_x - MV2_0 - MV3_0)

# other category for MV2 comparison
(SBvsMV2        <- Wake1 - LIPA0 - SL0 - MV1_0 - MV2_x - MV3_0 - MV4_0)
(LIPAvsMV2      <- Wake1 - SB0 - SL0 - MV1_0 - MV2_x - MV3_0 - MV4_0)
(SLvsMV2       <- Wake1 - SB0 - LIPA0 - MV1_0 - MV2_x - MV3_0 - MV4_0)
(MV1vsMV2       <- Wake1 - SB0 - LIPA0 - SL0 - MV2_x - MV3_0 - MV4_0)
(MV3vsMV2       <- Wake1 - SB0 - LIPA0 - SL0 - MV1_0 - MV2_x - MV4_0)
(MV4vsMV2       <- Wake1 - SB0 - LIPA0 - SL0 - MV1_0 - MV2_x - MV3_0)

# other category for MV3 comparison
(SBvsMV3        <- Wake1 - LIPA0 - SL0 - MV1_0 - MV2_0 - MV3_x - MV4_0)
(LIPAvsMV3	<- Wake1 - SB0 - SL0 - MV1_0 - MV2_0 - MV3_x - MV4_0)
(SLvsMV3        <- Wake1 - SB0 - LIPA0 - MV1_0 - MV2_0 - MV3_x - MV4_0)
(MV1vsMV3	<- Wake1 - SB0 - LIPA0 - SL0 - MV2_0 - MV3_x - MV4_0)
(MV2vsMV3	<- Wake1 - SB0 - LIPA0 - SL0 - MV1_0 - MV3_x - MV4_0)
(MV4vsMV3	<- Wake1 - SB0 - LIPA0 - SL0 - MV1_0 - MV2_0 - MV3_x)

# other category for MV4 comparison
(SBvsMV4        <- Wake1 - LIPA0 - SL0 - MV1_0 - MV2_0 - MV3_0 - MV4_x)
(LIPAvsMV4	<- Wake1 - SB0 - SL0 - MV1_0 - MV2_0 - MV3_0 - MV4_x)
(SLvsMV4        <- Wake1 - SB0 - LIPA0 - MV1_0 - MV2_0 - MV3_0 - MV4_x)
(MV1vsMV4	<- Wake1 - SB0 - LIPA0 - SL0 - MV2_0 - MV3_0 - MV4_x)
(MV2vsMV4	<- Wake1 - SB0 - LIPA0 - SL0 - MV1_0 - MV3_0 - MV4_x)
(MV3vsMV4	<- Wake1 - SB0 - LIPA0 - SL0 - MV1_0 - MV2_0 - MV4_x)




# Combine and add additional behaviours

# MV1 comparisons
dataMV1_SB  <-data.frame(SB=SBvsMV1, LIPA=LIPA0, SL=SL0, MV1=MV1_x, MV2=MV2_0, MV3=MV3_0, MV4=MV4_0)
dataMV1_LIPA  <-data.frame(SB=SB0, LIPA=LIPAvsMV1, SL=SL0, MV1=MV1_x, MV2=MV2_0, MV3=MV3_0, MV4=MV4_0)
dataMV1_SL  <-data.frame(SB=SB0, LIPA=LIPA0, SL=SLvsMV1, MV1=MV1_x, MV2=MV2_0, MV3=MV3_0, MV4=MV4_0)
dataMV1_MV2  <-data.frame(SB=SB0, LIPA=LIPA0, SL=SL0, MV1=MV1_x, MV2=MV2vsMV1, MV3=MV3_0, MV4=MV4_0)
dataMV1_MV3  <-data.frame(SB=SB0, LIPA=LIPA0, SL=SL0, MV1=MV1_x, MV2=MV2_0, MV3=MV3vsMV1, MV4=MV4_0)
dataMV1_MV4  <-data.frame(SB=SB0, LIPA=LIPA0, SL=SL0, MV1=MV1_x, MV2=MV2_0, MV3=MV3_0, MV4=MV4vsMV1)

# MV2 comparisons
dataMV2_SB  <-data.frame(SB=SBvsMV2, LIPA=LIPA0, SL=SL0, MV1=MV1_0, MV2=MV2_x, MV3=MV3_0, MV4=MV4_0)
dataMV2_LIPA  <-data.frame(SB=SB0, LIPA=LIPAvsMV2, SL=SL0, MV1=MV1_0, MV2=MV2_x, MV3=MV3_0, MV4=MV4_0)
dataMV2_SL  <-data.frame(SB=SB0, LIPA=LIPA0, SL=SLvsMV2, MV1=MV1_0, MV2=MV2_x, MV3=MV3_0, MV4=MV4_0)
dataMV2_MV1  <-data.frame(SB=SB0, LIPA=LIPA0, SL=SL0, MV1=MV1vsMV2, MV2=MV2_x, MV3=MV3_0, MV4=MV4_0)
dataMV2_MV3  <-data.frame(SB=SB0, LIPA=LIPA0, SL=SL0, MV1=MV1_0, MV2=MV2_x, MV3=MV3vsMV2, MV4=MV4_0)
dataMV2_MV4  <-data.frame(SB=SB0, LIPA=LIPA0, SL=SL0, MV1=MV1_0, MV2=MV2_x, MV3=MV3_0, MV4=MV4vsMV2)

# MV3 comparisons
dataMV3_SB  <-data.frame(SB=SBvsMV3, LIPA=LIPA0, SL=SL0, MV1=MV1_0, MV2=MV2_0, MV3=MV3_x, MV4=MV4_0)
dataMV3_LIPA  <-data.frame(SB=SB0, LIPA=LIPAvsMV3, SL=SL0, MV1=MV1_0, MV2=MV2_0, MV3=MV3_x, MV4=MV4_0)
dataMV3_SL  <-data.frame(SB=SB0, LIPA=LIPA0, SL=SLvsMV3, MV1=MV1_0, MV2=MV2_0, MV3=MV3_x, MV4=MV4_0)
dataMV3_MV1  <-data.frame(SB=SB0, LIPA=LIPA0, SL=SL0, MV1=MV1vsMV3, MV2=MV2_0, MV3=MV3_x, MV4=MV4_0)
dataMV3_MV2  <-data.frame(SB=SB0, LIPA=LIPA0, SL=SL0, MV1=MV1_0, MV2=MV2vsMV3, MV3=MV3_x, MV4=MV4_0)
dataMV3_MV4  <-data.frame(SB=SB0, LIPA=LIPA0, SL=SL0, MV1=MV1_0, MV2=MV2_0, MV3=MV3_x, MV4=MV4vsMV3)

# MV4 comparisons
dataMV4_SB  <-data.frame(SB=SBvsMV4, LIPA=LIPA0, SL=SL0, MV1=MV1_0, MV2=MV2_0, MV3=MV3_0, MV4=MV4_x)
dataMV4_LIPA  <-data.frame(SB=SB0, LIPA=LIPAvsMV4, SL=SL0, MV1=MV1_0, MV2=MV2_0, MV3=MV3_0, MV4=MV4_x)
dataMV4_SL  <-data.frame(SB=SB0, LIPA=LIPA0, SL=SLvsMV4, MV1=MV1_0, MV2=MV2_0, MV3=MV3_0, MV4=MV4_x)
dataMV4_MV1  <-data.frame(SB=SB0, LIPA=LIPA0, SL=SL0, MV1=MV1vsMV4, MV2=MV2_0, MV3=MV3_0, MV4=MV4_x)
dataMV4_MV2  <-data.frame(SB=SB0, LIPA=LIPA0, SL=SL0, MV1=MV1_0, MV2=MV2vsMV4, MV3=MV3_0, MV4=MV4_x)
dataMV4_MV3  <-data.frame(SB=SB0, LIPA=LIPA0, SL=SL0, MV1=MV1_0, MV2=MV2_0, MV3=MV3vsMV4, MV4=MV4_x)





#####
##### remove any rows with negative values

# Remove any negative values arising - MV1
dataMV1_SB  <- dataMV1_SB[which(dataMV1_SB$SB>0),]
dataMV1_LIPA  <- dataMV1_LIPA[which(dataMV1_LIPA$LIPA>0),]
dataMV1_SL  <- dataMV1_SL[which(dataMV1_SL$SL>0),]
dataMV1_MV2  <- dataMV1_MV2[which(dataMV1_MV2$MV2>0),]
dataMV1_MV3  <- dataMV1_MV3[which(dataMV1_MV3$MV3>0),]
dataMV1_MV4  <- dataMV1_MV4[which(dataMV1_MV4$MV4>0),]

#Remove any negative values arising - MV2
dataMV2_SB  <- dataMV2_SB[which(dataMV2_SB$SB>0),]
dataMV2_LIPA  <- dataMV2_LIPA[which(dataMV2_LIPA$LIPA>0),]
dataMV2_SL  <- dataMV2_SL[which(dataMV2_SL$SL>0),]
dataMV2_MV1  <- dataMV2_MV1[which(dataMV2_MV1$MV1>0),]
dataMV2_MV3  <- dataMV2_MV3[which(dataMV2_MV3$MV3>0),]
dataMV2_MV4  <- dataMV2_MV4[which(dataMV2_MV4$MV4>0),]

#Remove any negative values arising - MV3
dataMV3_SB  <- dataMV3_SB[which(dataMV3_SB$SB>0),]
dataMV3_LIPA  <- dataMV3_LIPA[which(dataMV3_LIPA$LIPA>0),]
dataMV3_SL  <- dataMV3_SL[which(dataMV3_SL$SL>0),]
dataMV3_MV1  <- dataMV3_MV1[which(dataMV3_MV1$MV1>0),]
dataMV3_MV2  <- dataMV3_MV2[which(dataMV3_MV2$MV2>0),]
dataMV3_MV4  <- dataMV3_MV4[which(dataMV3_MV4$MV4>0),]

#Remove any negative values arising - MV4
dataMV4_SB  <- dataMV4_SB[which(dataMV4_SB$SB>0),]
dataMV4_LIPA  <- dataMV4_LIPA[which(dataMV4_LIPA$LIPA>0),]
dataMV4_SL  <- dataMV4_SL[which(dataMV4_SL$SL>0),]
dataMV4_MV1  <- dataMV4_MV1[which(dataMV4_MV1$MV1>0),]
dataMV4_MV2  <- dataMV4_MV2[which(dataMV4_MV2$MV2>0),]
dataMV4_MV3  <- dataMV4_MV3[which(dataMV4_MV3$MV3>0),]





# Calculate hazard ratios
# SL HRs, comparing with SB1,SB2 and SB3 with the other categories



# Setup two-row reference composition so we can use robcompositions
refCoDa <- data.frame(SB=c(SB0,SB0),
                      LIPA=c(LIPA0,LIPA0),
                      SL=c(SL0,SL0), 
			MV1=c(MV1_0,MV1_0),MV2=c(MV2_0,MV2_0),MV3=c(MV3_0,MV3_0),MV4=c(MV4_0,MV4_0))




plotData <- function(datax, cat1, cat2, cat1_0) {

	HR1 <- with(datax,(MV4/MV4_0)^vecA[7] *
			(MV3/MV3_0)^vecA[6] *
			(MV2/MV2_0)^vecA[5] *
			(MV1/MV1_0)^vecA[4] *
                        (SL/SL0)^vecA[3] *
                        (LIPA/LIPA0)^vecA[2] *
                        (SB/SB0) ^ vecA[1])

	dx1 <- pivotCoord(datax)-pivotCoord(refCoDa)[rep(1,dim(datax)[1]),]
	test.V1 <- diag(as.matrix(dx1) %*% vmat %*% t(as.matrix(dx1)))
	HR1u <- (exp(+sqrt(test.V1)*qnorm(0.975)))*HR1
	HR1d <- (exp(-sqrt(test.V1)*qnorm(0.975)))*HR1

	plotdatax <- data.frame(datax, HR=HR1, HRu=HR1u, HRd=HR1d, x=(datax[,cat1]-cat1_0),
                        xvar1=cat1, xvar2=cat2)


	return(plotdatax)
}
	


plotdata8 = plotData(dataMV1_SB, 'MV1', 'SB', MV1_0)
plotdata9 = plotData(dataMV1_LIPA, 'MV1', 'LIPA', MV1_0)
plotdata10 = plotData(dataMV1_SL, 'MV1', 'SL', MV1_0)
plotdata12 = plotData(dataMV1_MV2, 'MV1', 'MV2', MV1_0)
plotdata13 = plotData(dataMV1_MV3, 'MV1', 'MV3', MV1_0)
plotdata14 = plotData(dataMV1_MV4, 'MV1', 'MV4', MV1_0)

plotdata15 = plotData(dataMV2_SB, 'MV2', 'SB', MV2_0)
plotdata16 = plotData(dataMV2_LIPA, 'MV2', 'LIPA', MV2_0)
plotdata17 = plotData(dataMV2_SL, 'MV2', 'SL', MV2_0)
plotdata18 = plotData(dataMV2_MV1, 'MV2', 'MV1', MV2_0)
plotdata20 = plotData(dataMV2_MV3, 'MV2', 'MV3', MV2_0)
plotdata21 = plotData(dataMV2_MV4, 'MV2', 'MV4', MV2_0)

plotdata22 = plotData(dataMV3_SB, 'MV3', 'SB', MV3_0)
plotdata23 = plotData(dataMV3_LIPA, 'MV3', 'LIPA', MV3_0)
plotdata24 = plotData(dataMV3_SL, 'MV3', 'SL', MV3_0)
plotdata25 = plotData(dataMV3_MV1, 'MV3', 'MV1', MV3_0)
plotdata26 = plotData(dataMV3_MV2, 'MV3', 'MV2', MV3_0)
plotdata28 = plotData(dataMV3_MV4, 'MV3', 'MV4', MV3_0)

plotdata29 = plotData(dataMV4_SB, 'MV4', 'SB', MV4_0)
plotdata30 = plotData(dataMV4_LIPA, 'MV4', 'LIPA', MV4_0)
plotdata31 = plotData(dataMV4_SL, 'MV4', 'SL', MV4_0)
plotdata32 = plotData(dataMV4_MV1, 'MV4', 'MV1', MV4_0)
plotdata33 = plotData(dataMV4_MV2, 'MV4', 'MV2', MV4_0)
plotdata34 = plotData(dataMV4_MV3, 'MV4', 'MV3', MV4_0)




data.results <- rbind(
  plotdata8,
  plotdata9,
  plotdata10,
  plotdata12,
  plotdata13,
  plotdata14,
  plotdata15,
  plotdata16,
  plotdata17,
  plotdata18,
  plotdata20,
  plotdata21,
  plotdata22,
  plotdata23,
  plotdata24,
  plotdata25,
  plotdata26,
  plotdata28,
  plotdata29,
  plotdata30,
  plotdata31,
  plotdata32,
  plotdata33,
  plotdata34
)



write.csv(data.results, paste0(resDir,'/ilr-results-mod.csv'), row.names=FALSE)



