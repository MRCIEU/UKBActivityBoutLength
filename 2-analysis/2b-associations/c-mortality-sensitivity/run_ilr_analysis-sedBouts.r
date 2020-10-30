## Adapted from code provided by Duncan McGregor https://doi.org/10.1177/0962280219864125


resDir = Sys.getenv('RES_DIR')


######
######
###### load data and put in right format


source('../../loadData.r')
dataCD = loadData('CD', TRUE)


## laplace correction (isometric log ratio transformation can't work with zeros)

dataCD$total = dataCD$overall_classSleep + dataCD$overall_classLight + dataCD$overall_classSed + dataCD$overall_100mg + 6

dataCD$overall_classSleep = 1440*(dataCD$overall_classSleep+1)/(dataCD$total)
dataCD$overall_classLight = 1440*(dataCD$overall_classLight+1) / (dataCD$total)
dataCD$overall_100mg = 1440*(dataCD$overall_100mg+1) / (dataCD$total)
dataCD$dur1sed = 1440*(dataCD$dur1sed+1) / (dataCD$total)
dataCD$dur2sed = 1440*(dataCD$dur2sed+1) / (dataCD$total)
dataCD$dur3sed = 1440*(dataCD$dur3sed+1) / (dataCD$total)

data = dataCD[,c('dur1sed', 'overall_classLight', 'overall_100mg', 'overall_classSleep', 'dur2sed', 'dur3sed', 'survivalStatus', 'survivalTime')]

colnames(data) = c('SB1', 'LIPA', 'MVPA', 'SL', 'SB2', 'SB3','d', 't')


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


nCats = 6

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


print('z')
print(head(z1))
print(head(z2))
print(head(z3))
print(head(z4))
print(head(z5))
print(head(z6))



# Merge onto dataframe
data       <- data.frame(data,z1,z2,z3,z4,z5,z6)


print(head(data))
print(summary(data))



## FOR SOME REASON MY VERSION HAS - NOT . SO I REPLACE THESE
colnames(data) = gsub('-', '.', colnames(data))



## each cox regression is a binary partition tree with a different starting category
cox1 <- coxph(Surv(t,d) ~ sex + survivalStartAge + townsend + income + smokestatus + prev_firstocc_circ + prev_firstocc_resp + prev_firstocc_canc + ethblack + ethasian + ethother + ed1college + ed2alevels + ed3gcse + ed4cse + ed5nvq + ed6other_profes + seasonCos + seasonSin + bmi + SB1_LI.MV.SL.SB.SB + LIPA_MV.SL.SB.SB + MVPA_SL.SB.SB + SL_SB.SB + SB2_SB , data=data)
cox2 <- coxph(Surv(t,d) ~ sex + survivalStartAge + townsend + income + smokestatus + prev_firstocc_circ + prev_firstocc_resp + prev_firstocc_canc + ethblack + ethasian + ethother + ed1college + ed2alevels + ed3gcse + ed4cse + ed5nvq + ed6other_profes + seasonCos + seasonSin + bmi + LIPA_SB.MV.SL.SB.SB + SB1_MV.SL.SB.SB + MVPA_SL.SB.SB.1 + SL_SB.SB.1 + SB2_SB.1, data=data)
cox3 <- coxph(Surv(t,d) ~ sex + survivalStartAge + townsend + income + smokestatus + prev_firstocc_circ + prev_firstocc_resp + prev_firstocc_canc + ethblack + ethasian + ethother + ed1college + ed2alevels + ed3gcse + ed4cse + ed5nvq + ed6other_profes + seasonCos + seasonSin + bmi + MVPA_SB.LI.SL.SB.SB + SB1_LI.SL.SB.SB + LIPA_SL.SB.SB + SL_SB.SB.2 + SB2_SB.2, data=data)
cox4 <- coxph(Surv(t,d) ~ sex + survivalStartAge + townsend + income + smokestatus + prev_firstocc_circ + prev_firstocc_resp + prev_firstocc_canc + ethblack + ethasian + ethother + ed1college + ed2alevels + ed3gcse + ed4cse + ed5nvq + ed6other_profes + seasonCos + seasonSin + bmi + SL_SB.LI.MV.SB.SB + SB1_LI.MV.SB.SB + LIPA_MV.SB.SB + MVPA_SB.SB + SB2_SB.3, data=data)
cox5 <- coxph(Surv(t,d) ~ sex + survivalStartAge + townsend + income + smokestatus + prev_firstocc_circ + prev_firstocc_resp + prev_firstocc_canc + ethblack + ethasian + ethother + ed1college + ed2alevels + ed3gcse + ed4cse + ed5nvq + ed6other_profes + seasonCos + seasonSin + bmi + SB2_SB.LI.MV.SL.SB + SB1_LI.MV.SL.SB + LIPA_MV.SL.SB + MVPA_SL.SB + SL_SB, data=data)
cox6 <- coxph(Surv(t,d) ~ sex + survivalStartAge + townsend + income + smokestatus + prev_firstocc_circ + prev_firstocc_resp + prev_firstocc_canc + ethblack + ethasian + ethother + ed1college + ed2alevels + ed3gcse + ed4cse + ed5nvq + ed6other_profes + seasonCos + seasonSin + bmi + SB3_SB.LI.MV.SL.SB + SB1_LI.MV.SL.SB.1 + LIPA_MV.SL.SB.1 + MVPA_SL.SB.1 + SL_SB.1, data=data)


summary(cox1)
summary(cox2)
summary(cox3)
summary(cox4)
summary(cox5)
summary(cox6)


# Setup necessary for calculations based on cox1

# variance-covariance matrix for our activity variables
vmat <- vcov(cox1)[(nConfs+1):(nConfs+5),(nConfs+1):(nConfs+5)]

# this is the geometric mean or the number of categories in the numerator and denominator
# also +ve for numerator and -ve for denominator 
#£matA <- matrix(c(sqrt(2/3),-sqrt(2/3),-sqrt(2/3),0,sqrt(1/2),-sqrt(1/2)),nrow=2,byrow=T)


matA <- matrix(c(	sqrt(5/6), -sqrt(5/6)/5, -sqrt(5/6)/5, -sqrt(5/6)/5, -sqrt(5/6)/5, -sqrt(5/6)/5,
			0, sqrt(4/5), -sqrt(4/5)/4, -sqrt(4/5)/4, -sqrt(4/5)/4, -sqrt(4/5)/4,
			0, 0, sqrt(3/4),-sqrt(3/4)/3,-sqrt(3/4)/3,-sqrt(3/4)/3,
                                0, 0, 0,sqrt(2/3),-sqrt(2/3)/2, -sqrt(2/3)/2,
                                0, 0, 0, 0, sqrt(1/2),-sqrt(1/2)),nrow=5,byrow=T)




# this is a as in equation 25 on page 1454 of Duncan's paper
vecA <- coef(cox1)[(nConfs+1):(nConfs+5)] %*% matA
colnames(vecA) <- c("SB","LIPA","MVPA", "SL", "SB2", "SB3")

Wake1 <- 1440




# Calculate geometric average as reference point
# Geometric mean function
gm <- function(x) {
  exp(mean(log(x)))
}

#Close to Wake1
Cavg <- apply(dat_CoDA,2,gm)*Wake1/sum(apply(dat_CoDA,2,gm))   # close to "Wake1" hours

# Replace Cavg with something other than the average
names(Cavg) <- c("SB1","LIPA","MVPA", "SL", "SB2", "SB3")








# Setup isotemporal plots / calculations

# Setup reference point
Cref        <- Cavg
names(Cref) <- NULL
SB3_0       <-  Cref[6]
SB2_0       <-  Cref[5]
SL0         <-  Cref[4]
MVPA0       <-  Cref[3]
LIPA0       <-  Cref[2]
SB0         <-  Cref[1]

# Setup maximum and minimum
MVPA_max  <-  max(data$MVPA) #3.0
LIPA_max  <-  max(data$LIPA) #9.0
SB_max    <-  max(data$SB) #8.0
SL_max   <- max(data$SL) #8.0 #********************
SB2_max    <-  max(data$SB2) #8.0
SB3_max    <-  max(data$SB3) #8.0
MVPA_min  <-  min(data$MVPA) #1/60
LIPA_min  <-  min(data$LIPA) #2.0 
SB_min    <-  min(data$SB) #4.0 
SL_min    <-  min(data$SL) #4.0 #*****************
SB2_min    <-  min(data$SB2) #4.0
SB3_min    <-  min(data$SB3) #4.0


MVPA_max=LIPA_max=SB_max=SB2_max=SB3_max=1440
MVPA_min=LIPA_min=SB_min=SL_min=SB2_min=SB3_min=0

# Setup vectors of variable on x-axis - these are the values used when each category is the comparison (i.e. x axis on plot)
MVPA_x  <- seq(MVPA_min ,MVPA_max ,length.out=400)
LIPA_x <-  seq(LIPA_min ,LIPA_max ,length.out=400)
SB_x   <-  seq(SB_min   ,SB_max   ,length.out=400)
SL_x  <-  seq(SL_min   ,SL_max   ,length.out=400)
SB2_x   <-  seq(SB2_min   ,SB2_max   ,length.out=400)
SB3_x   <-  seq(SB3_min   ,SB3_max   ,length.out=400)


#Setup vector of variable in legend


# other category for SB1 comparison
(MVPAvsSB   <- Wake1-SB_x-LIPA0 - SL0 - SB2_0 - SB3_0)
(LIPAvsSB   <- Wake1-SB_x-MVPA0 - SL0 - SB2_0 - SB3_0)
(SLvsSB   <- Wake1-SB_x-MVPA0 - LIPA0 - SB2_0 - SB3_0)
(SB2vsSB   <- Wake1-SB_x-MVPA0 - LIPA0 - SL0 - SB3_0)
(SB3vsSB   <- Wake1-SB_x-MVPA0 - LIPA0 - SL0 - SB2_0)

# other category for SB2 comparison
(MVPAvsSB2   <- Wake1-SB2_x-LIPA0 - SL0 - SB0 - SB3_0)
(LIPAvsSB2   <- Wake1-SB2_x-MVPA0 - SL0 - SB0 - SB3_0)
(SLvsSB2   <- Wake1-SB2_x-MVPA0 - LIPA0 - SB0 - SB3_0)
(SBvsSB2   <- Wake1-SB2_x-MVPA0 - LIPA0 - SL0 - SB3_0)
(SB3vsSB2   <- Wake1-SB2_x-MVPA0 - LIPA0 - SL0 - SB0)

# other category for SB3 comparison
(MVPAvsSB3   <- Wake1-SB3_x-LIPA0 - SL0 - SB0 - SB2_0)
(LIPAvsSB3   <- Wake1-SB3_x-MVPA0 - SL0 - SB0 - SB2_0)
(SLvsSB3   <- Wake1-SB3_x-MVPA0 - LIPA0 - SB0 - SB2_0)
(SB2vsSB3   <- Wake1-SB3_x-MVPA0 - LIPA0 - SL0 - SB0)
(SBvsSB3   <- Wake1-SB3_x-MVPA0 - LIPA0 - SL0 - SB2_0)


# Combine and add additional behaviours

# SB1 comparisons
dataSB_MVPA  <-data.frame(MVPA=MVPAvsSB, LIPA=LIPA0, SB=SB_x, SL=SL0, SB2=SB2_0, SB3=SB3_0)
dataSB_LIPA  <-data.frame(MVPA=MVPA0, LIPA=LIPAvsSB, SB=SB_x, SL=SL0, SB2=SB2_0, SB3=SB3_0)
dataSB_SL  <-data.frame(MVPA=MVPA0, LIPA=LIPA0, SB=SB_x, SL=SLvsSB, SB2=SB2_0, SB3=SB3_0)
dataSB_SB2  <-data.frame(MVPA=MVPA0, LIPA=LIPA0, SB=SB_x, SL=SL0, SB2=SB2vsSB, SB3=SB3_0)
dataSB_SB3  <-data.frame(MVPA=MVPA0, LIPA=LIPA0, SB=SB_x, SL=SL0, SB2=SB2_0, SB3=SB3vsSB)

# SB2 comparisons
dataSB2_MVPA  <-data.frame(MVPA=MVPAvsSB2, LIPA=LIPA0, SB=SB0, SL=SL0, SB2=SB2_x, SB3=SB3_0)
dataSB2_LIPA  <-data.frame(MVPA=MVPA0, LIPA=LIPAvsSB2, SB=SB0, SL=SL0, SB2=SB2_x, SB3=SB3_0)
dataSB2_SL  <-data.frame(MVPA=MVPA0, LIPA=LIPA0, SB=SB0, SL=SLvsSB2, SB2=SB2_x, SB3=SB3_0)
dataSB2_SB  <-data.frame(MVPA=MVPA0, LIPA=LIPA0, SB=SBvsSB2, SL=SL0, SB2=SB2_x, SB3=SB3_0)
dataSB2_SB3  <-data.frame(MVPA=MVPA0, LIPA=LIPA0, SB=SB0, SL=SL0, SB2=SB2_x, SB3=SB3vsSB2)

# SB3 comparisons
dataSB3_MVPA  <-data.frame(MVPA=MVPAvsSB3, LIPA=LIPA0, SB=SB0, SL=SL0, SB2=SB2_0, SB3=SB3_x)
dataSB3_LIPA  <-data.frame(MVPA=MVPA0, LIPA=LIPAvsSB3, SB=SB0, SL=SL0, SB2=SB2_0, SB3=SB3_x)
dataSB3_SL  <-data.frame(MVPA=MVPA0, LIPA=LIPA0, SB=SB0, SL=SLvsSB3, SB2=SB2_0, SB3=SB3_x)
dataSB3_SB  <-data.frame(MVPA=MVPA0, LIPA=LIPA0, SB=SBvsSB3, SL=SL0, SB2=SB2_0, SB3=SB3_x)
dataSB3_SB2  <-data.frame(MVPA=MVPA0, LIPA=LIPA0, SB=SB0, SL=SL0, SB2=SB2vsSB3, SB3=SB3_x)





#Remove any negative MVPA arising
dataSB_MVPA  <- dataSB_MVPA[which(dataSB_MVPA$MVPA>0),]
dataSB_LIPA  <- dataSB_LIPA[which(dataSB_LIPA$LIPA>0),]
dataSB_SL  <- dataSB_SL[which(dataSB_SL$SL>0),]
dataSB_SB2  <- dataSB_SB2[which(dataSB_SB2$SB2>0),]
dataSB_SB3  <- dataSB_SB3[which(dataSB_SB3$SB3>0),]

#Remove any negative MVPA arising
dataSB2_MVPA  <- dataSB2_MVPA[which(dataSB2_MVPA$MVPA>0),]
dataSB2_LIPA  <- dataSB2_LIPA[which(dataSB2_LIPA$LIPA>0),]
dataSB2_SL  <- dataSB2_SL[which(dataSB2_SL$SL>0),]
dataSB2_SB  <- dataSB2_SB[which(dataSB2_SB$SB>0),]
dataSB2_SB3  <- dataSB2_SB3[which(dataSB2_SB3$SB3>0),]

#Remove any negative MVPA arising
dataSB3_MVPA  <- dataSB3_MVPA[which(dataSB3_MVPA$MVPA>0),]
dataSB3_LIPA  <- dataSB3_LIPA[which(dataSB3_LIPA$LIPA>0),]
dataSB3_SB  <- dataSB3_SB[which(dataSB3_SB$SB>0),]
dataSB3_SL  <- dataSB3_SL[which(dataSB3_SL$SL>0),]
dataSB3_SB2  <- dataSB3_SB2[which(dataSB3_SB2$SB2>0),]




# Calculate hazard ratios
# SL HRs, comparing with SB1,SB2 and SB3 with the other categories



# Setup two-row reference composition so we can use robcompositions
refCoDa <- data.frame(MVPA=c(MVPA0,MVPA0),
                      LIPA=c(LIPA0,LIPA0),
                      SB=c(SB0,SB0), SL=c(SL0,SL0), SB2=c(SB2_0,SB2_0), SB3=c(SB3_0,SB3_0))




plotData <- function(datax, cat1, cat2, cat1_0) {

	HR1 <- with(datax,(SB3/SB3_0)^vecA[6] *
                        (SB2/SB2_0)^vecA[5] *
                        (SL/SL0)^vecA[4] *
                        (MVPA/MVPA0)^vecA[3] *
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


plotdata1 = plotData(dataSB_MVPA, 'SB', 'MVPA', SB0)
plotdata2 = plotData(dataSB_LIPA, 'SB', 'LIPA', SB0)
plotdata3 = plotData(dataSB_SL, 'SB', 'SL', SB0)
plotdata4 = plotData(dataSB_SB2, 'SB', 'SB2', SB0)
plotdata5 = plotData(dataSB_SB3, 'SB', 'SB3', SB0)

plotdata6 = plotData(dataSB2_MVPA, 'SB2', 'MVPA', SB2_0)
plotdata7 = plotData(dataSB2_LIPA, 'SB2', 'LIPA', SB2_0)
plotdata8 = plotData(dataSB2_SL, 'SB2', 'SL', SB2_0)
plotdata9 = plotData(dataSB2_SB, 'SB2', 'SB', SB2_0)
plotdata10 = plotData(dataSB2_SB3, 'SB2', 'SB3', SB2_0)

plotdata11 = plotData(dataSB3_MVPA, 'SB3', 'MVPA', SB3_0)
plotdata12 = plotData(dataSB3_LIPA, 'SB3', 'LIPA', SB3_0)
plotdata13 = plotData(dataSB3_SL, 'SB3', 'SL', SB3_0)
plotdata14 = plotData(dataSB3_SB2, 'SB3', 'SB2', SB3_0)
plotdata15 = plotData(dataSB3_SB, 'SB3', 'SB', SB3_0)

data.results <- rbind(
  plotdata1, 
  plotdata2, 
  plotdata3, 
  plotdata4, 
  plotdata5, 
  plotdata6,
  plotdata7,
  plotdata8,
  plotdata9,
  plotdata10,
  plotdata11,
  plotdata12,
  plotdata13,
  plotdata14,
  plotdata15
)



write.csv(data.results, paste0(resDir,'/ilr-results-sed.csv'), row.names=FALSE)



