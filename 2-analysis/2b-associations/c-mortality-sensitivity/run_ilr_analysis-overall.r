## Adapted from code provided by Duncan McGregor https://doi.org/10.1177/0962280219864125


resDir = Sys.getenv('RES_DIR')


######
######
###### load data and put in right format

source('../../loadData.r')
#dataCD = loadData('CD', TRUE)
source('../../loadDataFromSaved.r')
dataCD = loadDataFromSaved('cd', TRUE)

## laplace correction

dataCD$total = dataCD$overall_classSleep + dataCD$overall_classLight + dataCD$overall_classSed + dataCD$overall_100mg + 4

dataCD$overall_classSleep = 1440*(dataCD$overall_classSleep+1)/(dataCD$total)
dataCD$overall_classLight = 1440*(dataCD$overall_classLight+1) / (dataCD$total)
dataCD$overall_classSed = 1440*(dataCD$overall_classSed+1) / (dataCD$total)
dataCD$overall_100mg = 1440*(dataCD$overall_100mg+1) / (dataCD$total)


#data = dataCD[,c('overall_classSed', 'overall_classLight', 'overall_100mg', 'overall_classSleep', 'age', 'sex', 'survivalStatus', 'survivalTime')]
#colnames(data) = c('SB', 'LIPA', 'MVPA', 'SL', 'age', 'sex', 'd', 't') 

data = dataCD[,c('overall_classSed', 'overall_classLight', 'overall_100mg', 'overall_classSleep', 'survivalStatus', 'survivalTime')]
colnames(data) = c('SB', 'LIPA', 'MVPA', 'SL', 'd', 't')

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



dat_CoDA = data[,1:4]

print(head(dat_CoDA))
print(summary(dat_CoDA))


# Generate ilr coordinates
z1          <- pivotCoord(dat_CoDA,1)
z2          <- pivotCoord(dat_CoDA,2)
z3          <- pivotCoord(dat_CoDA,3)
z4          <- pivotCoord(dat_CoDA,4)

print('z')
print(head(z1))
print(head(z2))
print(head(z3))
print(head(z4))



# Merge onto dataframe
data       <- data.frame(data,z1,z2,z3,z4)


print(head(data))
print(summary(data))



## FOR SOME REASON MY VERSION HAS - NOT . SO I REPLACE THESE
colnames(data) = gsub('-', '.', colnames(data))



## each cox regression is a binary partition tree with a different starting category
cox1 <- coxph(Surv(t,d) ~ sex + survivalStartAge + townsend + income + smokestatus + prev_firstocc_circ + prev_firstocc_resp + prev_firstocc_canc + ethblack + ethasian + ethother + ed1college + ed2alevels + ed3gcse + ed4cse + ed5nvq + ed6other_profes + seasonCos + seasonSin + bmi + SB_LI.MV.SL + LIPA_MV.SL + MVPA_SL , data=data)
cox2 <- coxph(Surv(t,d) ~ sex + survivalStartAge + townsend + income + smokestatus + prev_firstocc_circ + prev_firstocc_resp + prev_firstocc_canc + ethblack + ethasian + ethother + ed1college + ed2alevels + ed3gcse + ed4cse + ed5nvq + ed6other_profes + seasonCos + seasonSin + bmi + LIPA_SB.MV.SL + SB_MV.SL + MVPA_SL.1, data=data)
cox3 <- coxph(Surv(t,d) ~ sex + survivalStartAge + townsend + income + smokestatus + prev_firstocc_circ + prev_firstocc_resp + prev_firstocc_canc + ethblack + ethasian + ethother + ed1college + ed2alevels + ed3gcse + ed4cse + ed5nvq + ed6other_profes + seasonCos + seasonSin + bmi + MVPA_SB.LI.SL + SB_LI.SL + LIPA_SL, data=data)
cox4 <- coxph(Surv(t,d) ~ sex + survivalStartAge + townsend + income + smokestatus + prev_firstocc_circ + prev_firstocc_resp + prev_firstocc_canc + ethblack + ethasian + ethother + ed1college + ed2alevels + ed3gcse + ed4cse + ed5nvq + ed6other_profes + seasonCos + seasonSin + bmi + SL_SB.LI.MV + SB_LI.MV + LIPA_MV, data=data)

summary(cox1)
summary(cox2)
summary(cox3)
summary(cox4)



# Setup necessary for calculations based on cox1

# variance-covariance matrix for our activity variables
vmat <- vcov(cox1)[(nConfs+1):(nConfs+3),(nConfs+1):(nConfs+3)]

# this is the geometric mean or the number of categories in the numerator and denominator
# also +ve for numerator and -ve for denominator 
#£matA <- matrix(c(sqrt(2/3),-sqrt(2/3),-sqrt(2/3),0,sqrt(1/2),-sqrt(1/2)),nrow=2,byrow=T)

matA <- matrix(c(sqrt(3/4),-sqrt(3/4)/3,-sqrt(3/4)/3,-sqrt(3/4)/3,
				0,sqrt(2/3),-sqrt(2/3)/2, -sqrt(2/3)/2,
				0, 0,sqrt(1/2),-sqrt(1/2)),nrow=3,byrow=T)


# this is a as in equation 25 on page 1454 of Duncan's paper
vecA <- coef(cox1)[(nConfs+1):(nConfs+3)] %*% matA
colnames(vecA) <- c("SB","LIPA","MVPA", "SL")

Wake1 <- 1440





# Calculate geometric average as reference point
# Geometric mean function
gm <- function(x) {
  exp(mean(log(x)))
}

#Close to Wake1
Cavg <- apply(dat_CoDA,2,gm)*Wake1/sum(apply(dat_CoDA,2,gm))   # close to "Wake1" hours

# Replace Cavg with something other than the average
#Cavg <- c(8,5,1)
names(Cavg) <- c("SB","LIPA","MVPA", "SL")








# Setup isotemporal plots / calculations

# Setup reference point
Cref        <- Cavg
names(Cref) <- NULL
SL0         <-  Cref[4]
MVPA0       <-  Cref[3]
LIPA0       <-  Cref[2]
SB0         <-  Cref[1]

# Setup maximum and minimum
MVPA_max  <-  max(data$MVPA) #3.0
LIPA_max  <-  max(data$LIPA) #9.0
SB_max    <-  max(data$SB) #8.0
SL_max   <- max(data$SL) #8.0 #********************
MVPA_min  <-  min(data$MVPA) #1/60
LIPA_min  <-  min(data$LIPA) #2.0 
SB_min    <-  min(data$SB) #4.0 
SL_min    <-  min(data$SL) #4.0 #*****************

MVPA_max=LIPA_max=SB_max=1440
MVPA_min=LIPA_min=SB_min=SL_min=0

# Setup vectors of variable on x-axis - these are the values used when each category is the comparison (i.e. x axis on plot)
MVPA_x  <- seq(MVPA_min ,MVPA_max ,length.out=400)
LIPA_x <-  seq(LIPA_min ,LIPA_max ,length.out=400)
SB_x   <-  seq(SB_min   ,SB_max   ,length.out=400)
SL_x  <-  seq(SL_min   ,SL_max   ,length.out=400)


#Setup vector of variable in legend

# other category for mvpa comparison
(LIPAvsMVPA   <- Wake1-MVPA_x-SB0 - SL0)
(SBvsMVPA     <- Wake1-MVPA_x-LIPA0 - SL0)
(SLvsMVPA     <- Wake1-MVPA_x-LIPA0 - SB0)
# other category for lipa comparison
(MVPAvsLIPA   <- Wake1-LIPA_x-SB0 - SL0)
(SBvsLIPA     <- Wake1-LIPA_x-MVPA0 - SL0)
(SLvsLIPA     <- Wake1-LIPA_x-MVPA0 - SB0)
# other category for sb comparison
(MVPAvsSB   <- Wake1-SB_x-LIPA0 - SL0)
(LIPAvsSB   <- Wake1-SB_x-MVPA0 - SL0)
(SLvsSB   <- Wake1-SB_x-MVPA0 - LIPA0)
# other category for sl comparison
(MVPAvsSL   <- Wake1-SB0- LIPA0 - SL_x)
(LIPAvsSL   <- Wake1-SB0-MVPA0 - SL_x)
(SBvsSL   <- Wake1-MVPA0 - LIPA0 - SL_x)

# Combine and add additional behaviours
# MVPA comparisons
dataMVPA_LIPA  <-data.frame(MVPA=MVPA_x, LIPA= LIPAvsMVPA, SB=SB0, SL=SL0)
dataMVPA_SB  <-data.frame(MVPA=MVPA_x, LIPA=LIPA0, SB= SBvsMVPA, SL=SL0)
dataMVPA_SL  <-data.frame(MVPA=MVPA_x, LIPA=LIPA0, SB=SB0, SL=SLvsMVPA)
# LIPA comparisons
dataLIPA_MVPA  <-data.frame(MVPA= MVPAvsLIPA, LIPA=LIPA_x, SB=SB0, SL=SL0)
dataLIPA_SB  <-data.frame(MVPA=MVPA0, LIPA=LIPA_x, SB=SBvsLIPA, SL=SL0)
dataLIPA_SL  <-data.frame(MVPA=MVPA0, LIPA=LIPA_x, SB=SB0, SL=SLvsLIPA)
# SB comparisons
dataSB_MVPA  <-data.frame(MVPA=MVPAvsSB, LIPA=LIPA0, SB=SB_x, SL=SL0)
dataSB_LIPA  <-data.frame(MVPA=MVPA0, LIPA=LIPAvsSB, SB=SB_x, SL=SL0)
dataSB_SL  <-data.frame(MVPA=MVPA0, LIPA=LIPA0, SB=SB_x, SL=SLvsSB)
# SL comparisons
dataSL_MVPA  <-data.frame(MVPA=MVPAvsSL, LIPA=LIPA0, SB=SB0, SL=SL_x)
dataSL_LIPA  <-data.frame(MVPA=MVPA0, LIPA=LIPAvsSL, SB=SB0, SL=SL_x)
dataSL_SB  <-data.frame(MVPA=MVPA0, LIPA=LIPA0, SB=SBvsSL, SL=SL_x)



#Remove any negative MVPA arising
dataMVPA_LIPA  <- dataMVPA_LIPA[which(dataMVPA_LIPA$LIPA>0),]
dataMVPA_SB  <- dataMVPA_SB[which(dataMVPA_SB$SB>0),]
dataMVPA_SL  <- dataMVPA_SL[which(dataMVPA_SL$SL>0),]
dataLIPA_MVPA  <- dataLIPA_MVPA[which(dataLIPA_MVPA$MVPA>0),]
dataLIPA_SB  <- dataLIPA_SB[which(dataLIPA_SB$SB>0),]
dataLIPA_SL  <- dataLIPA_SL[which(dataLIPA_SL$SL>0),]
dataSB_MVPA  <- dataSB_MVPA[which(dataSB_MVPA$MVPA>0),]
dataSB_LIPA  <- dataSB_LIPA[which(dataSB_LIPA$LIPA>0),]
dataSB_SL  <- dataSB_SL[which(dataSB_SL$SL>0),]
dataSL_MVPA  <- dataSL_MVPA[which(dataSL_MVPA$MVPA>0),]
dataSL_LIPA  <- dataSL_LIPA[which(dataSL_LIPA$LIPA>0),]
dataSL_SB  <- dataSL_SB[which(dataSL_SB$SB>0),]

# Calculate hazard ratios
# MVPA HRs, comparing MVPA with SB, LIPA and SL
HR1 <- with(dataMVPA_LIPA,(SL/SL0)^vecA[4] * 
				(MVPA/MVPA0)^vecA[3] * 
              (LIPA/LIPA0)^vecA[2] * 
              (SB/SB0) ^ vecA[1])
HR2 <- with(dataMVPA_SB,(SL/SL0)^vecA[4] * 
				(MVPA/MVPA0)^vecA[3] * 
              (LIPA/LIPA0)^vecA[2] * 
              (SB/SB0) ^ vecA[1])
HR3 <- with(dataMVPA_SL,(SL/SL0)^vecA[4] * 
				(MVPA/MVPA0)^vecA[3] * 
              (LIPA/LIPA0)^vecA[2] * 
              (SB/SB0) ^ vecA[1])
# LIPA HRs, comparing LIPA with MVPA, SB and SL
HR4 <- with(dataLIPA_MVPA,(SL/SL0)^vecA[4] * 
				(MVPA/MVPA0)^vecA[3] * 
              (LIPA/LIPA0)^vecA[2] * 
              (SB/SB0) ^ vecA[1])
HR5 <- with(dataLIPA_SB,(SL/SL0)^vecA[4] * 
				(MVPA/MVPA0)^vecA[3] * 
              (LIPA/LIPA0)^vecA[2] * 
              (SB/SB0) ^ vecA[1])
HR6 <- with(dataLIPA_SL,(SL/SL0)^vecA[4] * 
				(MVPA/MVPA0)^vecA[3] * 
              (LIPA/LIPA0)^vecA[2] * 
              (SB/SB0) ^ vecA[1])
# SB HRs, comparing SB with LIPA, MVPA and SL
HR7 <- with(dataSB_MVPA,(SL/SL0)^vecA[4] * 
				(MVPA/MVPA0)^vecA[3] * 
              (LIPA/LIPA0)^vecA[2] * 
              (SB/SB0) ^ vecA[1])
HR8 <- with(dataSB_LIPA,(SL/SL0)^vecA[4] * 
				(MVPA/MVPA0)^vecA[3] * 
              (LIPA/LIPA0)^vecA[2] * 
              (SB/SB0) ^ vecA[1])
HR9 <- with(dataSB_SL,(SL/SL0)^vecA[4] * 
				(MVPA/MVPA0)^vecA[3] * 
              (LIPA/LIPA0)^vecA[2] * 
              (SB/SB0) ^ vecA[1])
# SL HRs, comparing SL with SB, LIPA and MVPA
HR10 <- with(dataSL_MVPA,(SL/SL0)^vecA[4] * 
				(MVPA/MVPA0)^vecA[3] * 
              (LIPA/LIPA0)^vecA[2] * 
              (SB/SB0) ^ vecA[1])
HR11 <- with(dataSL_LIPA,(SL/SL0)^vecA[4] * 
				(MVPA/MVPA0)^vecA[3] * 
              (LIPA/LIPA0)^vecA[2] * 
              (SB/SB0) ^ vecA[1])
HR12 <- with(dataSL_SB,(SL/SL0)^vecA[4] * 
				(MVPA/MVPA0)^vecA[3] * 
              (LIPA/LIPA0)^vecA[2] * 
              (SB/SB0) ^ vecA[1])


# Setup two-row reference composition so we can use robcompositions
refCoDa <- data.frame(SB=c(SB0,SB0),
                      LIPA=c(LIPA0,LIPA0),
                      MVPA=c(MVPA0,MVPA0), SL=c(SL0,SL0))

comp_labels = c("SB","LIPA","MVPA", "SL")

# Calclate differences in ilr-space
dx1 <- pivotCoord(dataMVPA_LIPA[,comp_labels])-pivotCoord(refCoDa)[rep(1,dim(dataMVPA_LIPA)[1]),]
dx2 <- pivotCoord(dataMVPA_SB[,comp_labels])-pivotCoord(refCoDa)[rep(1,dim(dataMVPA_SB)[1]),]
dx3 <- pivotCoord(dataMVPA_SL[,comp_labels])-pivotCoord(refCoDa)[rep(1,dim(dataMVPA_SL)[1]),]
dx4 <- pivotCoord(dataLIPA_MVPA[,comp_labels])-pivotCoord(refCoDa)[rep(1,dim(dataLIPA_MVPA)[1]),]
dx5 <- pivotCoord(dataLIPA_SB[,comp_labels])-pivotCoord(refCoDa)[rep(1,dim(dataLIPA_SB)[1]),]
dx6 <- pivotCoord(dataLIPA_SL[,comp_labels])-pivotCoord(refCoDa)[rep(1,dim(dataLIPA_SL)[1]),]
dx7 <- pivotCoord(dataSB_MVPA[,comp_labels])-pivotCoord(refCoDa)[rep(1,dim(dataSB_MVPA)[1]),]
dx8 <- pivotCoord(dataSB_LIPA[,comp_labels])-pivotCoord(refCoDa)[rep(1,dim(dataSB_LIPA)[1]),]
dx9 <- pivotCoord(dataSB_SL[,comp_labels])-pivotCoord(refCoDa)[rep(1,dim(dataSB_SL)[1]),]
dx10 <- pivotCoord(dataSL_MVPA[,comp_labels])-pivotCoord(refCoDa)[rep(1,dim(dataSL_MVPA)[1]),]
dx11 <- pivotCoord(dataSL_LIPA[,comp_labels])-pivotCoord(refCoDa)[rep(1,dim(dataSL_LIPA)[1]),]
dx12 <- pivotCoord(dataSL_SB[,comp_labels])-pivotCoord(refCoDa)[rep(1,dim(dataSL_SB)[1]),]


test.V1 <- diag(as.matrix(dx1) %*% vmat %*% t(as.matrix(dx1)))
test.V2 <- diag(as.matrix(dx2) %*% vmat %*% t(as.matrix(dx2)))
test.V3 <- diag(as.matrix(dx3) %*% vmat %*% t(as.matrix(dx3)))
test.V4 <- diag(as.matrix(dx4) %*% vmat %*% t(as.matrix(dx4)))
test.V5 <- diag(as.matrix(dx5) %*% vmat %*% t(as.matrix(dx5)))
test.V6 <- diag(as.matrix(dx6) %*% vmat %*% t(as.matrix(dx6)))
test.V7 <- diag(as.matrix(dx7) %*% vmat %*% t(as.matrix(dx7)))
test.V8 <- diag(as.matrix(dx8) %*% vmat %*% t(as.matrix(dx8)))
test.V9 <- diag(as.matrix(dx9) %*% vmat %*% t(as.matrix(dx9)))
test.V10 <- diag(as.matrix(dx10) %*% vmat %*% t(as.matrix(dx10)))
test.V11 <- diag(as.matrix(dx11) %*% vmat %*% t(as.matrix(dx11)))
test.V12 <- diag(as.matrix(dx12) %*% vmat %*% t(as.matrix(dx12)))

# Calculate asymptotic confidence intervals
HR1u <- (exp(+sqrt(test.V1)*qnorm(0.975)))*HR1
HR2u <- (exp(+sqrt(test.V2)*qnorm(0.975)))*HR2
HR3u <- (exp(+sqrt(test.V3)*qnorm(0.975)))*HR3
HR4u <- (exp(+sqrt(test.V4)*qnorm(0.975)))*HR4
HR5u <- (exp(+sqrt(test.V5)*qnorm(0.975)))*HR5
HR6u <- (exp(+sqrt(test.V6)*qnorm(0.975)))*HR6
HR7u <- (exp(+sqrt(test.V7)*qnorm(0.975)))*HR7
HR8u <- (exp(+sqrt(test.V8)*qnorm(0.975)))*HR8
HR9u <- (exp(+sqrt(test.V9)*qnorm(0.975)))*HR9
HR10u <- (exp(+sqrt(test.V10)*qnorm(0.975)))*HR10
HR11u <- (exp(+sqrt(test.V11)*qnorm(0.975)))*HR11
HR12u <- (exp(+sqrt(test.V12)*qnorm(0.975)))*HR12

HR1d <- (exp(-sqrt(test.V1)*qnorm(0.975)))*HR1
HR2d <- (exp(-sqrt(test.V2)*qnorm(0.975)))*HR2
HR3d <- (exp(-sqrt(test.V3)*qnorm(0.975)))*HR3
HR4d <- (exp(-sqrt(test.V4)*qnorm(0.975)))*HR4
HR5d <- (exp(-sqrt(test.V5)*qnorm(0.975)))*HR5
HR6d <- (exp(-sqrt(test.V6)*qnorm(0.975)))*HR6
HR7d <- (exp(-sqrt(test.V7)*qnorm(0.975)))*HR7
HR8d <- (exp(-sqrt(test.V8)*qnorm(0.975)))*HR8
HR9d <- (exp(-sqrt(test.V9)*qnorm(0.975)))*HR9
HR10d <- (exp(-sqrt(test.V10)*qnorm(0.975)))*HR10
HR11d <- (exp(-sqrt(test.V11)*qnorm(0.975)))*HR11
HR12d <- (exp(-sqrt(test.V12)*qnorm(0.975)))*HR12

# Combine data needed for plot in single dataframe

plotdata1 <- data.frame(dataMVPA_LIPA, HR=HR1, HRu=HR1u, HRd=HR1d, x=(dataMVPA_LIPA$MVPA-MVPA0),
                        xvar1="(a) MVPA", xvar2="LIPA")
plotdata2 <- data.frame(dataMVPA_SB, HR=HR2, HRu=HR2u, HRd=HR2d, x=(dataMVPA_SB$MVPA-MVPA0),
                        xvar1="(a) MVPA", xvar2="SB")
plotdata3 <- data.frame(dataMVPA_SL, HR=HR3, HRu=HR3u, HRd=HR3d, x=(dataMVPA_SL$MVPA-MVPA0),
                        xvar1="(a) MVPA", xvar2="SL")
plotdata4 <- data.frame(dataLIPA_MVPA, HR=HR4, HRu=HR4u, HRd=HR4d, x=(dataLIPA_MVPA$LIPA-LIPA0),
                        xvar1="(b) LIPA", xvar2="MVPA")
plotdata5 <- data.frame(dataLIPA_SB, HR=HR5, HRu=HR5u, HRd=HR5d, x=(dataLIPA_SB$LIPA-LIPA0),
                        xvar1="(b) LIPA", xvar2="SB")
plotdata6 <- data.frame(dataLIPA_SL, HR=HR6, HRu=HR6u, HRd=HR6d, x=(dataLIPA_SL$LIPA-LIPA0),
                        xvar1="(b) LIPA", xvar2="SL")
plotdata7 <- data.frame(dataSB_MVPA, HR=HR7, HRu=HR7u, HRd=HR7d, x=(dataSB_MVPA$SB-SB0),
                        xvar1="(c) SB", xvar2="MVPA")
plotdata8 <- data.frame(dataSB_LIPA, HR=HR8, HRu=HR8u, HRd=HR8d, x=(dataSB_LIPA$SB-SB0),
                        xvar1="(c) SB", xvar2="LIPA")
plotdata9 <- data.frame(dataSB_SL, HR=HR9, HRu=HR9u, HRd=HR9d, x=(dataSB_SL$SB-SB0),
                        xvar1="(c) SB", xvar2="SL")
plotdata10 <- data.frame(dataSL_MVPA, HR=HR10, HRu=HR10u, HRd=HR10d, x=(dataSL_MVPA $SL-SL0),
                        xvar1="(d) SL", xvar2="MVPA")
plotdata11 <- data.frame(dataSL_LIPA, HR=HR11, HRu=HR11u, HRd=HR11d, x=(dataSL_LIPA$SL-SL0),
                       xvar1="(d) SL", xvar2="LIPA")
plotdata12 <- data.frame(dataSL_SB, HR=HR12, HRu=HR12u, HRd=HR12d, x=(dataSL_SB$SL-SL0),
                        xvar1="(d) SL", xvar2="SB")




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
  plotdata12
)



write.csv(data.results, paste0(resDir,'/ilr-results-overall.csv'), row.names=FALSE)



# Setup theme
el2 <- theme_bw(16) +  theme(legend.position="bottom",legend.title = element_blank())


(head(data.results))
(unique(data.results$xvar2))

# Create plot in tiff file
#tiff("iso2.tiff", units="in", width=10, height=5, res=300)
pdf(paste0(resDir, "/plotiso2.pdf"), width=20, height=5)
ggplot(data.results,aes(x=x,y=HR,colour=xvar2)) + 
  geom_hline(yintercept=1, linetype="dashed", color="black") +
  geom_line(aes(group=xvar2),size=1,alpha=0.5) + 
  geom_ribbon(aes(ymin=HRd,ymax=HRu,fill=xvar2),alpha=0.3)+
  facet_grid(~xvar1) +  
  scale_x_continuous(limits=c(-30,30),breaks=seq(-30,30,by=10))+
  ylim(c(0.4,1.25))+
  xlab("Increase in time (min) allocated to title behaviour with corresponding reduction in:") + 
  el2
dev.off()



