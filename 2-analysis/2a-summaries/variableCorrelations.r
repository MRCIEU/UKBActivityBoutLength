
resDir = Sys.getenv('RES_DIR')
dataDir = Sys.getenv('PROJECT_DATA')

source('../loadData.r')

dataCDHybrid = loadData('CD', hybrid=TRUE)
dataCDPred = loadData('CD', hybrid=FALSE)

##
## process hybrid variables

colnames(dataCDHybrid)
dataCD = dataCDHybrid
 
# convert to average per day
dataCD$dur1sed = dataCD$dur1sed/dataCD$num_days
dataCD$dur2sed = dataCD$dur2sed/dataCD$num_days
dataCD$dur3sed = dataCD$dur3sed/dataCD$num_days
dataCD$overall_classSleep = dataCD$overall_classSleep/dataCD$num_days
dataCD$overall_classLight = dataCD$overall_classLight / dataCD$num_days
dataCD$overall_classSed = dataCD$overall_classSed / dataCD$num_days
dataCD$overall_100mg = dataCD$overall_100mg / dataCD$num_days
 
dataCD$dur1mod100 = dataCD$dur1mod100/dataCD$num_days
dataCD$dur2mod100 = dataCD$dur2mod100/dataCD$num_days
dataCD$dur3mod100 = dataCD$dur3mod100/dataCD$num_days
dataCD$dur4mod100 = dataCD$dur4mod100/dataCD$num_days
 
dataCDHybrid = dataCD

##
## process predicted variables

dataCD = dataCDPred
 
# convert to average per day
dataCD$dur1sed = dataCD$dur1sed/dataCD$num_days
dataCD$dur2sed = dataCD$dur2sed/dataCD$num_days
dataCD$dur3sed = dataCD$dur3sed/dataCD$num_days
dataCD$overall_classSleep = dataCD$overall_classSleep/dataCD$num_days
dataCD$overall_classLight = dataCD$overall_classLight / dataCD$num_days
dataCD$overall_classSed = dataCD$overall_classSed / dataCD$num_days
dataCD$overall_classMod = dataCD$overall_classMod / dataCD$num_days
 
 
# convert to average per day
dataCD$dur1mod = dataCD$dur1mod/dataCD$num_days
dataCD$dur2mod = dataCD$dur2mod/dataCD$num_days
dataCD$dur3mod = dataCD$dur3mod/dataCD$num_days
dataCD$dur4mod = dataCD$dur4mod/dataCD$num_days

dataCDPred = dataCD
 
 
 
colnames(dataCDHybrid)
 
 
ix = which(dataCDPred$eid != dataCDHybrid$eid)
length(ix)

dataComb = cbind(dataCDHybrid[,c('overall_classSleep', 'overall_classSed', 'dur1sed', 'dur2sed', 'dur3sed', 'overall_classLight', 'overall_100mg', 'dur1mod100', 'dur2mod100', 'dur3mod100', 'dur4mod100')], dataCDPred[,c('overall_classSleep','overall_classSed','dur1sed', 'dur2sed', 'dur3sed','overall_classWalk', 'overall_classLight', 'overall_classMod', 'dur1mod', 'dur2mod', 'dur3mod', 'dur4mod')])

#colnames(dataComb)

#colnames(dataComb) = c('eid', 'dur1sed', 'dur2sed', 'dur3sed', 'dur1mod100', 'dur2mod100', 'dur3mod100', 'overall_classSleep', 'overall_classSed', 'overall_classLight', 'overall_100mg', 'Pdur1sed', 'Pdur2sed', 'Pdur3sed', 'Pdur1mod', 'Pdur2mod', 'Pdur3mod', 'Poverall_classSleep', 'Poverall_classSed', 'Poverall_classLight', 'Poverall_classMod')
colnames(dataComb) = c('overall_classSleep', 'overall_classSed', 'dur1sed', 'dur2sed', 'dur3sed', 'overall_classLight', 'overall_100mg', 'dur1mod100', 'dur2mod100', 'dur3mod100', 'dur4mod100', 'Poverall_classSleep', 'Poverall_classSed', 'Pdur1sed', 'Pdur2sed', 'Pdur3sed', 'Poverall_classWalk', 'Poverall_classLight', 'Poverall_classMod', 'Pdur1mod', 'Pdur2mod', 'Pdur3mod', 'Pdur4mod')


sumx = summary(dataComb)
write.table(sumx, paste0(resDir, '/variable-summary.csv'), sep=',')


cx = cor(dataComb)
write.table(format(cx, digits=2), paste0(resDir, '/variable-cors.csv'), sep=',', quote=FALSE)



# scatter plots of main vs sensitity variables

pdf(paste0(dataDir, '/scatter-sed.pdf'))
plot(dataComb$overall_classSed, dataComb$Poverall_classSed)
dev.off()

pdf(paste0(dataDir, '/scatter-sed1.pdf'))
plot(dataComb$dur1sed, dataComb$Pdur1sed)
dev.off()

pdf(paste0(dataDir, '/scatter-sed2.pdf'))
plot(dataComb$dur2sed, dataComb$Pdur2sed)
dev.off()

pdf(paste0(dataDir, '/scatter-sed3.pdf'))
plot(dataComb$dur3sed, dataComb$Pdur3sed)
dev.off()


pdf(paste0(dataDir, '/scatter-mod.pdf'))
plot(dataComb$overall_100mg, dataComb$Poverall_classMod)
dev.off()

pdf(paste0(dataDir, '/scatter-mod1.pdf'))
plot(dataComb$dur1mod100, dataComb$Pdur1mod)
dev.off()

pdf(paste0(dataDir, '/scatter-mod2.pdf'))
plot(dataComb$dur2mod100, dataComb$Pdur2mod)
dev.off()

pdf(paste0(dataDir, '/scatter-mod3.pdf'))
plot(dataComb$dur3mod100, dataComb$Pdur3mod)
dev.off()

pdf(paste0(dataDir, '/scatter-mod4.pdf'))
plot(dataComb$dur4mod100, dataComb$Pdur4mod)
dev.off()


pdf(paste0(dataDir, '/scatter-light.pdf'))
plot(dataComb$overall_classLight, dataComb$Poverall_classLight)
dev.off()

pdf(paste0(dataDir, '/scatter-sleep.pdf'))
plot(dataComb$overall_classSleep, dataComb$Poverall_classSleep)
dev.off()
