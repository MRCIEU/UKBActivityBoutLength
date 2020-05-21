
# this script creates histograms for the distribution of the number of valid days
# for the 'complete days' and 'other day' imputed versions
# supplementary figure 2


source('../loadData.r')
resdir = Sys.getenv('RES_DIR')



##
## Complete days version

print('complete days')
dataCD = loadData('CD')
pdf(paste0(resdir,'/hist-validdays-cd.pdf'))
barplot(table(dataCD$num_days), xlab='Number of valid days', cex.axis=1.5, cex.lab=1.5, cex.names=1.5)
dev.off()

tabImp = table(dataCD$num_days)
print(tabImp)
print(tabImp[6])
print(sum(tabImp))

print('% with some missing data (for paper text):')
misnum = tabImp[6]/sum(tabImp)
print(misnum)

##
## Other day imputed version

print('imputed')
dataImp = loadData('imp')
pdf(paste0(resdir,'/hist-validdays-imp.pdf'))
barplot(table(dataImp$num_days), xlab='Number of valid days', cex.axis=1.5, cex.lab=1.5, cex.names=1.5)
dev.off()

tabImp = table(dataImp$num_days)
print(tabImp)
print(tabImp[6])
print(sum(tabImp))

print('% with some missing data (for paper text):')
misnum = tabImp[6]/sum(tabImp)
print(misnum)

