
dataDir = Sys.getenv('PROJECT_DATA')

source('loadData.r')
x = loadData('CD', hybrid=TRUE)
dd = as.Date(x$datedeath0)

# plot histogram - number of deaths in each month
pdf(paste0(dataDir, '/phenotypes/derived/mortalityMonthHist.pdf'))
hist(dd, "months", freq=TRUE, format = "%d %b %y", las=2, cex.lab=0.5, xlab='')
dev.off()
