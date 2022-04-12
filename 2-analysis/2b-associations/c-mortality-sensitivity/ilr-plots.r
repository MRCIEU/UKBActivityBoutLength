
## plotting run locally

resDir = Sys.getenv('RES_DIR')


library(ggplot2)

el2 <- theme_bw(16) +  theme(legend.position="bottom",legend.title = element_blank())


# overall

data.results = read.csv(paste0(resDir, '/ilr-results-overall.csv'))

data.results$xvar1[which(data.results$xvar1=="(d) SL")] = "Sleep"
data.results$xvar1[which(data.results$xvar1=="(b) LIPA")] = "Light"
data.results$xvar1[which(data.results$xvar1=="(c) SB")] = "Sedentary"
data.results$xvar1[which(data.results$xvar1=="(a) MVPA")] = "MVPA"


data.results$xvar2[which(data.results$xvar2=="SL")] = "Sleep"
data.results$xvar2[which(data.results$xvar2=="LIPA")] = "Light"
data.results$xvar2[which(data.results$xvar2=="SB")] = "Sedentary"
data.results$xvar2[which(data.results$xvar2=="MVPA")] = "MVPA"


# remove rows with no HR value (or CI)
ix = which(data.results$HR=='Inf')
data.results = data.results[-ix,]

pdf(paste0(resDir, "/plotiso2-overall.pdf"), width=20, height=5)
ggplot(data.results,aes(x=x,y=HR,colour=xvar2)) +
  geom_hline(yintercept=1, linetype="dashed", color="grey") +
  geom_line(aes(group=xvar2),size=1,alpha=0.5) +
  geom_ribbon(aes(ymin=HRd,ymax=HRu,fill=xvar2),alpha=0.3)+
  facet_grid(~xvar1) +
  scale_x_continuous(limits=c(-30,30),breaks=seq(-30,30,by=10))+
  ylim(c(0.4,1.25))+
  xlab("Increase in time (min) allocated to title behaviour with corresponding reduction in:") +
  el2
dev.off()



# sed plot


data.results = read.csv(paste0(resDir, '/ilr-results-sed.csv'))

data.results$xvar1[which(data.results$xvar1=="SB")] = "Sedentary 1-15 mins"
data.results$xvar1[which(data.results$xvar1=="SB2")] = "Sedentary 16-40 mins"
data.results$xvar1[which(data.results$xvar1=="SB3")] = "Sedentary 41+ mins"
data.results$xvar1[which(data.results$xvar1=="SL")] = "Sleep"
data.results$xvar1[which(data.results$xvar1=="LIPA")] = "Light"

data.results$xvar2[which(data.results$xvar2=="SB")] = "Sedentary 1-15 mins"
data.results$xvar2[which(data.results$xvar2=="SB2")] = "Sedentary 16-40 mins"
data.results$xvar2[which(data.results$xvar2=="SB3")] = "Sedentary 41+ mins"
data.results$xvar2[which(data.results$xvar2=="SL")] = "Sleep"
data.results$xvar2[which(data.results$xvar2=="LIPA")] = "Light"




# remove rows with no HR value (or CI)
ix = which(data.results$HR=='Inf')
data.results = data.results[-ix,]

pdf(paste0(resDir, "/plotiso2-sed.pdf"), width=20, height=5)
ggplot(data.results,aes(x=x,y=HR,colour=xvar2)) +
  geom_hline(yintercept=1, linetype="dashed", color="grey") +
  geom_line(aes(group=xvar2),size=1,alpha=0.5) +
  geom_ribbon(aes(ymin=HRd,ymax=HRu,fill=xvar2),alpha=0.3)+
  facet_grid(~xvar1) +
  scale_x_continuous(limits=c(-30,30),breaks=seq(-30,30,by=10))+
  ylim(c(0.4,1.25))+
  xlab("Increase in time (min) allocated to title behaviour with corresponding reduction in:") +
  el2
dev.off()


# MVPA plot

data.results = read.csv(paste0(resDir, '/ilr-results-mod.csv'))

data.results$xvar1[which(data.results$xvar1=="MV1")] = "MVPA 1-9 mins"
data.results$xvar1[which(data.results$xvar1=="MV2")] = "MVPA 10-15 mins"
data.results$xvar1[which(data.results$xvar1=="MV3")] = "MVPA 16-40 mins"
data.results$xvar1[which(data.results$xvar1=="MV4")] = "MVPA 41+ mins"
data.results$xvar1[which(data.results$xvar1=="SL")] = "Sleep"
data.results$xvar1[which(data.results$xvar1=="LIPA")] = "Light"
data.results$xvar1[which(data.results$xvar1=="SB")] = "Sedentary"

data.results$xvar2[which(data.results$xvar2=="MV1")] = "MVPA 1-9 mins"
data.results$xvar2[which(data.results$xvar2=="MV2")] = "MVPA 10-15 mins"
data.results$xvar2[which(data.results$xvar2=="MV3")] = "MVPA 16-40 mins"
data.results$xvar2[which(data.results$xvar2=="MV4")] = "MVPA 41+ mins"
data.results$xvar2[which(data.results$xvar2=="SL")] = "Sleep"
data.results$xvar2[which(data.results$xvar2=="LIPA")] = "Light"
data.results$xvar2[which(data.results$xvar2=="SB")] = "Sedentary"



# remove rows with no HR value (or CI)
ix = which(data.results$HR=='Inf')
data.results = data.results[-ix,]


pdf(paste0(resDir, "/plotiso2-mod.pdf"), width=20, height=5)
ggplot(data.results,aes(x=x,y=HR,colour=xvar2)) +
  geom_hline(yintercept=1, linetype="dashed", color="grey") +
  geom_line(aes(group=xvar2),size=1,alpha=0.5) +
  geom_ribbon(aes(ymin=HRd,ymax=HRu,fill=xvar2),alpha=0.3)+
  facet_grid(~xvar1) +
  scale_x_continuous(limits=c(-25,30),breaks=seq(-20,30,by=10))+
  ylim(c(0.7,1.25))+
  xlab("Increase in time (min) allocated to columns behaviour with corresponding reduction in:") +

  el2
dev.off()



el2 <- theme_bw(16) +  theme(legend.position = "none")




pdf(paste0(resDir, "/plotiso2-mod-grid.pdf"), width=20, height=20)
ggplot(data.results,aes(x=x,y=HR,colour=xvar2)) +
  geom_hline(yintercept=1, linetype="dashed", color="grey") +
  geom_line(aes(group=xvar2),size=1,alpha=0.5) +
  geom_ribbon(aes(ymin=HRd,ymax=HRu,fill=xvar2),alpha=0.3)+
#  facet_grid(~xvar1) +
  facet_grid(xvar2~xvar1) +
  scale_x_continuous(limits=c(-25,30),breaks=seq(-20,30,by=10))+
  ylim(c(0.7,1.25))+
  xlab("Increase in time (min) allocated to columns behaviour with corresponding reduction in row behaviour") +
  el2
dev.off()




