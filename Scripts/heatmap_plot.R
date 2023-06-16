library("reshape2")
library("ggplot2")
library("scales")

df<-read.csv('/home/exp-1/Desktop/data/Alerts/ds2_snort_evo_proc.csv', check.names=FALSE)

data1.melt<-melt(df,id=c("id"))
#col.plot<-c('green','red')
z<-factor(data1.melt$variable)

su<-ggplot(data1.melt, aes(x=variable, y=id))+geom_tile(aes(fill=value)) +
  scale_fill_gradient(low='white',high='red',guide='colorbar') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text.x = element_text(angle = 90), plot.title=element_text(hjust=0.5), axis.text=element_text(size=14, face="bold"), axis.title = element_text(size=14, face="bold")) +
  ggtitle("Snort alerts evolution through the datepoints, Dataset 1") + 
  labs(y="Enumerated  Positive Alerts", x="Date Points") +
  scale_x_discrete(expand=c(0,0), breaks= z[seq(1,length(z),by=2)]) + scale_y_discrete(expand = c(0,0))
su 
