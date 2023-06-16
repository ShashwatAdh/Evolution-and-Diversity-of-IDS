library(ggplot2)
library(reshape)
library(RColorBrewer)
library(paletteer)
library(colorRamps)

df<-read.csv('/home/exp-1/Desktop/data/snort_sid_ds2.csv', check.names=FALSE)


#df$'2017'<- (df$`2017`)/11039
#df$'2018'<- (df$`2018`)/11039
#df$'2019'<- (df$`2019`)/11039
#df$'2020'<- (df$`2020`)/11039

m <- melt(df, id=c("sid"))


myLoc <- ((which(levels(factor(m$variable)) == "2019") ) + ( which(levels(factor(m$variable)) == "2020"))) / 2
myLoc2 <- ((which(levels(factor(m$variable)) == "2018") ) + ( which(levels(factor(m$variable)) == "2019"))) / 2
myLoc3 <- ((which(levels(factor(m$variable)) == "2017") ) + ( which(levels(factor(m$variable)) == "2018"))) / 2

lb <- c(1,"","","","","",10,"","","","","",100,"","","","","",1000,"","","","","",10000)

col<-colorRampPalette(brewer.pal(9, "YlOrRd"))(26)



plt <- ggplot(m, aes(x=factor(variable),  y=factor(sid), fill=factor(value))) +
  geom_tile() + #scale_fill_gradientn(colors = c("beige", "darkgoldenrod1", "darkgoldenrod3", "brown1", "brown4" ), breaks = c(0.01,0.5,1), values=c(0,0.005,0.02,0.1009,1), na.value="aliceblue" )+
  scale_fill_manual(values=col, na.value="azure3", labels=lb, guide= guide_coloursteps(even.steps = TRUE, show.limits = FALSE ))+
  geom_point( aes(size="0"), shape =NA, colour = "azure3")+
  guides(size=guide_legend("", override.aes=list(shape=15, size = 6)))+
  theme(plot.title=element_text(hjust=0.5, face="bold"), axis.text = element_text(size=16, face="bold"), axis.title.y = element_text(size=16, face="bold"),
        axis.title.x = element_text( size=16, face="bold"), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), legend.margin = margin(0, 0, 0, 0), legend.text = element_text(size=13, face="bold"), legend.title = element_text(size=13, face="bold"))+ scale_x_discrete(expand=c(0,0)) + scale_y_discrete(expand=c(0,0)) + labs(y="Rule SIDs", x="Years" )+
  labs(fill="Counts", title = "Signature ID evolution, Snort, Dataset 1")+
  scale_colour_manual(values=NA) +              
  guides(colour=guide_legend("0", override.aes=list(colour="white")))+
  geom_label(aes(x="2017", y="40360"), label="Revision 1",  color="black", fill="cornsilk", fontface="bold", size=5)+
  geom_label(aes(x="2018", y="40360"), label="Revision 2", color="black", fill="cornsilk", fontface="bold", size=5)+
  geom_label(aes(x=myLoc, y="40360"), label="Revision 3", color="black", fill="cornsilk", fontface="bold", size=5)+
  geom_label(aes(x="2017", y="19439"), label="Revision 8", color="black", fill="cornsilk", fontface="bold", size=5)+
  geom_label(aes(x="2018", y="19439"), label="Revision 9", color="black", fill="cornsilk", fontface="bold", size=5)+
  geom_label(aes(x=myLoc, y="19439"), label="Revision 10", color="black", fill="cornsilk", fontface="bold", size=5)+
  geom_label(aes(x=myLoc2, y="1"), label="Blacklisted IPs", color="black", fill="cornsilk", fontface="bold", size=5)+
  geom_label(aes(x=myLoc2, y="34"), label="PreProcessor rule", color="black", fill="cornsilk", fontface="bold",size=5)
plt

ggsave(filename = "/home/exp-1/Desktop/data/plots/final/snort_ds2_SID.png", device="png",  width=22, height=12.5, units="cm")



df2<-read.csv('/home/exp-1/Desktop/data/suricata_sid_ds2.csv', check.names=FALSE)
m2<- melt(df2, id=c("sid"))
#mycol <- c("#d53e4f", "#f46d43", "#fdae61", "#fee08b", "#e6f598", "#abdda4", "#ddf1da")
col<-colorRampPalette(brewer.pal(9, "YlOrRd"))(25)
#col<- green2red(25)
lb2 <- c(2,"","","","",20,"","","","",200,"","","","","",2000,"","","","",20000)

plt2 <- ggplot(m2, aes(x=factor(variable),  y=factor(sid), fill=factor(value))) +
  geom_tile(color="white") + #scale_fill_gradientn(colors = c("darkolivegreen", "brown1"), breaks = c(0.01,0.5,1), values=c(0.1,0.5), na.value="white" )+
  scale_fill_manual(values=col, na.value="azure3", labels=lb2, guide= guide_coloursteps(even.steps = TRUE, show.limits = FALSE ))+
  geom_point( aes(size="0"), shape =NA, colour = "azure3")+
  guides(size=guide_legend("", override.aes=list(shape=15, size = 6)))+
  theme(plot.title=element_text(hjust=0.5, face="bold"), axis.text = element_text(size=15, face="bold"), axis.title.y = element_text( size=16, face="bold"),
        axis.title.x = element_text( size=16, face="bold"), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), legend.text = element_text(size=13, face="bold"), legend.title =  element_text(size=13, face="bold"))+ scale_x_discrete(expand=c(0,0)) + scale_y_discrete(expand=c(0,0)) + labs(y="Rule SIDs", x="Years")+
  labs(fill="Counts", title = "Signature ID evolution, Suricata, Dataset 1")+
  geom_label(aes(x="2020", y="2019401"), label="Revision 28",  color="black", fill="cornsilk", fontface="bold", size=3)+
  geom_label(aes(x="2019", y="2019401"), label="Revision 26",  color="black", fill="cornsilk", fontface="bold", size=3)+
  geom_label(aes(x="2018", y="2019401"), label="Revision 22", color="black", fill="cornsilk", fontface="bold", size=3)+
  geom_label(aes(x="2017", y="2019401"), label="Revision 18", color="black", fill="cornsilk", fontface="bold", size=3)+
  geom_label(aes(x="2020", y="2012647"), label="Revision 6", color="black", fill="cornsilk", fontface="bold", size=3)+
  geom_label(aes(x="2019", y="2012647"), label="Revision 5", color="black", fill="cornsilk", fontface="bold", size=3)+
  geom_label(aes(x=myLoc3, y="2012647"), label="Revision 4", color="black", fill="cornsilk", fontface="bold", size=3)+
  geom_label(aes(x=myLoc2, y="2025275"), label="Revision 1", color="black", fill="cornsilk", fontface="bold", size=3)+
  geom_label(aes(x="2020", y="2025275"), label="Revision 2", color="black", fill="cornsilk", fontface="bold",size=3)+
  #geom_label(aes(x="2019", y="2522342"), label="Blacklisted IPs", color="black", fill="cornsilk", fontface="bold",size=3)+
  geom_label(aes(x="2020", y="2025275"), label="Revision 2", color="black", fill="cornsilk", fontface="bold",size=3)+
  geom_label(aes(x=myLoc2, y="2522342"), label="Blacklisted IPs", color="black", fill="cornsilk", fontface="bold",size=3)+
  geom_label(aes(x="2017", y="2000419"), label="Revision 22", color="black", fill="cornsilk", fontface="bold",size=3)+
  geom_label(aes(x="2019", y="2000419"), label="Revision 24", color="black", fill="cornsilk", fontface="bold",size=3)+
  geom_label(aes(x="2017", y="2013031"), label="Revision 4", color="black", fill="cornsilk", fontface="bold",size=3)+
  geom_label(aes(x="2018", y="2013031"), label="Revision 5", color="black", fill="cornsilk", fontface="bold",size=3)+
  geom_label(aes(x="2019", y="2013031"), label="Revision 6", color="black", fill="cornsilk", fontface="bold",size=3)+
  geom_label(aes(x="2020", y="2013031"), label="Revision 7", color="black", fill="cornsilk", fontface="bold",size=3)+
  geom_label(aes(x=myLoc2, y="2522790"), label="Blacklisted IPs", color="black", fill="cornsilk", fontface="bold",size=3)+
  geom_label(aes(x="2017", y="2015744"), label="Revision 3", color="black", fill="cornsilk", fontface="bold",size=3)+
  geom_label(aes(x="2019", y="2015744"), label="Revision 4", color="black", fill="cornsilk", fontface="bold",size=3)+
  geom_label(aes(x="2017", y="2001581"), label="Revision 14", color="black", fill="cornsilk", fontface="bold",size=3)+
  geom_label(aes(x=myLoc2, y="2522306"), label="Blacklisted IPs", color="black", fill="cornsilk", fontface="bold",size=3)+
  geom_label(aes(x=myLoc2, y="2522836"), label="Blacklisted IPs", color="black", fill="cornsilk", fontface="bold",size=3)+
  geom_label(aes(x="2019", y="2024364"), label="Revision 2", color="black", fill="cornsilk", fontface="bold",size=3)+
  #geom_label(aes(x=myLoc, y="2024390"), label="Revision 2", color="black", fill="cornsilk", fontface="bold",size=3)+
  geom_label(aes(x=myLoc3, y="2014381"), label="Revision 2", color="black", fill="cornsilk", fontface="bold",size=3)+
  geom_label(aes(x=myLoc, y="2014381"), label="Revision 3", color="black", fill="cornsilk", fontface="bold",size=3)+
  geom_label(aes(x=myLoc3, y="2014380"), label="Revision 4", color="black", fill="cornsilk", fontface="bold",size=3)+
  geom_label(aes(x=myLoc, y="2014381"), label="Revision 4", color="black", fill="cornsilk", fontface="bold",size=3)+
  geom_label(aes(x="2020", y="2027863"), label="Revision 1", color="black", fill="cornsilk", fontface="bold",size=3)+
  geom_label(aes(x="2020", y="2027757"), label="Revision 1", color="black", fill="cornsilk", fontface="bold",size=3)+
  geom_label(aes(x="2020", y="2027758"), label="Revision 1", color="black", fill="cornsilk", fontface="bold",size=3)+
  geom_label(aes(x=myLoc, y="2027390"), label="Revision 2", color="black", fill="cornsilk", fontface="bold",size=3)
  
  

plt2

ggsave(filename = "/home/exp-1/Desktop/data/plots/final/suricata_ds2_SID.png", device="png",  width=24, height=14.5, units="cm")


df3<-read.csv('/home/exp-1/Desktop/data/snort_sid_ds3.csv', check.names=FALSE)
m3<- melt(df3, id=c("sid"))
col<-colorRampPalette(brewer.pal(9, "YlOrRd"))(26)

plt3 <- ggplot(m3, aes(x=factor(variable),  y=factor(sid), fill=factor(value))) +
  geom_tile(color="white") + #scale_fill_gradientn(colors = c("darkolivegreen", "brown1"), breaks = c(0.01,0.5,1), values=c(0.1,0.5), na.value="white" )+
  scale_fill_manual(values=col, na.value="azure3", labels=lb, guide= guide_coloursteps(even.steps = TRUE, show.limits = FALSE ))+
  geom_point( aes(size="0"), shape =NA, colour = "azure3")+
  guides(size=guide_legend("", override.aes=list(shape=15, size = 6)))+
  theme(plot.title=element_text(hjust=0.5, face="bold"), axis.text = element_text(size=16, face="bold"), axis.title.y = element_text( size=16, face="bold"),
        axis.title.x = element_text( size=16, face="bold"), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), legend.text = element_text(size=13, face="bold"), legend.title = element_text(size=13, face="bold"))+ scale_x_discrete(expand=c(0,0)) + scale_y_discrete(expand=c(0,0)) + labs(y="Rule SIDs", x="Years")+
  labs(fill="Counts", title = "Signature ID evolution, Snort, Dataset 2")+
  geom_label(aes(x="2017", y="42016"), label="Revision 1",  color="black", fill="cornsilk", fontface="bold", size=5)+
  geom_label(aes(x=myLoc2, y="42016"), label="Revision 2", color="black", fill="cornsilk", fontface="bold", size=5)+
  geom_label(aes(x="2020", y="42016"), label="Revision 3", color="black", fill="cornsilk", fontface="bold", size=5)+
  geom_label(aes(x="2018", y="31136"), label="Revision 2", color="black", fill="cornsilk", fontface="bold", size=5)+
  geom_label(aes(x="2020", y="31136"), label="Revision 3", color="black", fill="cornsilk", fontface="bold", size=5)+
  geom_label(aes(x=myLoc3, y="23496"), label="Revision 5", color="black", fill="cornsilk", fontface="bold", size=5)+
  geom_label(aes(x="2019", y="23496"), label="Revision 6", color="black", fill="cornsilk", fontface="bold", size=5)+
  geom_label(aes(x="2020", y="23496"), label="Revision 7", color="black", fill="cornsilk", fontface="bold", size=5)+
  geom_label(aes(x=myLoc2, y="12"), label="Pre-Processor rule", color="black", fill="cornsilk", fontface="bold", size=5)+
  geom_label(aes(x=myLoc2, y="1"), label="Blacklisted IPs", color="black", fill="cornsilk", fontface="bold",size=5)

plt3

ggsave(filename = "/home/exp-1/Desktop/data/plots/final/snort_ds3_SID.png", device="png",  width=22, height=12.5, units="cm")



df4 <-read.csv('/home/exp-1/Desktop/data/suricata_sid_ds3.csv', check.names=FALSE)
#df4<-df4[order(df4$sid),]
m4<- melt(df4, id=c("Index"))
col<-colorRampPalette(brewer.pal(9, "YlOrRd"))(111)

#z=length(m4$sid)
#breaks= z[seq(1,length(z),by=2)]

sp<-rep("",25)
lbs<-c(1,sp,"",100,"",sp,"",1000,"",sp,"",10000,sp,100000)


plt4 <- ggplot(m4, aes(x=factor(variable),  y=Index, fill=factor(value))) +
  geom_tile() + 
  scale_fill_manual(values=col, na.value="azure3",  labels=lbs, guide= guide_coloursteps(even.steps = TRUE, show.limits = FALSE ))+
  geom_point( aes(size="0"), shape =NA, colour = "azure3")+
  guides(size=guide_legend("", override.aes=list(shape=15, size = 6)))+
  theme(plot.title=element_text(hjust=0.5, face="bold"), axis.text = element_text(size=16, face="bold"), axis.title.y = element_text( size=15, face="bold"),
        axis.title.x = element_text( size=15, face="bold"), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), legend.text = element_text(size=13, face="bold"),legend.title = element_text(size=13, face="bold"))+ 
  scale_y_continuous(expand=c(0,0) ) +  scale_x_discrete(expand=c(0,0))   +
  labs(y="Enumerated Rule SIDs", x="Years", fill="Counts", title = "Signature ID evolution, Suricata, Dataset 2")+
  annotate("text", x=4.55, y=138, label="|<------------------ Blacklisted IP SIDs--->|", angle=90, size=3.5,fontface="bold")+
  coord_cartesian(xlim=c(0.5,4.5) , clip="off")+
  geom_hline(yintercept = 75, linetype='dotdash', col="purple")
  
  #geom_label(aes(x="2017", y="40360"), label="Revision 1",  color="black", fill="cornsilk", fontface="bold", size=5)+
  #geom_label(aes(x="2018", y="40360"), label="Revision 2", color="black", fill="cornsilk", fontface="bold", size=5)+
  #geom_label(aes(x=myLoc, y="40360"), label="Revision 3", color="black", fill="cornsilk", fontface="bold", size=5)+
  #geom_label(aes(x="2017", y="19439"), label="Revision 8", color="black", fill="cornsilk", fontface="bold", size=5)+
  #geom_label(aes(x="2018", y="19439"), label="Revision 9", color="black", fill="cornsilk", fontface="bold", size=5)+
  #geom_label(aes(x=myLoc, y="19439"), label="Revision 10", color="black", fill="cornsilk", fontface="bold", size=5)+
  #geom_label(aes(x=myLoc2, y="2025275"), label="Revision 1", color="black", fill="cornsilk", fontface="bold", size=3)+
  #geom_label(aes(x="2020", y="2025275"), label="Revision 2", color="black", fill="cornsilk", fontface="bold",size=3)

plt4

ggsave(filename = "/home/exp-1/Desktop/data/plots/final/suricata_ds3_SID.png", device="png",  width=22, height=12.5, units="cm")
