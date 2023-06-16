library(ggplot2)
library(lubridate)
library(patchwork)
library(dplyr)
library(hrbrthemes)


f<- read.csv("/home/exp-1/Desktop/data/Runtime Plot/runtime.csv")

a<- as.Date(f$snort_date, format = "%d-%b-%Y")
b<- as.Date(f$suricata_date, format = "%d-%b-%Y")

a <- as.character(a)
b <- as.character(b)
d <- as.numeric(f$snort_rule_filesizes) / 1024
e <- as.numeric(f$suricata_rule_filesize) / 1024

plt<- ggplot(data=f, aes(x=a)) +
  geom_line(aes(x=a, y=snort_runtime_ds2), size=1.5, color="Red", group=1) +
  geom_line(aes(x=a, y=d/73.7792), size=1.5, color="Blue", group=1)+
  #labs(x='Rules sample dates')+
  scale_y_continuous( name = "Runtime in Seconds", sec.axis = sec_axis(trans=~.*73.7792, name ="Rule File Size in KBs") )+
  coord_cartesian(ylim = c(200, 270)) +
  ggtitle("Snort runtime when processed against various rule files, Dataset 1")+
  theme_bw() + theme(plot.title=element_text(hjust=0.5), axis.text.x = element_blank(), axis.ticks.x=element_blank(),  axis.text = element_text(size=14, face="bold"), axis.title.y= element_text(color="Red", size=14, face="bold"), axis.title.y.right = element_text(color="Blue", size=14, face="bold"), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
  
plt2 <- ggplot(data=f, aes(x=a)) +
  geom_line(aes(x=a, y=f$snort_runtime_ds3), size=1.5, color="Red", group=1) +
  geom_line(aes(x=a, y=d/35), size=1.5, color="Blue", group=1)+
  labs(x='Rules sample dates')+
  scale_y_continuous( name = "Runtime in Seconds",  sec.axis = sec_axis(trans=~.*35, name ="Rule File Size in KBs") )+
  coord_cartesian(ylim = c(400, 850)) +
  scale_x_discrete( breaks=a[seq(1,length(a),by=2)])+
  theme_bw() + theme(plot.title = element_text(hjust=0.5), axis.text.x = element_text(angle = 90, size = 14, face="bold"), axis.text = element_text(size=14, face="bold"), axis.title.y= element_text(color="Red", size=14, face="bold"), axis.title.y.right = element_text(color="Blue", size=14, face="bold"), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  ggtitle("Snort runtime when processed against various rule files, Dataset 2")
  

plt/plt2 + plot_layout(heights = c(4,4))
ggsave(filename = "/home/exp-1/Desktop/data/Plot/REDO plots/Snort_runtime_vs_filesize.png", device="png", dpi=1200, width=20, height=15, units="cm")


plt3<- ggplot(data=f, aes(x=b)) +
  geom_line(aes(x=b, y=f$suricata_runtime_ds2), size=1.5, color="Red", group=1) +
  geom_line(aes(x=b, y=e/300), size=1.5, color="Blue", group=1)+
  #labs(x='Rules sample dates')+
  scale_y_continuous( name = "Runtime in Seconds", sec.axis = sec_axis(trans=~.*300, name ="Rule File Size in KBs") )+
  coord_cartesian(ylim = c(30, 60)) +
  ggtitle("Suricata runtime when processed against various rule files, Dataset 1")+
  theme_bw() + theme(plot.title=element_text(hjust=0.5), axis.text.x = element_blank(), axis.ticks.x=element_blank(), axis.text = element_text(size=14, face="bold"), axis.title.y= element_text(color="Red", size=14, face="bold"), axis.title.y.right = element_text(color="Blue", size=14, face="bold"), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
  


plt4 <- ggplot(data=f, aes(x=b)) +
  geom_line(aes(x=b, y=f$suricata_runtime_ds3), size=1.5, color="Red", group=1) +
  geom_line(aes(x=b, y=e/100), size=1.5, color="Blue", group=1)+
  labs(x='Rules sample dates')+
  scale_y_continuous( name = "Runtime in Seconds", sec.axis = sec_axis(trans=~.*100, name ="Rule File Size in KBs") )+
  coord_cartesian(ylim = c(100, 200)) +
  scale_x_discrete( breaks=b[seq(1,length(b),by=2)])+
  theme_bw() + theme(plot.title = element_text(hjust=0.5), axis.text.x = element_text(angle = 90), axis.text = element_text(size=14, face="bold"), axis.title = element_text(size=14, face="bold"), axis.title.y= element_text(color="Red", size=14, face="bold"), axis.title.y.right = element_text(color="Blue", size=14, face="bold"), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  ggtitle("Suricata runtime when processed against various rule files, Dataset 2")



plt3/plt4 + plot_layout(heights = c(4,4))
ggsave(filename = "/home/exp-1/Desktop/data/Plot/REDO plots/Suricata_runtime_vs_filesize.png", device="png", dpi=1200, width=20, height=15, units="cm")
