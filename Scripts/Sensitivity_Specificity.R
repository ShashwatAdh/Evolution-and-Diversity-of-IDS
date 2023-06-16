library(tidyverse)
library(readxl)
library(patchwork)
library(lubridate)
library(bdscale)

ds2_snort <- read_excel("/home/exp-1/Desktop/data/Count values.xlsx", sheet = "Dataset_2_snort", col_names = TRUE)
ds3_snort <- read_excel("/home/exp-1/Desktop/data/Count values.xlsx", sheet = "Dataset_3_Snort", col_names = TRUE)
ds2_suricata <- read_excel("/home/exp-1/Desktop/data/Count values.xlsx", sheet = "Dataset_2_Suricata", col_names = TRUE)
ds3_suricata <- read_excel("/home/exp-1/Desktop/data/Count values.xlsx", sheet = "Dataset_3_Suricata", col_names = TRUE)
div_ds2 <- read_excel("/home/exp-1/Desktop/data/Count values.xlsx", sheet = "Diversity_Dataset_2", col_names= TRUE)
div_ds3 <- read_excel("/home/exp-1/Desktop/data/Count values.xlsx", sheet= "Diversity_Dataset_3", col_names= TRUE)
#val$Years <- factor(val$Year)


sen <- ggplot(data = ds2_snort, aes( x = factor(Year), y= Sensitivity_TPR, group = 1)) +
  geom_point(color = 'red', size = 2) + 
  geom_line(color='purple', size = 1) + 
  geom_vline(linetype="dotdash", xintercept = "2017-07-06") + 
  #annotate("text", x="2017-07-03", y=0.83, label="PCAP capture date", angle=90, size = 2)+
  theme_bw() +
  labs(title = "Sensitivity over time, Snort, Dataset 1", x="", y="Sensitivity (TPR)", size=14) +
  theme( plot.title = element_text(hjust=0.5), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text = element_text(size=14, face="bold"), axis.title = element_text(size=12, face="bold"), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) 

z=factor(ds2_snort$Year)


spec <- ggplot(data = ds2_snort, aes( x = factor(Year), y= Specificity_TNR, group = 1)) +
  geom_point(color = 'red', size = 2) + 
  geom_line(color='purple', size = 1) +
  geom_vline(linetype="dotdash", xintercept = "2017-07-06") + 
  annotate("text", x="2018-03-27", y=0.927, label="PCAP capture date", size = 4)+
  theme_bw() +
  scale_x_discrete( breaks= z[seq(1,length(z),by=2)])+
  labs(title = "Specificity over time, Snort, Dataset 1", x="Dates corresponding to rule files", y="Specificity (TNR)", face="bold") +
  theme(plot.title = element_text(hjust=0.5), axis.text.x = element_text(angle =90, hjust = 0), axis.text = element_text(size=14, face="bold"), axis.title = element_text(size=12, face="bold"), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) 
  

sen / spec
z<- factor(ds2_snort$Year)

plt<- ggplot(data=ds2_snort, aes(x=factor(Year))) +
  geom_line(aes(x=factor(Year), y=Sensitivity_TPR), size=1.5, color="darkorchid3", group=1) +
  geom_point(aes(x=factor(Year), y=Sensitivity_TPR), color='darkorchid4', size=2)+
  geom_line(aes(x=factor(Year), y=Specificity_TNR/1.2), size=1.5, color="darkgoldenrod2", group=1)+
  geom_point(aes(x=factor(Year), y=Specificity_TNR/1.2), color='darkgoldenrod3', size=2)+
  labs(x='Rule files collection dates')+
  geom_vline(linetype="dotdash", xintercept = "2017-07-06") + 
  annotate("text", x="2018-03-27", y=0.83, label="PCAP capture date", size = 4, fontface="bold")+
  scale_y_continuous( name = "Sensitivity (TPR)", sec.axis = sec_axis(trans=~.*1.2, name ="Specificity (TNR)", breaks= scales::pretty_breaks(n=5) ) )+
  coord_cartesian(ylim = c(0.70, 0.90)) +
  ggtitle("Sensitivity and Specificity over Time, Snort, Dataset 1")+
  scale_x_discrete( breaks= z[seq(1,length(z),by=2)])+
  theme_bw() + theme(plot.title=element_text(hjust=0.5, face="bold"), axis.text.x = element_text(angle =90, hjust = 0), axis.text = element_text(size=14, face="bold"), axis.title.y= element_text(color="darkorchid3", size=15, face="bold"), axis.title.y.right = element_text(color="darkgoldenrod3", size=15, face="bold"), axis.title.x = element_text( size=15, face="bold"), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  geom_label(aes(x="2018-04-03", y=0.74), label="-5.93%", color="darkorchid4", fontface="bold", nudge_x=1, size=5)+
  geom_label(aes(x="2019-05-14", y=0.735), label="-0.64%", color="darkorchid4", fontface="bold", size=5)+
  geom_label(aes(x="2020-01-16", y=0.84), label="+19.67%", color="darkorchid4", nudge_x=2.5, fontface="bold", size=5)+
  geom_label(aes(x="2017-07-25", y=0.80), label="+0.27%", color="darkgoldenrod4", fontface="bold", size=5)+
  geom_label(aes(x="2019-06-20", y=0.77), label="-1.65%", color="darkgoldenrod4", fontface="bold", size=5, nudge_x=-0.5)

plt


ggsave(filename = "/home/exp-1/Desktop/data/plots/final/ds2_snort_sensitivity_specificity.png", device="png",  width=22, height=12.5, units="cm")

myLoc <- ((which(levels(factor(ds3_snort$Year)) == "2017-07-25") ) + ( which(levels(factor(ds3_snort$Year)) == "2018-03-23"))) / 2

sen1 <- ggplot(data = ds3_snort, aes( x = factor(Year), y= Sensitivity_TPR, group = 1)) +
  geom_point(color = 'red', size = 2) + 
  geom_line(color='purple', size = 1) + 
  geom_vline(linetype="dotdash", xintercept = myLoc) + 
  #annotate("text", x="2017-07-03", y=0.83, label="PCAP capture date", angle=90, size = 2)+
  theme_bw() +
  labs(title = "Sensitivity over time, Snort, Dataset 2", x="", y="Sensitivity (TPR)") +
  theme(plot.title = element_text(hjust=0.5), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text = element_text(size=14, face="bold"), axis.title = element_text(size=12, face="bold"), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

z=factor(ds3_snort$Year)

spec1 <- ggplot(data = ds3_snort, aes( x = factor(Year), y= Specificity_TNR, group = 1)) +
  geom_point(color = 'red', size = 2) + 
  geom_line(color='purple', size = 1) +
  geom_vline(linetype="dotdash", xintercept = myLoc) + 
  annotate("text", x="2018-04-10", y=0.86357, label="PCAP capture date", size = 4)+
  theme_bw() +
  scale_x_discrete( breaks= z[seq(1,length(z),by=2)])+
  labs(title = "Specificity over time, Snort, Dataset 2", x="Dates corresponding to rule files", y="Specificity (TNR)") +
  theme(plot.title = element_text(hjust=0.5), axis.text.x = element_text(angle =90, hjust = 1), axis.text = element_text(size=14, face="bold"), axis.title = element_text(size=12, face="bold"), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) 
sen1 / spec1


plt1<- ggplot(data=ds3_snort, aes(x=factor(Year))) +
  geom_line(aes(x=factor(Year), y=Sensitivity_TPR), size=1.5, color="darkorchid3", group=1) +
  geom_point(aes(x=factor(Year), y=Sensitivity_TPR), color='darkorchid4', size=2)+
  geom_line(aes(x=factor(Year), y=Specificity_TNR/1.137), size=1.5, color="darkgoldenrod2", group=1)+
  geom_point(aes(x=factor(Year), y=Specificity_TNR/1.137), color='darkgoldenrod3', size=2)+
  labs(x='Rule files collection dates')+
  geom_vline(linetype="dotdash", xintercept = "2018-04-10") +  
  annotate("text", x="2018-06-14", y=0.77, label="PCAP capture date", size = 4, fontface="bold")+
  scale_y_continuous( name = "Sensitivity (TPR)", sec.axis = sec_axis(trans=~.*1.137, name ="Specificity (TNR)") )+
  coord_cartesian(ylim = c(0.74, 0.78)) +
  ggtitle("Sensitivity and Specificity over Time, Snort, Dataset 2")+
  scale_x_discrete( breaks= z[seq(1,length(z),by=2)])+
  theme_bw() + theme(plot.title=element_text(hjust=0.5, face="bold"), axis.text.x = element_text(angle =90, hjust = 0), axis.text = element_text(size=14, face="bold"), axis.title.y= element_text(color="darkorchid3", size=15, face="bold"), axis.title.y.right = element_text(color="darkgoldenrod3", size=15, face="bold"), axis.title.x = element_text( size=15, face="bold"), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  geom_label(aes(x="2017-07-11", y=0.748), label="-0.91%", color="darkorchid4", fontface="bold", nudge_x=0, size=5)+
  geom_label(aes(x="2019-05-14", y=0.748), label="+1.29%", color="darkorchid4", fontface="bold", nudge_x=1, size=5)+
  geom_label(aes(x="2019-06-20", y=0.767), label="+2.21%", color="darkorchid4", fontface="bold", size=5, nudge_x=-0.5)+
  geom_label(aes(x="2017-07-25", y=0.762), label="-0.0011%", color="darkgoldenrod4", fontface="bold", size=5)+
  geom_label(aes(x="2019-05-07", y=0.762), label="-0.0010%", color="darkgoldenrod4", nudge_x=0, fontface="bold", size=5)+
  geom_label(aes(x="2020-01-16", y=0.762), label="-0.0017%", color="darkgoldenrod4", fontface="bold", size=5, nudge_x=3)
  

plt1


ggsave(filename = "/home/exp-1/Desktop/data/plots/final/ds3_snort_sensitivity_specificity.png", device="png",  width=22, height=12.5, units="cm")


myLoc2 <- ((which(levels(factor(ds2_suricata$Year)) == "2017-06-09") ) + ( which(levels(factor(ds2_suricata$Year)) == "2018-02-08"))) / 2
z<-factor(ds2_suricata$Year)

sen3 <- ggplot(data = ds2_suricata, aes( x = factor(Year), y= Sensitivity_TPR, group = 1)) +
  geom_point(color = 'red', size = 2) + 
  geom_line(color='purple', size = 1) + 
  geom_vline(linetype="dotdash", xintercept = myLoc2) + 
  #annotate("text", x="2017-07-03", y=0.83, label="PCAP capture date", angle=90, size = 2)+
  theme_bw() +
  labs(title = "Sensitivity over time, Suricata, Dataset 1", x="", y="Sensitivity (TPR)") +
  theme(plot.title = element_text(hjust=0.5), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text = element_text(size=14, face="bold"), axis.title = element_text(size=14, face="bold"), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) 


spec3 <- ggplot(data = ds2_suricata, aes( x = factor(Year), y= Specificity_TNR, group = 1)) +
  geom_point(color = 'red', size = 2) + 
  geom_line(color='purple', size = 1) +
  geom_vline(linetype="dotdash", xintercept = myLoc2) + 
  annotate("text", x="2018-02-20", y=0.9488, label="PCAP capture date",  size = 4)+
  theme_bw() +
  scale_x_discrete( breaks= z[seq(1,length(z),by=2)])+
  labs(title = "Specificity over time, Suricata, Dataset 1", x="Dates corresponding to rule files", y="Specificity (TNR)") +
  theme(plot.title=element_text(hjust=0.5), axis.text.x = element_text(angle =90, hjust = 1, face="bold"), axis.text = element_text(size=14, face="bold"), axis.title = element_text(size=14, face="bold"), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))  

sen3 / spec3


plt2<- ggplot(data=ds2_suricata, aes(x=factor(Year))) +
  geom_line(aes(x=factor(Year), y=Sensitivity_TPR), size=1.5, color="darkorchid3", group=1) +
  geom_point(aes(x=factor(Year), y=Sensitivity_TPR), color='darkorchid4', size=2)+
  geom_line(aes(x=factor(Year), y=Specificity_TNR/1.097), size=1.5, color="darkgoldenrod2", group=1)+
  geom_point(aes(x=factor(Year), y=Specificity_TNR/1.097), color='darkgoldenrod3', size=2)+
  labs(x='Rule files collection dates')+
  geom_vline(linetype="dotdash", xintercept = "2017-06-09") +  
  annotate("text", x="2018-02-20", y=0.872, label="PCAP capture date", size = 4, fontface="bold")+
  scale_y_continuous( name = "Sensitivity (TPR)", sec.axis = sec_axis(trans=~.*1.097, name ="Specificity (TNR)") )+
  coord_cartesian(ylim = c(0.855, 0.875)) +
  ggtitle("Sensitivity and Specificity over Time, Suricata, Dataset 1")+
  scale_x_discrete( breaks= z[seq(1,length(z),by=2)])+
  theme_bw() + theme(plot.title=element_text(hjust=0.5, face="bold"), axis.text.x = element_text(angle =90, hjust = 0), axis.text = element_text(size=14, face="bold"), axis.title.y= element_text(color="darkorchid3", size=15, face="bold"), axis.title.y.right = element_text(color="darkgoldenrod3", size=15, face="bold"), axis.title.x = element_text( size=15, face="bold"), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  geom_label(aes(x="2018-02-16", y=0.862), label="+0.50%", color="darkorchid4", fontface="bold", nudge_x=0, size=5)+
  geom_label(aes(x="2018-03-12", y=0.8625), label="+0.23%", color="darkorchid4", fontface="bold", nudge_x=1, size=5)+
  geom_label(aes(x="2020-01-22", y=0.8675), label="+0.69%", color="darkorchid4", fontface="bold", size=5, nudge_x=1)+
  geom_label(aes(x="2018-02-16", y=0.867), label="-0.016%", color="darkgoldenrod3", fontface="bold", size=5)+
  geom_label(aes(x="2018-03-12", y=0.867), label="-0.046%", color="darkgoldenrod3", nudge_x=0, fontface="bold", size=5)+
  geom_label(aes(x="2020-01-22", y=0.863), label="-0.058%", color="darkgoldenrod3", fontface="bold", size=5, nudge_x=0)+
  geom_segment(aes(x="2017-05-03", y= 0.859, xend="2020-02-20", yend=0.872),  linetype = "dotdash", color="#E64B35B2")


plt2


ggsave(filename = "/home/exp-1/Desktop/data/plots/final/ds2_suricata_sensitivity_specificity.png", device="png", width=22, height=12.5, units="cm")


z<-factor(ds3_suricata$Year)
sen4 <- ggplot(data = ds3_suricata, aes( x = factor(Year), y= Sensitivity_TPR, group = 1)) +
  geom_point(color = 'red', size = 2) + 
  geom_line(color='purple', size = 1) + 
  geom_vline(linetype="dotdash", xintercept = "2018-02-22") + 
  #annotate("text", x="2017-07-03", y=0.83, label="PCAP capture date", angle=90, size = 2)+
  theme_bw() +
  labs(title = "Sensitivity over time, Suricata, Dataset 2", x="", y="Sensitivity (TPR)") +
  theme(plot.title=element_text(hjust=0.5), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text = element_text(size=14, face="bold"), axis.title = element_text(size=14, face="bold"), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) 


spec4 <- ggplot(data = ds3_suricata, aes( x = factor(Year), y= Specificity_TNR, group = 1)) +
  geom_point(color = 'red', size = 2) + 
  geom_line(color='purple', size = 1) +
  geom_vline(linetype="dotdash", xintercept = "2018-02-22") + 
  annotate("text", x="2018-03-08", y=0.54873, label="PCAP capture date",  size = 4)+
  theme_bw() +
  scale_x_discrete( breaks= z[seq(1,length(z),by=2)])+
  labs(title = "Specificity over time, Suricata, Dataset 2", x="Dates corresponding to rule files", y="Specificity (TNR)") +
  theme(plot.title=element_text(hjust=0.5), axis.text.x = element_text(angle =90, hjust = 1, face="bold"), axis.text = element_text(size=14, face="bold"), axis.title = element_text(size=14, face="bold"), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) 

sen4 / spec4




plt3<- ggplot(data=ds3_suricata, aes(x=factor(Year))) +
  geom_line(aes(x=factor(Year), y=Sensitivity_TPR), size=1.5, color="darkorchid3", group=1) +
  geom_point(aes(x=factor(Year), y=Sensitivity_TPR), color='darkorchid4', size=2)+
  geom_line(aes(x=factor(Year), y=Specificity_TNR/0.78), size=1.5, color="darkgoldenrod2", group=1)+
  geom_point(aes(x=factor(Year), y=Specificity_TNR/0.78), color='darkgoldenrod3', size=2)+
  labs(x='Rule files collection dates')+
  geom_vline(linetype="dotdash", xintercept ="2018-02-22") +  
  annotate("text", x="2017-06-07", y=0.725, label="PCAP capture date", size = 4, fontface="bold")+
  scale_y_continuous( name = "Sensitivity (TPR)", sec.axis = sec_axis(trans=~.*0.78, name ="Specificity (TNR)") )+
  coord_cartesian(ylim = c(0.69, 0.73)) +
  ggtitle("Sensitivity and Specificity over Time, Suricata, Dataset 2")+
  scale_x_discrete( breaks= z[seq(1,length(z),by=2)])+
  theme_bw() + theme(plot.title=element_text(hjust=0.5, face="bold"), axis.text.x = element_text(angle =90, hjust = 0), axis.text = element_text(size=14, face="bold"), axis.title.y= element_text(color="darkorchid3", size=15, face="bold"), axis.title.y.right = element_text(color="darkgoldenrod3", size=15, face="bold"), axis.title.x = element_text( size=15, face="bold"), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  geom_label(aes(x="2018-02-14", y=0.695), label="+1.89%", color="darkorchid4", fontface="bold", nudge_x=1, size=5)+
  geom_label(aes(x="2018-03-12", y=0.713), label="+0.29%", color="darkorchid4", fontface="bold", nudge_x=1, size=5)+
  geom_label(aes(x="2020-01-10", y=0.7265), label="+2.62%", color="darkorchid4", fontface="bold", size=5, nudge_x=0)+
  geom_label(aes(x="2018-02-09", y=0.709), label="-0.0079%", color="darkgoldenrod3", fontface="bold", size=5)+
  geom_label(aes(x="2018-03-12", y=0.70), label="-0.0011%", color="darkgoldenrod3", nudge_x=0, fontface="bold", size=5)+
  geom_label(aes(x="2020-01-22", y=0.70), label="-0.016%", color="darkgoldenrod3", fontface="bold", size=5, nudge_x=0)+
  geom_segment(aes(x="2017-05-03", y= 0.691, xend="2020-02-20", yend=0.725),  linetype = "dotdash", color="#E64B35B2")

plt3

ggsave(filename = "/home/exp-1/Desktop/data/plots/final/ds3_suricata_sensitivity_specificity.png", device="png", width=22, height=12.5, units="cm")

z<-factor(div_ds2$Year, levels=div_ds2$Year)
sen5 <- ggplot(data = div_ds2, aes( x = factor(Year), y= TPR_Sensitivity...17, group = 1)) +
  geom_point(color = 'red', size = 2) + 
  geom_line(color='purple', size = 1) + 
  geom_vline(linetype="dotdash", xintercept = "2017-r") + 
  #annotate("text", x="2017-q", y=0.929, label="PCAP capture date", angle=90, size = 3)+
  theme_bw() +
  labs( title = "Sensitivity over time, 1 of 2, Dataset 1", x="", y="Sensitivity (TPR)") +
  theme(plot.title = element_text(hjust=0.5), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text = element_text(size=14, face="bold"), axis.title = element_text(size=14, face="bold"), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) 


spec5 <- ggplot(data = div_ds2, aes( x = factor(Year), y=TNR_Specificity...18, group = 1)) +
  geom_point(color = 'red', size = 2) + 
  geom_line(color='purple', size = 1) +
  geom_vline(linetype="dotdash", xintercept = "2017-r") + 
  annotate("text", x="2018-e", y=0.875, label="PCAP capture date", size = 4)+
  theme_bw() +
  scale_x_discrete( breaks= z[seq(1,length(z),by=2)])+
  labs(title = "Specificity over time, 1 of 2, Dataset 1", x="Dates points", y="Specificity (TNR)") +
  theme(plot.title = element_text(hjust=0.5), axis.text.x = element_text(angle =90, hjust = 1), axis.text = element_text(size=14, face="bold"), axis.title = element_text(size=14, face="bold"), panel.border=element_blank(),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) 

sen5 / spec5


plt3<- ggplot(data=div_ds2, aes(x=factor(Year, levels=Year))) +
  geom_line(aes(x=factor(Year, levels=Year), y=TPR_Sensitivity...17), size=1.5, color="darkorchid3", group=1) +
  geom_point(aes(x=factor(Year, levels=Year), y=TPR_Sensitivity...17), color='darkorchid4', size=2)+
  geom_line(aes(x=factor(Year, levels=Year), y=TNR_Specificity...18/0.96), size=1.5, color="darkgoldenrod2", group=1)+
  geom_point(aes(x=factor(Year, levels=Year), y=TNR_Specificity...18/0.96), color='darkgoldenrod3', size=2)+
  labs(x='')+
  geom_vline(linetype="dotdash", xintercept ="2017_17") +  
  annotate("text", x="2018_5", y=0.80, label="PCAP capture date", size = 4, fontface="bold")+
  scale_y_continuous( name = "Sensitivity (TPR)", sec.axis = sec_axis(trans=~.*0.96, name ="Specificity (TNR)") )+
  coord_cartesian(ylim = c(0.80, 1)) +
  ggtitle("Sensitivity and Specificity over Time, 1 out of 2, Dataset 1")+
  scale_x_discrete( breaks= z[seq(1,length(z),by=2)])+
  theme_bw() + theme(plot.title=element_text(hjust=0.5, face="bold"), axis.text.x = element_text(angle =90, hjust = 0),  axis.text = element_text(size=14, face="bold"), axis.title.y= element_text(color="darkorchid3", size=15, face="bold"), axis.title.y.right = element_text(color="darkgoldenrod3", size=15, face="bold"), axis.title.x = element_text( size=15, face="bold"), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  geom_label(aes(x="2018_1", y=0.85), label="-3.53%", color="darkorchid4", fontface="bold", nudge_x=2, size=5)+
  geom_label(aes(x="2018_19", y=0.85), label="-0.10%", color="darkorchid4", fontface="bold", nudge_x=1, size=5)+
  geom_label(aes(x="2020_2", y=0.99), label="+12.11%", color="darkorchid4", fontface="bold", size=5, nudge_x=1.5)+
  geom_label(aes(x="2018_1", y=0.935), label="-0.037%", color="darkgoldenrod3", fontface="bold", size=5)+
  #geom_label(aes(x="2018-03-12", y=0.70), label="-0.014%", color="brown4", nudge_x=0, fontface="bold", size=5)+
  geom_label(aes(x="2019_15", y=0.935), label="-1.89%", color="darkgoldenrod3", fontface="bold", size=5, nudge_x=0)

plt3
ggsave(filename = "/home/exp-1/Desktop/data/plots/final/ds1_1_2_sen_spec.png", device="png", width=22, height=12.5, units="cm")


plt4<- ggplot(data=div_ds2, aes(x=factor(Year, level=Year))) +
  geom_line(aes(x=factor(Year, level=Year), y=TPR_Sensitivity...8), size=1.5, color="darkorchid3", group=1) +
  geom_point(aes(x=factor(Year, level=Year), y=TPR_Sensitivity...8), color='darkorchid4', size=2)+
  geom_line(aes(x=factor(Year, level=Year), y=TNR_Specificity...9/1.7), size=1.5, color="darkgoldenrod2", group=1)+
  geom_point(aes(x=factor(Year, level=Year), y=TNR_Specificity...9/1.7), color='darkgoldenrod3', size=2)+
  labs(x='Rule files collection dates')+
  geom_vline(linetype="dotdash", xintercept ="2017_17") +  
  annotate("text", x="2018_3", y=0.8, label="PCAP capture date", size = 4, fontface="bold")+
  scale_y_continuous( name = "Sensitivity (TPR)", sec.axis = sec_axis(trans=~.*1.7, name ="Specificity (TNR)") )+
  coord_cartesian(ylim = c(0.2, 0.8)) +
  ggtitle("Sensitivity and Specificity over Time, 2 out of 2, Dataset 1")+
  scale_x_discrete( breaks= z[seq(1,length(z),by=2)])+
  theme_bw() + theme(plot.title=element_text(hjust=0.5, face="bold"), axis.text.x = element_text(angle =90, hjust = 0), axis.text = element_text(size=14, face="bold"), axis.title.y= element_text(color="darkorchid3", size=15, face="bold"), axis.title.y.right = element_text(color="darkgoldenrod3", size=15, face="bold"), axis.title.x = element_text( size=15, face="bold"), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  geom_label(aes(x="2018_1", y=0.29), label="+23.83%", color="darkorchid4", fontface="bold", nudge_x=1, size=5)+
  geom_label(aes(x="2018_19", y=0.4), label="+12.83", color="darkorchid4", fontface="bold", nudge_x=1, size=5)+
  geom_label(aes(x="2020_2", y=0.72), label="+36.19%", color="darkorchid4", fontface="bold", size=5, nudge_x=2)+
  geom_label(aes(x="2018_1", y=0.625), label="-0.77%", color="darkgoldenrod4", fontface="bold", size=5)+
  geom_label(aes(x="2019_1", y=0.625), label="-0.51%", color="darkgoldenrod4", nudge_x=0, fontface="bold", size=5)+
  geom_label(aes(x="2019_15", y=0.625), label="-0.29%", color="darkgoldenrod4", fontface="bold", size=5, nudge_x=0)


 plt4

 ggsave(filename = "/home/exp-1/Desktop/data/plots/final/ds1_2_2_sen_spec.png", device="png", width=22, height=12.5, units="cm")
 


ggsave(filename = "/home/exp-1/Desktop/data/plots/div_ds2_sen_spec.png", device="png", dpi=1200, width=20, height=15, units="cm")

z<-factor(div_ds2$Year, level=div_ds2$Year)
sen6 <- ggplot(data = div_ds2, aes( x = factor(Year), y= TPR_Sensitivity...8, group = 1)) +
  geom_point(color = 'red', size = 2) + 
  geom_line(color='purple', size = 1) + 
  geom_vline(linetype="dotdash", xintercept = "2017-r") + 
  #annotate("text", x="2017-q", y=0.5, label="PCAP capture date", angle=90, size = 3)+
  theme_bw() +
  labs(title = "Sensitivity over time, 2 of 2, Dataset 1", x="", y="Sensitivity (TPR)") +
  theme(plot.title=element_text(hjust=0.5), axis.text.x=element_blank(), axis.ticks.x=element_blank(),  axis.text = element_text(size=14, face="bold"), axis.title = element_text(size=14, face="bold"), panel.border=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) 


spec6 <- ggplot(data = div_ds2, aes( x = factor(Year), y=TNR_Specificity...9, group = 1)) +
  geom_point(color = 'red', size = 2) + 
  geom_line(color='purple', size = 1) +
  geom_vline(linetype="dotdash", xintercept = "2017-r") + 
  annotate("text", x="2017-k", y=0.9600, label="PCAP capture date", size = 4)+
  theme_bw() +
  scale_x_discrete( breaks= z[seq(1,length(z),by=2)])+
  labs(title = "Specificity over time, 2 of 2, Dataset 1", x="Dates points", y="Specificity (TNR)") +
  theme( axis.text.x = element_text(angle =90, hjust = 1), axis.text = element_text(size=14, face="bold"),plot.title=element_text(hjust=0.5), axis.title = element_text(size=14, face="bold"), panel.border=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) 

sen6 / spec6

ggsave(filename = "/home/exp-1/Desktop/data/plots/div_ds2_2_2_sen_spec.png", device="png", dpi=1200, width=20, height=15, units="cm")

z<-factor(div_ds3$Year)
sen7 <- ggplot(data = div_ds3, aes( x = factor(Year), y= TPR_1_2, group = 1)) +
  geom_point(color = 'red', size = 2) + 
  geom_line(color='purple', size = 1) + 
  geom_vline(linetype="dotdash", xintercept = "2018-c") + 
  #annotate("text", x="2018-a", y=0.935, label="PCAP capture date", angle=90, size = 3)+
  theme_bw() +
  labs(title = "Sensitivity over time, 1 of 2, Dataset 2", x="", y="Sensitivity (TPR)") +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text=element_text(size=14, face="bold"), plot.title=element_text(hjust=0.5), axis.title = element_text(size=14, face="bold"), panel.border=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) 


spec7 <- ggplot(data = div_ds3, aes( x = factor(Year), y=TNR_1_2, group = 1)) +
  geom_point(color = 'red', size = 2) + 
  geom_line(color='purple', size = 1) +
  geom_vline(linetype="dotdash", xintercept = "2018-c") + 
  annotate("text", x="2018-j", y=0.4435, label="PCAP capture date", size = 4)+
  theme_bw() +
  scale_x_discrete( breaks= z[seq(1,length(z),by=2)])+
  labs(title = "Specificity over time, 1 of 2, Dataset 2", x="Date points", y="Specificity (TNR)") +
  theme(axis.text.x = element_text(angle =90, hjust = 1),  axis.text=element_text(size=14, face="bold"), plot.title=element_text(hjust=0.5), axis.title = element_text(size=14, face="bold"), panel.border=element_blank(),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) 

sen7 / spec7


plt5<- ggplot(data=div_ds3, aes(x=factor(Year, levels = Year))) +
  geom_line(aes(x=factor(Year, levels = Year), y=TPR_1_2), size=1.5, color="darkorchid3", group=1) +
  geom_point(aes(x=factor(Year, levels = Year), y=TPR_1_2), color='darkorchid4', size=2)+
  geom_line(aes(x=factor(Year, levels = Year), y=TNR_1_2/0.4735), size=1.5, color="darkgoldenrod2", group=1)+
  geom_point(aes(x=factor(Year, levels = Year), y=TNR_1_2/0.4735), color='darkgoldenrod3', size=2)+
  labs(x='')+
  geom_vline(linetype="dotdash", xintercept ="2018_3") +  
  annotate("text", x="2018_5", y=0.930, label="PCAP capture date", size = 4, fontface="bold")+
  scale_y_continuous( name = "Sensitivity (TPR)", sec.axis = sec_axis(trans=~.*0.4735, name ="Specificity (TNR)") )+
  coord_cartesian(ylim = c(0.930, 0.945)) +
  ggtitle("Sensitivity and Specificity over Time, 1 out of 2, Dataset 2")+
  scale_x_discrete( breaks= z[seq(1,length(z),by=2)])+
  theme_bw() + theme(plot.title=element_text(hjust=0.5, face="bold"), axis.text.x =  element_text(angle =90, hjust = 0),  axis.text = element_text(size=14, face="bold"), axis.title.y= element_text(color="darkorchid3", size=15, face="bold"), axis.title.y.right = element_text(color="darkgoldenrod3", size=15, face="bold"), axis.title.x = element_text( size=15, face="bold"), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  geom_label(aes(x="2018_1", y=0.942), label="+0.15%", color="darkorchid4", fontface="bold", nudge_x=0, size=5)+
  geom_label(aes(x="2019_5", y=0.940), label="-1.04%", color="darkorchid4", fontface="bold", nudge_x=0, size=5)+
  geom_label(aes(x="2019_19", y=0.940), label="+0.78%", color="darkorchid4", fontface="bold", size=5, nudge_x=1.5)+
  geom_label(aes(x="2018_1", y=0.935), label="-0.070%", color="darkgoldenrod4", fontface="bold", size=5)+
  geom_label(aes(x="2018_15", y=0.935), label="-0.028%", color="darkgoldenrod4", nudge_x=0, fontface="bold", size=5)+
  geom_label(aes(x="2020_2", y=0.9355), label="+0.033%", color="darkgoldenrod4", fontface="bold", size=5, nudge_x=3.5)

plt5

ggsave(filename = "/home/exp-1/Desktop/data/plots/final/ds3_1_2_sen_spec.png", device="png", width=22, height=12.5, units="cm")


plt6<- ggplot(data=div_ds3, aes(x=factor(Year, levels = Year))) +
  geom_line(aes(x=factor(Year, levels = Year), y=TPR_2_2), size=1.5, color="darkorchid3", group=1) +
  geom_point(aes(x=factor(Year, levels = Year), y=TPR_2_2), color='darkorchid4', size=2)+
  geom_line(aes(x=factor(Year, levels = Year), y=TNR_2_2/1.5), size=1.5, color="darkgoldenrod2", group=1)+
  geom_point(aes(x=factor(Year, levels = Year), y=TNR_2_2/1.5), color='darkgoldenrod3', size=2)+
  labs(x='Rule files collection dates')+
  geom_vline(linetype="dotdash", xintercept ="2018_3") +  
  annotate("text", x="2018_11", y=0.617, label="PCAP capture date", size = 4, fontface="bold")+
  scale_y_continuous( name = "Sensitivity (TPR)", sec.axis = sec_axis(trans=~.*1.5, name ="Specificity (TNR)") )+
  coord_cartesian(ylim = c(0.61, 0.66)) +
  ggtitle("Sensitivity and Specificity over Time, 2 out of 2, Dataset 2")+
  scale_x_discrete( breaks= z[seq(1,length(z),by=2)])+
  theme_bw() + theme(plot.title=element_text(hjust=0.5, face="bold"), axis.text.x = element_text(angle =90, hjust = 0), axis.text = element_text(size=14, face="bold"), axis.title.y= element_text(color="darkorchid3", size=15, face="bold"), axis.title.y.right = element_text(color="darkgoldenrod3", size=15, face="bold"), axis.title.x = element_text( size=15, face="bold"), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  geom_label(aes(x="2018_1", y=0.655), label="+0.755%", color="darkorchid4", fontface="bold", nudge_x=0, size=5)+
  geom_label(aes(x="2018_19", y=0.655), label="-2.99%", color="darkorchid4", fontface="bold", nudge_x=0, size=5)+
  geom_label(aes(x="2020_4", y=0.626), label="-2.01%", color="darkorchid4", fontface="bold", size=5, nudge_x=2)+
  geom_label(aes(x="2018_1", y=0.635), label="+0.028%", color="darkgoldenrod3", fontface="bold", size=5)+
  #geom_label(aes(x="2019_1", y=0.7), label="-0.51%", color="darkgoldenrod3", nudge_x=0, fontface="bold", size=5)+
  geom_label(aes(x="2020_2", y=0.645), label="-0.011%", color="darkgoldenrod3", fontface="bold", size=5, nudge_x=0)

 plt6


ggsave(filename = "/home/exp-1/Desktop/data/plots/final/ds3_2_2_sen_spec.png", device="png",  width=22, height=12.5, units="cm")


sen8 <- ggplot(data = div_ds3, aes( x = factor(Year), y= TPR_2_2, group = 1)) +
  geom_point(color = 'red', size = 2) + 
  geom_line(color='purple', size = 1) + 
  geom_vline(linetype="dotdash", xintercept = "2018-c") + 
  #annotate("text", x="2018-a", y=0.63, label="PCAP capture date", angle=90, size = 3)+
  theme_bw() +
  labs(title = "Sensitivity over time, 2 of 2, Dataset 2", x="", y="Sensitivity (TPR)") +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),  axis.text=element_text(size=14, face="bold"), plot.title=element_text(hjust=0.5), axis.title = element_text(size=14, face="bold"), panel.border=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) 


spec8 <- ggplot(data = div_ds3, aes( x = factor(Year), y=TNR_2_2, group = 1)) +
  geom_point(color = 'red', size = 2) + 
  geom_line(color='purple', size = 1) +
  geom_vline(linetype="dotdash", xintercept = "2018-c") + 
  annotate("text", x="2018-i", y=0.9595, label="PCAP capture date", size = 4)+
  theme_bw() +
  scale_x_discrete( breaks= z[seq(1,length(z),by=2)])+
  labs(title = "Specificity over time, 2 of 2, Dataset 2", x="Date points", y="Specificity (TNR)") +
  theme(axis.text.x = element_text(angle =90, hjust = 1),  axis.text=element_text(size=14, face="bold"), plot.title=element_text(hjust=0.5), axis.title = element_text(size=14, face="bold"), panel.border=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) 

sen8 / spec8
ggsave(filename = "/home/exp-1/Desktop/data/plots/div_ds3_2_2_sen_spec.png", device="png", dpi=1200, width=20, height=15, units="cm")


sen9 <- ggplot(data = div_ds3, aes( x = factor(Year), y= TPR_1_2, group = 1)) +
  geom_point(color = 'red', size = 2) + 
  geom_line(color='purple', size = 1) + 
  geom_vline(linetype="dotdash", xintercept = "2018-c") + 
  #annotate("text", x="2018-a", y=0.935, label="PCAP capture date", angle=90, size = 3)+
  theme_bw() +
  labs(title = "Sensitivity over time, 1 of 2, Dataset 2", x="", y="Sensitivity (TPR)") +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text=element_text(size=14, face="bold"), plot.title=element_text(hjust=0.5), axis.title = element_text(size=14, face="bold"), panel.border=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) 

spec9 <- ggplot(data = div_ds3, aes( x = factor(Year), y=TNR_2_2, group = 1)) +
  geom_point(color = 'red', size = 2) + 
  geom_line(color='purple', size = 1) +
  geom_vline(linetype="dotdash", xintercept = "2018-c") + 
  annotate("text", x="2018-i", y=0.9595, label="PCAP capture date", size = 4)+
  theme_bw() +
  scale_x_discrete( breaks= z[seq(1,length(z),by=2)])+
  labs(title = "Specificity over time, 2 of 2, Dataset 2", x="Date points", y="Specificity (TNR)") +
  theme(axis.text.x = element_text(angle =90, hjust = 1),  axis.text=element_text(size=14, face="bold"), plot.title=element_text(hjust=0.5), axis.title = element_text(size=14, face="bold"), panel.border=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) 

sen9 / spec9

plt8<- ggplot(data=div_ds3, aes(x=factor(Year))) +
  geom_line(aes(x=factor(Year), y=TPR_1_2), size=1.5, color="darkorchid3", group=1) +
  geom_point(aes(x=factor(Year), y=TPR_1_2), color='darkorchid4', size=2)+
  geom_line(aes(x=factor(Year), y=TNR_2_2/1.025), size=1.5, color="darkgoldenrod2", group=1)+
  geom_point(aes(x=factor(Year), y=TNR_2_2/1.025), color='darkgoldenrod3', size=2)+
  labs(x='')+
  geom_vline(linetype="dotdash", xintercept ="2018-c") +  
  annotate("text", x="2018-e", y=0.80, label="PCAP capture date", size = 4, fontface="bold")+
  scale_y_continuous( name = "Sensitivity (TPR), 1 out of 2", sec.axis = sec_axis(trans=~.*1.025, name ="Specificity (TNR), 2 out of 2") )+
  coord_cartesian(ylim = c(0.930, 0.942)) +
  ggtitle("Sensitivity over Time, 1 out of 2, and Specificity over time, 2 out of 2, Dataset 2")+
  scale_x_discrete( breaks= z[seq(1,length(z),by=2)])+
  theme_bw() + theme(plot.title=element_text(hjust=0.5, face="bold"), axis.text.x = element_text(angle =90, hjust = 0), axis.text = element_text(size=14, face="bold"), axis.title.y= element_text(color="darkorchid3", size=15, face="bold"), axis.title.y.right = element_text(color="darkgoldenrod3", size=15, face="bold"), axis.title.x = element_text( size=15, face="bold"), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  geom_label(aes(x="2018-a", y=0.9420), label="+0.15%", color="darkolivegreen", fontface="bold", nudge_x=0, size=5)+
  geom_label(aes(x="2018-s", y=0.9420), label="-1.04%", color="brown4", fontface="bold", nudge_x=0, size=5)+
  geom_label(aes(x="2019-s", y=0.9395), label="+0.78%", color="darkolivegreen", fontface="bold", size=5, nudge_x=1.5)+
  geom_label(aes(x="2018-a", y=0.9352), label="+0.028%", color="darkolivegreen", fontface="bold", size=5)+
  geom_label(aes(x="2018-o", y=0.9352), label="-0.0019%", color="brown4", nudge_x=0, fontface="bold", size=5)+
  geom_label(aes(x="2020-b", y=0.9352), label="+0.010%", color="darkolivegreen", fontface="bold", size=5, nudge_x=3.5)
plt8
ggsave(filename = "/home/exp-1/Desktop/data/plots/div_ds3.png", device="png",  width=22, height=12.5, units="cm")
