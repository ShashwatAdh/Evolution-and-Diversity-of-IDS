library(ggplot2)
library(lubridate)
library(patchwork)


csv <- read.csv('/home/exp-1/Desktop/data/timestamp_plot/ds2_label_plot.csv')
csv[csv==""]<-NA
a <- as.POSIXct(csv$label_timestamp, tz="UTC", format="%Y-%m-%d %I:%M:%S %p")
b <- csv$label
a1 <- read.csv("/home/exp-1/Desktop/data/Diversity/Diversity_ds2_2017.csv")
a1[a1==""] <- NA
b[b==0.5] <-0

c <- as.POSIXct(a1$snort_timestamp, tz="UTC")
d <- as.POSIXct(csv$suricata_timestamp, tz="UTC")

plt <- ggplot()+ geom_line(aes(x=a, y=b)) + xlab("Labels timestamps")  + 
  scale_x_datetime(breaks = scales::date_breaks("60 mins"), date_labels = "%H:%M", expand=c(0,0)) + scale_y_continuous(expand=c(0,0), breaks=1 )+
  theme_bw() + theme(axis.text.y=element_blank(), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

plt2 <- ggplot()+ geom_point(aes(x=(c-3*3600), y=1)) + xlab("Snort Timestamps") + 
  scale_x_datetime(breaks = scales::date_breaks("60 mins"), date_labels = "%H:%M") + scale_y_continuous(expand=c(0,0), breaks=1 )+
  theme_bw() + theme( axis.text.y=element_blank(), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

plt3 <- ggplot()+ geom_point(aes(x=(d-3*3600), y=1)) + xlab("Suricata Timestamps") + 
  scale_x_datetime(breaks = scales::date_breaks("60 mins"), date_labels = "%H:%M") + scale_y_continuous(expand=c(0,0), breaks=1 )+
  theme_bw() + theme(axis.text.y=element_blank(), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

plt3 / plt2 / plt 

pl4 <- ggplot() + geom_line(aes(x=a, y=b), size=0.5) + xlab("Timestamps from the Labels ") + ylab("Labels, 1=Malicious, 0=Benign") +
  scale_x_datetime(breaks = scales::date_breaks("60 mins"), date_labels = "%H:%M", expand=c(0,0)) + scale_y_continuous(expand=c(0,0), breaks=1 ) + theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
pl4

