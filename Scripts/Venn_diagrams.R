library(ggplot2)
library(lubridate)
library(VennDiagram)
library(patchwork)
library(gridExtra)
library(grid)


#sn217 <- read.csv('/home/exp-1/Desktop/data/Alerts/Alerts_csv/snort_ds2/snort_ds2_2017.csv')
#sn218 <- read.csv('/home/exp-1/Desktop/data/Alerts/Alerts_csv/snort_ds2/snort_ds2_2018.csv')
#sn219 <- read.csv('/home/exp-1/Desktop/data/Alerts/Alerts_csv/snort_ds2/snort_ds2_2019.csv')
#sn220 <- read.csv('/home/exp-1/Desktop/data/Alerts/Alerts_csv/snort_ds2/snort_ds2_2020.csv')
#su217 <- read.csv('/home/exp-1/Desktop/data/Alerts/Alerts_csv/suricata_ds2/suricata_ds2_2017.csv')
#su218 <- read.csv('/home/exp-1/Desktop/data/Alerts/Alerts_csv/suricata_ds2/suricata_ds2_2018.csv')
#su219 <- read.csv('/home/exp-1/Desktop/data/Alerts/Alerts_csv/suricata_ds2/suricata_ds2_2019.csv')
#su220 <- read.csv('/home/exp-1/Desktop/data/Alerts/Alerts_csv/suricata_ds2/suricata_ds2_2020.csv')

#sn317 <- read.csv('/home/exp-1/Desktop/data/Alerts/Alerts_csv/snort_ds3/snort_ds3_2017.csv')
#sn318 <- read.csv('/home/exp-1/Desktop/data/Alerts/Alerts_csv/snort_ds3/snort_ds3_2018.csv')
#sn319 <- read.csv('/home/exp-1/Desktop/data/Alerts/Alerts_csv/snort_ds3/snort_ds3_2019.csv')
#sn320 <- read.csv('/home/exp-1/Desktop/data/Alerts/Alerts_csv/snort_ds3/snort_ds3_2020.csv')
#su317 <- read.csv('/home/exp-1/Desktop/data/Alerts/Alerts_csv/suricata_ds3/suricata_ds3_2017.csv')
#su318 <- read.csv('/home/exp-1/Desktop/data/Alerts/Alerts_csv/suricata_ds3/suricata_ds3_2018.csv')
#su319 <- read.csv('/home/exp-1/Desktop/data/Alerts/Alerts_csv/suricata_ds3/suricata_ds3_2019.csv')
#su320 <- read.csv('/home/exp-1/Desktop/data/Alerts/Alerts_csv/suricata_ds3/suricata_ds3_2020.csv')




vplt <-  wrap_elements(draw.pairwise.venn(
  area1= 29909, area2= 24115, cross.area = 14242,
  fill = c("blue", "red"),
  alpha = rep(0.5, 2),
  print.mode=c("percent"),
  cex=0,
  #category = c("snort timestamps", "suricata timestamps")
))

vplt2 <- wrap_elements( draw.pairwise.venn(
  area1= 29909, area2= 24115, cross.area = 22084, 
  fill = c("blue", "red"),
  alpha = rep(0.5, 2),
  cex=0,
  ext.text=TRUE,
  print.mode=c("percent"),
   # category =c("Snort Source IPs", "Suricata Source IPs")
))


vplt3 <- wrap_elements( draw.pairwise.venn(
  area1= 29909, area2= 24115, cross.area = 22084, 
  fill = c("blue", "red"),
  alpha = rep(0.5, 2),
  print.mode=c("percent"),
  cex=0,
  #category =c("Snort Destination IPs", "Suricata Destination IPs")
))


vplt4 <-  wrap_elements(draw.pairwise.venn(
  area1= 29909, area2= 24115, cross.area = 7055,
  fill = c("blue", "red"),
  alpha = rep(0.5, 2),
  print.mode=c("percent"),
  cex=0,
  #category =c("Snort Source Ports", "Suricata Source Ports")
))


vplt5 <-  wrap_elements(draw.pairwise.venn(
  area1= 29909, area2= 24115, cross.area = 19007, 
  fill = c("blue", "red"),
  alpha = rep(0.5, 2),
  print.mode=c("percent"),
  cex=0,
  #category =c("Snort Destination Ports", "Suricata Destination Ports")
))


vplt6 <- wrap_elements(draw.pairwise.venn(
  area1= 29909, area2= 24193, cross.area = 14089,
  fill = c("blue", "red"),
  alpha = rep(0.5, 2),
  print.mode=c("percent"),
  cex=0,
  #category = c("snort timestamps", "suricata timestamps")
))

vplt7 <-  wrap_elements(draw.pairwise.venn(
  area1= 29909, area2= 24193, cross.area = 18231, 
  fill = c("blue", "red"),
  alpha = rep(0.5, 2),
  print.mode=c("percent"),
  cex=0,
  #category =c("Snort Source IPs", "Suricata Source IPs")
))


vplt8 <- wrap_elements( draw.pairwise.venn(
  area1= 29909, area2= 24193, cross.area = 19812, 
  fill = c("blue", "red"),
  alpha = rep(0.5, 2),
  print.mode=c("percent"),
  cex=0,
  #category =c("Snort Destination IPs", "Suricata Destination IPs")
))

vplt9 <-  wrap_elements(draw.pairwise.venn(
  area1= 29909, area2= 24193, cross.area = 7306, 
  fill = c("blue", "red"),
  alpha = rep(0.5, 2),
  print.mode=c("percent"),
  cex=0,
  #category =c("Snort Source Ports", "Suricata Source Ports")
))


vplt10 <-  wrap_elements(draw.pairwise.venn(
  area1= 29909, area2= 24193, cross.area = 19012, 
  fill = c("blue", "red"),
  alpha = rep(0.5, 2),
  print.mode=c("percent"),
  cex=0,
  #category =c("Snort Destination Ports", "Suricata Destination Ports")
))


vplt11 <- wrap_elements( draw.pairwise.venn(
  area1= 29909, area2= 24247, cross.area = 14092,
  fill = c("blue", "red"),
  alpha = rep(0.5, 2),
  print.mode=c("percent"),
  cex=0,
  #category = c("snort timestamps", "suricata timestamps")
))

vplt12 <-  wrap_elements( draw.pairwise.venn(
  area1= 29909, area2= 24247, cross.area = 20630, 
  fill = c("blue", "red"),
  alpha = rep(0.5, 2),
  print.mode=c("percent"),
  cex=0,
  #category =c("Snort Source IPs", "Suricata Source IPs")
))


vplt13 <- wrap_elements(  draw.pairwise.venn(
  area1= 29909, area2= 24247, cross.area = 21227, 
  fill = c("blue", "red"),
  alpha = rep(0.5, 2),
  print.mode=c("percent"),
  cex=0,
  #category =c("Snort Destination IPs", "Suricata Destination IPs")
))


vplt14 <-  wrap_elements( draw.pairwise.venn(
  area1= 29909, area2= 24247, cross.area =4903, 
  fill = c("blue", "red"),
  alpha = rep(0.5, 2),
  print.mode=c("percent"),
  cex=0,
  #category =c("Snort Source Ports", "Suricata Source Ports")
))


vplt15 <-  wrap_elements( draw.pairwise.venn(
  area1= 29909, area2= 24247, cross.area = 19011, 
  fill = c("blue", "red"),
  alpha = rep(0.5, 2),
  print.mode=c("percent"),
  cex=0,
  #category =c("Snort Destination Ports", "Suricata Destination Ports")
))


vplt16 <- wrap_elements(  draw.pairwise.venn(
  area1= 37247, area2= 34811, cross.area = 14078,
  fill = c("blue", "red"),
  alpha = rep(0.5, 2),
  print.mode=c("percent"),
  cex=0,
  #category = c("snort timestamps", "suricata timestamps")
))

vplt17 <- wrap_elements( draw.pairwise.venn(
  area1= 37247, area2= 34811, cross.area = 28312, 
  fill = c("blue", "red"),
  alpha = rep(0.5, 2),
  print.mode=c("percent"),
  cex=0,
  #category =c("Snort Source IPs", "Suricata Source IPs")
))


vplt18 <-  wrap_elements( draw.pairwise.venn(
  area1= 37247, area2= 34811, cross.area = 23385, 
  fill = c("blue", "red"),
  alpha = rep(0.5, 2),
  print.mode=c("percent"),
  cex=0,
  #category =c("Snort Destination IPs", "Suricata Destination IPs")
))


vplt19 <- wrap_elements( draw.pairwise.venn(
  area1= 37247, area2= 34811, cross.area = 4532, 
  fill = c("blue", "red"),
  alpha = rep(0.5, 2),
  print.mode=c("percent"),
  cex=0,
  #category =c("Snort Source Ports", "Suricata Source Ports")
))


vplt20 <- wrap_elements( draw.pairwise.venn(
  area1= 37247, area2= 34811, cross.area = 26302, 
  fill = c("blue", "red"),
  alpha = rep(0.5, 2),
  print.mode=c("percent"),
  cex=0,
  #category =c("Snort Destination Ports", "Suricata Destination Ports")
))


cols <- c("cornflowerblue","coral2")
lg <- legendGrob(labels=c("Snort","Suricata"), pch=rep(19,length(c("Snort","Suricata"))),
                 gp=gpar(col=cols, fill="gray", fontsize=15, fontface="bold"),
                ncol=2)

blob <- (vplt / vplt2 / vplt3 / vplt4 / vplt5) | (vplt6 / vplt7 / vplt8 / vplt9 / vplt10) | (vplt11 / vplt12 / vplt13 / vplt14 / vplt15) | (vplt16 / vplt17 / vplt18 / vplt19 / vplt20) + theme_dark()


yaxis <- ggplot () + geom_text(aes(x=1, y=1, label="Timestamps  \n\n Source \n IPs  \n\n  Destination \n IPs  \n\n  Source \n Ports  \n\n  Destination \n Ports"), size=6, fontface="bold") + 
  theme_void() + coord_cartesian(clip="off")

xaxis <- ggplot() + geom_text(aes(x=1, y=1, label="2017 \t\t 2018 \t\t\t 2019 \t\t\t  2020   \n  Years"), size=6, fontface="bold") + theme_void() + coord_cartesian(clip="off")

(yaxis + blob ) + plot_spacer() + xaxis + plot_spacer() +lg +  plot_layout(nrow = 3, ncol =2 , widths = c(2,8), heights = c(10, 1,1))

grid.draw(linesGrob( x= unit(c(0.200, 0.99  ), "npc"), y=unit(c(0.222, 0.222), "npc")))

grid.draw(linesGrob( x= unit(c(0.200, 0.200 ), "npc"), y=unit(c(0.222, 0.98 ), "npc")))



 