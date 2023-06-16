
library(readxl)
library(ggrepel)



#ds2_div <- read_excel("/home/exp-1/Desktop/data/Count values.xlsx", sheet = "Diversity_Dataset_2", col_names = TRUE)
#ds3_div <- read_excel("/home/exp-1/Desktop/data/Count values.xlsx", sheet = "Diversity_Dataset_3", col_names = TRUE)


ds3_TPR_2_2       <- 0.638649322
ds3_FPR_2_2       <- 0.040432

ds3_TPR_1_2       <- 0.937587748
ds3_FPR_1_2	      <- 0.556574736	

ds3_snort_TPR   	<- 0.7524039
ds3_snort_FPR     <- 0.136420738

ds3_suricata_TPR  <- 0.706169181
ds3_suricata_FPR  <- 0.451236739
	

				

ds2_TPR_2_2	      <- 0.465418613 
ds2_FPR_2_2	      <- 0.037964

ds2_TPR_1_2	      <- 0.887847738
ds2_FPR_1_2       <- 0.11643009

ds2_TPR_snort     <- 0.757187198
ds2_FPR_snort     <- 0.065128053

ds2_TPR_suricata  <- 0.865254591
ds2_FPR_suricata  <- 0.050858161
				



roc_plt <- ggplot() + geom_point( aes( x = ds2_FPR_2_2, y= ds2_TPR_2_2), size = 4, color='red', shape = 18) + geom_text( aes( x = ds2_FPR_2_2, y= ds2_TPR_2_2), label="(2,2)", nudge_y=-0.03, color='red', size=3) +
                      geom_point( aes( x = ds2_FPR_1_2, y= ds2_TPR_1_2), size = 4, color='red', shape=18) + geom_text ( aes(x = ds2_FPR_1_2, y= ds2_TPR_1_2), label="(1,2)", nudge_y=-0.03, color="red", size=3)+
                      geom_point( aes( x = ds2_FPR_snort, y= ds2_TPR_snort), size = 4, color='red', shape=17) + geom_text ( aes(x = ds2_FPR_snort, y= ds2_TPR_snort), label="DS1_Snort", nudge_y=-0.03, color="red", size=3)+
                      geom_point( aes( x = ds2_FPR_suricata, y= ds2_TPR_suricata), size = 4, color='red', shape=15) +geom_text ( aes(x = ds2_FPR_suricata, y= ds2_TPR_suricata), label="DS1_Suricata", nudge_y=-0.03, color="red", size=3)+
                      geom_point( aes( x = ds3_FPR_2_2, y= ds3_TPR_2_2), size = 4, color='blue', shape = 18) +geom_text ( aes(x = ds3_FPR_2_2, y= ds3_TPR_2_2), label="(2,2)", nudge_y=-0.03, color="blue", size=3)+
                      geom_point( aes( x = ds3_FPR_1_2, y= ds3_TPR_1_2), size = 4, color='blue',  shape=18) +geom_text ( aes(x = ds3_FPR_1_2, y= ds3_TPR_1_2), label="(1,2)", nudge_y=-0.03, color="blue", size=3)+
                      geom_point( aes( x = ds3_snort_FPR, y= ds3_snort_TPR), size = 4,  color='blue', shape=17) +geom_text ( aes(x = ds3_snort_FPR, y= ds3_snort_TPR), label="DS2_Snort", nudge_y=-0.03, color="blue", size=3)+
                      geom_point( aes( x = ds3_suricata_FPR, y= ds3_suricata_TPR), size = 4, color='blue', shape=15) +geom_text ( aes(x = ds3_suricata_FPR, y= ds3_suricata_TPR), label="DS2_Suricata", nudge_y=-0.03, color="blue", size=3)+
                      theme_bw() +  theme( axis.text=element_text(size=16, face="bold"), plot.title=element_text(hjust=0.5), axis.title = element_text(size=16, face="bold"),  panel.border = element_blank(),   panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  scale_y_continuous("True Positive Rate", limits=c(0,1), expand=c(0,0)) + scale_x_continuous("False Postivie rate", limits=c(0,1), expand=c(0,0))

roc_plt

ggsave(filename = "/home/exp-1/Desktop/data/plots/ROC.png", device="png",  width=22, height=12.5, units="cm")


ROC <- read.csv("/home/exp-1/Desktop/data/ROC_plots/ROC.csv")

library('ggthemes')
library('ggplot2')
#library('ggh4x')
rocplot <- ggplot(data=ROC) + geom_point( aes(x=FPR, y=TPR, shape=factor(sys), color=factor(dataset), fill=factor(dataset)), size=4)+ 
  geom_label_repel(aes(x=FPR, y= TPR, label=Labels, color=factor(dataset)) )+
  scale_shape_manual(values = c(21, 22, 25, 24), labels = c("Snort","Suricata","2OO2","1OO2"), name="IDS System") +
  scale_color_manual(values =c("darkorchid4", "goldenrod4"), guide="none")+
  scale_fill_manual(values = alpha(c("darkorchid4", "goldenrod4"), 0.7),  guide="none" )+  theme_bw()+
  labs(x="False Positive Rate (FPR)", y="True Positive Rate (TPR)")+
  scale_y_continuous(limits=c(0.3,1))+
  #coord_cartesian(ylim= c(0 ,1))+
  #scale_size_manual( values=c(2,4,6,8), labels=c(2017, 2108, 2019, 2020), name="Year", guide="none")+
  theme( axis.text=element_text(size=16, face="bold"), plot.title=element_text(hjust=0.5), axis.title = element_text(size=16, face="bold"),
         panel.border = element_blank(),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
         axis.line = element_line(colour = "black"))

rocplot + facet_wrap(.~dataset, scales='free',labeller= label_value ) + theme(labeller(dataset = c("Dataset 1", "Dataset 2")))
      # + scale_x_continuous(limits=c(0, 0.6)) + scale_y_continuous(limits=c(0.3,1))

ggsave(filename = "/home/exp-1/Desktop/data/plots/ROC2.png", device="png",  width=22, height=12.5, units="cm")
