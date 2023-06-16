library(ggplot2)
library(lubridate)

a <- read.csv("/home/exp-1/Desktop/data/Diversity/Diversity_ds2_2017.csv")
b <- read.csv("/home/exp-1/Desktop/data/Diversity/Diversity_ds2_2018.csv")
c <- read.csv("/home/exp-1/Desktop/data/Diversity/Diversity_ds2_2019.csv")
d <- read.csv("/home/exp-1/Desktop/data/Diversity/Diversity_ds2_2020.csv")

a2 <- read.csv("/home/exp-1/Desktop/data/Diversity/Diversity_ds3_2017.csv")
b2 <- read.csv("/home/exp-1/Desktop/data/Diversity/Diversity_ds3_2018.csv")
c2 <- read.csv("/home/exp-1/Desktop/data/Diversity/Diversity_ds3_2019.csv")
d2 <- read.csv("/home/exp-1/Desktop/data/Diversity/Diversity_ds3_2020.csv")


a[a==""]<-NA
b[b==""]<-NA
c[c==""]<-NA
d[d==""]<-NA

a2[a2==""]<-NA
b2[b2==""]<-NA
c2[c2==""]<-NA
d2[d2==""]<-NA


p <- as.POSIXct(a$snort_timestamp, tz="GMT")
q <- as.POSIXct(a$suricata_timestamp, tz="GMT") 
r <- as.POSIXct(b$snort_timestamp, tz="GMT")
s <- as.POSIXct(b$suricata_timestamp, tz="GMT")
t <- as.POSIXct(c$snort_timestamp, tz="GMT")
u <- as.POSIXct(c$suricata_timestamp, tz="GMT")
v <- as.POSIXct(d$snort_timestamp, tz="GMT")
w <- as.POSIXct(d$suricata_timestamp, tz="GMT")


p2 <- as.POSIXct(a2$snort_timestamp, tz="GMT")
q2 <- as.POSIXct(a2$suricata_timestamp, tz="GMT") 
r2 <- as.POSIXct(b2$snort_timestamp, tz="GMT")
s2 <- as.POSIXct(b2$suricata_timestamp, tz="GMT")
t2 <- as.POSIXct(c2$snort_timestamp, tz="GMT")
u2 <- as.POSIXct(c2$suricata_timestamp, tz="GMT")
v2 <- as.POSIXct(d2$snort_timestamp, tz="GMT")
w2 <- as.POSIXct(d2$suricata_timestamp, tz="GMT")


plt <- ggplot() +
  geom_point(data=a, aes(x= (p-95), y=0)) + 
  geom_point(data=a, aes(x= (p+100), y=0.5))+
  geom_point(data=a, aes(x= p, y=2))+
  geom_point(data=a, aes(x= (p-250), y=2.5))+
  geom_point(data=a, aes(x= p, y=4))+
  geom_point(data=a, aes(x= (p+190), y=4.5))+
  geom_point(data=a, aes(x= (p+200), y=6))+
  geom_point(data=a, aes(x= p , y=6.5))
plt

plt2 <- ggplot() +
  geom_point(data=a2, aes(x=p2, y=0)) + 
  geom_point(data=a2, aes(x=(p2 + 12), y=0.5))+
  geom_point(data=a2, aes(x=p2, y=2))+
  geom_point(data=a2, aes(x=(p2 + 9), y=2.5))+
  geom_point(data=a2, aes(x=p2, y=4))+
  geom_point(data=a2, aes(x=(p2 + 7), y=4.5))+
  geom_point(data=a2, aes(x=p2, y=6))+
  geom_point(data=a2, aes(x=(p2 + 9), y=6.5))+
  xlab("")
plt2
