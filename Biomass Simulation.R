rm(list=ls())#remove all types of data remained in R
#initial biomass setting (only two level - H~ & L~)
LN0<-20000
HN0<-80500
#Simulated biomass construction
t<-c(1:51)
B<-matrix(ncol=6, nrow=51)
B[1,1]<-HN0
B[1,2]<-HN0
B[1,3]<-HN0
B[1,4]<-LN0
B[1,5]<-LN0
B[1,6]<-LN0
for(i in 1:50){
  if(i < 30){
    B[i+1,1] <- ((5.01239*i)^2+256.5412*i+LN0-50307.88)*(-1)+50307.88
    B[i+1,2] <- ((5.01239*i)^2+256.5412*i+LN0-50307.88)*(-1)+50307.88
    B[i+1,3] <- ((5.01239*i)^2+256.5412*i+LN0-50307.88)*(-1)+50307.88
    B[i+1,4] <- (5.01239*i)^2+256.5412*i+LN0
    B[i+1,5] <- (5.01239*i)^2+256.5412*i+LN0
    B[i+1,6] <- (5.01239*i)^2+256.5412*i+LN0
  } else {
    B[i+1,1] <- (-0.89*i)^3+(100*i)^(2.8/2)+1200*i-40443.24
    B[i+1,2] <- 50307.88
    B[i+1,3] <- ((-0.89*i)^3+(100*i)^(2.8/2)+1200*i-40443.24-50307.88)*(-1)+50307.88
    B[i+1,4] <- (-0.89*i)^3+(100*i)^(2.8/2)+1200*i-40443.24
    B[i+1,5] <- 50307.88
    B[i+1,6] <- ((-0.89*i)^3+(100*i)^(2.8/2)+1200*i-40443.24-50307.88)*(-1)+50307.88
}
}
#fill the BHDPF model using the YBS anchovy stock parameters
#then calculate the corresponding catch depends on simulated biomass
#BHDPF
K<-100000
M=0.9
h<-0.66
b<-(h-0.2)/(0.2*K-0.2*h*K)
a<-4*M*h/(1-h)
C<-matrix(ncol=6, nrow=50)
for(j in 1:6){for(i in 1:50){
  C[i,j] <- a*B[i, j]/(1+b*B[i,j])+(1-M)*B[i,j]-B[i+1, j]
}
}
#Schaefer model
r<-1.18
CSF<-matrix(ncol=6, nrow=50)
for(j in 1:6){for(i in 1:50){
  CSF[i,j] <- r*(1-B[i,j]/K)*B[i,j]+B[i,j]-B[i+1, j]
}
}
#visual presentation
simB<-as.data.frame(matrix(ncol=1, nrow=408))
simB[,"value"] <- as.numeric(c(B[1:30,1], rep("NA", 50), B[30:51,1], rep("NA", 29), B[30:51,2], rep("NA", 29), B[30:51,3],B[1:30,4], rep("NA", 50), B[30:51,4], rep("NA", 29), B[30:51,5], rep("NA", 29), B[30:51,6]))
simB[,"Year"] <- as.numeric(c(rep(c(0:50), 8)))
simB[, "type"] <- c(rep("Hi", 51), rep("HH", 51), rep("HM", 51), rep("HL", 51),rep("Li", 51), rep("LH", 51), rep("LM", 51), rep("LL", 51)) 

CC<-as.data.frame(matrix(ncol=1, nrow=400))
CC[,"value"] <- as.numeric(c(C[1:29,1], rep("NA", 49), C[29:50,1], rep("NA", 28), C[29:50,2], rep("NA", 28), C[29:50,3],C[1:29,4], rep("NA", 49), C[29:50,4], rep("NA", 28), C[29:50,5], rep("NA", 28), C[29:50,6]))
CC[,"Year"] <- as.numeric(c(rep(c(1:50), 8)))
CC[, "type"] <- c(rep("Hi", 50), rep("HH", 50), rep("HM", 50), rep("HL", 50),rep("Li", 50), rep("LH", 50), rep("LM", 50), rep("LL", 50)) 

CCSF<-as.data.frame(matrix(ncol=1, nrow=400))
CCSF[,"value"] <- as.numeric(c(CSF[1:29,1], rep("NA", 49), CSF[29:50,1], rep("NA", 28), CSF[29:50,2], rep("NA", 28), CSF[29:50,3],CSF[1:29,4], rep("NA", 49), CSF[29:50,4], rep("NA", 28), CSF[29:50,5], rep("NA", 28), CSF[29:50,6]))
CCSF[,"Year"] <- as.numeric(c(rep(c(1:50), 8)))
CCSF[, "type"] <- c(rep("Hi", 50), rep("HH", 50), rep("HM", 50), rep("HL", 50),rep("Li", 50), rep("LH", 50), rep("LM", 50), rep("LL", 50)) 
library(ggplot2)
#install.packages("ggpubr")
library(ggpubr)
p1<-ggplot(simB, aes(x=Year, y=value, group=type, colour=type, linetype=type))+geom_line(size=1.54)+scale_color_manual(values=c("Hi"="black","HH"="green3","HM"="purple","HL"="cyan","Li"="black","LH"="green3","LM"="purple","LL"="cyan"))+scale_linetype_manual(values=c("Hi"="solid","HH"="solid","HM"="solid","HL"="solid","Li"="longdash","LH"="longdash","LM"="longdash","LL"="longdash"))+theme(legend.position="")+ylim(0, 88000)+facet_grid(. ~ "Biomass Simulations")+ylab("Biomass")+xlab("")
p2<-ggplot(CC, aes(x=Year, y=value, group=type, colour=type, linetype=type))+geom_line(size=1.54)+scale_color_manual(values=c("Hi"="black","HH"="green3","HM"="purple","HL"="cyan","Li"="black","LH"="green3","LM"="purple","LL"="cyan"))+scale_linetype_manual(values=c("Hi"="solid","HH"="solid","HM"="solid","HL"="solid","Li"="longdash","LH"="longdash","LM"="longdash","LL"="longdash"))+theme(legend.position="")+ylim(0, 46000)+facet_grid(. ~ "BHDPF(K=100000, M=0.9, h=0.66)")+ylab("Catch")+xlab("")
p3<-ggplot(CCSF, aes(x=Year, y=value, group=type, colour=type, linetype=type))+geom_line(size=1.54)+scale_color_manual(values=c("Hi"="black","HH"="green3","HM"="purple","HL"="cyan","Li"="black","LH"="green3","LM"="purple","LL"="cyan"))+scale_linetype_manual(values=c("Hi"="solid","HH"="solid","HM"="solid","HL"="solid","Li"="longdash","LH"="longdash","LM"="longdash","LL"="longdash"))+theme(legend.position="")+ylim(0, 46000)+facet_grid(. ~ "SF(K=100000, r=1.18)") +ylab("Catch")+xlab("Year")
dev.new()
ggarrange(p1,p2,p3, ncol=1, nrow=3)
