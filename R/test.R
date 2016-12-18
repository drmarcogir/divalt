
library(reshape);library(ggplot2)
source("/mnt/data1tb/Dropbox/Bryophytes/divalt/R/support.R")
source("/mnt/data1tb/Dropbox/Bryophytes/divalt/R/betacar.R")

longino11=read.csv("/mnt/data1tb/Dropbox/Bryophytes/Pigotdataset/ants.csv")

#indat=read.csv("/mnt/data1tb/Dropbox/Bryophytes/Pigotdataset/ants2sites.csv")

beta1<-betacar(indat=indat,alt="Elevation")

ggsave(filename="/mnt/data1tb/Dropbox/Bryophytes/divalt/betacar.png",plot =beta1[[2]],width=4,height=3,dpi=100,scale=1)

ggsave(filename="/mnt/data1tb/Dropbox/Bryophytes/divalt/betacar.png",plot =beta1[[2]],width=4,height=3,dpi=300)