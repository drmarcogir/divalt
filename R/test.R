source("/mnt/data1tb/Dropbox/Bryophytes/divalt/R/support.R")

indat=read.csv("/mnt/data1tb/Dropbox/Bryophytes/Pigotdataset/ants.csv")

#indat=read.csv("/mnt/data1tb/Dropbox/Bryophytes/Pigotdataset/ants2sites.csv")

beta1<-betacar(indat=indat,alt="Elevation")
