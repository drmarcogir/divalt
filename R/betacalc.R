#library(BAT)

source("/mnt/data1tb/Dropbox/Bryophytes/divalt/R/support.R")
indat=read.csv("/mnt/data1tb/Dropbox/Bryophytes/Pigotdataset/ants.csv")
alt="Elevation"
continous="TRUE"
group="NULL"
y=1

betacar<- function(indat=NULL,alt=NULL,interpol=NULL,continous=NULL,group=NULL) {
    results <- NULL    
    if(is.null(group)){
        alt.l<-sort(unique(indat[,alt])) # get altitude column   
        for (y in 1:(length(alt.l)- 1)){
            d1<-indat[indat[,alt] %in% alt.l[y],] # data for specific altitudinal band
            d2<-indat[indat[,alt] %in% alt.l[y+ 1],] # data for successive altitudinal band
            Btotal <- NULL
            Brepl <- NULL
            Brich <- NULL
            for (z in 1:dim(d1)[1]) {
                d1<-d1[,!names(d1) %in% alt] # exclude altitude column from d1  
                d2<-d2[,!names(d2) %in% alt]  # exclude altitude column from d2
                commBoth <- as.matrix(rbind(d1,d2)) # communities successive altitudes
                betaValues <- betaObs(comm = commBoth, func = "jaccard", abund = FALSE)
                Btotal[z] <- betaValues$Btotal
                Brepl[z] <- betaValues$Brepl
                Brich[z] <- betaValues$Brich
            } # close 
            mean(alt.l[y],alt.l[y+1])
            res <- data.frame(Btotal = mean(Btotal), Brepl = mean(Brepl),Brich = mean(Brich), Altitude=paste(alt.l[y],alt.l[y+1],sep="-"),Altitude.mipoint=((alt.l[y]+alt.l[y+1])/2)) 
            results <- rbind(res, results)
            } # close y loop
        
    } else {
        
    }  # close ifelse statememt
    return(results)
}
        
        
        
# find out which species occur in a particular group and which do not 
dataclean<-function(){
    









        
        
        
        } else {
            
            for (y in 1:(length(bands) - 1)) {
                d1 <- subset(all, Alt_norm == bands[y])
                d2 <- subset(all, Alt_norm == bands[y + 1])
                # beta diversity calculations
                Btotal <- NULL
                Brepl <- NULL
                Brich <- NULL
                for (z in 1:dim(d1)[1]) {
                    commBoth <- as.matrix(rbind(d1[3:dim(d1)[2]][z, ], d2[3:dim(d1)[2]][z, 
                                                                                        ]))
                    betaValues <- betaObs(comm = commBoth, func = "jaccard", abund = FALSE)
                    Btotal[z] <- betaValues$Btotal
                    Brepl[z] <- betaValues$Brepl
                    Brich[z] <- betaValues$Brich
                }
                res <- data.frame(Island = isl.l[i], Btotal = mean(Btotal), Brepl = mean(Brepl), 
                                  Brich = mean(Brich), band2 = bands[y +1])
                results <- rbind(res, results)
            }  # close y loop            
            
            # loop trhough each island
        }  # close i loop
    return(results)
}   
            
      
            
        
        
    isl.l <- unique(inputdf$Island)
    for (i in 1:length(isl.l)) {
        # data for one island
        isldat <- subset(inputdf, Island == isl.l[i])
  # find out which species do not occur on island of interest
        colsdel <- c("Alt_norm", "withinagg", "Island","Plot","Quadrat","Tree","A.B.C")
        isldat.tmp<-isldat[, !colnames(isldat) %in% colsdel]
        totalsp <- colSums(isldat.tmp)
        cols <- names(totalsp[totalsp > 0])
        firstnotdelete<-dim(isldat)[2]-dim(isldat.tmp)[2]
        cols1 <- c(names(isldat[1:firstnotdelete]),cols)
        # subset columns only species occuring on the island
        isldat1 <- isldat[, cols1]
        # melt in long format
        isldat1l <- melt(isldat1, id.vars = names(isldat[1:firstnotdelete]))
        if (is.null(isldat1l$value) == "TRUE") {
            next
        } else {
            # aggregate microplot level data
isldat1l1 <- aggregate(value ~ Island + Alt_norm+variable , data = isldat1l, FUN = sum)
            # spread data
            all <- spread(isldat1l1, variable, value)
        colsdel <- c("Alt_norm", "withinagg", "Island","Plot","Quadrat","Tree","A.B.C")
        spcols<-all[, !colnames(all) %in% colsdel]
        exclude<-(dim(all)[2]-dim(spcols)[2])+1
            if(interpol=="TRUE"){
            all[exclude:dim(all)[2]][all[exclude:dim(all)[2]] > 1]<-1
            }
            # extract altitudinal bands
            bands <- unique(all$Alt_norm)
            # extract data for successive altitudinal bands
            for (y in 1:(length(bands) - 1)) {
                d1 <- subset(all, Alt_norm == bands[y])
                d2 <- subset(all, Alt_norm == bands[y + 1])
                # beta diversity calculations
                Btotal <- NULL
                Brepl <- NULL
                Brich <- NULL
                for (z in 1:dim(d1)[1]) {
                  commBoth <- as.matrix(rbind(d1[3:dim(d1)[2]][z, ], d2[3:dim(d1)[2]][z, 
                    ]))
                  betaValues <- betaObs(comm = commBoth, func = "jaccard", abund = FALSE)
                  Btotal[z] <- betaValues$Btotal
                  Brepl[z] <- betaValues$Brepl
                  Brich[z] <- betaValues$Brich
                }
                res <- data.frame(Island = isl.l[i], Btotal = mean(Btotal), Brepl = mean(Brepl), 
                  Brich = mean(Brich), band2 = bands[y +1])
                results <- rbind(res, results)
            }  # close y loop 
        }  # close else statement
    }  # close i loop
    return(results)
}

