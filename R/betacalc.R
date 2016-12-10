betacalc <- function(inputdf,alt,interpol,continous) {
    results <- NULL    
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
                  Brich = mean(Brich), band2 = bands[y + 
                    1])
                results <- rbind(res, results)
            }  # close y loop 
        }  # close else statement
    }  # close i loop
    return(results)
}

