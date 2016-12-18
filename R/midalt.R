



# MDE function
mdemg<-function(inputdf){
    # island list
    isl.l <- as.character(unique(inputdf$Island))
    results<-NULL
    for (i in 1:length(isl.l)){
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
            # aggregate data
            isldat1l1 <- aggregate(value ~ Island + Alt_norm+variable , data = isldat1l, FUN = sum)
            # verify if there are no species at certain altitudinal bands
            isldat1l1$value<-replace(isldat1l1$value,isldat1l1$value > 0,1)
            check<-aggregate(value ~ Island+Alt_norm,data=isldat1l1,FUN=sum)
            check1<-subset(check,value==0)
            obsrich<-subset(check,value > 0)
            # exclude bands with no data
            isldat1l1<-isldat1l1[!isldat1l1$Alt_norm %in% check1$Alt_norm,]
            # spread data
            all <- spread(isldat1l1, variable, value)
            # MDE 
            cat(as.character(isl.l[i]))
            cat("\n")
            temp1<- rangemod1d(all[3:dim(all)[2]],nb = NULL,var = NULL,rsize = "observed",reps = 10000)
            df<-data.frame(temp1[1:2],all[1:2])
            df$grouping<-"Simulated Richness"
            obsrich1<-data.frame(mod.rich=obsrich[,3],mod.sd=NA,obsrich[1:2])
            obsrich1$grouping<-"Observed Richness"
            combined<-rbind(df,obsrich1)
            results<-rbind(combined,results)
            
        } 
    }
    return(results)
}
