#' Function for testing Rapoport's rule
#'  
#' @ indat = matrix containing community data
#' @ alt = character vector indicating column name for altitude values
#' @ group =character vector indicating column name for site IDs (if there is more than one site )
#' 

rapalt<- function(indat=NULL,alt=NULL,group=NULL) {
    if(is.null(group)){
                    tmp <- melt(indat, id.vars = c(alt))
                    tmp1 <- subset(tmp, value > 0)   
                    form<-as.formula(paste(alt,"~variable",sep=""))
                    minalt <- stats::aggregate(form, data = tmp1, FUN = min)
                    colnames(minalt) <- c("species", "minalt")
                    maxalt <- stats::aggregate(form, data = tmp1, FUN = max)
                    colnames(maxalt) <- c("species", "maxalt")
                    combined <- merge(minalt, maxalt)
                    combined$meanalt <- rowMeans(combined[2:3])
                    combined1 <- arrange(combined, +meanalt)
                    combined1$order <- 1:dim(combined1)[1]
                    rapp<-ggplot(data=combined1)+aes(x=order,y=meanalt)+theme_bw()+theme(legend.title=element_blank())+xlab("Species")+
                        ylab("Elevational range (m)")+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
                        geom_linerange(aes(ymin = minalt, ymax = maxalt),size=0.1,col="black")+geom_point(size=0.5,col="red")+
                        theme(strip.text.x = element_text(size=9, face="bold"),strip.text.y = element_text(size=9, face="bold"))
                    results<-list(combined1,rapp)
    } else {         
        resgroup<- NULL
        site.l<-unique(indat[,group]) 
        for (i in 1:length(site.l)){
            indat1<-indat[indat[,group] %in% site.l[i],] 
            colsdel<-c(alt,group) 
            indat.tmp<-indat1[, !colnames(indat1) %in% colsdel]
            totalsp <- colSums(indat.tmp)
            cols <- names(totalsp[totalsp > 0])
            cols1 <- c(alt,group,cols)
            indat2 <- indat1[, cols1]    
            tmp <- melt(indat2, id.vars = c(alt,group))
            tmp1 <- subset(tmp, value > 0)   
            form<-as.formula(paste(alt,"~variable",sep=""))
            minalt <- stats::aggregate(form, data = tmp1, FUN = min)
            colnames(minalt) <- c("species", "minalt")
            maxalt <- stats::aggregate(form, data = tmp1, FUN = max)
            colnames(maxalt) <- c("species", "maxalt")
            combined <- merge(minalt, maxalt)
            combined$meanalt <- rowMeans(combined[2:3])
            combined1 <- arrange(combined, +meanalt)
            combined1$order <- 1:dim(combined1)[1]
            combined2<-data.frame(site=site.l[i],combined1)
            resgroup<-rbind(combined2,resgroup)
        }
        rapp1<-ggplot(data=resgroup)+aes(x=order,y=meanalt)+theme_bw()+theme(legend.title=element_blank())+xlab("Species")+
            ylab("Elevational range (m)")+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
            geom_linerange(aes(ymin = minalt, ymax = maxalt),size=0.1,col="black")+geom_point(size=0.5,col="red")+
            theme(strip.text.x = element_text(size=9, face="bold"),strip.text.y = element_text(size=9, face="bold"))+
            facet_wrap(~site)
        results<-list(resgroup,rapp1)
        
    }
    return(results)  
}
