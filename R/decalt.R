#' Creates a plot of dissimilarity between communities at different elevational bands and elevational distance
#'  
#' @ indat = matrix containing community data
#' @ alt = character vector indicating column name for altitude values
#' @ group =character vector indicating column name for elevational gradient IDs (if there is more than one stuy area)

decalt<-function(indat=NULL,alt=NULL,group=NULL) {
    restmp<-NULL
    if(is.null(group)){
        alt.l<-sort(unique(indat[,alt])) 
        names(alt.l)<-alt.l
        alt.l.d <- dist(alt.l)
        alt.l.df <- liste(alt.l.d , entry = "dist")
        for (y in 1:dim(alt.l.df)[1]) {
            d1<-indat[indat[,alt] %in% alt.l.df[y, ]$NBX,] 
            d2<-indat[indat[,alt] %in% alt.l.df[y, ]$NBY,] 
            Btotal <- NULL
            Brepl <- NULL
            Brich <- NULL
            for (z in 1:dim(d1)[1]) {
                d1<-d1[,!names(d1) %in% alt] 
                d2<-d2[,!names(d2) %in% alt]  
                commBoth <- as.matrix(rbind(d1,d2)) 
                betaValues <- betaObs(comm = commBoth, func = "jaccard", abund = FALSE)
                Btotal[z] <- betaValues$Btotal
                Brepl[z] <- betaValues$Brepl
                Brich[z] <- betaValues$Brich
            } 
            res <- data.frame(Btotal = mean(Btotal), Brepl = mean(Brepl),Brich = mean(Brich),Elevational.distance=alt.l.df[y, ]$dist) 
            restmp<- rbind(res, restmp)
        }
        restmp1<-melt(restmp,id.vars=c("Elevational.distance")) 
        decayp<-ggplot(restmp1,aes(x=Elevational.distance,y=value))+geom_point(size=1.5)+
            theme_bw()+ylab("Beta Diversity")+xlab("Elevational Distance")+theme(legend.title=element_blank())+
            theme(strip.text.x = element_text(size=9, face="bold"),strip.text.y = element_text(size=9, face="bold"))+
            facet_wrap(~variable)
        results<-list(restmp1,decayp)
    } else {
        restmp2 <- NULL
        site.l<-unique(indat[,group]) 
        for (i in 1:length(site.l)){  
            indat1<-indat[indat[,group] %in% site.l[i],] 
            colsdel<-c(alt,group) 
            indat.tmp<-indat1[, !colnames(indat1) %in% colsdel]
            totalsp <- colSums(indat.tmp)
            cols <- names(totalsp[totalsp > 0])
            cols1 <- c(alt,group,cols)
            indat2 <- indat1[, cols1]
            alt.l<-sort(unique(indat2[,alt]))   
            names(alt.l)<-alt.l
            alt.l.d <- dist(alt.l)
            alt.l.df <- liste(alt.l.d , entry = "dist")
            for (y in 1:dim(alt.l.df)[1]) {
                d1<-indat2[indat2[,alt] %in% alt.l.df[y, ]$NBX,] 
                d2<-indat2[indat2[,alt] %in% alt.l.df[y, ]$NBY,] 
                Btotal <- NULL
                Brepl <- NULL
                Brich <- NULL
                for (z in 1:dim(d1)[1]) {
                    d1<-d1[,!names(d1) %in% alt] 
                    d2<-d2[,!names(d2) %in% alt]  
                    commBoth <- as.matrix(rbind(d1,d2)) 
                    betaValues <- betaObs(comm = commBoth, func = "jaccard", abund = FALSE)
                    Btotal[z] <- betaValues$Btotal
                    Brepl[z] <- betaValues$Brepl
                    Brich[z] <- betaValues$Brich
                } 
                res <- data.frame(site=site.l[i],Btotal = mean(Btotal), Brepl = mean(Brepl),Brich = mean(Brich),Elevational.distance=alt.l.df[y, ]$dist) 
                restmp2<- rbind(res, restmp2)
            }
        }
            restmp3<-melt(restmp2,id.vars=c("site","Elevational.distance")) 
            decayp1<-ggplot(restmp3,aes(x=Elevational.distance,y=value))+geom_point(size=1.5)+
                theme_bw()+ylab("Beta Diversity")+xlab("Elevational Distance")+theme(legend.title=element_blank())+
                theme(strip.text.x = element_text(size=9, face="bold"),strip.text.y = element_text(size=9, face="bold"))+
                facet_grid(site~variable)
            results<-list(restmp3,decayp1)
        }
        return(results)
}   
        