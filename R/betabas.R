#' Function for calculating beta diversity between altitudinal bands following the
#' methods of Baselga et al. (2010)
#'  
#' @ indat = matrix containing community data
#' @ alt = character vector indicating column name for altitude values
#' @ group =character vector indicating column name for site IDs (if there is more than one site )
#' 
betabas<- function(indat=NULL,alt=NULL,group=NULL) {
    results <- NULL    
    if(is.null(group)){
        alt.l<-sort(unique(indat[,alt]))    
        for (y in 1:(length(alt.l)- 1)){
            d1<-indat[indat[,alt] %in% alt.l[y],] 
            d2<-indat[indat[,alt] %in% alt.l[y+ 1],] 
            Btotal <- NULL
            Brepl <- NULL
            Bnest <- NULL
            for (z in 1:dim(d1)[1]) {
                d1<-d1[,!names(d1) %in% alt] 
                d2<-d2[,!names(d2) %in% alt]
                d1[d1 > 1] <- 1
                d2[d2 > 1] <- 1
                rownames(d1)<-c("1")
                rownames(d2)<-c("1")
                betaValues<- beta.temp(d1, d2, index.family="jaccard")
                betaValues[is.na(betaValues)]<-0
                Btotal[z] <- betaValues[3][,1]
                Bnest[z] <- betaValues[2][,1]
                Brepl[z] <- betaValues[1][,1]
            } 
            res <- data.frame(Btotal = mean(Btotal), Brepl = mean(Brepl),Bnest = mean(Bnest), Altitude=paste(alt.l[y],alt.l[y+1],sep="-"),Altitude.midpoint=((alt.l[y]+alt.l[y+1])/2)) 
            results <- rbind(res, results)
        } 
        results.l<-melt(results,id.vars=c("Altitude","Altitude.midpoint"))  
        plot.results<-ggplot(results.l,aes(x=Altitude.midpoint,y=value,colour=variable))+
            geom_line(size=0.5)+geom_point(size=1.5)+theme_bw()+ylab("Beta Diversity")+
            xlab("Altitude")+theme(legend.title=element_blank())+theme(strip.text.x = element_text(size=9, face="bold"),strip.text.y = element_text(size=9, face="bold"))
        results1<-list(results,plot.results)
    } else {
        results <- NULL
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
            for (y in 1:(length(alt.l)- 1)){
                d1<-indat2[indat2[,alt] %in% alt.l[y],] 
                d2<-indat2[indat2[,alt] %in% alt.l[y+ 1],] 
                Btotal <- NULL
                Brepl <- NULL
                Bnest <- NULL
                for (z in 1:dim(d1)[1]) {
                    d1<-d1[,!names(d1) %in% alt] 
                    d2<-d2[,!names(d2) %in% alt]
                    d1[d1 > 1] <- 1
                    d2[d2 > 1] <- 1
                    rownames(d1)<-c("1")
                    rownames(d2)<-c("1")
                    betaValues<- beta.temp(d1, d2, index.family="jaccard")
                    betaValues[is.na(betaValues)]<-0
                    Btotal[z] <- betaValues[3][,1]
                    Bnest[z] <- betaValues[2][,1]
                    Brepl[z] <- betaValues[1][,1]
                } 
                res <- data.frame(Btotal = mean(Btotal), Brepl = mean(Brepl),Bnest = mean(Bnest), Altitude=paste(alt.l[y],alt.l[y+1],sep="-"),Altitude.midpoint=((alt.l[y]+alt.l[y+1])/2)) 
                results <- rbind(res, results) 
            } 
        }  
        results.l<-melt(results,id.vars=c(group,"Altitude","Altitude.midpoint"))  
        plot.results<-ggplot(results.l,aes(x=Altitude.midpoint,y=value,colour=variable))+
            geom_line(size=0.5)+geom_point(size=1.5)+theme_bw()+ylab("Beta Diversity")+
            xlab("Altitude")+theme(legend.title=element_blank())+theme(strip.text.x = element_text(size=9, face="bold"),strip.text.y = element_text(size=9, face="bold"))+facet_wrap(~site)
        results1<-list(results,plot.results)       
    }
    return(results1)
}