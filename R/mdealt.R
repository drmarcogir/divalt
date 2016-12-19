#' Function for test the mid-domain effect
#'  
#' @ indat = matrix containing community data
#' @ alt = character vector indicating column name for altitude values
#' @ group =character vector indicating column name for site IDs (if there is more than one site )
#' 

mdealt<- function(indat=NULL,alt=NULL,group=NULL) {
    if(is.null(group)){
        indat.tmp<-indat[, !colnames(indat) %in% alt]
        indat.tmp[indat.tmp > 1]<-1
        row.names(indat.tmp)<-1:dim(indat.tmp)[1]
        temp1<- rangemod1d(indat.tmp,nb= NULL,var = NULL,rsize = "observed",reps =1000)
        df<-data.frame(temp1[1:2],Altitude=indat[,alt])
        df$grouping<-"Simulated Richness"
        obsrich1<-data.frame(mod.rich=rowSums(indat.tmp),mod.sd=NA,Altitude=indat[,alt])
        obsrich1$grouping<-"Observed Richness"
        combined<-rbind(df,obsrich1)
        combined$grouping<-as.factor(combined$grouping)
        mdep<-ggplot(data=combined,aes(x=Altitude,y=mod.rich))+theme_bw()+ylab("Species richness")+xlab("Altitude")+theme(legend.title=element_blank())+
            theme(strip.text.x = element_text(size=9, face="bold"),strip.text.y = element_text(size=9, face="bold"))+
            geom_ribbon(aes(ymin =mod.rich- mod.sd, ymax = mod.rich+mod.sd), fill = "grey70",na.rm=TRUE)+
            geom_line(aes(shape=factor(grouping)),size=0.6)+geom_point(aes(shape=factor(grouping)),size=2.2)
        results<-list(combined,mdep)
    } else {
        res_sites <- NULL
        site.l<-unique(indat[,group]) 
        for (i in 1:length(site.l)){  
            indat1<-indat[indat[,group] %in% site.l[i],] 
            colsdel<-c(alt,group) 
            indat.tmp<-indat1[, !colnames(indat1) %in% colsdel]
            totalsp <- colSums(indat.tmp)
            cols <- names(totalsp[totalsp > 0])
            cols1 <- c(alt,group,cols)
            indat2 <- indat1[, cols1]     
            indat.tmp<-indat2[, !colnames(indat2) %in% c(alt,group)]
            indat.tmp[indat.tmp > 1]<-1
            row.names(indat.tmp)<-1:dim(indat.tmp)[1]
            temp1<- rangemod1d(indat.tmp,nb= NULL,var = NULL,rsize = "observed",reps =1000)
            df<-data.frame(temp1[1:2],Altitude=indat2[,alt])
            df$grouping<-"Simulated Richness"
            obsrich1<-data.frame(mod.rich=rowSums(indat.tmp),mod.sd=NA,Altitude=indat2[,alt])
            obsrich1$grouping<-"Observed Richness"
            combined<-rbind(df,obsrich1)
            combined$grouping<-as.factor(combined$grouping)
            combined$site<-site.l[i]
            res_sites<-rbind(combined,res_sites)
            mdep<-ggplot(data=res_sites,aes(x=Altitude,y=mod.rich))+theme_bw()+ylab("Species richness")+xlab("Altitude")+theme(legend.title=element_blank())+
                theme(strip.text.x = element_text(size=9, face="bold"),strip.text.y = element_text(size=9, face="bold"))+
                geom_ribbon(aes(ymin =mod.rich- mod.sd, ymax = mod.rich+mod.sd), fill = "grey70",na.rm=TRUE)+
                geom_line(aes(shape=factor(grouping)),size=0.6)+geom_point(aes(shape=factor(grouping)),size=2.2)+facet_wrap(~site)
            results<-list(res_sites,mdep)
        }
    }
return(results)
}
        