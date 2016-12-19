#' Calculate Beta-Whittaker
#'  
#' @ indat = matrix containing community data
#' @ alt = character vector indicating column name for altitude values
#' @ group =character vector indicating column name for elevational gradient IDs (if there is more than one stuy area)
#' @ sname =character vector sample IDs within each band of the elevational gradient

betaw<- function(indat=NULL,alt=NULL,group=NULL,sname=NULL) {
    restmp<-NULL
    if(is.null(group)){
        alt.l<-sort(unique(indat[,alt]))
        for (y in 1:(length(alt.l))){
            tmpdat<-indat[indat[,alt] %in% alt.l[y],] 
            tmpdat.l<-melt(tmpdat,id.vars=c(alt,sname))
            tmpdat.l1<-subset(tmpdat.l,value > 0)
            alpha.tmp<-aggregate(value~sample,data=tmpdat.l1,FUN=sum)
            Alpha=mean(alpha.tmp[,2])
            Gamma<-length(unique(tmpdat.l1[,3]))
            resdf<-data.frame(Altitude=alt.l[y],Gamma,Alpha,Betaw=(Gamma/Alpha)-1)
            restmp<-rbind(resdf,restmp)
        }
        restmp1<-melt(restmp,id.vars=c("Altitude"))
combp<-ggplot(restmp1,aes(x=Altitude,y=value,colour=variable))+geom_line(size=0.5)+geom_point(size=1.5)+
theme_bw()+ylab("Diversity")+xlab("Altitude")+theme(legend.title=element_blank())+
theme(strip.text.x = element_text(size=9, face="bold"),strip.text.y = element_text(size=9, face="bold"))
results<-list(restmp1,combp)  
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
        for (y in 1:(length(alt.l))){
            tmpdat<-indat2[indat2[,alt] %in% alt.l[y],] 
            tmpdat.l<-melt(tmpdat,id.vars=c(alt,sname,group))
            tmpdat.l1<-subset(tmpdat.l,value > 0)
            alpha.tmp<-aggregate(value~sample,data=tmpdat.l1,FUN=sum)
            Alpha=mean(alpha.tmp[,2])
            Gamma<-length(unique(tmpdat.l1[,c("variable")]))
            resdf<-data.frame(site=site.l[i],Altitude=alt.l[y],Gamma,Alpha,Betaw=(Gamma/Alpha)-1)
            restmp2<-rbind(resdf,restmp2)
        }
    }
    restmp3<-melt(restmp2,id.vars=c("Altitude","site"))
combp1<-ggplot(restmp3,aes(x=Altitude,y=value,colour=variable))+geom_line(size=0.5)+geom_point(size=1.5)+
theme_bw()+ylab("Diversity")+xlab("Altitude")+theme(legend.title=element_blank())+
theme(strip.text.x = element_text(size=9, face="bold"),strip.text.y = element_text(size=9, face="bold"))+
facet_wrap(~site)
results<-list(restmp3,combp1)    
}
return(results)
}
    
    
    
    