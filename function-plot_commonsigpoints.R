#######################################################################################
#######################################################################################
#######################################################################################
#Author: WILLIAN T.A.F. SILVA
#E-mail: willian.silva@evobiolab.com
#######################################################################################
#######################################################################################
#######################################################################################

#Common significant points.
plot_commonsigpoints<-function(
    Xvalues=1:100,
    Yvalues=1:100,
    Xthreshold=Inf,
    Ythreshold=Inf,
    xlabel="X",
    ylabel="Y",
    plottitle="Common significant points",
    Q1color="blue",
    Q2color="deeppink",
    Q3color="gray",
    Q4color="lawngreen"){
  
  #Load libraries.
  library(ggplot2)
  library(tidyverse)
  
  #Plot theme.
  myggplottheme<-theme(title=element_text(size=10,face="bold"),
                       axis.title=element_text(size=10,face="bold"),
                       axis.text=element_text(size=10),
                       axis.text.x=element_text(angle=60,size=8,vjust=0.5),
                       legend.position="none",
                       legend.title=element_text(size=10,face="bold"),
                       legend.text=element_text(size=10),
                       legend.key=element_blank(),
                       panel.grid=element_line(colour="gray90"),
                       panel.grid.major.x=element_blank(),
                       panel.grid.minor.x=element_blank(),
                       panel.background=element_rect(fill="white",colour="black"),
                       panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
                       strip.background=element_rect(colour="black",
                                                     fill="white"))
  
  #Format data.
  DATA<-data.frame(Xvalues=Xvalues,
                   Yvalues=Yvalues,
                   Color=Q3color,
                   Alpha=1)
  
  #Define color.
  DATA$Color[DATA$Xvalues>=Xthreshold & DATA$Yvalues>=Ythreshold]<-Q1color
  DATA$Color[DATA$Xvalues<Xthreshold & DATA$Yvalues>=Ythreshold]<-Q2color
  DATA$Color[DATA$Xvalues<Xthreshold & DATA$Yvalues<Ythreshold]<-Q3color
  DATA$Color[DATA$Xvalues>=Xthreshold & DATA$Yvalues<Ythreshold]<-Q4color
  
  #Define transparency (alpha value).
  #DATA$Alpha[DATA$Xvalues>=Xthreshold & DATA$Yvalues>=Ythreshold]<-1
  #DATA$Alpha[DATA$Xvalues<Xthreshold & DATA$Yvalues>=Ythreshold]<-1
  #DATA$Alpha[DATA$Xvalues<Xthreshold & DATA$Yvalues<Ythreshold]<-1
  #DATA$Alpha[DATA$Xvalues>=Xthreshold & DATA$Yvalues<Ythreshold]<-1
 
  #Plot.
  p<-ggplot(data=DATA)+
    geom_point(aes(Xvalues,Yvalues),
               color=DATA$Color,
               alpha=DATA$Alpha,
               shape=18,
               size=1)+
    geom_vline(xintercept=Xthreshold,color="black",linetype="dashed")+
    geom_hline(yintercept=Ythreshold,color="black",linetype="dashed")+
    labs(x=xlabel,y=ylabel)+
    ggtitle(plottitle)+
    myggplottheme
  
  return(p)
}