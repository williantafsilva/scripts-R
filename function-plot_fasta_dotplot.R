#######################################################################################
#######################################################################################
#######################################################################################
#Author: WILLIAN T.A.F. SILVA
#E-mail: willian.silva@evobiolab.com
#######################################################################################
#######################################################################################
#######################################################################################
 
#Dotplot of two FASTA sequences.
plot_fasta_dotplot<-function(
  fastafile,
  windowsize=1,
  windowstep=1){

  library(magrittr)
  library(seqvisr)
  library(ggplot2)
  library(seqinr)
  library(dotplot)

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

  SEQS<-seqinr::read.fasta(file=fastafile,as.string=T)
  SEQNAMES<-names(SEQS)
  SEQ1<-SEQS[1][[1]]
  SEQ2<-SEQS[2][[1]]

  DOTPLOTDATA<-mkDotPlotDataFrame(SEQ1,SEQ2,wsize=windowsize,wstep=windowstep,nmatch=windowsize)

  #Create plot.
  p<-ggplot(DOTPLOTDATA,aes(x=x,y=y))+
    geom_point(shape=10,size=0.5)+
    labs(x=SEQNAMES[1],
         y=SEQNAMES[2],
         title=paste0(fastafile," (dotplot::mkDotPlotDataFrame; wsize=",windowsize,", wstep=",windowstep,", nmatch=",windowsize,")"))+
  myggplottheme
  
  return(p)

}