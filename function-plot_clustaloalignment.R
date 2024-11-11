#######################################################################################
#######################################################################################
#######################################################################################
#Author: WILLIAN T.A.F. SILVA
#E-mail: willian.silva@evobiolab.com
#######################################################################################
#######################################################################################
#######################################################################################
 
#Sequence alignment plot (ClustalOmega).
plot_clustaloalignment<-function(alignfile){

  library(magrittr)
  library(seqinr)
  library(seqvisr)
  library(phylotools)
  library(ggplot2)
  library(dotplot)

  SEQNAMES<-phylotools::read.fasta(file=alignfile)[[1]]

  #Create plot.
  p<-msavisr(mymsa=alignfile,
    myref=SEQNAMES[1],
    refontop=TRUE,
    basecolors=c("green","blue","red"))

  return(p)

}