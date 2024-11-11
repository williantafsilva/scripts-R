#!/usr/bin/env Rscript
rm(list=ls()) #Clear environment.
ARGS=commandArgs(trailingOnly=TRUE) 
RUNDATE=format(Sys.time(),"%Y%m%d%H%M%S")
SCRIPTNAME<-"formatdata-Rehh.R"
JOBID<-ARGS[1]
OUTPUTLOCATION<-normalizePath(ARGS[2])
sink(paste0(OUTPUTLOCATION,"/job",JOBID,".Rlog"),type=c("output","message"))
############################################################################
################################ R SCRIPT ##################################
############################################################################
##Author: Willian T.A.F. Silva (willian.silva@evobiolab.com).
############################################################################
#SCRIPT DESCRIPTION:

##Description:
##Format SweepFinder2 data from individual chromosomes.

##Input $1: Job ID.
##Input $2: Output location.
##Input $3: File containing iHS (Rehh) data (.rds).
##Output: Formatted data to be used for plotting (.csv).

##Usage: 
##Rscript --vanilla formatdata-Rehh.R <JOB ID> <OUTPUT LOCATION> <INPUT DIRECTORY>

############################################################################

cat("############################################################################\n")
cat(paste0("STARTING R SCRIPT...",format(Sys.time(),"%Y%m%d%H%M%S\n")))
cat("############################################################################\n")

cat(paste0("Rjob ID: ",JOBID,"\n"))
cat("\n")
cat(paste0("Submission (Rjob ID ",JOBID,"): Rscript --vanilla ",SCRIPTNAME," ",paste(ARGS,collapse=" "),"\n"))
cat("\n")
cat("############################################################################\n")
cat("#LOAD LIBRARIES:\n")

library(R.utils)
library(ggplot2)
library(viridisLite)
library(tidyverse)
library(rehh)
library(gridExtra)

cat("\n")
cat("############################################################################\n")
cat("#DEFAULT SETTINGS:\n")



cat("\n")
cat("############################################################################\n")
cat("#SYSTEM CONTROL:\n")

cat("sessionInfo():\n")
sessionInfo()

cat("\n")
cat("############################################################################\n")
cat("#ACTIONS:\n")
cat("Processing:\n")

INPUTFILE<-normalizePath(ARGS[3])
INPUTFILENAME<-basename(INPUTFILE)
OUTPUTFILEPREFIX<-system(paste0("echo ",INPUTFILENAME,"| sed 's/-job[0-9].*$//'"),
                         intern=TRUE)
OUTPUTFILENAME<-paste0(OUTPUTFILEPREFIX,".Rehhdata-job",JOBID,".csv")
OUTPUTFILE<-paste0(OUTPUTLOCATION,"/",OUTPUTFILENAME)

cat(paste0("INPUTFILE: ",INPUTFILE,"\n"))
cat(paste0("INPUTFILENAME: ",INPUTFILENAME,"\n"))
cat(paste0("OUTPUTLOCATION: ",OUTPUTLOCATION,"\n"))
cat(paste0("OUTPUTFILEPREFIX: ",OUTPUTFILEPREFIX,"\n"))
cat(paste0("OUTPUTFILENAME: ",OUTPUTFILENAME,"\n"))
cat(paste0("OUTPUTFILE: ",OUTPUTFILE,"\n"))

cat("\n")
cat(paste0("PROCESSING FILE: ",INPUTFILE,"\n"))

#Read data.
iHS_DATA<-readRDS(INPUTFILE)

#Sequence of chromosomes to be plotted.
CHRSEQ<-c(1:40) #Autosomes only.

#Standard chromosome length (for plotting aesthetics purposes; standardize chromosome length).
STDCHRLENGTH<-100

#Set alternating chromosome colors.
CHRCOLORS<-rep(c("blue","red"),length(CHRSEQ))

#Format data.
SWEEPDATA<-iHS_DATA$ihs
colnames(SWEEPDATA)<-c("Chromosome","Location","iHS","LogPvalue")
SWEEPDATA<-SWEEPDATA[with(SWEEPDATA,order(Chromosome,Location)),]
SWEEPDATA$xPosition<-NA
SWEEPDATA$Color<-NA

for(C in CHRSEQ){
  #Find data for chromosome C.
  CHRCROWS<-which(SWEEPDATA$Chromosome %in% c(C,paste0("Chr",C),paste0("chr",C)))
  if(length(CHRCROWS)>0){
    cat(paste0("READING CHROMOSOME ",C," data.\n"))
    SWEEPDATA$Chromosome[CHRCROWS]<-paste0("Chr",C)
    SWEEPDATA$xPosition[CHRCROWS]<-seq(from=(C-1)*STDCHRLENGTH+1,
                                        to=C*STDCHRLENGTH,
                                        length.out=length(CHRCROWS))
    SWEEPDATA$Color[CHRCROWS]<-CHRCOLORS[C]
  }else if(length(CHRCROWS)==0){
    cat(paste0("ERROR: Chromosome ",C," not found. Creating an empty chromosome data set.\n"))
    TMPDATA<-data.frame(Chromosome=paste0("Chr",C),
                        Location=NA,
                        iHS=NA,
                        LogPvalue=NA,
                        xPosition=seq(from=(C-1)*STDCHRLENGTH+1,
                                      to=C*STDCHRLENGTH,
                                      length.out=STDCHRLENGTH),
                        Color=CHRCOLORS[C])
    SWEEPDATA<-rbind(SWEEPDATA,TMPDATA)
  }else{
    cat(paste0("ERROR: Chromosome ",C,".\n"))
  }
}

#Save formatted data.
write.csv(SWEEPDATA,file=OUTPUTFILE,row.names=FALSE)

cat("\n")
cat("############################################################################\n")
cat("##SAVE CONTROL FILES (README, script, slurm):\n")

cat(paste0("############################################################################
Date: ",RUNDATE,"
Job ID: ",JOBID,"
Script: ",SCRIPTNAME,"
Input file: ",INPUTFILE,"
Output file: ",OUTPUTFILE,"

"),file=paste0(OUTPUTLOCATION,"/README.txt"),append=TRUE)

cat(paste0("FILE ",INPUTFILE," PROCESSED.","\n"))

cat("\n")
cat("############################################################################\n")
cat(paste0("END OF R SCRIPT...",format(Sys.time(),"%Y%m%d%H%M%S\n")))
cat("############################################################################\n")
sink()
############################################################################
############################################################################
############################################################################