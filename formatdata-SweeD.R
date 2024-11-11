#!/usr/bin/env Rscript
rm(list=ls()) #Clear environment.
ARGS=commandArgs(trailingOnly=TRUE) 
RUNDATE=format(Sys.time(),"%Y%m%d%H%M%S")
SCRIPTNAME<-"formatdata-SweeD.R"
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
##Input $3: Directory containing SweeD output files (per chromosome).
##Output: Formatted data to be used for plotting (.csv).

##Usage: 
##Rscript --vanilla formatdata-SweeD.R <JOB ID> <OUTPUT LOCATION> <INPUT DIRECTORY>

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

INPUTDIR<-normalizePath(ARGS[3])
INPUTDIRNAME<-basename(INPUTDIR)
OUTPUTFILEPREFIX<-system(paste0("echo ",INPUTDIRNAME,"| sed 's/-job[0-9].*$//'"),
                         intern=TRUE)
OUTPUTFILENAME<-paste0(OUTPUTFILEPREFIX,".SweeDdata-job",JOBID,".csv")
OUTPUTFILE<-paste0(OUTPUTLOCATION,"/",OUTPUTFILENAME)

cat(paste0("INPUTDIR: ",INPUTDIR,"\n"))
cat(paste0("INPUTDIRNAME: ",INPUTDIRNAME,"\n"))
cat(paste0("OUTPUTLOCATION: ",OUTPUTLOCATION,"\n"))
cat(paste0("OUTPUTFILEPREFIX: ",OUTPUTFILEPREFIX,"\n"))
cat(paste0("OUTPUTFILENAME: ",OUTPUTFILENAME,"\n"))
cat(paste0("OUTPUTFILE: ",OUTPUTFILE,"\n"))

cat("\n")
cat(paste0("PROCESSING DIRECTORY: ",INPUTDIR,"\n"))

#Set working directory.
setwd(INPUTDIR)

#List of files in the input directory.
FILES<-list.files(INPUTDIR) 
FILES<-FILES[grep("SweeD_Report.*$",FILES)]

#Sequence of chromosomes to be plotted.
CHRSEQ<-c(1:40) #Autosomes only.

#Standard chromosome length (for plotting aesthetics purposes; standardize chromosome length).
STDCHRLENGTH<-100

#Set alternating chromosome colors.
CHRCOLORS<-rep(c("blue","red"),length(CHRSEQ))

#Collect and format data.
SWEEPDATA<-data.frame(Location=NA,
                      Likelihood=NA,
                      Alpha=NA,
                      StartPos=NA,
                      EndPos=NA,
                      Chromosome=NA,
                      xPosition=NA,
                      Color=NA,
                      RescaledLikelihood=NA)

for(C in CHRSEQ){
  #Find file for chromosome C.
  FILECHRC<-FILES[grep(paste0("\\.Chr",C,"\\.|\\.chr",C,"\\.|\\.",C,"\\."),FILES)] 
  if(length(FILECHRC)==1){
    cat(paste0("READING FILE: ",FILECHRC,"\n"))
    TMPDATA<-read.table(FILECHRC,sep="\t",header=TRUE)
    colnames(TMPDATA)<-c("Location","Likelihood","Alpha","StartPos","EndPos")
    TMPDATA$Chromosome<-paste0("Chr",C)
    TMPDATA$xPosition<-seq(from=(C-1)*STDCHRLENGTH+1,
                           to=C*STDCHRLENGTH,
                           length.out=nrow(TMPDATA))
    TMPDATA$Color<-CHRCOLORS[C]
    TMPDATA$RescaledLikelihood<-NA
  }else if(length(FILECHRC)==0){
    cat(paste0("ERROR: File for chromosome ",C," not found. Creating an empty chromosome data set.\n"))
    TMPDATA<-data.frame(Location=NA,
                        Likelihood=NA,
                        Alpha=NA,
                        StartPos=NA,
                        EndPos=NA,
                        Chromosome=paste0("Chr",C),
                        xPosition=seq(from=(C-1)*STDCHRLENGTH+1,
                                      to=C*STDCHRLENGTH,
                                      length.out=STDCHRLENGTH),
                        Color=CHRCOLORS[C],
                        RescaledLikelihood=NA)
  }else{
    cat(paste0("ERROR: Multiple files match chromosome ",C,".\n"))
  }
  SWEEPDATA<-rbind(SWEEPDATA,TMPDATA)
}

#Remove NA chromosomes.
SWEEPDATA<-SWEEPDATA[!is.na(SWEEPDATA$Chromosome),]

#Rescale likelihood values to range from 0 to 1.
SWEEPDATA$RescaledLikelihood[!is.na(SWEEPDATA$Likelihood)]<-
  (SWEEPDATA$Likelihood[!is.na(SWEEPDATA$Likelihood)]-min(SWEEPDATA$Likelihood[!is.na(SWEEPDATA$Likelihood)]))/
  (max(SWEEPDATA$Likelihood[!is.na(SWEEPDATA$Likelihood)])-min(SWEEPDATA$Likelihood[!is.na(SWEEPDATA$Likelihood)]))

#Save concatenated data.
write.csv(SWEEPDATA,file=OUTPUTFILE,row.names=FALSE)

cat("\n")
cat("############################################################################\n")
cat("##SAVE CONTROL FILES (README, script, slurm):\n")

cat(paste0("############################################################################
Date: ",RUNDATE,"
Job ID: ",JOBID,"
Script: ",SCRIPTNAME,"
Input directory: ",INPUTDIR,"
Output file: ",OUTPUTFILE,"

"),file=paste0(OUTPUTLOCATION,"/README.txt"),append=TRUE)

cat(paste0("DIRECTORY ",INPUTDIR," PROCESSED.","\n"))

cat("\n")
cat("############################################################################\n")
cat(paste0("END OF R SCRIPT...",format(Sys.time(),"%Y%m%d%H%M%S\n")))
cat("############################################################################\n")
sink()
############################################################################
############################################################################
############################################################################