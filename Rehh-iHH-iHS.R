#!/usr/bin/env Rscript
rm(list=ls()) #Clear environment.
ARGS=commandArgs(trailingOnly=TRUE)
RUNDATE=format(Sys.time(),"%Y%m%d%H%M%S")
SCRIPTNAME<-"Rehh-iHH-iHS.R"
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
##Run the extended haplotype homozygosity (EHH) on an unphased VCF file using Rehh.

##Input $1: Job ID.
##Input $2: Output location.
##Input $3: Directory containing iHH data files (.rds) split by chromosome.
##Output: iHS data file (.csv).

##Usage: 
##Rscript --vanilla Rehh-iHH-iHS.R <JOB ID> <OUTPUT LOCATION> <INPUT DIRECTORY>

############################################################################

cat("############################################################################\n")
cat(paste0("STARTING R SCRIPT...",format(Sys.time(),"%Y%m%d%H%M%S\n")))
cat("############################################################################\n")

cat(paste0("Job ID: ",JOBID,"\n"))
cat("\n")
cat(paste0("Submission (Job ID ",JOBID,"): Rscript --vanilla ",SCRIPTNAME," ",paste(ARGS,collapse=" "),"\n"))
cat("\n")
cat("############################################################################\n")
cat("#LOAD LIBRARIES:\n")

library(R.utils)
library(ggplot2)
library(viridisLite)
library(tidyverse)
library(rehh)

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
OUTPUTFILENAME<-paste0(OUTPUTFILEPREFIX,".iHSdata-job",JOBID,".rds")
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
FILES<-FILES[grep("iHH-job",FILES)]
FILES<-FILES[grep(".rds",FILES)]

cat(paste0("Processing file: ",FILES[1],"\n"))
iHHFILE<-normalizePath(FILES[1])
iHH_DATA<-readRDS(iHHFILE)
for(C in 2:length(FILES)){ #for each chromosome.
  cat(paste0("Processing file ",FILES[C],".\n"))
  iHHFILE<-normalizePath(FILES[C])
  iHH_DATA_TMP<-readRDS(iHHFILE)
  iHH_DATA<-rbind(iHH_DATA,iHH_DATA_TMP)
}

#Sleep.
Sys.sleep(5)

#Calculate genome-wide iHS values
iHS_DATA<-ihh2ihs(iHH_DATA,
                  freqbin=0.025,
                  p.adjust.method="BH")

#Sleep.
Sys.sleep(5)

#Save data.
saveRDS(iHS_DATA,file=OUTPUTFILE)

#Sleep.
Sys.sleep(5)

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