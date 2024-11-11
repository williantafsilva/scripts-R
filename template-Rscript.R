#!/usr/bin/env Rscript
rm(list=ls()) #Clear environment.
ARGS=commandArgs(trailingOnly=TRUE)
RUNDATE=format(Sys.time(),"%Y%m%d%H%M%S")
SCRIPTNAME<-"template-Rscript.R" #?????
JOBID<-ARGS[1]
OUTPUTLOCATION<-normalizePath(ARGS[2])
sink(paste0(OUTPUTLOCATION,"/job",JOBID,".Rlog"),type=c("output","message"))
############################################################################
################################ R SCRIPT ##################################
############################################################################
#Author: Willian T.A.F. Silva (willian.silva@evobiolab.com).
############################################################################
#SCRIPT DESCRIPTION:

#Description:
#?????

#Input $1: Job ID.
#Input $2: Output location.
#Input $3: #?????
#Output: #?????

#Usage: 
#Rscript --vanilla template-Rscript.R <JOB ID> <OUTPUT LOCATION> <INPUT> #?????

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

#?????
library(R.utils)
library(ggplot2)
library(viridisLite)
library(tidyverse)
library(rehh)
library(gridExtra)

cat("\n")
cat("############################################################################\n")
cat("#DEFAULT SETTINGS:\n")

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

cat("\n")
cat("############################################################################\n")
cat("#SYSTEM CONTROL:\n")

cat("sessionInfo():\n")
sessionInfo()

cat("\n")
cat("############################################################################\n")
cat("#ACTIONS:\n")
cat("Processing:\n")

#?????
INPUTFILE<-normalizePath(ARGS[3])
INPUTFILENAME<-basename(INPUTFILE)
OUTPUTFILEPREFIX<-system(paste0("echo ",INPUTFILENAME,"| sed 's/-job[0-9].*$//'"),
                         intern=TRUE)
OUTPUTFILENAME<-paste0(INPUTFILENAME,".Rscripttemplate-job",JOBID,".txt")
OUTPUTFILE<-paste0(OUTPUTLOCATION,"/",OUTPUTFILENAME)

cat(paste0("INPUTFILE: ",INPUTFILE,"\n"))
cat(paste0("INPUTFILENAME: ",INPUTFILENAME,"\n"))
cat(paste0("OUTPUTLOCATION: ",OUTPUTLOCATION,"\n"))
cat(paste0("OUTPUTFILEPREFIX: ",OUTPUTFILEPREFIX,"\n"))
cat(paste0("OUTPUTFILENAME: ",OUTPUTFILENAME,"\n"))
cat(paste0("OUTPUTFILE: ",OUTPUTFILE,"\n"))

cat("\n")
cat(paste0("PROCESSING FILE: ",INPUTFILE,"\n"))
############################################################################
############################################################################
############################################################################


#?????
##Add your tasks here.


############################################################################
############################################################################
############################################################################
cat("\n")
cat("############################################################################\n")
cat("##SAVE CONTROL FILES (README, script, slurm):\n")

#?????
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