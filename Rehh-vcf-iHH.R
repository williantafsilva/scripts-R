#!/usr/bin/env Rscript
rm(list=ls()) #Clear environment.
ARGS=commandArgs(trailingOnly=TRUE)
RUNDATE=format(Sys.time(),"%Y%m%d%H%M%S")
SCRIPTNAME<-"Rehh-vcf-iHH.R"
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
##Input $3: VCF file (.vcf.gz) split by chromosome.
##Input $4: Phased? (TRUE/FALSE).
##Output: iHH data file (.rds).

##Usage: 
##Rscript --vanilla Rehh-vcf-iHH.R <JOB ID> <OUTPUT LOCATION> <.vcf.gz FILE> <TRUE/FALSE>

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

INPUTFILE<-normalizePath(ARGS[3])
INPUTFILENAME<-basename(INPUTFILE)
PHASINGSTATUS<-ARGS[4]
OUTPUTFILEPREFIX<-system(paste0("echo ",INPUTFILENAME,"| sed 's/-job[0-9].*$//'"),
                         intern=TRUE)
if(is.na(PHASINGSTATUS)){PHASINGSTATUS<-FALSE} #If phasing status is not provided, assume unphased data.
if(PHASINGSTATUS){
   OUTPUTFILENAME<-paste0(OUTPUTFILEPREFIX,".phasediHH-job",JOBID,".rds")
}else{
   OUTPUTFILENAME<-paste0(OUTPUTFILEPREFIX,".unphasediHH-job",JOBID,".rds")
}
OUTPUTFILE<-paste0(OUTPUTLOCATION,"/",OUTPUTFILENAME)

cat(paste0("INPUTFILE: ",INPUTFILE,"\n"))
cat(paste0("INPUTFILENAME: ",INPUTFILENAME,"\n"))
cat(paste0("PHASINGSTATUS: ",PHASINGSTATUS,"\n"))
cat(paste0("OUTPUTLOCATION: ",OUTPUTLOCATION,"\n"))
cat(paste0("OUTPUTFILEPREFIX: ",OUTPUTFILEPREFIX,"\n"))
cat(paste0("OUTPUTFILENAME: ",OUTPUTFILENAME,"\n"))
cat(paste0("OUTPUTFILE: ",OUTPUTFILE,"\n"))

cat("\n")
cat(paste0("PROCESSING FILE: ",INPUTFILE,"\n"))

#Import VCF file.
cat("Converting data into an object of class haplohh (rehh::data2haplohh).\n")
HAPLODATA<-data2haplohh(hap_file=INPUTFILE,
                        polarize_vcf=FALSE, #Unpolarized data.
                        min_maf=0.0, #Filter data on a minor allele frequency or MAF.
                        vcf_reader="data.table",
                        verbose=TRUE)

cat("Computing EHH based statistics over a whole chromosome (rehh::scan_hh).\n")
#Calculate iHH statistics.
HAPLODATA_iHH<-scan_hh(HAPLODATA,
                        phased=PHASINGSTATUS, #Phased data?
                        polarized=FALSE) #Unpolarized data.

#Sleep.
Sys.sleep(5)

cat("Saving data.\n")
#Save data.
saveRDS(HAPLODATA_iHH,file=OUTPUTFILE)

#Sleep.
Sys.sleep(5)

cat("\n")
cat("############################################################################\n")
cat("##SAVE CONTROL FILES (README, script, slurm):\n")

cat(paste0("############################################################################
Date: ",RUNDATE,"
Job ID: ",JOBID,"
Script: ",SCRIPTNAME,"
Input file: ",INPUTFILE,"
Phasing status: ",PHASINGSTATUS,"
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