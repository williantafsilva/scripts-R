#!/usr/bin/env Rscript
rm(list=ls()) #Clear environment.
ARGS=commandArgs(trailingOnly=TRUE)
RUNDATE=format(Sys.time(),"%Y%m%d%H%M%S")
SCRIPTNAME<-"biomaRt-annotations-transcripts-gal7b.R"
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
#Get transcript annotations for a genomic region, given the chromosome, the start position 
#and the end position of the region of interest.

#Input $1: Job ID.
#Input $2: Output location.
#Input $3: Output file tag.
#Input $4: Chromosome (or comma-separated list of chromosome names; use | paste -sd,).
#Input $5: Start position (or comma-separated list of start positions; use | paste -sd,).
#Input $6: End position (or comma-separated list of end positions; use | paste -sd,).
#Input $7: Region-specific note to be included in the output table (or comma-separated list of notes; use | paste -sd,).
#Output: File with annotations (*.txt).

#Usage: 
#Rscript --vanilla biomaRt-annotations-transcripts-gal7b.R <JOB ID> <OUTPUT LOCATION> <FILE TAG> <CHROMOSOME> <START POSITION> <END POSITIONS>

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

library(tidyverse)
library(biomaRt)

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

OUTPUTFILETAG<-ARGS[3]
CHRLIST<-ARGS[4]
STARTPOSLIST<-ARGS[5]
ENDPOSLIST<-ARGS[6]
NOTES<-ARGS[7]

OUTPUTFILENAME<-paste0("biomartgal7b.annottranscripts",OUTPUTFILETAG,"-job",JOBID,".txt")
OUTPUTFILE<-paste0(OUTPUTLOCATION,"/",OUTPUTFILENAME)

cat(paste0("OUTPUTFILETAG: ",OUTPUTFILETAG,"\n"))
cat(paste0("CHRLIST: ",CHRLIST,"\n"))
cat(paste0("STARTPOSLIST: ",STARTPOSLIST,"\n"))
cat(paste0("ENDPOSLIST: ",ENDPOSLIST,"\n"))
cat(paste0("NOTES: ",NOTES,"\n"))
cat(paste0("OUTPUTLOCATION: ",OUTPUTLOCATION,"\n"))
cat(paste0("OUTPUTFILENAME: ",OUTPUTFILENAME,"\n"))
cat(paste0("OUTPUTFILE: ",OUTPUTFILE,"\n"))

cat("\n")
cat(paste0("PROCESSING INPUT DATA.\n"))
############################################################################
############################################################################
############################################################################

#Input data.
INPUTDATA<-data.frame(Chromosome=strsplit(CHRLIST,",")[[1]],
                      Start=strsplit(STARTPOSLIST,",")[[1]],
                      End=strsplit(ENDPOSLIST,",")[[1]],
                      Note=strsplit(NOTES,",")[[1]])
INPUTDATA$Chromosome<-gsub("Chr","",INPUTDATA$Chromosome) #Remove "Chr" from chromosome names.
INPUTDATA$Chromosome<-gsub("chr","",INPUTDATA$Chromosome) #Remove "chr" from chromosome names.
INPUTDATA

#Load Mart object with desired dataset.
ensembl_genes_gal7b<-useEnsembl(biomart="genes",dataset="ggallus_gene_ensembl") #Genes.

#Query database.
DATA_TRANSCRIPTS<-getBM(attributes=c("chromosome_name","start_position","end_position","strand",
                               "ensembl_transcript_id","external_transcript_name",
                               "transcript_start","transcript_end",
                               "transcript_length","transcript_biotype",
                               #"ensembl_peptide_id","protein_id",
                               "description"),
                  filters=c("chromosome_name","start","end"),
                  values=list("chromosome_name"=INPUTDATA$Chromosome[1],
                              "start"=INPUTDATA$Start[1],
                              "end"=INPUTDATA$End[1]),
                  mart=ensembl_genes_gal7b)
DATA_TRANSCRIPTS$note<-INPUTDATA$Note[1]

if(nrow(INPUTDATA)>1){
  for(i in 2:nrow(INPUTDATA)){
    TMP<-getBM(attributes=c("chromosome_name","start_position","end_position","strand",
                               "ensembl_transcript_id","external_transcript_name",
                               "transcript_start","transcript_end",
                               "transcript_length","transcript_biotype",
                               #"ensembl_peptide_id","protein_id",
                               "description"),
                  filters=c("chromosome_name","start","end"),
                  values=list("chromosome_name"=INPUTDATA$Chromosome[i],
                              "start"=INPUTDATA$Start[i],
                              "end"=INPUTDATA$End[i]),
                  mart=ensembl_genes_gal7b)
    TMP$note<-INPUTDATA$Note[i]
    DATA_TRANSCRIPTS<-rbind(DATA_TRANSCRIPTS,TMP)
  }
}

#Save formatted data.
write.table(DATA_TRANSCRIPTS,file=OUTPUTFILE,sep="\t",row.names=FALSE,col.names=TRUE)

############################################################################
############################################################################
############################################################################
cat("\n")
cat("############################################################################\n")
cat("##SAVE CONTROL FILES (README, script, slurm):\n")

cat(paste0("############################################################################
Date: ",RUNDATE,"
Job ID: ",JOBID,"
Script: ",SCRIPTNAME,"
Output file tag: ",OUTPUTFILETAG,"
Chromosome: ",CHRLIST,"
Start position: ",STARTPOSLIST,"
End position: ",ENDPOSLIST,"
Output file: ",OUTPUTFILE,"

"),file=paste0(OUTPUTLOCATION,"/README.txt"),append=TRUE)

cat(paste0("DATA PROCESSED.","\n"))

cat("\n")
cat("############################################################################\n")
cat(paste0("END OF R SCRIPT...",format(Sys.time(),"%Y%m%d%H%M%S\n")))
cat("############################################################################\n")
sink()
############################################################################
############################################################################
############################################################################