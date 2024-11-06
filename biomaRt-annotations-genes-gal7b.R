#!/usr/bin/env Rscript
rm(list=ls()) #Clear environment.
ARGS=commandArgs(trailingOnly=TRUE)
RUNDATE=format(Sys.time(),"%Y%m%d%H%M%S")
SCRIPTNAME<-"biomaRt-annotations-genes-gal7b.R"
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
#Get gene annotations for a genomic region, given the chromosome, the start position 
#and the end position of the region of interest.

#Input $1: Job ID.
#Input $2: Output location.
#Input $3: Output file tag.
#Input $4: Chromosome (or comma-separated vector of chromosome names).
#Input $5: Start position (or comma-separated vector of start positions).
#Input $6: End position (or comma-separated vector of end positions).
#Output: File with annotations (*.csv).

#Usage: 
#Rscript --vanilla template-Rscript.R <JOB ID> <OUTPUT LOCATION> <FILE TAG> <CHROMOSOME> <START POSITION> <END POSITIONS>

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

OUTPUTFILENAME<-paste0("biomartgal7b.annotgenes",OUTPUTFILETAG,"-job",JOBID,".csv")
OUTPUTFILE<-paste0(OUTPUTLOCATION,"/",OUTPUTFILENAME)

cat(paste0("OUTPUTFILETAG: ",OUTPUTFILETAG,"\n"))
cat(paste0("CHRLIST: ",CHRLIST,"\n"))
cat(paste0("STARTPOSLIST: ",STARTPOSLIST,"\n"))
cat(paste0("ENDPOSLIST: ",ENDPOSLIST,"\n"))
cat(paste0("OUTPUTLOCATION: ",OUTPUTLOCATION,"\n"))
cat(paste0("OUTPUTFILENAME: ",OUTPUTFILENAME,"\n"))
cat(paste0("OUTPUTFILE: ",OUTPUTFILE,"\n"))

cat("\n")
cat(paste0("PROCESSING INPUT DATA.\n"))
############################################################################
############################################################################
############################################################################

#Input data.
INPUTDATA<-data.frame(Chromosome=c(CHRLIST),
                      Start=c(STARTPOSLIST),
                      End=c(ENDPOSLIST))
INPUTDATA$Chromosome<-gsub("Chr","",INPUTDATA$Chromosome) #Remove "Chr" from chromosome names.
INPUTDATA$Chromosome<-gsub("chr","",INPUTDATA$Chromosome) #Remove "chr" from chromosome names.

#List available types of annotation.
#listEnsembl() 
#ensembl_genes<-useEnsembl(biomart="genes") #Genes.
#ensembl_snps<-useEnsembl(biomart="snps") #SNPs.

#List available datasets.
#listDatasets(ensembl_genes)
#searchDatasets(mart=ensembl_genes,pattern="gallus") #Genes.
#listDatasets(ensembl_snps)
#searchDatasets(mart=ensembl_snps,pattern="gallus") #SNPs.

#Update Mart object with desired dataset.
ensembl_genes_gal7b<-useEnsembl(biomart="genes",dataset="ggallus_gene_ensembl") #Genes.
#ensembl_snps_gal7b<-useEnsembl(biomart="snps",dataset="ggallus_snp") #SNPs.

#List filters and attributes
#listFilters(ensembl_genes_gal7b) #List filters.
#listAttributes(ensembl_genes_gal7b) #List attributes.
#listFilters(ensembl_snps_gal7b) #List filters.
#listAttributes(ensembl_snps_gal7b) #List attributes.

#Search for specific filters and attributes.
#searchFilters(mart=ensembl_genes_gal7b,pattern=".*chromosome.*")
#searchFilters(mart=ensembl_genes_gal7b,pattern=".*start.*")
#searchFilters(mart=ensembl_genes_gal7b,pattern=".*end.*")
#searchAttributes(mart=ensembl_genes_gal7b,pattern=".*end.*")
#searchAttributes(mart=ensembl_genes_gal7b,pattern=".*id.*")
#searchAttributes(mart=ensembl_genes_gal7b,pattern=".*name.*")
#listAttributes(ensembl_genes_gal7b,page="feature_page")
#listAttributes(ensembl_genes_gal7b,page="sequences")
#listAttributes(ensembl_snps_gal7b,page="snp")

#List filter options.
#listFilterOptions(mart=ensembl_genes_gal7b,filter="chromosome_name") 
#listFilterOptions(mart=ensembl_snps_gal7b,filter="chr_name") 

#Query gene database.
DATA_GENES<-getBM(attributes=c("chromosome_name","start_position","end_position","strand",
                               "ensembl_gene_id","external_gene_name","gene_biotype",
                               #"ensembl_transcript_id","external_transcript_name",
                               #"transcript_start","transcript_end",
                               #"transcript_length","transcript_biotype",
                               #"ensembl_peptide_id","protein_id",
                               "description"),
                  filters=c("chromosome_name","start","end"),
                  values=list("chromosome_name"=INPUTDATA$Chromosome,
                              "start"=INPUTDATA$Start,
                              "end"=INPUTDATA$End),
                  mart=ensembl_genes_gal7b)

#Query gene database.
#DATA_TRANSCRIPTS<-getBM(attributes=c("chromosome_name","start_position","end_position","strand",
#                               #"ensembl_gene_id","external_gene_name","gene_biotype",
#                               "ensembl_transcript_id","external_transcript_name",
#                               "transcript_start","transcript_end",
#                               "transcript_length","transcript_biotype",
#                               #"ensembl_peptide_id","protein_id",
#                               "description"),
#                  filters=c("chromosome_name","start","end"),
#                  values=list("chromosome_name"=INPUTDATA$Chromosome,
#                              "start"=INPUTDATA$Start,
#                              "end"=INPUTDATA$End),
#                  mart=ensembl_genes_gal7b)

#Query SNPs database.
#DATA_SNPS<-getBM(attributes=c("chr_name","chrom_start","chrom_strand","refsnp_id",
#                              "allele","allele_1","ensembl_gene_stable_id","ensembl_type"),
#                 filters=c("chr_name","start","end"),
#                 values=list("chr_name"=INPUTDATA$Chromosome,
#                             "start"=INPUTDATA$Start,
#                             "end"=INPUTDATA$End),
#                 mart=ensembl_snps_gal7b)
#DATA_SNPS

#Save formatted data.
write.csv(DATA_GENES,file=OUTPUTFILE,row.names=FALSE,col.names=TRUE,sep=",")
#write.csv(DATA_TRANSCRIPTS,file=OUTPUTFILE,row.names=FALSE,col.names=TRUE,sep=",")
#write.csv(DATA_SNPS,file=OUTPUTFILE,row.names=FALSE,col.names=TRUE,sep=",")

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