#!/usr/bin/env Rscript
rm(list=ls()) #Clear environment.
ARGS=commandArgs(trailingOnly=TRUE) 
RUNDATE=format(Sys.time(),"%Y%m%d%H%M%S")
SCRIPTNAME<-"plot-selectivesweep-analysis.R"
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
##Plot results of selective sweep analysis with SweepFinder2, SweeD, OmegaPlus and/or Rehh.

##Input $1: Job ID.
##Input $2: Output location.
##Input $3: File tag (group code).
##Input $4: NA or SweepFinder2 data file (*.SF2data-job*.csv).
##Input $5: NA or SweeD data file (*.SweeDdata-job*.csv).
##Input $6: NA or OmegaPlus data file (*.OmegaPlusdata-job*.csv).
##Input $7: NA or Rehh data file (*.Rehhdata-job*.csv).
##Output: Plots (*.pdf) and method comparison data (*.txt).

##Usage: 
##Rscript --vanilla plot-selectivesweep-analysis.R <JOB ID> <OUTPUT LOCATION> \
##<INPUT DIRECTORY> <> <> <> <>

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

library(ggplot2)
library(gridExtra)
library(tidyverse)
library(patchwork)
library(ggvenn)
library(qpdf)

source("https://github.com/williantafsilva/scripts-R/raw/refs/heads/main/basicRsettings.R")
source("https://github.com/williantafsilva/scripts-R/raw/refs/heads/main/function-closest_index.R")
source("https://github.com/williantafsilva/scripts-R/raw/refs/heads/main/function-plot_commonsigpoints.R")
source("https://github.com/williantafsilva/scripts-R/raw/refs/heads/main/function-plot_manhattan.R")

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

#Input files.
GROUPCODE<-ARGS[3]
if(ARGS[4]!="NA"){FILE_SWEEPFINDER2<-normalizePath(ARGS[4])}else{FILE_SWEEPFINDER2<-NA}
if(ARGS[5]!="NA"){FILE_SWEED<-normalizePath(ARGS[5])}else{FILE_SWEED<-NA}
if(ARGS[6]!="NA"){FILE_OMEGAPLUS<-normalizePath(ARGS[6])}else{FILE_OMEGAPLUS<-NA}
if(ARGS[7]!="NA"){FILE_REHH<-normalizePath(ARGS[7])}else{FILE_REHH<-NA}

#Output file.
OUTPUTFILE1NAME<-paste0("plots",GROUPCODE,".selectivesweepanalysis-job",JOBID,".pdf")
OUTPUTFILE2NAME<-paste0("methodcomparisondata",GROUPCODE,".selectivesweepanalysis-job",JOBID,".txt")
OUTPUTFILE1<-paste0(OUTPUTLOCATION,"/",OUTPUTFILE1NAME)
OUTPUTFILE2<-paste0(OUTPUTLOCATION,"/",OUTPUTFILE2NAME)

cat(paste0("FILE_SWEEPFINDER2: ",FILE_SWEEPFINDER2,"\n"))
cat(paste0("FILE_SWEED: ",FILE_SWEED,"\n"))
cat(paste0("FILE_OMEGAPLUS: ",FILE_OMEGAPLUS,"\n"))
cat(paste0("FILE_REHH: ",FILE_REHH,"\n"))
cat(paste0("OUTPUTLOCATION: ",OUTPUTLOCATION,"\n"))
cat(paste0("OUTPUTFILE1NAME: ",OUTPUTFILE1NAME,"\n"))
cat(paste0("OUTPUTFILE2NAME: ",OUTPUTFILE2NAME,"\n"))
cat(paste0("OUTPUTFILE1: ",OUTPUTFILE1,"\n"))
cat(paste0("OUTPUTFILE2: ",OUTPUTFILE2,"\n"))

cat("\n")
cat(paste0("PROCESSING FILES:",FILE_SWEEPFINDER2,", ",FILE_SWEED,", ",FILE_OMEGAPLUS," and ",FILE_REHH,".\n"))

#Import data and find threshold of significance (top 1% likelihood values for 
#SweepFinder2, SweeD and OmegaPlus, and p-value for Rehh).
METHODLIST<-c()
if(!is.na(FILE_SWEEPFINDER2)){
  METHODLIST<-c(METHODLIST,"SweepFinder2")
  DATA_SWEEPFINDER2<-read.csv(FILE_SWEEPFINDER2,header=TRUE) %>% drop_na(Location)
  THRESHOLD_SWEEPFINDER2<-top_frac(DATA_SWEEPFINDER2,0.01,Likelihood) %>%
    pull(Likelihood) %>% 
    min
}
if(!is.na(FILE_SWEED)){
  METHODLIST<-c(METHODLIST,"SweeD")
  DATA_SWEED<-read.csv(FILE_SWEED,header=TRUE) %>% drop_na(Location)
  THRESHOLD_SWEED<-top_frac(DATA_SWEED,0.01,Likelihood) %>%
    pull(Likelihood) %>% 
    min
}
if(!is.na(FILE_OMEGAPLUS)){
  METHODLIST<-c(METHODLIST,"OmegaPlus")
  DATA_OMEGAPLUS<-read.csv(FILE_OMEGAPLUS,header=TRUE) %>% drop_na(Location)
  THRESHOLD_OMEGAPLUS<-top_frac(DATA_OMEGAPLUS,0.01,Likelihood) %>%
    pull(Likelihood) %>% 
    min
}
if(!is.na(FILE_REHH)){
  METHODLIST<-c(METHODLIST,"Rehh")
  DATA_REHH<-read.csv(FILE_REHH,header=TRUE) %>% drop_na(Location)
  THRESHOLD_REHH<-(-log10(0.05))
}

#Compare methods.
if("SweepFinder2" %in% METHODLIST & "SweeD" %in% METHODLIST){
  #SweepFinder2 x SweeD.
  COMPARE_SWEEPFINDER2_SWEED<-data.frame(SweepFinder2_Chromosome=DATA_SWEEPFINDER2$Chromosome,
                                         SweepFinder2_Location=DATA_SWEEPFINDER2$Location,
                                         SweepFinder2_Likelihood=DATA_SWEEPFINDER2$Likelihood,
                                         SweeD_Chromosome=DATA_SWEED$Chromosome,
                                         SweeD_Location=DATA_SWEED$Location,
                                         SweeD_Likelihood=DATA_SWEED$Likelihood)
}

if("SweepFinder2" %in% METHODLIST & "OmegaPlus" %in% METHODLIST){
  #SweepFinder2 x OmegaPlus.
  COMPARE_SWEEPFINDER2_OMEGAPLUS<-data.frame(SweepFinder2_Chromosome=DATA_SWEEPFINDER2$Chromosome,
                                             SweepFinder2_Location=DATA_SWEEPFINDER2$Location,
                                             SweepFinder2_Likelihood=DATA_SWEEPFINDER2$Likelihood,
                                             OmegaPlus_Chromosome=DATA_OMEGAPLUS$Chromosome,
                                             OmegaPlus_Location=DATA_OMEGAPLUS$Location,
                                             OmegaPlus_Likelihood=DATA_OMEGAPLUS$Likelihood)
}

if("SweepFinder2" %in% METHODLIST & "Rehh" %in% METHODLIST){
  #SweepFinder2 x Rehh.
  COMPARE_SWEEPFINDER2_REHH<-data.frame(SweepFinder2_Chromosome=DATA_SWEEPFINDER2$Chromosome,
                                        SweepFinder2_Location=DATA_SWEEPFINDER2$Location,
                                        SweepFinder2_Likelihood=DATA_SWEEPFINDER2$Likelihood,
                                        Rehh_Chromosome=NA,
                                        Rehh_Location=NA,
                                        Rehh_LogPvalue=NA)
  #Remove NA values, leaving only locations with a P-value, but increasing the
  #distance between equivalent locations.
  #DATA_REHH<-DATA_REHH[!is.na(DATA_REHH$LogPvalue),] 
  #Find equivalent locations.
  for(CHR in unique(COMPARE_SWEEPFINDER2_REHH$SweepFinder2_Chromosome)){
    if(sum(DATA_REHH$Chromosome==CHR)>0){
      TMP_REHHDATA<-DATA_REHH[DATA_REHH$Chromosome==CHR,]
      COMPARE_CHRINDEX<-which(COMPARE_SWEEPFINDER2_REHH$SweepFinder2_Chromosome==CHR)
      for(i in COMPARE_CHRINDEX){
        LOC<-COMPARE_SWEEPFINDER2_REHH$SweepFinder2_Location[i]
        INDEX_CLOSEST<-closest_index(LOC,TMP_REHHDATA$Location)
        COMPARE_SWEEPFINDER2_REHH$Rehh_Chromosome[i]<-CHR
        COMPARE_SWEEPFINDER2_REHH$Rehh_Location[i]<-TMP_REHHDATA$Location[INDEX_CLOSEST]
        COMPARE_SWEEPFINDER2_REHH$Rehh_LogPvalue[i]<-TMP_REHHDATA$LogPvalue[INDEX_CLOSEST]
      }
    }
  }
}

if("SweeD" %in% METHODLIST & "OmegaPlus" %in% METHODLIST){
  #SweeD x OmegaPlus.
  COMPARE_SWEED_OMEGAPLUS<-data.frame(SweeD_Chromosome=DATA_SWEED$Chromosome,
                                      SweeD_Location=DATA_SWEED$Location,
                                      SweeD_Likelihood=DATA_SWEED$Likelihood,
                                      OmegaPlus_Chromosome=DATA_OMEGAPLUS$Chromosome,
                                      OmegaPlus_Location=DATA_OMEGAPLUS$Location,
                                      OmegaPlus_Likelihood=DATA_OMEGAPLUS$Likelihood)
}

if("SweeD" %in% METHODLIST & "Rehh" %in% METHODLIST){
  #SweeD x Rehh.
  COMPARE_SWEED_REHH<-data.frame(SweeD_Chromosome=DATA_SWEED$Chromosome,
                                 SweeD_Location=DATA_SWEED$Location,
                                 SweeD_Likelihood=DATA_SWEED$Likelihood,
                                 Rehh_Chromosome=NA,
                                 Rehh_Location=NA,
                                 Rehh_LogPvalue=NA)
  #Remove NA values, leaving only locations with a P-value, but increasing the
  #distance between equivalent locations.
  #DATA_REHH<-DATA_REHH[!is.na(DATA_REHH$LogPvalue),] 
  #Find equivalent locations.
  for(CHR in unique(COMPARE_SWEED_REHH$SweeD_Chromosome)){
    if(sum(DATA_REHH$Chromosome==CHR)>0){
      TMP_REHHDATA<-DATA_REHH[DATA_REHH$Chromosome==CHR,]
      COMPARE_CHRINDEX<-which(COMPARE_SWEED_REHH$SweeD_Chromosome==CHR)
      for(i in COMPARE_CHRINDEX){
        LOC<-COMPARE_SWEED_REHH$SweeD_Location[i]
        INDEX_CLOSEST<-closest_index(LOC,TMP_REHHDATA$Location)
        COMPARE_SWEED_REHH$Rehh_Chromosome[i]<-CHR
        COMPARE_SWEED_REHH$Rehh_Location[i]<-TMP_REHHDATA$Location[INDEX_CLOSEST]
        COMPARE_SWEED_REHH$Rehh_LogPvalue[i]<-TMP_REHHDATA$LogPvalue[INDEX_CLOSEST]
      }
    }
  }
}

if("OmegaPlus" %in% METHODLIST & "Rehh" %in% METHODLIST){
  #OmegaPlus x Rehh.
  COMPARE_OMEGAPLUS_REHH<-data.frame(OmegaPlus_Chromosome=DATA_OMEGAPLUS$Chromosome,
                                     OmegaPlus_Location=DATA_OMEGAPLUS$Location,
                                     OmegaPlus_Likelihood=DATA_OMEGAPLUS$Likelihood,
                                     Rehh_Chromosome=NA,
                                     Rehh_Location=NA,
                                     Rehh_LogPvalue=NA)
  #Remove NA values, leaving only locations with a P-value, but increasing the
  #distance between equivalent locations.
  #DATA_REHH<-DATA_REHH[!is.na(DATA_REHH$LogPvalue),] 
  #Find equivalent locations.
  for(CHR in unique(COMPARE_OMEGAPLUS_REHH$OmegaPlus_Chromosome)){
    if(sum(DATA_REHH$Chromosome==CHR)>0){
      TMP_REHHDATA<-DATA_REHH[DATA_REHH$Chromosome==CHR,]
      COMPARE_CHRINDEX<-which(COMPARE_OMEGAPLUS_REHH$OmegaPlus_Chromosome==CHR)
      for(i in COMPARE_CHRINDEX){
        LOC<-COMPARE_OMEGAPLUS_REHH$OmegaPlus_Location[i]
        INDEX_CLOSEST<-closest_index(LOC,TMP_REHHDATA$Location)
        COMPARE_OMEGAPLUS_REHH$Rehh_Chromosome[i]<-CHR
        COMPARE_OMEGAPLUS_REHH$Rehh_Location[i]<-TMP_REHHDATA$Location[INDEX_CLOSEST]
        COMPARE_OMEGAPLUS_REHH$Rehh_LogPvalue[i]<-TMP_REHHDATA$LogPvalue[INDEX_CLOSEST]
      }
    }
  }
}

#Plot Venn diagram with number of significant locations per method.
if("SweepFinder2" %in% METHODLIST){ #Use SweepFinder2 location as reference.
  REFMETHOD<-"SweepFinder2"
  ALLVALUES<-data.frame(
    Ref_Chr_Loc=paste0(DATA_SWEEPFINDER2$Chromosome,":",DATA_SWEEPFINDER2$Location),
    Ref_Chr_Loc_Method="SweepFinder2",
    Chromosome=DATA_SWEEPFINDER2$Chromosome)
  }else if("SweeD" %in% METHODLIST){ #Use SweeD location as reference.
    REFMETHOD<-"SweeD"
    ALLVALUES<-data.frame(
      Ref_Chr_Loc=paste0(DATA_SWEED$Chromosome,":",DATA_SWEED$Location),
      Ref_Chr_Loc_Method="SweeD",
      Chromosome=DATA_SWEED$Chromosome)
  }else if("OmegaPlus" %in% METHODLIST){ #Use OmegaPlus location as reference.
    REFMETHOD<-"OmegaPlus"
    ALLVALUES<-data.frame(
      Ref_Chr_Loc=paste0(DATA_OMEGAPLUS$Chromosome,":",DATA_OMEGAPLUS$Location),
      Ref_Chr_Loc_Method="OmegaPlus",
      Chromosome=DATA_OMEGAPLUS$Chromosome)
  }

ALLVALUES$SignificantMethods<-"Significant:"
SIGNIFICANT<-list()
if("SweepFinder2" %in% METHODLIST){
  ALLVALUES$Location_SweepFinder2<-DATA_SWEEPFINDER2$Location
  Likelihood_SweepFinder2<-DATA_SWEEPFINDER2$Likelihood
  Significant_SweepFinder2<-(Likelihood_SweepFinder2>=THRESHOLD_SWEEPFINDER2)
  ALLVALUES$SignificantMethods[Significant_SweepFinder2]<-paste0(
    ALLVALUES$SignificantMethods[Significant_SweepFinder2],"SweepFinder2")
  SIGNIFICANT$SweepFinder2<-ALLVALUES$Ref_Chr_Loc[Significant_SweepFinder2]
  if("Rehh" %in% METHODLIST){
    ALLVALUES$Location_Rehh_SweepFinder2<-COMPARE_SWEEPFINDER2_REHH$Rehh_Location
    LogPvalue_Rehh_SweepFinder2<-COMPARE_SWEEPFINDER2_REHH$Rehh_LogPvalue
    Significant_Rehh_SweepFinder2<-(LogPvalue_Rehh_SweepFinder2>=THRESHOLD_REHH & !is.na(LogPvalue_Rehh_SweepFinder2))
  }
}
if("SweeD" %in% METHODLIST){
  ALLVALUES$Location_SweeD<-DATA_SWEED$Location
  Likelihood_SweeD<-DATA_SWEED$Likelihood
  Significant_SweeD<-(Likelihood_SweeD>=THRESHOLD_SWEED)
  ALLVALUES$SignificantMethods[Significant_SweeD]<-paste0(
    ALLVALUES$SignificantMethods[Significant_SweeD],"SweeD")
  SIGNIFICANT$SweeD<-ALLVALUES$Ref_Chr_Loc[Significant_SweeD]
  if("Rehh" %in% METHODLIST){
    ALLVALUES$Location_Rehh_SweeD<-COMPARE_SWEED_REHH$Rehh_Location
    LogPvalue_Rehh_SweeD<-COMPARE_SWEED_REHH$Rehh_LogPvalue
    Significant_Rehh_SweeD<-(LogPvalue_Rehh_SweeD>=THRESHOLD_REHH & !is.na(LogPvalue_Rehh_SweeD))
  }
}
if("OmegaPlus" %in% METHODLIST){
  ALLVALUES$Location_OmegaPlus<-DATA_OMEGAPLUS$Location
  Likelihood_OmegaPlus<-DATA_OMEGAPLUS$Likelihood
  Significant_OmegaPlus<-(Likelihood_OmegaPlus>=THRESHOLD_OMEGAPLUS)
  ALLVALUES$SignificantMethods[Significant_OmegaPlus]<-paste0(
    ALLVALUES$SignificantMethods[Significant_OmegaPlus],"OmegaPlus")
  SIGNIFICANT$OmegaPlus<-ALLVALUES$Ref_Chr_Loc[Significant_OmegaPlus]
  if("Rehh" %in% METHODLIST){
    ALLVALUES$Location_Rehh_OmegaPlus<-COMPARE_OMEGAPLUS_REHH$Rehh_Location
    LogPvalue_Rehh_OmegaPlus<-COMPARE_OMEGAPLUS_REHH$Rehh_LogPvalue
    Significant_Rehh_OmegaPlus<-(LogPvalue_Rehh_OmegaPlus>=THRESHOLD_REHH & !is.na(LogPvalue_Rehh_OmegaPlus))
  }
}

if("Rehh" %in% METHODLIST){
  if(REFMETHOD=="SweepFinder2"){
    ALLVALUES$SignificantMethods[Significant_Rehh_SweepFinder2]<-paste0(
      ALLVALUES$SignificantMethods[Significant_Rehh_SweepFinder2],"Rehh")
    SIGNIFICANT$Rehh<-ALLVALUES$Ref_Chr_Loc[Significant_Rehh_SweepFinder2]
  }
  if(REFMETHOD=="SweeD"){
    ALLVALUES$SignificantMethods[Significant_Rehh_SweeD]<-paste0(
      ALLVALUES$SignificantMethods[Significant_Rehh_SweeD],"Rehh")
    SIGNIFICANT$Rehh<-ALLVALUES$Ref_Chr_Loc[Significant_Rehh_SweeD]
  }
  if(REFMETHOD=="OmegaPlus"){
    ALLVALUES$SignificantMethods[Significant_Rehh_OmegaPlus]<-paste0(
      ALLVALUES$SignificantMethods[Significant_Rehh_OmegaPlus],"Rehh")
    SIGNIFICANT$Rehh<-ALLVALUES$Ref_Chr_Loc[Significant_Rehh_OmegaPlus]
  }
}
ALLVALUES$SignificantMethods[ALLVALUES$SignificantMethods=="Significant:"]<-"Significant:NONE"
ALLVALUES$SignificantMethods<-gsub("Significant:","",ALLVALUES$SignificantMethods)

p.venndiagram<-ggvenn(
  SIGNIFICANT, 
  fill_color=c("blue","red","yellow","green"),
  #fill_color=c("#0073C2FF","#EFC000FF","#868686FF","#CD534CFF"),
  stroke_size=0.5,set_name_size=4,show_stats="c"
)

#Manhattan plots.
if("SweepFinder2" %in% METHODLIST){
  p.manhattan.sweepfinder2<-plot_manhattan(
    chrnamevector=DATA_SWEEPFINDER2$Chromosome,
    chrpositionvector=DATA_SWEEPFINDER2$xPosition,
    values=DATA_SWEEPFINDER2$Likelihood,
    abovethreshold=THRESHOLD_SWEEPFINDER2,
    belowthreshold=-Inf,
    colors=c("blue","red"),
    xlabel="Chromosome",
    ylabel="Likelihood",
    plottitle=paste0(GROUPCODE,": SweepFinder2"))
}else{p.manhattan.sweepfinder2<-ggplot()+theme_void()}
if("SweeD" %in% METHODLIST){
  p.manhattan.sweed<-plot_manhattan(
    chrnamevector=DATA_SWEED$Chromosome,
    chrpositionvector=DATA_SWEED$xPosition,
    values=DATA_SWEED$Likelihood,
    abovethreshold=THRESHOLD_SWEED,
    belowthreshold=-Inf,
    colors=c("blue","red"),
    xlabel="Chromosome",
    ylabel="Likelihood",
    plottitle=paste0(GROUPCODE,": SweeD"))
}else{p.manhattan.sweed<-ggplot()+theme_void()}
if("OmegaPlus" %in% METHODLIST){
  p.manhattan.omegaplus<-plot_manhattan(
    chrnamevector=DATA_OMEGAPLUS$Chromosome,
    chrpositionvector=DATA_OMEGAPLUS$xPosition,
    values=DATA_OMEGAPLUS$Likelihood,
    abovethreshold=THRESHOLD_OMEGAPLUS,
    belowthreshold=-Inf,
    colors=c("blue","red"),
    xlabel="Chromosome",
    ylabel="Likelihood",
    plottitle=paste0(GROUPCODE,": OmegaPlus"))
}else{p.manhattan.omegaplus<-ggplot()+theme_void()}
if("Rehh" %in% METHODLIST){
  DATA_REHH_PLOT<-DATA_REHH[
    c(which(DATA_REHH$LogPvalue>=THRESHOLD_REHH),sample(which(DATA_REHH$LogPvalue<THRESHOLD_REHH),10000,replace=FALSE)),]
  p.manhattan.rehh<-plot_manhattan(
    chrnamevector=DATA_REHH_PLOT$Chromosome,
    chrpositionvector=DATA_REHH_PLOT$xPosition,
    values=DATA_REHH_PLOT$LogPvalue,
    abovethreshold=THRESHOLD_REHH,
    belowthreshold=-Inf,
    colors=c("blue","red"),
    xlabel="Chromosome",
    ylabel="-Log10(P-value)",
    plottitle=paste0(GROUPCODE,": Rehh"))
}else{p.manhattan.rehh<-ggplot()+theme_void()}

p.manhattan.all<-
  p.manhattan.sweepfinder2+p.manhattan.sweed+
  p.manhattan.omegaplus+p.manhattan.rehh+
  plot_layout(ncol=2)

#Comparison plots.
if(length(METHODLIST)>1){
  
  #Plot common significant points.
  if("SweepFinder2" %in% METHODLIST & "SweeD" %in% METHODLIST){
    #SweepFinder2 x SweeD.
    p.commonsig.sweepfinder2.sweed<-plot_commonsigpoints(
      Xvalues=COMPARE_SWEEPFINDER2_SWEED$SweepFinder2_Likelihood,
      Yvalues=COMPARE_SWEEPFINDER2_SWEED$SweeD_Likelihood,
      Xthreshold=THRESHOLD_SWEEPFINDER2,
      Ythreshold=THRESHOLD_SWEED,
      xlabel="SweepFinder2 likelihood",
      ylabel="SweeD likelihood",
      plottitle=paste0(GROUPCODE,": SweepFinder2 x SweeD"))
  }else{p.commonsig.sweepfinder2.sweed<-ggplot()+theme_void()}
  if("SweepFinder2" %in% METHODLIST & "OmegaPlus" %in% METHODLIST){
    #SweepFinder2 x OmegaPlus.
    p.commonsig.sweepfinder2.omegaplus<-plot_commonsigpoints(
      Xvalues=COMPARE_SWEEPFINDER2_OMEGAPLUS$SweepFinder2_Likelihood,
      Yvalues=COMPARE_SWEEPFINDER2_OMEGAPLUS$OmegaPlus_Likelihood,
      Xthreshold=THRESHOLD_SWEEPFINDER2,
      Ythreshold=THRESHOLD_OMEGAPLUS,
      xlabel="SweepFinder2 likelihood",
      ylabel="OmegaPlus likelihood",
      plottitle=paste0(GROUPCODE,": SweepFinder2 x OmegaPlus"))
  }else{p.commonsig.sweepfinder2.omegaplus<-ggplot()+theme_void()}
  if("SweepFinder2" %in% METHODLIST & "Rehh" %in% METHODLIST){
    #SweepFinder2 x Rehh.
    p.commonsig.sweepfinder2.rehh<-plot_commonsigpoints(
      Xvalues=COMPARE_SWEEPFINDER2_REHH$SweepFinder2_Likelihood,
      Yvalues=COMPARE_SWEEPFINDER2_REHH$Rehh_LogPvalue,
      Xthreshold=THRESHOLD_SWEEPFINDER2,
      Ythreshold=THRESHOLD_REHH,
      xlabel="SweepFinder2 likelihood",
      ylabel="Rehh -log10(p-value)",
      plottitle=paste0(GROUPCODE,": SweepFinder2 x Rehh"))
  }else{p.commonsig.sweepfinder2.rehh<-ggplot()+theme_void()}
  if("SweeD" %in% METHODLIST & "OmegaPlus" %in% METHODLIST){
    #SweeD x OmegaPlus.
    p.commonsig.sweed.omegaplus<-plot_commonsigpoints(
      Xvalues=COMPARE_SWEED_OMEGAPLUS$SweeD_Likelihood,
      Yvalues=COMPARE_SWEED_OMEGAPLUS$OmegaPlus_Likelihood,
      Xthreshold=THRESHOLD_SWEED,
      Ythreshold=THRESHOLD_OMEGAPLUS,
      xlabel="SweeD likelihood",
      ylabel="OmegaPlus likelihood",
      plottitle=paste0(GROUPCODE,": SweeD x OmegaPlus"))
  }else{p.commonsig.sweed.omegaplus<-ggplot()+theme_void()}
  if("SweeD" %in% METHODLIST & "Rehh" %in% METHODLIST){
    #SweeD x Rehh.
    p.commonsig.sweed.rehh<-plot_commonsigpoints(
      Xvalues=COMPARE_SWEED_REHH$SweeD_Likelihood,
      Yvalues=COMPARE_SWEED_REHH$Rehh_LogPvalue,
      Xthreshold=THRESHOLD_SWEED,
      Ythreshold=THRESHOLD_REHH,
      xlabel="SweeD likelihood",
      ylabel="Rehh -log10(p-value)",
      plottitle=paste0(GROUPCODE,": SweeD x Rehh"))
  }else{p.commonsig.sweed.rehh<-ggplot()+theme_void()}
  if("OmegaPlus" %in% METHODLIST & "Rehh" %in% METHODLIST){
    #OmegaPlus x Rehh.
    p.commonsig.omegaplus.rehh<-plot_commonsigpoints(
      Xvalues=COMPARE_OMEGAPLUS_REHH$OmegaPlus_Likelihood,
      Yvalues=COMPARE_OMEGAPLUS_REHH$Rehh_LogPvalue,
      Xthreshold=THRESHOLD_OMEGAPLUS,
      Ythreshold=THRESHOLD_REHH,
      xlabel="OmegaPlus likelihood",
      ylabel="Rehh -log10(p-value)",
      plottitle=paste0(GROUPCODE,": OmegaPlus x Rehh"))
  }else{p.commonsig.omegaplus.rehh<-ggplot()+theme_void()}
  
  p.commonsig.all<-
    p.commonsig.sweepfinder2.sweed+p.commonsig.sweepfinder2.omegaplus+
    p.commonsig.sweed.omegaplus+p.commonsig.sweepfinder2.rehh+
    p.commonsig.sweed.rehh+p.commonsig.omegaplus.rehh+
    plot_layout(ncol=2)
  
  #Plot equivalent locations from different methods.
  if("SweepFinder2" %in% METHODLIST & "SweeD" %in% METHODLIST){
    #SweepFinder2 x SweeD.
    p.loc.sweepfinder2.sweed<-ggplot(data=COMPARE_SWEEPFINDER2_SWEED)+
      geom_abline(slope=1,intercept=0)+
      geom_point(aes(SweepFinder2_Location,SweeD_Location),
                 color="purple",
                 alpha=1,
                 shape=18,
                 size=1)+
      labs(x="Location (SweepFinder2)",y="Location (SweeD)")+
      ggtitle(paste0(GROUPCODE,": SweepFinder2 x SweeD"))+
      myggplottheme
  }else{p.loc.sweepfinder2.sweed<-ggplot()+theme_void()}
  if("SweepFinder2" %in% METHODLIST & "OmegaPlus" %in% METHODLIST){
    #SweepFinder2 x OmegaPlus.
    p.loc.sweepfinder2.omegaplus<-ggplot(data=COMPARE_SWEEPFINDER2_OMEGAPLUS)+
      geom_abline(slope=1,intercept=0)+
      geom_point(aes(SweepFinder2_Location,OmegaPlus_Location),
                 color="purple",
                 alpha=1,
                 shape=18,
                 size=1)+
      labs(x="Location (SweepFinder2)",y="Location (OmegaPlus)")+
      ggtitle(paste0(GROUPCODE,": SweepFinder2 x OmegaPlus"))+
      myggplottheme
  }else{p.loc.sweepfinder2.omegaplus<-ggplot()+theme_void()}
  if("SweepFinder2" %in% METHODLIST & "Rehh" %in% METHODLIST){
    #SweepFinder2 x Rehh.
    p.loc.sweepfinder2.rehh<-ggplot(data=COMPARE_SWEEPFINDER2_REHH)+
      geom_abline(slope=1,intercept=0)+
      geom_point(aes(SweepFinder2_Location,Rehh_Location),
                 color="purple",
                 alpha=1,
                 shape=18,
                 size=1)+
      labs(x="Location (SweepFinder2)",y="Location (Rehh)")+
      ggtitle(paste0(GROUPCODE,": SweepFinder2 x Rehh"))+
      myggplottheme
  }else{p.loc.sweepfinder2.rehh<-ggplot()+theme_void()}
  if("SweeD" %in% METHODLIST & "OmegaPlus" %in% METHODLIST){
    #SweeD x OmegaPlus.
    p.loc.sweed.omegaplus<-ggplot(data=COMPARE_SWEED_OMEGAPLUS)+
      geom_abline(slope=1,intercept=0)+
      geom_point(aes(SweeD_Location,OmegaPlus_Location),
                 color="purple",
                 alpha=1,
                 shape=18,
                 size=1)+
      labs(x="Location (SweeD)",y="Location (OmegaPlus)")+
      ggtitle(paste0(GROUPCODE,": SweeD x OmegaPlus"))+
      myggplottheme
  }else{p.loc.sweed.omegaplus<-ggplot()+theme_void()}
  if("SweeD" %in% METHODLIST & "Rehh" %in% METHODLIST){
    #SweeD x Rehh.
    p.loc.sweed.rehh<-ggplot(data=COMPARE_SWEED_REHH)+
      geom_abline(slope=1,intercept=0)+
      geom_point(aes(SweeD_Location,Rehh_Location),
                 color="purple",
                 alpha=1,
                 shape=18,
                 size=1)+
      labs(x="Location (SweeD)",y="Location (Rehh)")+
      ggtitle(paste0(GROUPCODE,": SweeD x Rehh"))+
      myggplottheme
  }else{p.loc.sweed.rehh<-ggplot()+theme_void()}
  if("OmegaPlus" %in% METHODLIST & "Rehh" %in% METHODLIST){
    #OmegaPlus x Rehh.
    p.loc.omegaplus.rehh<-ggplot(data=COMPARE_OMEGAPLUS_REHH)+
      geom_abline(slope=1,intercept=0)+
      geom_point(aes(OmegaPlus_Location,Rehh_Location),
                 color="purple",
                 alpha=1,
                 shape=18,
                 size=1)+
      labs(x="Location (OmegaPlus)",y="Location (Rehh)")+
      ggtitle(paste0(GROUPCODE,": OmegaPlus x Rehh"))+
      myggplottheme
  }else{p.loc.omegaplus.rehh<-ggplot()+theme_void()}
  
  p.loc.all<-
    p.loc.sweepfinder2.sweed+p.loc.sweepfinder2.omegaplus+
    p.loc.sweed.omegaplus+p.loc.sweepfinder2.rehh+
    p.loc.sweed.rehh+p.loc.omegaplus.rehh+
    plot_layout(ncol=2)
  
  #Plot distances between equivalent locations from different methods.
  if("SweepFinder2" %in% METHODLIST & "SweeD" %in% METHODLIST){
    #SweepFinder2 x SweeD.
    LOCDIFF<-data.frame(Index=1:nrow(COMPARE_SWEEPFINDER2_SWEED),
                        Distance=COMPARE_SWEEPFINDER2_SWEED$SweepFinder2_Location-
                          COMPARE_SWEEPFINDER2_SWEED$SweeD_Location)
    p.locdiff.sweepfinder2.sweed<-ggplot(data=LOCDIFF)+ 
      geom_hline(yintercept=0,color="black",linetype="dashed")+
      geom_point(aes(Index,Distance),
                 color="orange",
                 alpha=1,
                 shape=18,
                 size=1)+
      labs(x="Index",y="SweepFinder2 location - SweeD location")+
      ggtitle(paste0(GROUPCODE,": SweepFinder2 x SweeD"))+
      myggplottheme
  }else{p.locdiff.sweepfinder2.sweed<-ggplot()+theme_void()}
  if("SweepFinder2" %in% METHODLIST & "OmegaPlus" %in% METHODLIST){
    #SweepFinder2 x OmegaPlus.
    LOCDIFF<-data.frame(Index=1:nrow(COMPARE_SWEEPFINDER2_OMEGAPLUS),
                        Distance=COMPARE_SWEEPFINDER2_OMEGAPLUS$SweepFinder2_Location-
                          COMPARE_SWEEPFINDER2_OMEGAPLUS$OmegaPlus_Location)
    p.locdiff.sweepfinder2.omegaplus<-ggplot(data=LOCDIFF)+ 
      geom_hline(yintercept=0,color="black",linetype="dashed")+
      geom_point(aes(Index,Distance),
                 color="orange",
                 alpha=1,
                 shape=18,
                 size=1)+
      labs(x="Index",y="SweepFinder2 location - OmegaPlus location")+
      ggtitle(paste0(GROUPCODE,": SweepFinder2 x OmegaPlus"))+
      myggplottheme
  }else{p.locdiff.sweepfinder2.omegaplus<-ggplot()+theme_void()}
  if("SweepFinder2" %in% METHODLIST & "Rehh" %in% METHODLIST){
    #SweepFinder2 x Rehh.
    LOCDIFF<-data.frame(Index=1:nrow(COMPARE_SWEEPFINDER2_REHH),
                        Distance=COMPARE_SWEEPFINDER2_REHH$SweepFinder2_Location-
                          COMPARE_SWEEPFINDER2_REHH$Rehh_Location)
    p.locdiff.sweepfinder2.rehh<-ggplot(data=LOCDIFF)+ 
      geom_hline(yintercept=0,color="black",linetype="dashed")+
      geom_point(aes(Index,Distance),
                 color="orange",
                 alpha=1,
                 shape=18,
                 size=1)+
      labs(x="Index",y="SweepFinder2 location - Rehh location")+
      ggtitle(paste0(GROUPCODE,": SweepFinder2 x Rehh"))+
      myggplottheme
  }else{p.locdiff.sweepfinder2.rehh<-ggplot()+theme_void()}
  if("SweeD" %in% METHODLIST & "OmegaPlus" %in% METHODLIST){
    #SweeD x OmegaPlus.
    LOCDIFF<-data.frame(Index=1:nrow(COMPARE_SWEED_OMEGAPLUS),
                        Distance=COMPARE_SWEED_OMEGAPLUS$SweeD_Location-
                          COMPARE_SWEED_OMEGAPLUS$OmegaPlus_Location)
    p.locdiff.sweed.omegaplus<-ggplot(data=LOCDIFF)+ 
      geom_hline(yintercept=0,color="black",linetype="dashed")+
      geom_point(aes(Index,Distance),
                 color="orange",
                 alpha=1,
                 shape=18,
                 size=1)+
      labs(x="Index",y="SweeD location - OmegaPlus location")+
      ggtitle(paste0(GROUPCODE,": SweeD x OmegaPlus"))+
      myggplottheme
  }else{p.locdiff.sweed.omegaplus<-ggplot()+theme_void()}
  if("SweeD" %in% METHODLIST & "Rehh" %in% METHODLIST){
    #SweeD x Rehh.
    LOCDIFF<-data.frame(Index=1:nrow(COMPARE_SWEED_REHH),
                        Distance=COMPARE_SWEED_REHH$SweeD_Location-
                          COMPARE_SWEED_REHH$Rehh_Location)
    p.locdiff.sweed.rehh<-ggplot(data=LOCDIFF)+ 
      geom_hline(yintercept=0,color="black",linetype="dashed")+
      geom_point(aes(Index,Distance),
                 color="orange",
                 alpha=1,
                 shape=18,
                 size=1)+
      labs(x="Index",y="SweeD location - Rehh location")+
      ggtitle(paste0(GROUPCODE,": SweeD x Rehh"))+
      myggplottheme
  }else{p.locdiff.sweed.rehh<-ggplot()+theme_void()}
  if("OmegaPlus" %in% METHODLIST & "Rehh" %in% METHODLIST){
    #OmegaPlus x Rehh.
    LOCDIFF<-data.frame(Index=1:nrow(COMPARE_OMEGAPLUS_REHH),
                        Distance=COMPARE_OMEGAPLUS_REHH$OmegaPlus_Location-
                          COMPARE_OMEGAPLUS_REHH$Rehh_Location)
    p.locdiff.omegaplus.rehh<-ggplot(data=LOCDIFF)+ 
      geom_hline(yintercept=0,color="black",linetype="dashed")+
      geom_point(aes(Index,Distance),
                 color="orange",
                 alpha=1,
                 shape=18,
                 size=1)+
      labs(x="Index",y="OmegaPlus location - Rehh location")+
      ggtitle(paste0(GROUPCODE,": OmegaPlus x Rehh"))+
      myggplottheme
  }else{p.locdiff.omegaplus.rehh<-ggplot()+theme_void()}
  
  p.locdiff.all<-
    p.locdiff.sweepfinder2.sweed+p.locdiff.sweepfinder2.omegaplus+
    p.locdiff.sweed.omegaplus+p.locdiff.sweepfinder2.rehh+
    p.locdiff.sweed.rehh+p.locdiff.omegaplus.rehh+
    plot_layout(ncol=2)
  
}

#Save plots.
if(length(METHODLIST)==1){
  ggsave(paste0(OUTPUTLOCATION,"/tmp-plot1-job",JOBID,".pdf"),
         plot=p.venndiagram,width=15,height=15,units="cm")
  ggsave(paste0(OUTPUTLOCATION,"/tmp-plot2-job",JOBID,".pdf"),
         plot=p.manhattan.all,width=50,height=30,units="cm")
}
if(length(METHODLIST)>1){
  ggsave(paste0(OUTPUTLOCATION,"/tmp-plot1-job",JOBID,".pdf"),
         plot=p.venndiagram,width=15,height=15,units="cm")
  ggsave(paste0(OUTPUTLOCATION,"/tmp-plot2-job",JOBID,".pdf"),
         plot=p.manhattan.all,width=50,height=30,units="cm")
  ggsave(paste0(OUTPUTLOCATION,"/tmp-plot3-job",JOBID,".pdf"),
         plot=p.commonsig.all,width=30,height=45,units="cm")
  ggsave(paste0(OUTPUTLOCATION,"/tmp-plot4-job",JOBID,".pdf"),
         plot=p.loc.all,width=30,height=45,units="cm")
  ggsave(paste0(OUTPUTLOCATION,"/tmp-plot5-job",JOBID,".pdf"),
         plot=p.locdiff.all,width=30,height=45,units="cm")
}

pdf_combine(
  input=list.files(path=OUTPUTLOCATION,
                   full.names=TRUE,
                   pattern=paste0("tmp-plot*.*-job",JOBID,".pdf")),
  output=OUTPUTFILE1)

file.remove(list.files(path=OUTPUTLOCATION,
                       full.names=TRUE,
                       pattern=paste0("tmp-plot*.*-job",JOBID,".pdf")))

write.table(ALLVALUES,file=OUTPUTFILE2,sep="\t",row.names=FALSE,col.names=TRUE)

cat("\n")
cat("############################################################################\n")
cat("##SAVE CONTROL FILES (README, script, slurm):\n")

cat(paste0("############################################################################
Date: ",RUNDATE,"
Job ID: ",JOBID,"
Script: ",SCRIPTNAME,"
File tag: ",GROUPCODE,"
SweepFinder2 file: ",FILE_SWEEPFINDER2,"
SweeD file: ",FILE_SWEED,"
OmegaPlus file: ",FILE_OMEGAPLUS,"
Rehh file: ",FILE_REHH,"
Output file: ",OUTPUTFILE1,"
Output file: ",OUTPUTFILE2,"

"),file=paste0(OUTPUTLOCATION,"/README.txt"),append=TRUE)

cat(paste0("FILES:",FILE_SWEEPFINDER2,", ",FILE_SWEED,", ",FILE_OMEGAPLUS," and ",FILE_REHH," PROCESSED.\n"))

cat("\n")
cat("############################################################################\n")
cat(paste0("END OF R SCRIPT...",format(Sys.time(),"%Y%m%d%H%M%S\n")))
cat("############################################################################\n")
sink()
############################################################################
############################################################################
############################################################################