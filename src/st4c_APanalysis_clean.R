## Loading packages -----------

library(tidyverse)
library(lubridate)
library(stringr)
library(stargazer)
library(rsq)
library(reshape2)
library(zoo)
library(lubridate)
library(broom)
library(nlme)
library(moments)
library(psych)
library(zeallot)
source("utilities_R.R")
setwd("~/Documents/PhD (local)/Research/Github/KnowledgeKRisk_10Ks/src")
start_time <- Sys.time()
# Disable linter warnings:
# nolint start

## Basic configurations -----------
modelname = "dicfullmc10thr10defnob40noa0_8_4t"
quantiles = 4
dir.create(file.path("/Users/pedrovallocci/Documents/PhD (local)/Research/Github/KnowledgeKRisk_10Ks/text/", modelname), showWarnings = FALSE)
figfolder = paste0("/Users/pedrovallocci/Documents/PhD (local)/Research/Github/KnowledgeKRisk_10Ks/text/", modelname, "/") # nolint # nolint
print_kurtosis = FALSE

## Loading all csvs -----------
patent_ik_orig <- read.csv("/Users/pedrovallocci/Documents/PhD (local)/Research/Github/KnowledgeKRisk_10Ks/data/KPSS_2020_public.csv")
amazon_nov01_short <- read.csv("/Users/pedrovallocci/Documents/PhD (local)/Research/Github/KnowledgeKRisk_10Ks/data/amazon_nov01_short.csv")
cequity_mapper <- read.csv("~/Documents/PhD (local)/Research/By Topic/Measuring knowledge capital risk/input/cequity_mapper.csv")
ff3fw_orig = read.csv("~/Documents/PhD (local)/Research/By Topic/Measuring knowledge capital risk/input/ff3fw.csv")
ff5fw_orig = read.csv("~/Documents/PhD (local)/Research/By Topic/Measuring knowledge capital risk/input/ff5fw.csv") 
ff3fm_orig = read.csv("~/Documents/PhD (local)/Research/By Topic/Measuring knowledge capital risk/input/FF3F.csv")
ff5fm_orig = read.csv("~/Documents/PhD (local)/Research/By Topic/Measuring knowledge capital risk/input/FF5F.csv") 
linkt_orig <- read.csv("~/Documents/PhD (local)/Research/By Topic/Measuring knowledge capital risk/input/CRSP-Compustat Merged Database - Linking Table.csv")
load("~/Documents/PhD (local)/Research/By Topic/Measuring knowledge capital risk/input/comp_funda2.Rdata")
peterstaylor <- read.csv("~/Documents/PhD (local)/Research/By Topic/Measuring knowledge capital risk/input/peterstaylor.csv")
skilldata_orig <- read_csv("~/Documents/PhD (local)/Research/By Topic/Measuring knowledge capital risk/input/belo_labor_skill_data.csv")
topic_map_orig <- read.csv(paste0("~/Documents/PhD (local)/Research/By Topic/Measuring knowledge capital risk/output/", modelname, "/topic_map_2006_2022.csv"))
load("/Users/pedrovallocci/Documents/PhD (local)/Research/By Topic/Measuring knowledge capital risk/input/stoxwe_post2005short.Rdata") 
stoxwe_orig = stoxwe_post2005short
load("/Users/pedrovallocci/Documents/PhD (local)/Research/Github/KnowledgeKRisk_10Ks/data/stoxmo_post2000short.Rdata")
load("/Users/pedrovallocci/Documents/PhD (local)/Research/By Topic/Measuring knowledge capital risk/input/stoxda_post2005veryshort.Rdata")
comparison_measures <- read.csv("~/Documents/PhD (local)/Research/Github/KnowledgeKRisk_10Ks/data/comparison_measures.csv")
stoxda_orig = stoxwe_post2005short
stoxmo_orig = stoxmo_post2000short ## Defining source of stocks information
#cequity_mapper = redo_equity_mapper(comp_funda2, figfolder) ## Recreating equity mapper. May be commented for speed

## Cleaning dataframes --------
print("Cleaning dataframes.")
linkt = clean_linkt_orig(linkt_orig)
end_time <- Sys.time()
print(paste("Execution time: ", end_time-start_time))

patent_ik = clean_patent_ik_orig(patent_ik_orig, linkt)
c(ff3fm, ff5fm, ff3fw, ff5fw) %<-% cleanff_all()
skilldata = clean_skilldata(skilldata_orig)
compustat_pt = clean_compustat(comp_funda2, peterstaylor)

# CIKs are not unique -- they may map to multiple PERMNOs. May need to consolidate later.
topic_map_unlabeled = create_topic_map_unlabeled(topic_map_orig, linkt, skilldata, patent_ik, compustat_pt)
topic_map_labeled = label_table_map(modelname)

topic_map = understand_topics(topic_map_labeled, labels, quantiles, figfolder) 
c(eret_mo, stoxmo_with_pfs) %<-% create_eret_mo_panel_ff5(stoxmo_orig, cequity_mapper, topic_map, "pf36_name")

c(first_stage_rollwin_mo, second_stage_rollwin_mo) %<-% fama_macbeth(12*5, "pf36_name")

c(eret_we, stoxwe_with_pfs) %<-% create_eret_we_panel_ff5(stoxwe_orig, cequity_mapper, topic_map, "pf36_name")

c(eret_we, stoxwe_with_pfs) %<-% create_eret_we_panel(stoxwe_orig, cequity_mapper, topic_map, ff3fw, "pf36_name")
first_stage2 = first_stage(eretw ~ kkrhml + HML + SMB + Mkt.RF, eret_we, "pf36_name")
second_stage_ols = lm(formula3ff <- eretw ~ kkrhml + HML + SMB + Mkt.RF, data = first_stage2)
second_stage_wls_nokk = lm(formula3ff <- eretw ~ HML + SMB + Mkt.RF, data = first_stage2, weights = first_stage2$sigmae)
second_stage_wls = lm(formula3ff <- eretw ~ kkrhml + HML + SMB + Mkt.RF, data = first_stage2, weights = first_stage2$sigmae)

latex_table <- stargazer(second_stage_wls, second_stage_wls_nokk, title = "Regression Summary", align = TRUE, out = paste0(figfolder, "summary_table.tex"), dep.var.labels = c("Average returns"))

plot_returns()
descriptive_statistics(topic_map, figfolder)
amazon_graph(amazon_nov01_short, figfolder) ## Creating Amazon graph for motivation
filecounter(figfolder)## Create filecounts.tex
stargaze_comparison(comparison_measures, figfolder) ## Create Stargazer comparison of measures

if(print_kurtosis){
  plot_kurtosis(stoxda_orig, cequity_mapper, topic_map)
}
