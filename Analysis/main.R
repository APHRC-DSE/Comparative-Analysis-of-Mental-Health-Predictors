######################################################################
### Restart R
#.rs.restartR()

### Start with a clean environment by removing objects in workspace
rm(list=ls())

### Setting work directory
working_directory <- base::setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#working_directory <- base::setwd(".")

### Load Rdata
Rdata_files <- list.files(path = working_directory, pattern = "*.RData", full.names = T)

if ( length(Rdata_files) >0) {
  invisible(lapply(Rdata_files,load,.GlobalEnv))
} else {
  paste(c(".RData files", "do not exist"), collapse = " ")
}

### Install required packages
source("requirements.R")

### helper/customized functions
source("helperfuns_read_excel_sheets.R")
source("helperfuns_gt_summary_themes.R")
source("helperfuns_table_summary_categorical.R")
source("helperfuns_ggplot_themes.R")
source("helperfuns_simple_plots.R")
source("helperfuns_effect_size.R")

### Load data 
source("load_data_local.R")

### Load recode file
source("load_recode_file.R")

### Data cleaning
source("cleaning_university.R")
source("cleaning_community.R")
source("merge_data.R")

### Create SES based on PCA
source("ses_pca.R")

### Select variables for descriptive and inferential analysis
source("analysis_data.R")

######################################################################

### Descriptive and Inferential stats
source("descriptive_inferential_categorical_stats.R")

### Effect Size stats
source("effect_size_categorical_stats.R")

### reliability and correlation stats
source("reliability_correlation_tools_university_stats.R")
source("reliability_correlation_tools_community_stats.R")

### Save stats output
source("save_descriptive_inferential_categorical_output.R")
source("save_effect_size_categorical_output.R")
source("save_reliability_correlation_tools_university_output.R")
source("save_reliability_correlation_tools_community_output.R")

### Descriptive plots
source("descriptive_plots.R")

######################################################################

## Model preparation

### Select variables required for modelling and model formula
source("modelling_data.R")

######################################################################

### Crude Odds Ratio and Adjusted Odds Ratio
source("CoR_categorical_analysis.R")
source("AoR_categorical_analysis.R")
source("AoR_categorical_analysis_plots.R")

### Save Crude Odds Ratio and Adjusted Odds Ratio output
source("save_odds_ratio_output.R")

######################################################################

### Predictive Modelling - AUC and Variable Importance
source("auc_categorical_modelling.R")
source("varimp_categorical_modelling.R")

######################################################################

## Save workspace at the end without working directory path

save(list = ls(all.names = TRUE)[!ls(all.names = TRUE) %in% c("working_directory", "mainDir", "subDir_data", "data_Dir",
                                                              "subDir_output", "output_Dir","Rdata_files"
                                                              )],
     file = "mh_study.RData",
     envir = .GlobalEnv #parent.frame()
     )

######################################################################

## Run all files in Rstudio
source("main.R")

######################################################################

