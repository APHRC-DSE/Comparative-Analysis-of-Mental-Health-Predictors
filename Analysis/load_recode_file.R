library(dplyr)
library(readxl)
library(tibble)

working_directory

## Reading the recode file sheet

recode_file <- read_excel_allsheets("mh_recode_file.xlsx")

study_details <- recode_file[["study"]]

tools_cutoff <- recode_file[["tools_cutoff"]]

university_rename_vars_df <- recode_file[["university_rename_vars"]] #df for renaming variable labels in university data

community_rename_vars_df <- recode_file[["community_rename_vars"]] #df for renaming variable labels in community data

selected_vars_df <- recode_file[["selected_vars"]] #df for choosing variables for analysis and plots

drop_selected_vars_df <- recode_file[["drop_selected_vars"]] #df for dropping analysis variables not needed for modelling

model_params_df <- recode_file[["model_params"]] #df for model pre-processing


## Creating a named vector to quickly assign the new variable name
university_new_variable_names <- university_rename_vars_df %>%
  dplyr::select(new_variable, new_names_janitor) %>%
  tidyr::drop_na() %>%
  tibble::deframe()

community_new_variable_names <- community_rename_vars_df %>%
  dplyr::select(new_variable, new_names_janitor) %>%
  tidyr::drop_na() %>%
  tibble::deframe()

## Creating a named vector to quickly assign the new variable labels
university_new_labels <- university_rename_vars_df %>%
  dplyr::select(new_variable, new_label) %>%
  tidyr::drop_na() %>%
  tibble::deframe()

community_new_labels <- community_rename_vars_df %>%
  dplyr::select(new_variable, new_label) %>%
  tidyr::drop_na() %>%
  tibble::deframe()


