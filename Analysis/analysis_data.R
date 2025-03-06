library(dplyr)
library(tidyr)

working_directory

## group variables 
### if empty vector use character()
analysis_vars_df <- selected_vars_df[selected_vars_df$select == "retain" & !is.na(selected_vars_df$select),]

outcome_vars <- analysis_vars_df$new_variable[analysis_vars_df$select_group  == "outcome" 
                                              & !is.na(analysis_vars_df$select_group)]
group_vars <- analysis_vars_df$new_variable[analysis_vars_df$select_group  == "group" 
                                              & !is.na(analysis_vars_df$select_group)]
socio_demo_vars <- analysis_vars_df$new_variable[analysis_vars_df$select_group  == "socio_demo" 
                                                 & !is.na(analysis_vars_df$select_group)]
socio_econ_vars <- analysis_vars_df$new_variable[analysis_vars_df$select_group  == "socio_econ" 
                                                 & !is.na(analysis_vars_df$select_group)]
behaviour_vars <- analysis_vars_df$new_variable[analysis_vars_df$select_group  == "behaviour" 
                                                 & !is.na(analysis_vars_df$select_group)]
ses_all_vars <- analysis_vars_df$new_variable[analysis_vars_df$select_group  == "ses pca" | 
                                                analysis_vars_df$select_group  == "ses" & !is.na(analysis_vars_df$select_group)]
gad7_vars <- analysis_vars_df$new_variable[analysis_vars_df$select_group  == "gad7" & !is.na(analysis_vars_df$select_group)]
phq9_vars <- analysis_vars_df$new_variable[analysis_vars_df$select_group  == "phq9" & !is.na(analysis_vars_df$select_group)]
psq_vars <- analysis_vars_df$new_variable[analysis_vars_df$select_group  == "psq" & !is.na(analysis_vars_df$select_group)]

university_only_vars <- analysis_vars_df$new_variable[analysis_vars_df$pos_site_type == "university_only" 
                                                      & !is.na(analysis_vars_df$pos_site_type)]

## make dataset with variables for descriptive and inferential statistics
df_analysis <- df_final%>%
  dplyr::select(any_of(c(outcome_vars, group_vars, socio_demo_vars, socio_econ_vars, behaviour_vars, ses_all_vars,
                         gad7_vars, phq9_vars, psq_vars)
                       )
                ) %>%
  tidyr::drop_na(any_of(outcome_vars))

analysis_report <- paste0(
  paste0(analysis_vars_df$new_variable,collapse=", ")," ", length(analysis_vars_df$new_variable)
  , " variables used for analysis" 
)
print(analysis_report)

none_analysis_report <- paste0(
  paste0(selected_vars_df$new_variable[selected_vars_df$select == "drop"],
         collapse=", ")," ", 
  length(selected_vars_df$new_variable[selected_vars_df$select == "drop"])
  , " variables not used for analysis"
)
print(none_analysis_report)

