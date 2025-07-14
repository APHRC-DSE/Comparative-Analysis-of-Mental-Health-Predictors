library(dplyr)
library(janitor)
library(labelled)
library(writexl)

working_directory

## Merging to get one dataset

### Comparison of dataframes to indicate whether they will successfully bind together by rows

df_comparison_rows_all <- janitor::compare_df_cols(df_university_clean, df_community_clean,
                                                   return = "all"
                                                   )

df_comparison_rows_match <- janitor::compare_df_cols(df_university_clean, df_community_clean,
                                                     return = "match"
                                                     )

df_comparison_rows_mismatch <- janitor::compare_df_cols(df_university_clean, df_community_clean,
                                                        return = "mismatch"
                                                        )

writexl::write_xlsx(list(comparison_all = df_comparison_rows_all,
                         comparison_match = df_comparison_rows_match,
                         comparison_mismatch = df_comparison_rows_mismatch
                         ),
                    path = base::file.path(output_Dir, paste0("bindrows_data_report.xlsx") )
                    )

### Check if output is true (Do the data.frames have the same columns & types)
print(janitor::compare_df_cols_same(df_university_clean, df_community_clean
                                   )
      )

df_clean <- dplyr::bind_rows( df_university_clean, df_community_clean,
                                     ) %>%
  dplyr::mutate(gad7_anxiety_severity = if_else(gad7_total_score < 5, "None/minimal (0-4)",
                                               if_else(gad7_total_score < 10, "Mild (5-9)",
                                                      if_else(gad7_total_score < 15, "Moderate (10-14)", "Severe (15-21)"
                                                             )
                                                      )
                                               )
                , phq9_depression_severity = if_else(phq9_totalscore < 5, "None/minimal (0-4)",
                                                     if_else(phq9_totalscore < 10, "Mild (5-9)",
                                                             if_else(phq9_totalscore < 15, "Moderate (10-14)",
                                                                     if_else(phq9_totalscore < 20, "Moderately severe (15-19)",
                                                                             "Severe (20-27)"
                                                                             )
                                                                     )
                                                             )
                                                     )
                  ) %>%
  labelled::set_variable_labels(education = "Level of Education"
                                , site = "Study Population"
                                , gad7_anxiety_severity = "GAD-7 Anxiety Severity"
                                , phq9_depression_severity = "PHQ-9 Depression Severity"
                                #labeling created variables
                                ) %>%
  labelled::set_variable_labels(!!!community_new_labels[names(community_new_labels) %in% names(.)]
                                ) %>% #labeling variables from data dictionary
  labelled::set_variable_labels(!!!university_new_labels[names(university_new_labels) %in% names(.)]
                                )

### creating data dictionary
attribute <- as.data.frame(labelled::generate_dictionary(df_clean, labels = TRUE, values = TRUE)
                           )

### Saving data dictionary
writexl::write_xlsx(attribute,
                    path = base::file.path(output_Dir, paste0("data_dictionary_merged_clean.xlsx") )
                    )

## saving merged dataset
# haven::write_dta(data= df_clean, 
#                  path = base::file.path(output_Dir, "university_community_row_merge.dta")
#                  )

# haven::write_sav(data= df_clean, 
#                  path = base::file.path(output_Dir, "university_community_row_merge.sav")
#                  )


## saving sample of merged dataset
# set.seed(231)
# 
# haven::write_dta(data= df_clean %>% 
#                    dplyr::slice_sample(prop = 0.1,
#                                        by = site
#                                        ), 
#                  path = base::file.path(output_Dir, "university_community_sample.dta")
#                  )
                 