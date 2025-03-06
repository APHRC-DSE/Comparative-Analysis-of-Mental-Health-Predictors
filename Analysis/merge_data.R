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
  labelled::set_variable_labels(education = "Level of Education"
                                , site = "Study Population"
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

