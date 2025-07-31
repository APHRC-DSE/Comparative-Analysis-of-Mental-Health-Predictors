library(dplyr)

working_directory

my_gtsummary_theme

gtsummary_compact_theme

## Descriptive statistics - Combined

descriptive_combined_stats <- descriptive_table(df = df_analysis,
                                               foot_note = "n (%); Mean (SD); Median (IQR); Range",
                                               caption = "",
                                               flex_table = FALSE,
                                               ci=FALSE,
                                               mean_vars = c("gad7_total_score","phq9_totalscore", "psq_totalscore" ),
                                               include_vars = names(df_analysis)
                                               )

print(descriptive_combined_stats)

## Descriptive statistics - Per Site
descriptive_stats <- categorical_inferential_table(df = df_analysis,
                                                   foot_note = "n (%); Mean (SD); Median (IQR); Range",
                                                   caption = "",
                                                   by_vars = group_vars, 
                                                   percent = "column",
                                                   flex_table = TRUE,
                                                   overall = FALSE,
                                                   ci=FALSE,
                                                   mean_vars = c("gad7_total_score","phq9_totalscore", "psq_totalscore" ),
                                                   include_vars = names(df_analysis)
                                                   )

print(descriptive_stats)


inferential_vars <- selected_vars_df$new_variable[selected_vars_df$inferential == "yes" &
                                                    !is.na(selected_vars_df$inferential)]

factor_character_vars <- names(df_analysis[sapply(df_analysis,
                                                function(v) is.factor(v) | is.character(v) | is.logical(v))])

numeric_integer_vars <- names(df_analysis[sapply(df_analysis, function(v) is.numeric(v) | is.integer(v))])

inferential_character_vars <- inferential_vars[inferential_vars %in% factor_character_vars]
inferential_numeric_vars <- inferential_vars[inferential_vars %in% numeric_integer_vars]

outcome_character_vars <- outcome_vars[outcome_vars %in% factor_character_vars]
outcome_numeric_vars <- outcome_vars[outcome_vars %in% numeric_integer_vars]

## Inferential statistics - Combined

inferential_character_combined_stats_1 <- 
  categorical_inferential_table(df = df_analysis,
                                foot_note = "n (%); Mean (SD); Median (IQR); Range",
                                caption = "",
                                by_vars = outcome_character_vars, 
                                percent = "row",
                                flex_table = FALSE,
                                overall = FALSE,
                                ci=FALSE,
                                mean_vars = c("gad7_total_score","phq9_totalscore", "psq_totalscore" ),
                                include_vars = names(df_analysis)
                                )

print(inferential_character_combined_stats_1)

## Inferential statistics - Per Site

### Population study as Strata 
inferential_character_stats_1 <- if (length(outcome_character_vars)>0) {
  
  sapply(paste0(c(unique(inferential_character_vars %in% outcome_character_vars))), function(x) {
  nn <- x
  index <- inferential_character_vars[(inferential_character_vars %in% outcome_character_vars) == nn]
  if (nn == "TRUE") {
    ### Row percentage
    categorical_inferential_strata_table(df = df_analysis,
                                         strata_var = group_vars,
                                         foot_note = "n (%); Mean (SD); Median (IQR); Range",
                                         caption = "",
                                         by_vars = index, 
                                         percent = "row",
                                         flex_table = TRUE,
                                         ci=FALSE,
                                         mean_vars = c("gad7_total_score","phq9_totalscore", "psq_totalscore" ),
                                         include_vars = names(df_analysis)
                                         )
    
  } else {
    ### Column percentage
    categorical_inferential_strata_table(df = df_analysis,
                                         strata_var = group_vars,
                                         foot_note = "n (%); Mean (SD); Median (IQR); Range",
                                         caption = "",
                                         by_vars = index, 
                                         percent = "column",
                                         flex_table = TRUE,
                                         ci=FALSE,
                                         mean_vars = c("gad7_total_score","phq9_totalscore", "psq_totalscore" ),
                                         include_vars = names(df_analysis)
                                         )
  }
  }, simplify = FALSE
  )
} else {
  ### Column percentage
  
  categorical_inferential_strata_table(df = df_analysis,
                                       strata_var = group_vars,
                                       foot_note = "n (%); Mean (SD); Median (IQR); Range",
                                       caption = "",
                                       by_vars = inferential_character_vars, 
                                       percent = "column",
                                       flex_table = TRUE,
                                       ci=FALSE,
                                       mean_vars = c("gad7_total_score","phq9_totalscore", "psq_totalscore" ),
                                       include_vars = names(df_analysis)
                                       )
}

print(inferential_character_stats_1)


### Outcome Variables as Strata 
inferential_character_stats_2 <- if (length(outcome_character_vars)>0) {
  
  sapply(inferential_character_vars[(inferential_character_vars %in% outcome_character_vars)], function(x) {
    nn <- x
    
    categorical_inferential_strata_table(df = df_analysis,
                                         strata_var = nn,
                                         foot_note = "n (%); Mean (SD); Median (IQR); Range",
                                         caption = "",
                                         by_vars = group_vars, 
                                         percent = "column",
                                         flex_table = TRUE,
                                         ci=FALSE,
                                         mean_vars = c("gad7_total_score","phq9_totalscore", "psq_totalscore" ),
                                         include_vars = names(df_analysis)
                                         )
                         
    
  }, simplify = FALSE
  )
  
} else {
  
  ### Column percentage
  sapply(inferential_character_vars, function(x) {
    nn <- x
    
    categorical_inferential_strata_table(df = df_analysis,
                                         strata_var = nn,
                                         foot_note = "n (%); Mean (SD); Median (IQR); Range",
                                         caption = "",
                                         by_vars = group_vars, 
                                         percent = "column",
                                         flex_table = FALSE,
                                         ci=FALSE,
                                         mean_vars = c("gad7_total_score","phq9_totalscore", "psq_totalscore" ),
                                         include_vars = names(df_analysis)
                                         )

  }, simplify = FALSE
  )
  
}

print(inferential_character_stats_2)

## Merge the combined descriptive and Inferential statistics

descriptive_inferential_combined_stats_merge <- 
  gtsummary::tbl_merge(tbls= c(list(descriptive_combined_stats),
                               inferential_character_combined_stats_1 
                               ),
                       tab_spanner = c("Overall", final_attribute$label[final_attribute$variable %in% outcome_character_vars]
                                         )
                       ) %>%
      gtsummary::as_flex_table()
