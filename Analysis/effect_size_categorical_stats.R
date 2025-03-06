library(dplyr)
library(forcats)

working_directory

## Effect size per study population

effect_size_categorical_stats <- if (length(outcome_character_vars)>0) {
  
  sapply(levels(df_analysis[[group_vars]]), function(x) {
  nn <- x
  
  df_new <- df_analysis %>%
    dplyr::filter(site == nn)
  
  effectsize_corr_table(df = df_new %>%
                          dplyr::mutate(across(c(any_of(outcome_character_vars)),~forcats::fct_rev(.x))
                                        ),
                        by_vars = outcome_character_vars)
    
  }, simplify = FALSE
  )
  
} else {
  print(paste0("No effect size analysis done. Select outcome variable"))
}

