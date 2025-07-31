library(dplyr)
library(forcats)
library(tidyselect)
library(gtsummary)

working_directory

## Adjusted Odds Ratio

adjusted_odds_ratio_categorical_combined_analysis_match <- sapply(outcome_character_vars, function(x) {
  nn <- x
  outcome_string <- strsplit(nn, "_")[[1]][1] #list output gad7, phq9, psq
  model_form <- as.formula(paste0(nn, "~."))
  
  df_new <- df_drop_vars %>%
    dplyr::select(-any_of(outcome_character_vars[!outcome_character_vars %in% nn])
                  , -c(any_of(university_only_vars)) #drop variables not in both populations
                  , -age #numerical age
                  , -site
                  )
  
  ### dropping variables with one level
  values_count <- sapply(lapply(df_new, unique), length)
  
  df_drop_level <- df_new[ , values_count > 1] %>%
    dplyr::mutate(across(where(is.factor),  ~fct_drop(.x )) #drop unused factor levels
                  )
  
  df <- df_drop_level #%>% dplyr::select(-any_of(outcome_character_vars[!outcome_character_vars %in% nn]))
  
  variables <- names(df)[!names(df) %in% nn] #independent variables
  
  variables_delete <- unname(tidyselect::vars_select(variables, starts_with(outcome_string, ignore.case = TRUE))
                             ) #select numeric variables related to outcome
  
  variables_final <- variables[!variables %in% variables_delete]
  
  df_final <- df %>%
    dplyr::select(any_of(c(nn, variables_final))
                  )
  
  model <- stats::glm(model_form, data = df_final, family = "binomial") %>%
       tbl_regression(exponentiate = TRUE,
                      intercept = FALSE,
                      #estimate_fun = ~style_sigfig(.x, digits = 3), label_style_sigfig(digits = 3)
                      estimate_fun = label_style_ratio(digits = 3),
                      pvalue_fun = ~style_pvalue(.x, digits = 3) #label_style_pvalue(digits = 3),
                      ) %>%
       #add_global_p(keep = TRUE) %>% # add global p-value for categorical variables
       #add_glance_source_note() %>%
       #add_glance_table() %>%
       add_vif() %>%
       bold_p(t= 0.05) %>% # bold p-values under a given threshold (default 0.05)
       bold_labels() %>%
       italicize_levels()%>% 
       modify_header(label = "**Logistic Regression**")%>% # update the column header
       add_significance_stars(
         pattern = "{estimate} [{conf.low} to {conf.high}]{stars}",
         hide_ci = TRUE, hide_se = TRUE , hide_p = FALSE) %>%
       modify_header(estimate ~ "**AoR (95% CI)**") %>%
       modify_footnote(estimate ~ "AoR = Adjusted Odds Ratio, CI = Confidence Interval", abbreviation = TRUE)
  
  }, simplify = FALSE
  )

## Merge Adjusted Odds Ratio output  
adjusted_odds_ratio_categorical_combined_match_merge <- gtsummary::tbl_merge(adjusted_odds_ratio_categorical_combined_analysis_match,
                                                                    tab_spanner = names(adjusted_odds_ratio_categorical_combined_analysis_match)
                                                                    ) %>%
    gtsummary::as_flex_table()
