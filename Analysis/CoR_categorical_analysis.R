library(dplyr)
library(forcats)
library(tidyselect)
library(gtsummary)

working_directory

## Crude Odds Ratio

crude_odds_ratio_categorical_analysis <- sapply(levels(df_drop_vars[[group_vars]]), function(x) {
  nn <- x
  
  df_new <- df_drop_vars %>%
    dplyr::filter(site == nn)
  
  ### dropping variables with one level
  values_count <- sapply(lapply(df_new, unique), length)
  
  df_drop_level <- df_new[ , values_count > 1] %>%
    dplyr::mutate(across(where(is.factor),  ~fct_drop(.x )) #drop unused factor levels
                  )
  
   out <- sapply(outcome_character_vars, function(y) {
     outcome_string <- strsplit(y, "_")[[1]][1]
     
     df <- df_drop_level #%>% dplyr::select(-any_of(outcome_character_vars[!outcome_character_vars %in% y]))
     
     model_form <- as.formula(paste0(y, "~."))
     
     variables <- names(df)[!names(df) %in% y] #independent variables
     
     variables_delete <- unname(tidyselect::vars_select(variables, starts_with(outcome_string, ignore.case = TRUE))
                                ) #select numeric variables related to outcome
     
     out_ <- sapply(variables[!variables %in% variables_delete], function(z) {
       df_final <- df %>% dplyr::select(any_of(c(y, z))
                                        ) 
       #sapply(lapply(df_final, unique), length)
       
       model <- stats::glm(model_form, data = df_final, family = "binomial") %>%
         tbl_regression(exponentiate = TRUE,
                        intercept = FALSE,
                        #estimate_fun = ~style_sigfig(.x, digits = 3), label_style_sigfig(digits = 3)
                        estimate_fun = label_style_ratio(digits = 3),
                        pvalue_fun = ~style_pvalue(.x, digits = 3) #label_style_pvalue(digits = 3),
                        ) %>%
         #add_global_p(keep = TRUE) %>% # add global p-value for categorical variables
         #add_glance_source_note() %>%
         #add_vif() %>%
         bold_p(t= 0.05) %>% # bold p-values under a given threshold (default 0.05)
         bold_labels() %>%
         italicize_levels()%>% 
         modify_header(label = "**Logistic Regression**")%>% # update the column header
         add_significance_stars(
           pattern = "{estimate} [{conf.low} to {conf.high}]{stars}",
           hide_ci = TRUE, hide_se = TRUE , hide_p = FALSE) %>%
         modify_header(estimate ~ "**CoR (95% CI)**") %>%
         modify_footnote(estimate ~ "CoR = Crude Odds Ratio, CI = Confidence Interval", abbreviation = TRUE)
       
     }, simplify = FALSE
     )
     
     out_ <- gtsummary::tbl_stack(out_)
     
   }, simplify = FALSE
   ) 
  
  }, simplify = FALSE
  )

## Merge Crude Odds Ratio output  
crude_odds_ratio_categorical_merge <- sapply(names(crude_odds_ratio_categorical_analysis), function(x) {
  nn <- x
  
  list_names <- names(crude_odds_ratio_categorical_analysis[[nn]])
  
  out <- gtsummary::tbl_merge(crude_odds_ratio_categorical_analysis[[nn]],
                              tab_spanner = list_names) %>%
    gtsummary::as_flex_table()
    
  
}, simplify = FALSE
)

