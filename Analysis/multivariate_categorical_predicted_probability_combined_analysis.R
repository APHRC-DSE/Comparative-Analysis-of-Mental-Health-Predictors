library(dplyr)
library(forcats)
library(tidyselect)
library(tibble)
library(writexl)
library(marginaleffects)

working_directory

## Marginal Effects - Adjusted Predicted Probability

multivariate_categorical_predicted_probability_combined_analysis_match <- sapply(outcome_character_vars, function(x) {
  nn <- x
  outcome_string <- strsplit(nn, "_")[[1]][1] #list output gad7, phq9, psq
  model_form <- as.formula(paste0(nn, "~."))
  
  df_new <- df_drop_vars %>%
    dplyr::select(-any_of(outcome_character_vars[!outcome_character_vars %in% nn])
                  , -c(any_of(university_only_vars)) #drop variables not in both populations
                  , -age #numerical age
                  #, -site
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
  
  #variables_final <- variables[!variables %in% variables_delete]
  variables_final <- variables[!variables %in% outcome_numeric_vars]
  
  df_final <- df %>%
    dplyr::select(any_of(c(nn, variables_final))
                  )
  
  model <- stats::glm(model_form, data = df_final, family = "binomial")
  
  model_effects <- sapply(variables_final, function(z) {
       marginal_effects <- marginaleffects::avg_predictions(model
                                                            , by = z 
                                                            , df = "residual" #Inf
                                                            #, vcov = "HC3"
                                                            ) #output is a df
       marginal_effects_new <- marginal_effects %>%
         dplyr::mutate(vars = z
                       ,outcome = nn
                       )
       
     }, simplify = FALSE
     )
  
  out <- dplyr::bind_rows(model_effects)
  
  }, simplify = FALSE
  )

### Saving the output
writexl::write_xlsx(multivariate_categorical_predicted_probability_combined_analysis_match,
                    path = base::file.path(output_Dir, "multivariate_categorical_predicted_probability_combined_analysis_match.xlsx" )
                    )
