library(dplyr)
library(forcats)
library(tidyselect)
library(tibble)
library(writexl)
library(marginaleffects)

working_directory

## Marginal Effects - Adjusted Predicted Probability

multivariate_categorical_predicted_probability_analysis_match <- sapply(levels(df_drop_vars[[group_vars]]), function(x) {
  nn <- x
  
  df_new <- df_drop_vars %>%
    dplyr::select(-c(any_of(university_only_vars), education, age) #drop variables not in both populations
                  ) %>%
    dplyr::filter(site == nn)
  
  ### dropping variables with one level
  values_count <- sapply(lapply(df_new, unique), length)
  
  df_drop_level <- df_new[ , values_count > 1] %>%
    dplyr::mutate(across(where(is.factor),  ~fct_drop(.x )) #drop unused factor levels
                  )
  
   out <- sapply(outcome_character_vars, function(y) {
     outcome_string <- strsplit(y, "_")[[1]][1]
     
     df <- df_drop_level %>% 
       dplyr::select(-any_of(outcome_character_vars[!outcome_character_vars %in% y])
                     ) #Remove outcome character 
     
     model_form <- as.formula(paste0(y, "~."))
     
     variables <- names(df)[!names(df) %in% y] #independent variables
     
     variables_delete <- unname(tidyselect::vars_select(variables, starts_with(outcome_string, ignore.case = TRUE))
                                ) #select numeric variables related to outcome
     
     #variables_final <- variables[!variables %in% variables_delete]
     variables_final <- variables[!variables %in% outcome_numeric_vars]
     
     df_final <- df %>%
       dplyr::select(any_of(c(y, variables_final))
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
                       )
       
     }, simplify = FALSE
     )
     
     marginal_effects_new <- dplyr::bind_rows(model_effects)  %>%
       dplyr::mutate(outcome = y
                     )
     
   }, simplify = FALSE
   )
   
   out <- dplyr::bind_rows(out) 
  
  }, simplify = FALSE
  )

### Saving the output
writexl::write_xlsx(multivariate_categorical_predicted_probability_analysis_match,
                    path = base::file.path(output_Dir, "multivariate_categorical_predicted_probability_analysis_match.xlsx" )
                    )
