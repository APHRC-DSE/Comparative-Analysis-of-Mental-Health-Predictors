library(dplyr)
library(data.table)
library(forcats)
library(ggplot2)
library(tidyselect)
library(DALEX)
library(performance)
library(bayestestR)
library(ggpubr)
library(gridExtra)

working_directory
ggtheme_rank_plot()

## Permutation-based variable-importance

varimp_categorical_combined_modelling_match <- sapply(outcome_character_vars, function(x) {
  nn <- x
  
  final_labels_new <- final_attribute %>%
    dplyr::select(label, variable) %>%
    tibble::deframe()
  
  df_new <- df_drop_vars %>%
    dplyr::select(-any_of(outcome_character_vars[!outcome_character_vars %in% nn])
                  ,-c(any_of(university_only_vars)) #drop variables not in both populations
                  , -age #numerical age
                  , -site
                  )
  
  ### dropping variables with one level
  values_count <- sapply(lapply(df_new, unique), length)
  
  df_drop_level <- df_new[ , values_count > 1] %>%
    dplyr::mutate(across(where(is.factor),  ~fct_drop(.x )) #drop unused factor levels
                  )
  
  outcome_string <- strsplit(nn, "_")[[1]][1]
  
  df <- df_drop_level #%>% dplyr::select(-any_of(outcome_character_vars[!outcome_character_vars %in% nn])) #Remove outcome character 
  
  model_form <- as.formula(paste0(nn, "~."))
  
  variables <- names(df)[!names(df) %in% nn] #independent variables
  
  variables_delete <- unname(tidyselect::vars_select(variables, starts_with(outcome_string, ignore.case = TRUE))
                             ) #select numeric variables related to outcome
  
  variables_final <- variables[!variables %in% variables_delete]
  
  df_final <- df %>%
    dplyr::select(any_of(c(nn, variables_final))
                  ) %>%
    dplyr::mutate(across(any_of(nn), ~as.numeric(.x))
                  , across(any_of(nn), ~ifelse(.x == 1, "No", "Yes"))
                  , across(any_of(nn), ~factor(.x, levels = c("No", "Yes")))
                  )
  
  y <- df_final[[nn]]
  y_test <- ifelse(y == "Yes", 1, 0)
  
  logistic_model <- stats::glm(model_form, data = df_final, family = "binomial")
  explainer_logistic <- DALEX::explain(model = logistic_model, 
                                       data = df_final[, colnames(df_final)[!colnames(df_final) %in% nn]],
                                       y = y_test,
                                       verbose = FALSE,
                                       precalculate = TRUE,
                                       label = "",
                                       type = "classification"
                                       )
  
  lossfunction_logistic <- DALEX::loss_one_minus_auc(observed = y_test,
                                                     predicted = predict(object = logistic_model, 
                                                                         newdata = df_final, type = "response")
                                                     )
  
  perform_logistic <- DALEX::model_performance(explainer_logistic)
  
  var_imp_logistic <- DALEX::model_parts(explainer = explainer_logistic,
                                         #loss_function = lossfunction_logistic, ## Use when type in explainer not specified
                                         variables = NULL,
                                         variable_groups = NULL,
                                         N = NULL, #number of observations sampled from the data available in the explainer-object
                                         B = 25, #number of permutations to be used for calculation of (mean) variable-importance
                                         type = "difference" #"difference", "ratio", "variable_importance", "raw"
                                         )
  
  var_imp_logistic_new <- var_imp_logistic %>%
    dplyr::mutate(variable = as.character(forcats::fct_recode(variable, !!!final_labels_new)))
  
  plot <- plot( var_imp_logistic_new
                , show_boxplots = TRUE
                , bar_width = 8 #default 10
                , desc_sorting = TRUE
                , title = "" #default 'Feature Importance' 
                , subtitle = nn
                ) +
    labs(x = "", y = "", title = "") +
    scale_y_continuous(expand = expansion(mult = c(0.01,0.1))
                       )
     
  
  }, simplify = FALSE
  )

varimp_categorical_combined_modelling_match_plot <- ggpubr::annotate_figure( 
  p = do.call(gridExtra::grid.arrange, c(varimp_categorical_combined_modelling_match, list(nrow = 1))
              ),
  top = NULL,
  bottom = text_grob("Variable Importance", color = "navyblue", face = "bold", size = 12),
  )

## Saving the plot
ggsave(plot=varimp_categorical_combined_modelling_match_plot, height = 7, width = 13,
       filename = "varimp_overall_plots.png", path = output_Dir, bg='white')
