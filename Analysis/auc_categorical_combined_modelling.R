library(dplyr)
library(data.table)
library(forcats)
library(ggplot2)
library(tidyselect)
library(performance)
library(bayestestR)

working_directory

## Area Under the Curve

auc_categorical_combined_modelling_match <- sapply(outcome_character_vars, function(x) {
  nn <- x
  
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
  predict_logistic <- predict(logistic_model, newdata = df_final, type = "response")
  
  roc_auc <- performance::performance_roc(x= y_test, predictions = predict_logistic)
  auc <- bayestestR::area_under_curve(x= roc_auc[["Specificity"]], y= roc_auc[["Sensitivity"]])
  roc_df <- data.frame(x = roc_auc[["Specificity"]], y = roc_auc[["Sensitivity"]]) %>%
    dplyr::mutate(auc = round(auc,3)
                  , method = "logistic"
                  , pop = "Overall"
                  , outcome = nn
                  , label_auc = base::paste0(outcome, " (", auc, ")")
                  )
  
  }, simplify = FALSE
  )

ggtheme_rank_plot()
auc_categorical_combined_modelling_match_plot <- data.table::rbindlist(auc_categorical_combined_modelling_match) %>%
     ggplot(aes(x=x, y=y)) +
     geom_line(aes(colour = method)) +
     geom_abline(intercept = 0, slope = 1, colour = "grey40", linetype = 2) +
     scale_x_continuous(limits = c(0, 1), n.breaks = 6, expand = expansion(mult = c(0.02,0.02))) +
     scale_y_continuous(limits = c(0, 1), n.breaks = 6, expand = expansion(mult = c(0.02,0.02))) +
     facet_wrap(pop~label_auc, scales="free", ncol = 3
                ,labeller = labeller(.default = label_value, .multi_line = FALSE)
                ) + 
     labs(y = "True Positive Rate", x = "False Positive Rate", colour = NULL) + 
     theme(legend.position="none"
           )

print(auc_categorical_combined_modelling_match_plot)

## Saving the plot
ggsave(plot=auc_categorical_combined_modelling_match_plot, height = 7, width = 13,
       filename = "auc_roc_overall_plots.png", path = output_Dir, bg='white')
