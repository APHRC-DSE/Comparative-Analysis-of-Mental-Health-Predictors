library(dplyr)
library(forcats)
library(tidyselect)
library(broom)
library(tidyr)
library(performance)
library(stringr)
library(ggplot2)

working_directory

## Adjusted Odds Ratio plots

adjusted_odds_ratio_categorical_combined_analysis_all <- sapply(outcome_character_vars, function(x) {
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
                  )
  
  ## Creating a named vector to quickly rename levels
  new_levels <- selected_vars_df %>%
    dplyr::select(new_label, new_variable) %>%
    tidyr::drop_na() %>%
    dplyr::filter(new_variable %in% names(df_drop_level)) %>%
    tibble::deframe()
  
  #Regression Model 
  model <- stats::glm(model_form, data = df_final, family = "binomial")
  
  #exponentiated model estimates table
  model_table <- broom::tidy(x = model, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE) %>%
    dplyr::mutate(p.value = round(p.value, 3)
                  , p_value = ifelse(p.value < 0.001, "<0.001", p.value)
                  #, lower.estimate = estimate-(1.96*std.error)
                  #, upper.estimate = estimate+(1.96*std.error)
                  , variable_name = stringr::str_extract(term, paste(names(df_final), sep= "", collapse = '|'))
                  , variable_label = as.character(forcats::fct_recode(variable_name, !!!new_levels))
                  , term_label = gsub(paste(names(df_final), sep= "", collapse = '|'), "", term)
                  #, term_label = gsub("Yes", NA, term_label)
                  , term_label = dplyr::na_if(term_label, "")
                  , term_label = if_else(is.na(term_label),variable_label ,term_label)
                  , outcome_name = nn
                  , outcome_label = as.character(forcats::fct_recode(outcome_name, !!!new_levels))
                  )
  
  
  }, simplify = FALSE
  )

ggtheme_regression_plot()
adjusted_odds_ratio_categorical_combined_analysis_all_plots <- dplyr::bind_rows(adjusted_odds_ratio_categorical_combined_analysis_all) %>%
     tidyr::drop_na(variable_name, conf.low) %>%
     dplyr::mutate(variable_label_new = variable_label
                   , term_label_new = if_else(variable_label != term_label , 
                                              paste0(term_label, " ", variable_label_new), term_label)
                   , term_label_new = forcats::as_factor(term_label_new)
                   ) %>%
  ggplot(aes(x=forcats::fct_rev(term_label_new), y=as.numeric(estimate), colour=outcome_label)) + 
     geom_pointrange(aes(ymin = as.numeric(conf.low), ymax = as.numeric(conf.high)),
                     stat = "identity",
                     position = position_dodge(0.5),
                     na.rm = TRUE,
                     fatten = 0.5,
                     show.legend = TRUE
                     ) +
     coord_flip() +
     geom_hline(yintercept = 1, colour = "grey60", linetype = 2) +
     labs(x=NULL,y="Adjusted Odds Ratio (95% CI)", colour = "", title = "") +
     scale_y_continuous( n.breaks = 10) +
     scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 60)) +
     guides(colour=guide_legend(nrow = 1, ncol = NULL)) +
     theme(legend.position="bottom"
           )

### Saving Regression Forest plot
ggsave(plot=adjusted_odds_ratio_categorical_combined_analysis_all_plots, height = 7.5, width = 9.5,
       filename = paste0("regression_forest_plot_combined",".png"),
       path = output_Dir, bg='white')

