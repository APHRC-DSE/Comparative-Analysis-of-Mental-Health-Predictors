library(dplyr)
library(forcats)
library(ggplot2)
library(gridExtra)
library(tidyr)

working_directory

ggtheme_descriptive_plot(striptext_size_x = 10)

## Simple plots

descriptive_plot <- sapply(unique(selected_vars_df$plot[selected_vars_df$plot != "none" & !is.na(selected_vars_df$plot)]),
                           function(x){
  nn <- x
  index <- selected_vars_df$new_variable[selected_vars_df$plot == nn & !is.na(selected_vars_df$plot)]
  n_index <- length(index)
  
  strata_vars <- group_vars
  
  df_new <- df_final %>%
    dplyr::select(any_of(c(index, strata_vars))
                  )
  
  plots <- sapply(as.character(unique(df_new[[strata_vars]])), function(y){
    df <- df_new %>%
      dplyr::filter(.data[[strata_vars]] == y)
    
    p <- if (nn == "GAD 7 Score" | nn == "PHQ 9 Score" | nn == "PSQ Score"){
      simple_plot(df = df, variable = index, title_label = paste0(y, " (N=", nrow(df) ,")")
                  )
    } else if (nn == "PHQ-9 Depression Severity") {
      simple_plot(df = df, variable = index, title_label = y, x_axis_label_wrap_width = 15
                  )
      } else {
      simple_plot(df = df, variable = index, title_label = y
                  )
    }
      
  }, simplify = FALSE
  )
  
  grid <- do.call(gridExtra::grid.arrange, c(plots, list(ncol = length(plots), nrow = NULL))
                  )
  grid
  
}, simplify = FALSE
)

### Saving descriptive simple plots using loops
for (i in seq(length(descriptive_plot))) {
  ggsave(plot=descriptive_plot[[i]], height = 7, width = 12,
         filename = paste0("descriptive_plot_",names(descriptive_plot)[[i]],".png"),
         path = output_Dir, bg='white')  
}

## Stack plot

descriptive_plot_stack <- df_final %>%
  dplyr::select(any_of(c(selected_vars_df$new_variable[selected_vars_df$plot %in% c("GAD-7 Anxiety", "PHQ-9 Depression", "PSQ Psychosis")],
                         group_vars
                         )
                       )
                ) %>%
  tidyr::pivot_longer(!any_of(group_vars)
                      , names_to = "name"
                      , values_to = "value"
                      ) %>%
  dplyr::mutate(value = forcats::fct_collapse(value
                                              , "Yes" = c("Anxiety (10 and above)", "Depression (13 and above)", "Psychosis (3 and above)")
                                              , "No" = c("No Anxiety (0-9)", "No Depression (0-12)", "No Psychosis (0-2)") 
                                              )
                ) %>%
  stacked_plot(variable = c("site")
              , fill_var = c("value")
              , facet_wrap = TRUE
              , facet_var = c("name")
              , title_label = ""
              , legend_label = ""
              , facet_ncol = 3
              )

### Saving stacked plot
ggsave(plot=descriptive_plot_stack, height = 7, width = 10,
       filename = paste0("stack_plot_outcome.png"),
       path = output_Dir, bg='white')
