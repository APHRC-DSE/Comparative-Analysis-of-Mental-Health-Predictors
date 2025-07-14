library(dplyr)
library(ggplot2)
library(gridExtra)

working_directory

ggtheme_descriptive_plot()

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
  
}, simplify = FALSE
)

### Saving descriptive simple plots using loops
for (i in seq(length(descriptive_plot))) {
  ggsave(plot=descriptive_plot[[i]], height = 7, width = 12,
         filename = paste0("descriptive_plot_",names(descriptive_plot)[[i]],".png"),
         path = output_Dir, bg='white')  
}

