library(dplyr)
library(forcats)
library(scales)
library(labelled)
library(sjlabelled)
library(stringr)
library(ggplot2)
library(gridExtra)

### simple Bar graphs and histograms without density plots
simple_plot <- function(df, variable, rotate_axis=FALSE, percent_yaxis=FALSE, title_label = NULL, text_label = TRUE,
                        histogram = TRUE, text_label_size = 3, y_axis_breaks =10, x_axis_breaks_histogram = 10,
                        expand_yaxis = c(0.01,0.05), y_axis_limits = c(NULL, NULL), histogram_bins = 50,
                        percent_text_accuracy = 0.1, x_axis_label_wrap_width = 20, title_label_wrap_width = 35,
                        bar_width = 0.8, bar_fill_colour = NULL) {
  
    df <- (df %>%
             dplyr::mutate(across(where(is.character), sjlabelled::as_factor)) %>%
             dplyr::mutate(across(where(is.logical), sjlabelled::as_factor))
           )
    
    df <- df[!is.na(df[[variable]]),]
    index <- df[[variable]]
    label <- if (is.null(labelled::var_label(index))) {variable
    } else {
      labelled::var_label(index)
    }
    
    plot <- if (is.factor(index)) {
      p1 <- if (rotate_axis == TRUE) {
        p1_1 <- ggplot(data=df, aes(x= forcats::fct_rev(forcats::fct_infreq(index)))) +
          coord_flip() 
        
        p1_2 <- if (percent_yaxis == TRUE) {
          p1_3 <- if (text_label == TRUE) {
            p1_4 <- if (is.null(bar_fill_colour)) {
              p1_1 +
                geom_bar(aes(y = after_stat(count)/sum(after_stat(count)), fill = index, group=index),
                         position="stack", stat="count", show.legend = FALSE, width = bar_width) 
              } else {
                p1_1 +
                  geom_bar(aes(y = after_stat(count)/sum(after_stat(count)), group=index), fill = bar_fill_colour,
                           position="stack", stat="count", show.legend = FALSE, width = bar_width)
              }
            p1_4 +
              scale_y_continuous(labels = scales::percent, n.breaks = y_axis_breaks, limits = y_axis_limits,
                                 expand = expansion(mult = expand_yaxis)) +
              geom_text(aes(y = after_stat(count)/sum(after_stat(count)),
                            label = scales::percent(after_stat(count)/sum(after_stat(count)),  
                                                    accuracy = percent_text_accuracy),group=index),
                        stat = "count", 
                        hjust = -0.05, 
                        colour = "black",
                        size = text_label_size)
          } else {
            p1_4 <- if (is.null(bar_fill_colour)) {
              p1_1 +
                geom_bar(aes(y = after_stat(count)/sum(after_stat(count)), fill = index, group=index),
                         position="stack", stat="count", show.legend = FALSE, width = bar_width)
            } else {
              p1_1 +
                geom_bar(aes(y = after_stat(count)/sum(after_stat(count)), group=index), fill = bar_fill_colour,
                         position="stack", stat="count", show.legend = FALSE, width = bar_width)
              }
            p1_4 +
              scale_y_continuous(labels = scales::percent, n.breaks = y_axis_breaks, limits = y_axis_limits,
                                 expand = expansion(mult = expand_yaxis)) 
          }
          p1_3
        }
        else {
          p1_3 <- if (text_label == TRUE) {
            p1_4 <- if (is.null(bar_fill_colour)) {
              p1_1 +
                geom_bar(aes(fill = index, group=index), 
                         position="stack", stat="count", show.legend = FALSE, width = bar_width) 
              } else {
                p1_1 +
                  geom_bar(aes(group=index), fill = bar_fill_colour, 
                           position="stack", stat="count", show.legend = FALSE, width = bar_width)
                }
            
            p1_4 +
              scale_y_continuous(n.breaks = y_axis_breaks, limits = y_axis_limits,
                                 expand = expansion(mult = expand_yaxis)) +
              geom_text(aes(label = paste0(after_stat(count), " (",
                                           scales::percent(after_stat(count)/sum(after_stat(count)),
                                                           accuracy = percent_text_accuracy),")" ), group=index ),
                        stat = "count", 
                        hjust = -0.05, 
                        colour = "black",
                        size = text_label_size)
          } else {
            p1_4 <- if (is.null(bar_fill_colour)) {
              p1_1 +
                geom_bar(aes(fill = index, group=index), 
                         position="stack", stat="count", show.legend = FALSE, width = bar_width) 
              } else {
                p1_1 +
                  geom_bar(aes(group=index), fill = bar_fill_colour, 
                           position="stack", stat="count", show.legend = FALSE, width = bar_width)
              }
            
            p1_4 +
              scale_y_continuous(n.breaks = y_axis_breaks, limits = y_axis_limits,
                                 expand = expansion(mult = expand_yaxis)) 
            }
          p1_3
        }
        p1_2
        
      } else { 
        p1_1 <- ggplot(data=df, aes(x= forcats::fct_infreq(index)))
        
        p1_2 <- if (percent_yaxis == TRUE) {
          p1_3 <- if (text_label == TRUE) {
            p1_4 <- if (is.null(bar_fill_colour)) {
              p1_1 +
                geom_bar(aes(y = after_stat(count)/sum(after_stat(count)), fill = index, group=index),
                         position="stack", stat="count", show.legend = FALSE, width = bar_width) 
            } else {
              p1_1 +
                geom_bar(aes(y = after_stat(count)/sum(after_stat(count)), group=index), fill = bar_fill_colour,
                         position="stack", stat="count", show.legend = FALSE, width = bar_width)
            }
            p1_4 +
              scale_y_continuous(labels = scales::percent, n.breaks = y_axis_breaks, limits = y_axis_limits,
                                 expand = expansion(mult = expand_yaxis)) +
              geom_text(aes(y = after_stat(count)/sum(after_stat(count)),
                            label = scales::percent(after_stat(count)/sum(after_stat(count)),  
                                                    accuracy = percent_text_accuracy),group=index),
                        stat = "count", 
                        vjust = -0.2, 
                        colour = "black",
                        size = text_label_size)
          } else {
            p1_4 <- if (is.null(bar_fill_colour)) {
              p1_1 +
                geom_bar(aes(y = after_stat(count)/sum(after_stat(count)), fill = index, group=index),
                         position="stack", stat="count", show.legend = FALSE, width = bar_width)
            } else {
              p1_1 +
                geom_bar(aes(y = after_stat(count)/sum(after_stat(count)), group=index), fill = bar_fill_colour,
                         position="stack", stat="count", show.legend = FALSE, width = bar_width)
            }
            p1_4 +
              scale_y_continuous(labels = scales::percent, n.breaks = y_axis_breaks, limits = y_axis_limits,
                                 expand = expansion(mult = expand_yaxis)) 
          }
          p1_3
        }
        else {
          p1_3 <- if (text_label == TRUE) {
            p1_4 <- if (is.null(bar_fill_colour)) {
              p1_1 +
                geom_bar(aes(fill = index, group=index), 
                         position="stack", stat="count", show.legend = FALSE, width = bar_width) 
            } else {
              p1_1 +
                geom_bar(aes(group=index), fill = bar_fill_colour, 
                         position="stack", stat="count", show.legend = FALSE, width = bar_width)
            }
            
            p1_4 +
              scale_y_continuous(n.breaks = y_axis_breaks, limits = y_axis_limits,
                                 expand = expansion(mult = expand_yaxis)) +
              geom_text(aes(label = paste0(after_stat(count), " (",
                                           scales::percent(after_stat(count)/sum(after_stat(count)),
                                                           accuracy = percent_text_accuracy),")" ), group=index ),
                        stat = "count", 
                        vjust = -0.2, 
                        colour = "black",
                        size = text_label_size)
          } else {
            p1_4 <- if (is.null(bar_fill_colour)) {
              p1_1 +
                geom_bar(aes(fill = index, group=index), 
                         position="stack", stat="count", show.legend = FALSE, width = bar_width) 
            } else {
              p1_1 +
                geom_bar(aes(group=index), fill = bar_fill_colour, 
                         position="stack", stat="count", show.legend = FALSE, width = bar_width)
            }
            
            p1_4 +
              scale_y_continuous(n.breaks = y_axis_breaks, limits = y_axis_limits,
                                 expand = expansion(mult = expand_yaxis)) 
          }
          p1_3
        }
        p1_2
      }
      
      p2 <- p1 +
        scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = x_axis_label_wrap_width))
      
      p2
    } else {
      if (histogram == TRUE) {
      ggplot(data=df, aes(x = as.numeric(index))) + 
        geom_histogram(aes(y = after_stat(density)), 
                       bins = histogram_bins, color = "black", fill = "gray") +
        geom_density(linewidth = 0.5, colour = "royalblue",
                       fill = "royalblue", alpha = 0.25) +  
        geom_vline(aes(xintercept = mean(index, na.rm=TRUE)), 
                   linetype = "dashed", linewidth = 0.6) +
        scale_x_continuous(n.breaks = x_axis_breaks_histogram) +
        scale_y_continuous(n.breaks = y_axis_breaks, limits = y_axis_limits,
                           expand = expansion(mult = expand_yaxis)) 
      } else {
        ggplot(data=df, aes(y = as.numeric(index))) + 
          geom_boxplot(outlier.colour = "red", outlier.shape = 1, show.legend = FALSE) +
          scale_x_discrete() +
          scale_y_continuous(n.breaks = y_axis_breaks, limits = y_axis_limits,
                             expand = expansion(mult = expand_yaxis))
      }
    }
    plot1 <- if (is.null(title_label)) {
      if (histogram == TRUE & !is.factor(index)) {
        plot +
          labs(x=NULL,y="Density", title = stringr::str_wrap(label, width = title_label_wrap_width))
      } else {
        plot +
          labs(x=NULL,y=NULL, title = stringr::str_wrap(label, width = title_label_wrap_width))
      }
    } else {
      if (histogram == TRUE & !is.factor(index)) {
        plot + 
          labs(x=NULL,y="Density", title = stringr::str_wrap(title_label, width = title_label_wrap_width))
      } else {
        plot + 
          labs(x=NULL,y=NULL, title = stringr::str_wrap(title_label, width = title_label_wrap_width))
      }
    }
    
    plot1
    
}


### Combined simple Bar graphs and histograms without density plots 
simple_plot_grid <- function(df, vars, rotate_axis=FALSE, percent_yaxis=FALSE, title_label = NULL, text_label = TRUE,
                             histogram = TRUE, text_label_size = 3, y_axis_breaks =10, x_axis_breaks_histogram = 10,
                             expand_yaxis = c(0.01,0.05), y_axis_limits = c(NULL, NULL), histogram_bins = 50,
                             percent_text_accuracy = 0.1, x_axis_label_wrap_width = 20, title_label_wrap_width = 35,
                             bar_width = 0.8, bar_fill_colour = NULL, grid_ncol =  1, grid_nrow = NULL) {
  out <- lapply(vars, function(x){
    df <- (df %>%
             dplyr::mutate(across(where(is.character), sjlabelled::as_factor)) %>%
             dplyr::mutate(across(where(is.logical), sjlabelled::as_factor))
           )
    
    df <- df[!is.na(df[[x]]),]
    index <- df[[x]]
    label <- if (is.null(labelled::var_label(index))) {x
    } else {
      labelled::var_label(index)
    }
    
    plot <- if (is.factor(index)) {
      p1 <- if (rotate_axis == TRUE) {
        p1_1 <- ggplot(data=df, aes(x= forcats::fct_rev(forcats::fct_infreq(index)))) +
          coord_flip() 
        
        p1_2 <- if (percent_yaxis == TRUE) {
          p1_3 <- if (text_label == TRUE) {
            p1_4 <- if (is.null(bar_fill_colour)) {
              p1_1 +
                geom_bar(aes(y = after_stat(count)/sum(after_stat(count)), fill = index, group=index),
                         position="stack", stat="count", show.legend = FALSE, width = bar_width) 
            } else {
              p1_1 +
                geom_bar(aes(y = after_stat(count)/sum(after_stat(count)), group=index), fill = bar_fill_colour,
                         position="stack", stat="count", show.legend = FALSE, width = bar_width)
            }
            p1_4 +
              scale_y_continuous(labels = scales::percent, n.breaks = y_axis_breaks, limits = y_axis_limits,
                                 expand = expansion(mult = expand_yaxis)) +
              geom_text(aes(y = after_stat(count)/sum(after_stat(count)),
                            label = scales::percent(after_stat(count)/sum(after_stat(count)),  
                                                    accuracy = percent_text_accuracy),group=index),
                        stat = "count", 
                        hjust = -0.05, 
                        colour = "black",
                        size = text_label_size)
          } else {
            p1_4 <- if (is.null(bar_fill_colour)) {
              p1_1 +
                geom_bar(aes(y = after_stat(count)/sum(after_stat(count)), fill = index, group=index),
                         position="stack", stat="count", show.legend = FALSE, width = bar_width)
            } else {
              p1_1 +
                geom_bar(aes(y = after_stat(count)/sum(after_stat(count)), group=index), fill = bar_fill_colour,
                         position="stack", stat="count", show.legend = FALSE, width = bar_width)
            }
            p1_4 +
              scale_y_continuous(labels = scales::percent, n.breaks = y_axis_breaks, limits = y_axis_limits,
                                 expand = expansion(mult = expand_yaxis)) 
          }
          p1_3
        }
        else {
          p1_3 <- if (text_label == TRUE) {
            p1_4 <- if (is.null(bar_fill_colour)) {
              p1_1 +
                geom_bar(aes(fill = index, group=index), 
                         position="stack", stat="count", show.legend = FALSE, width = bar_width) 
            } else {
              p1_1 +
                geom_bar(aes(group=index), fill = bar_fill_colour, 
                         position="stack", stat="count", show.legend = FALSE, width = bar_width)
            }
            
            p1_4 +
              scale_y_continuous(n.breaks = y_axis_breaks, limits = y_axis_limits,
                                 expand = expansion(mult = expand_yaxis)) +
              geom_text(aes(label = paste0(after_stat(count), " (",
                                           scales::percent(after_stat(count)/sum(after_stat(count)),
                                                           accuracy = percent_text_accuracy),")" ), group=index ),
                        stat = "count", 
                        hjust = -0.05, 
                        colour = "black",
                        size = text_label_size)
          } else {
            p1_4 <- if (is.null(bar_fill_colour)) {
              p1_1 +
                geom_bar(aes(fill = index, group=index), 
                         position="stack", stat="count", show.legend = FALSE, width = bar_width) 
            } else {
              p1_1 +
                geom_bar(aes(group=index), fill = bar_fill_colour, 
                         position="stack", stat="count", show.legend = FALSE, width = bar_width)
            }
            
            p1_4 +
              scale_y_continuous(n.breaks = y_axis_breaks, limits = y_axis_limits,
                                 expand = expansion(mult = expand_yaxis)) 
          }
          p1_3
        }
        p1_2
        
      } else { 
        p1_1 <- ggplot(data=df, aes(x= forcats::fct_infreq(index)))
        
        p1_2 <- if (percent_yaxis == TRUE) {
          p1_3 <- if (text_label == TRUE) {
            p1_4 <- if (is.null(bar_fill_colour)) {
              p1_1 +
                geom_bar(aes(y = after_stat(count)/sum(after_stat(count)), fill = index, group=index),
                         position="stack", stat="count", show.legend = FALSE, width = bar_width) 
            } else {
              p1_1 +
                geom_bar(aes(y = after_stat(count)/sum(after_stat(count)), group=index), fill = bar_fill_colour,
                         position="stack", stat="count", show.legend = FALSE, width = bar_width)
            }
            p1_4 +
              scale_y_continuous(labels = scales::percent, n.breaks = y_axis_breaks, limits = y_axis_limits,
                                 expand = expansion(mult = expand_yaxis)) +
              geom_text(aes(y = after_stat(count)/sum(after_stat(count)),
                            label = scales::percent(after_stat(count)/sum(after_stat(count)),  
                                                    accuracy = percent_text_accuracy),group=index),
                        stat = "count", 
                        vjust = -0.2, 
                        colour = "black",
                        size = text_label_size)
          } else {
            p1_4 <- if (is.null(bar_fill_colour)) {
              p1_1 +
                geom_bar(aes(y = after_stat(count)/sum(after_stat(count)), fill = index, group=index),
                         position="stack", stat="count", show.legend = FALSE, width = bar_width)
            } else {
              p1_1 +
                geom_bar(aes(y = after_stat(count)/sum(after_stat(count)), group=index), fill = bar_fill_colour,
                         position="stack", stat="count", show.legend = FALSE, width = bar_width)
            }
            p1_4 +
              scale_y_continuous(labels = scales::percent, n.breaks = y_axis_breaks, limits = y_axis_limits,
                                 expand = expansion(mult = expand_yaxis)) 
          }
          p1_3
        }
        else {
          p1_3 <- if (text_label == TRUE) {
            p1_4 <- if (is.null(bar_fill_colour)) {
              p1_1 +
                geom_bar(aes(fill = index, group=index), 
                         position="stack", stat="count", show.legend = FALSE, width = bar_width) 
            } else {
              p1_1 +
                geom_bar(aes(group=index), fill = bar_fill_colour, 
                         position="stack", stat="count", show.legend = FALSE, width = bar_width)
            }
            
            p1_4 +
              scale_y_continuous(n.breaks = y_axis_breaks, limits = y_axis_limits,
                                 expand = expansion(mult = expand_yaxis)) +
              geom_text(aes(label = paste0(after_stat(count), " (",
                                           scales::percent(after_stat(count)/sum(after_stat(count)),
                                                           accuracy = percent_text_accuracy),")" ), group=index ),
                        stat = "count", 
                        vjust = -0.2, 
                        colour = "black",
                        size = text_label_size)
          } else {
            p1_4 <- if (is.null(bar_fill_colour)) {
              p1_1 +
                geom_bar(aes(fill = index, group=index), 
                         position="stack", stat="count", show.legend = FALSE, width = bar_width) 
            } else {
              p1_1 +
                geom_bar(aes(group=index), fill = bar_fill_colour, 
                         position="stack", stat="count", show.legend = FALSE, width = bar_width)
            }
            
            p1_4 +
              scale_y_continuous(n.breaks = y_axis_breaks, limits = y_axis_limits,
                                 expand = expansion(mult = expand_yaxis)) 
          }
          p1_3
        }
        p1_2
      }
      
      p2 <- p1 +
        scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = x_axis_label_wrap_width))
      
      p2
    } else {
      if (histogram == TRUE) {
        ggplot(data=df, aes(x = as.numeric(index))) + 
          geom_histogram(aes(y = after_stat(density)), 
                         bins = histogram_bins, color = "black", fill = "gray") +
          geom_density(linewidth = 0.5, colour = "royalblue",
                       fill = "royalblue", alpha = 0.25) +  
          geom_vline(aes(xintercept = mean(index, na.rm=TRUE)), 
                     linetype = "dashed", linewidth = 0.6) +
          scale_x_continuous(n.breaks = x_axis_breaks_histogram) +
          scale_y_continuous(n.breaks = y_axis_breaks, limits = y_axis_limits,
                             expand = expansion(mult = expand_yaxis)) 
      } else {
        ggplot(data=df, aes(y = as.numeric(index))) + 
          geom_boxplot(outlier.colour = "red", outlier.shape = 1, show.legend = FALSE) +
          scale_x_discrete() +
          scale_y_continuous(n.breaks = y_axis_breaks, limits = y_axis_limits,
                             expand = expansion(mult = expand_yaxis))
      }
    }
    plot1 <- if (is.null(title_label)) {
      if (histogram == TRUE & !is.factor(index)) {
      plot +
        labs(x=NULL,y="Density", title = stringr::str_wrap(label, width = title_label_wrap_width))
      } else {
        plot +
          labs(x=NULL,y=NULL, title = stringr::str_wrap(label, width = title_label_wrap_width))
      }
    } else {
      if (histogram == TRUE & !is.factor(index)) {
      plot + 
        labs(x=NULL,y="Density", title = stringr::str_wrap(title_label, width = title_label_wrap_width))
      } else {
        plot + 
          labs(x=NULL,y=NULL, title = stringr::str_wrap(title_label, width = title_label_wrap_width))
        }
    }
    
    plot1
    
  })
  out
  
  grid <- do.call(gridExtra::grid.arrange, c(out, list(ncol = grid_ncol, nrow = grid_nrow))
                  )
}


### List output simple Bar graphs and histograms without density plots 
simple_plot_list <- function(df, vars, rotate_axis=FALSE, percent_yaxis=FALSE, title_label = NULL, text_label = TRUE,
                             histogram = TRUE, text_label_size = 3, y_axis_breaks =10, x_axis_breaks_histogram = 10,
                             expand_yaxis = c(0.01,0.05), y_axis_limits = c(NULL, NULL), histogram_bins = 50,
                             percent_text_accuracy = 0.1, x_axis_label_wrap_width = 20, title_label_wrap_width = 35,
                             bar_width = 0.8, bar_fill_colour = NULL) {
  out <- lapply(vars, function(x){
    df <- (df %>%
             dplyr::mutate(across(where(is.character), sjlabelled::as_factor)) %>%
             dplyr::mutate(across(where(is.logical), sjlabelled::as_factor))
    )
    
    df <- df[!is.na(df[[x]]),]
    index <- df[[x]]
    label <- if (is.null(labelled::var_label(index))) {x
    } else {
      labelled::var_label(index)
    }
    
    plot <- if (is.factor(index)) {
      p1 <- if (rotate_axis == TRUE) {
        p1_1 <- ggplot(data=df, aes(x= forcats::fct_rev(forcats::fct_infreq(index)))) +
          coord_flip() 
        
        p1_2 <- if (percent_yaxis == TRUE) {
          p1_3 <- if (text_label == TRUE) {
            p1_4 <- if (is.null(bar_fill_colour)) {
              p1_1 +
                geom_bar(aes(y = after_stat(count)/sum(after_stat(count)), fill = index, group=index),
                         position="stack", stat="count", show.legend = FALSE, width = bar_width) 
            } else {
              p1_1 +
                geom_bar(aes(y = after_stat(count)/sum(after_stat(count)), group=index), fill = bar_fill_colour,
                         position="stack", stat="count", show.legend = FALSE, width = bar_width)
            }
            p1_4 +
              scale_y_continuous(labels = scales::percent, n.breaks = y_axis_breaks, limits = y_axis_limits,
                                 expand = expansion(mult = expand_yaxis)) +
              geom_text(aes(y = after_stat(count)/sum(after_stat(count)),
                            label = scales::percent(after_stat(count)/sum(after_stat(count)),  
                                                    accuracy = percent_text_accuracy),group=index),
                        stat = "count", 
                        hjust = -0.05, 
                        colour = "black",
                        size = text_label_size)
          } else {
            p1_4 <- if (is.null(bar_fill_colour)) {
              p1_1 +
                geom_bar(aes(y = after_stat(count)/sum(after_stat(count)), fill = index, group=index),
                         position="stack", stat="count", show.legend = FALSE, width = bar_width)
            } else {
              p1_1 +
                geom_bar(aes(y = after_stat(count)/sum(after_stat(count)), group=index), fill = bar_fill_colour,
                         position="stack", stat="count", show.legend = FALSE, width = bar_width)
            }
            p1_4 +
              scale_y_continuous(labels = scales::percent, n.breaks = y_axis_breaks, limits = y_axis_limits,
                                 expand = expansion(mult = expand_yaxis)) 
          }
          p1_3
        }
        else {
          p1_3 <- if (text_label == TRUE) {
            p1_4 <- if (is.null(bar_fill_colour)) {
              p1_1 +
                geom_bar(aes(fill = index, group=index), 
                         position="stack", stat="count", show.legend = FALSE, width = bar_width) 
            } else {
              p1_1 +
                geom_bar(aes(group=index), fill = bar_fill_colour, 
                         position="stack", stat="count", show.legend = FALSE, width = bar_width)
            }
            
            p1_4 +
              scale_y_continuous(n.breaks = y_axis_breaks, limits = y_axis_limits,
                                 expand = expansion(mult = expand_yaxis)) +
              geom_text(aes(label = paste0(after_stat(count), " (",
                                           scales::percent(after_stat(count)/sum(after_stat(count)),
                                                           accuracy = percent_text_accuracy),")" ), group=index ),
                        stat = "count", 
                        hjust = -0.05, 
                        colour = "black",
                        size = text_label_size)
          } else {
            p1_4 <- if (is.null(bar_fill_colour)) {
              p1_1 +
                geom_bar(aes(fill = index, group=index), 
                         position="stack", stat="count", show.legend = FALSE, width = bar_width) 
            } else {
              p1_1 +
                geom_bar(aes(group=index), fill = bar_fill_colour, 
                         position="stack", stat="count", show.legend = FALSE, width = bar_width)
            }
            
            p1_4 +
              scale_y_continuous(n.breaks = y_axis_breaks, limits = y_axis_limits,
                                 expand = expansion(mult = expand_yaxis)) 
          }
          p1_3
        }
        p1_2
        
      } else { 
        p1_1 <- ggplot(data=df, aes(x= forcats::fct_infreq(index)))
        
        p1_2 <- if (percent_yaxis == TRUE) {
          p1_3 <- if (text_label == TRUE) {
            p1_4 <- if (is.null(bar_fill_colour)) {
              p1_1 +
                geom_bar(aes(y = after_stat(count)/sum(after_stat(count)), fill = index, group=index),
                         position="stack", stat="count", show.legend = FALSE, width = bar_width) 
            } else {
              p1_1 +
                geom_bar(aes(y = after_stat(count)/sum(after_stat(count)), group=index), fill = bar_fill_colour,
                         position="stack", stat="count", show.legend = FALSE, width = bar_width)
            }
            p1_4 +
              scale_y_continuous(labels = scales::percent, n.breaks = y_axis_breaks, limits = y_axis_limits,
                                 expand = expansion(mult = expand_yaxis)) +
              geom_text(aes(y = after_stat(count)/sum(after_stat(count)),
                            label = scales::percent(after_stat(count)/sum(after_stat(count)),  
                                                    accuracy = percent_text_accuracy),group=index),
                        stat = "count", 
                        vjust = -0.2, 
                        colour = "black",
                        size = text_label_size)
          } else {
            p1_4 <- if (is.null(bar_fill_colour)) {
              p1_1 +
                geom_bar(aes(y = after_stat(count)/sum(after_stat(count)), fill = index, group=index),
                         position="stack", stat="count", show.legend = FALSE, width = bar_width)
            } else {
              p1_1 +
                geom_bar(aes(y = after_stat(count)/sum(after_stat(count)), group=index), fill = bar_fill_colour,
                         position="stack", stat="count", show.legend = FALSE, width = bar_width)
            }
            p1_4 +
              scale_y_continuous(labels = scales::percent, n.breaks = y_axis_breaks, limits = y_axis_limits,
                                 expand = expansion(mult = expand_yaxis)) 
          }
          p1_3
        }
        else {
          p1_3 <- if (text_label == TRUE) {
            p1_4 <- if (is.null(bar_fill_colour)) {
              p1_1 +
                geom_bar(aes(fill = index, group=index), 
                         position="stack", stat="count", show.legend = FALSE, width = bar_width) 
            } else {
              p1_1 +
                geom_bar(aes(group=index), fill = bar_fill_colour, 
                         position="stack", stat="count", show.legend = FALSE, width = bar_width)
            }
            
            p1_4 +
              scale_y_continuous(n.breaks = y_axis_breaks, limits = y_axis_limits,
                                 expand = expansion(mult = expand_yaxis)) +
              geom_text(aes(label = paste0(after_stat(count), " (",
                                           scales::percent(after_stat(count)/sum(after_stat(count)),
                                                           accuracy = percent_text_accuracy),")" ), group=index ),
                        stat = "count", 
                        vjust = -0.2, 
                        colour = "black",
                        size = text_label_size)
          } else {
            p1_4 <- if (is.null(bar_fill_colour)) {
              p1_1 +
                geom_bar(aes(fill = index, group=index), 
                         position="stack", stat="count", show.legend = FALSE, width = bar_width) 
            } else {
              p1_1 +
                geom_bar(aes(group=index), fill = bar_fill_colour, 
                         position="stack", stat="count", show.legend = FALSE, width = bar_width)
            }
            
            p1_4 +
              scale_y_continuous(n.breaks = y_axis_breaks, limits = y_axis_limits,
                                 expand = expansion(mult = expand_yaxis)) 
          }
          p1_3
        }
        p1_2
      }
      
      p2 <- p1 +
        scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = x_axis_label_wrap_width))
      
      p2
    } else {
      if (histogram == TRUE) {
        ggplot(data=df, aes(x = as.numeric(index))) + 
          geom_histogram(aes(y = after_stat(density)), 
                         bins = histogram_bins, color = "black", fill = "gray") +
          geom_density(linewidth = 0.5, colour = "royalblue",
                       fill = "royalblue", alpha = 0.25) +  
          geom_vline(aes(xintercept = mean(index, na.rm=TRUE)), 
                     linetype = "dashed", linewidth = 0.6) +
          scale_x_continuous(n.breaks = x_axis_breaks_histogram) +
          scale_y_continuous(n.breaks = y_axis_breaks, limits = y_axis_limits,
                             expand = expansion(mult = expand_yaxis)) 
      } else {
        ggplot(data=df, aes(y = as.numeric(index))) + 
          geom_boxplot(outlier.colour = "red", outlier.shape = 1, show.legend = FALSE) +
          scale_x_discrete() +
          scale_y_continuous(n.breaks = y_axis_breaks, limits = y_axis_limits,
                             expand = expansion(mult = expand_yaxis))
      }
    }
    plot1 <- if (is.null(title_label)) {
      if (histogram == TRUE & !is.factor(index)) {
        plot +
          labs(x=NULL,y="Density", title = stringr::str_wrap(label, width = title_label_wrap_width))
      } else {
        plot +
          labs(x=NULL,y=NULL, title = stringr::str_wrap(label, width = title_label_wrap_width))
      }
    } else {
      if (histogram == TRUE & !is.factor(index)) {
        plot + 
          labs(x=NULL,y="Density", title = stringr::str_wrap(title_label, width = title_label_wrap_width))
      } else {
        plot + 
          labs(x=NULL,y=NULL, title = stringr::str_wrap(title_label, width = title_label_wrap_width))
      }
    }
    
    plot1
    
  })
  out
  
}

