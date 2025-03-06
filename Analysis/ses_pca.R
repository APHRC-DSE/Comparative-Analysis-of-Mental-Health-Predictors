library(dplyr)
library(factoextra)
library(labelled)
library(tidyr)
library(tibble)

working_directory

## group ses variables 
### if empty vector use character()

ses_pca_vars <- selected_vars_df$new_variable[selected_vars_df$select_group == "ses pca" & !is.na(selected_vars_df$select_group)]
id_vars <- selected_vars_df$new_variable[selected_vars_df$select_group == "study_id"& !is.na(selected_vars_df$select_group)]

if (length(ses_pca_vars)>1) {
  
ses_df <- df_clean %>%
  dplyr::select(any_of(c(id_vars, ses_pca_vars))
                ) %>%
  dplyr::mutate(across(any_of(ses_pca_vars), ~as.numeric(.x))
         ) %>%
  tidyr::drop_na()

## Perform PCA

### prcomp() uses the singular value decomposition (SVD) which examines the covariances / correlations between individuals
ses_pca <- stats::prcomp(ses_df %>% dplyr::select(-any_of(id_vars)), center = TRUE, scale. = TRUE)
ses_pca_summ <- summary(ses_pca)
print(ses_pca_summ)

### The first principal component explains the largest proportion of the total variance and it is used as the wealth index to represent 
### the household's wealth
ses_scores <- ses_pca$x[, 1, drop=TRUE]

ses_loadings <- ses_pca_summ$rotation[, 1, drop=TRUE]

if(min(sign(ses_loadings)) != max(sign(ses_loadings))){
	stop("PC1 is not a positively signed index")
}

ses_report <- paste0(paste0(ses_pca_vars, collapse=", "), " used to create SES index")
print(ses_report)

## Variance explained plot

### Visualize eigenvalues (scree plot). Show the percentage of variances explained by each principal component.
ses_explained_plot <- factoextra::fviz_screeplot(ses_pca, addlabels = TRUE)
print(ses_explained_plot)

ggsave(plot=ses_explained_plot, height = 7, width = 10,
       filename = "ses_scree_plot.png", path = output_Dir, bg='white')

## Principal Component plots

### Graph of variables. Positive correlated variables point to the same side of the plot. Negative correlated variables point to opposite sides 
###of the graph.

ses_pc_plot <- fviz_pca_var(ses_pca,
                            col.var = "contrib", # Color by contributions to the PC
                            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                            repel = TRUE     # Avoid text overlapping
)

print(ses_pc_plot)

ggsave(plot=ses_pc_plot, height = 7, width = 10,
       filename = "ses_pc_plot.png", path = output_Dir, bg='white')

## adding ses columns to data set
df_final <- df_clean %>%
  dplyr::left_join(ses_df %>%
              dplyr::mutate(ses_scores = ses_scores) %>%
              dplyr::select(any_of(id_vars), ses_scores), by = c(id_vars)) %>%
  dplyr::mutate(ses_index = cut(ses_scores, breaks = 3, 
                         labels = c("Low", "Middle", "High"))) %>%
  labelled::set_variable_labels(#labeling created variables
    ses_scores = "socio-economic status scores",
    ses_index = "socio-economic status"
  )

## creating data dictionary
final_attribute <- base::as.data.frame(labelled::generate_dictionary(df_final, labels = TRUE, values = TRUE))

## Creating a named vector to quickly assign the variable labels
final_labels <- final_attribute%>%
  dplyr::select(variable, label)%>%
  tibble::deframe()
} else {
  
  df_final <- df_clean
  
  ## creating data dictionary
  final_attribute <- base::as.data.frame(labelled::generate_dictionary(df_final, labels = TRUE, values = TRUE))
  
  ## Creating a named vector to quickly assign the variable labels
  final_labels <- final_attribute %>%
    dplyr::select(variable, label) %>%
    tibble::deframe()
  
}

