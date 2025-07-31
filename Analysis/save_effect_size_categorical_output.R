library(dplyr)
library(writexl)

working_directory

## Saving effect size stats Output

if (length(outcome_character_vars)>0) {
  for (i in names(effect_size_categorical_stats)) {
writexl::write_xlsx(effect_size_categorical_stats[[i]],
                    path = base::file.path(output_Dir, paste0("effect_size_categorical_",i,".xlsx") )
                    )
  }
  
  writexl::write_xlsx(effect_size_categorical_combined_stats,
                    path = base::file.path(output_Dir, paste0("effect_size_categorical_combined",".xlsx") )
                    )
  
 } else {
    print(paste0("No effect size analysis done"))
  }


