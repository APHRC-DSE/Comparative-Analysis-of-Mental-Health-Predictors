library(dplyr)
library(readr)
library(janitor)
library(haven)
library(labelled)

working_directory

## Reading data from local folder

data_files <- list.files(path = data_Dir, full.names = F)

df_list <- sapply(data_files, function(x){
  nn <- x
  
  readr::read_csv(base::file.path(data_Dir, nn))
  
}, simplify=FALSE)

df_university_raw <- df_list[["University_data_cleaned.csv"]] 
df_community_raw <- df_list[["community_data_cleaned_v2.csv"]] 

df_university_raw_final <- df_university_raw %>%
  janitor::clean_names()

df_community_raw_final <- df_community_raw %>%
  janitor::clean_names()

## creating data dictionary

university_raw_attribute <- base::as.data.frame(labelled::look_for(df_university_raw, labels = TRUE, values = TRUE))
community_raw_attribute <- base::as.data.frame(labelled::look_for(df_community_raw, labels = TRUE, values = TRUE))

university_raw_final_attribute <- base::as.data.frame(labelled::look_for(df_university_raw_final, labels = TRUE, values = TRUE))
community_raw_final_attribute <- base::as.data.frame(labelled::look_for(df_community_raw_final, labels = TRUE, values = TRUE))

#university_raw_attribute <- base::as.data.frame(labelled::generate_dictionary(df_university_raw, labels = TRUE, values = TRUE))
#community_raw_attribute <- base::as.data.frame(labelled::generate_dictionary(df_university_raw, labels = TRUE, values = TRUE))

## Save raw dictionary

writexl::write_xlsx(list(university_raw = university_raw_attribute
                         , university_raw_final = university_raw_final_attribute
                         , community_raw = community_raw_attribute
                         , community_raw_final = community_raw_final_attribute
                         ),
                    path = base::file.path(output_Dir, paste0("raw_attributes_dictionary.xlsx") )
                    )

