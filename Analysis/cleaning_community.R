library(dplyr)
library(forcats)
library(lubridate)
library(labelled)
library(tibble)
library(haven)
library(stringr)

working_directory

## Cleaning the community data

df_community_clean <- df_community_raw_final %>%
  dplyr::rename(any_of(community_new_variable_names)
                ) %>%
  dplyr::mutate(education = factor(education, levels = c("No formal education", "Primary level", "Secondary level",
                                                           "Tertiary level")
                                   ) # encode education column to factor
                , site = "Community"
                , site = factor(site, levels = c("Community", "University")
                                ) # encode site column to factor
                , agegrps = ifelse(age <20, "< 20 years",
                                   ifelse(age <35, "20-34 years",
                                          ifelse(age <50, "35-49 years", 
                                                 ifelse(age >49, "50 and above", NA
                                                        )
                                                 )
                                          )
                                   )
                , agegrps = factor(agegrps, levels = c("< 20 years", "20-34 years", "35-49 years", "50 and above")
                                   ) # encode agegrps column to factor
                , marstat = stringr::str_to_title(marstat)
                , marstat = factor(marstat, levels = c("Single", "Married")
                                   ) # encode marstat column to factor
                , activitylevel = factor(activitylevel, levels = c("Never", "Rarely", "Occasionally", "Frequently")
                                         ) # encode activitylevel column to factor
                , social_media = factor(social_media, levels = c("< 1 hour", "1-3 hours", "3-5 hours", "> 5 hours")
                                        ) # encode social_media column to factor
                , across(c(gad7_1:gad7_7), ~ifelse(.x == 0, "Not at all sure",
                                                   ifelse(.x == 1, "Several days", 
                                                          ifelse(.x == 2, "Over half the days", 
                                                                 ifelse(.x == 3, "Nearly every day", NA
                                                                        )
                                                                 )
                                                          )
                                                   ) 
                         )
                , across(c(gad7_1:gad7_7), ~factor(.x, levels = c("Not at all sure", "Several days", "Over half the days",
                                                                  "Nearly every day")
                                                   ) # encode gad7 questions columns to factor
                           )
                , gad7_anxiety = ifelse(gad7_total_score < 10, "No Anxiety (0-9)",
                                        ifelse(gad7_total_score > 9, "Anxiety (10 and above)", NA
                                               )
                                        )
                , gad7_anxiety = factor(gad7_anxiety, levels = c("No Anxiety (0-9)", "Anxiety (10 and above)")
                                        ) # encode gad7_anxiety column to factor
                , across(c(phq9_1:phq9_9), ~ifelse(.x == 0, "Not at all",
                                                   ifelse(.x == 1, "Several days", 
                                                          ifelse(.x == 2, "More than half the days", 
                                                                 ifelse(.x == 3, "Nearly every day", NA
                                                                        )
                                                                 )
                                                          )
                                                   ) 
                         )
                , across(c(phq9_1:phq9_9), ~factor(.x, levels = c("Not at all", "Several days", "More than half the days",
                                                                  "Nearly every day")
                                                   ) # encode phq9 questions columns to factor
                         )
                , phq9_depression = ifelse(phq9_totalscore < 13, "No Depression (0-12)",
                                           ifelse(phq9_totalscore > 12, "Depression (13 and above)", NA
                                                  )
                                           )
                , phq9_depression = factor(phq9_depression, levels = c("No Depression (0-12)", "Depression (13 and above)")
                                           ) # encode phq9_depression column to factor
                , across(c(psq_hypomania:psq_hallucinations2), ~ifelse(.x == 0, "No",
                                                                       ifelse(.x == 1, "Yes",NA
                                                                              )
                                                                       )
                         )
                , psq_psychosis = ifelse(psq_totalscore < 3, "No Psychosis (0-2)",
                                         ifelse(psq_totalscore > 2, "Psychosis (3 and above)", NA
                                                )
                                         )
                , psq_psychosis = factor(psq_psychosis, levels = c("No Psychosis (0-2)", "Psychosis (3 and above)")
                                         ) # encode psq_psychosis column to factor
                , across(c(sex, h_head, psq_psychosis, psq_hypomania:psq_hallucinations2), ~as.factor(.x)
                         ) #columns to factor alphabeticaly
                ) %>%
  labelled::set_variable_labels(site = "Study Population"
                                #labeling created variables
                                ) %>% 
  labelled::set_variable_labels(!!!community_new_labels[names(community_new_labels) %in% names(.)]
                                ) #labeling variables from data dictionary

