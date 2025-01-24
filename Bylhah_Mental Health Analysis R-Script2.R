################################################################################
# Mental Health Analysis
# Loading necessary packages
################################################################################
setwd("C:/Users/DIT/Desktop/BylhahMugotitsa/Cleaned Datasets")
getwd() # Getting the working directory

rm(list = ls())
#install.packages("cardx", dependencies = TRUE)

library(haven)
library(foreign)
library(tidyverse)
library(dplyr)
library(stringr) # manipulating strings
library(gtsummary)
library(gt)
library(purrr)
library(ggplot2)
library(skimr) # summaries of data
library(visdat)
library(naniar)
library(caret)
library(readxl)
library(reshape2)
library(cardx)
#update.packages(c("tidyverse","gtsummary","dplyr","purrr"))

################################################################################
# Reading in datasets
################################################################################
CD <- read.csv("community3.csv")
SD <- read.csv("studentdata2.csv")

#view(CD)
#View(SD)

dim(CD)
class(CD$GADcategory)
summary(CD)

dim(SD)
class(SD$GADcategory)
summary(SD)

# Replace all blanks "" with NA
#CD[CD] <- NA

# replace all blanks with NAs
CD = CD %>% 
          mutate(across(everything(), ~as.character(.))) %>% 
          mutate(across(everything(), ~na_if(., "")))

SD = SD %>% 
  mutate(across(everything(), ~as.character(.))) %>% 
  mutate(across(everything(), ~na_if(., "")))

# Changing the class of Categorical Variables from "Character" to "Factor"
# Community data
CD <- CD %>% 
            mutate(across(where(is.character), as.factor))

summary(CD)

# Students data
SD <- SD %>% 
  mutate(across(where(is.character), as.factor))

summary(SD)

# Filter data sets where GAD != NA & PHQ != NA
#CD <- CD %>% filter(!is.na(GAD))

################################################################################
# Combining the 2 data sets
################################################################################
colnames(CD)
colnames(SD)

# Change all column names to lower case
CD = CD %>% rename_with(~ str_to_lower(.), everything())

SD = SD %>% rename_with(~ str_to_lower(.), everything())

# Rename columns "University & Community" as "study_site"
CD = rename(CD, study_site = community)

SD = rename(SD, study_site = univeristy)

# Append data sets
CD_SD <- rbind(CD,SD)

View(CD_SD)

################################################################################
# Perform descriptive analysis for the combined dataset
################################################################################
# Selecting Var for Descriptive table
CD_SD$age = as.numeric(CD_SD$age)

# Changing outcome class to numeric
class(CD_SD$gad)
CD_SD$gad = as.numeric(CD_SD$gad)

class(CD_SD$phq)
CD_SD$phq = as.numeric(CD_SD$phq)

# Transform outcome GAD & PHQ as Binary
CD_SD$b_gad = ifelse(CD_SD$gad >= 10, "Have anxiety", "No anxiety")
class(CD_SD$b_gad)
CD_SD$b_gad = as.factor(CD_SD$b_gad)

CD_SD$b_phq = ifelse(CD_SD$phq > 10, "Have depression", "Not depressed")
class(CD_SD$b_phq)
CD_SD$b_phq = as.factor(CD_SD$b_phq)

CD_SD$psychosis = ifelse(CD_SD$psychosis == "Yes", "Have psychosis", "No psychosis")
class(CD_SD$psychosis)
CD_SD$psychosis = as.factor(CD_SD$psychosis)

table(CD_SD$b_gad)
table(CD_SD$b_phq)
table(CD_SD$psychosis)

CD_SD <- CD_SD %>% 
  select(study_site, new_agegrp, sex, marstat, education, hhead,activitylevel
         ,socialmedia, supportive, incomesource, difficult, howoften, awareness
         ,mhservices, symptoms, seekhelp, b_gad, b_phq, psychosis)

CD_SD <- CD_SD %>% 
        mutate(across(c(study_site, new_agegrp, sex, marstat, education, hhead,activitylevel
                        ,socialmedia, supportive, incomesource, difficult, howoften, awareness
                        ,mhservices, symptoms, seekhelp, b_gad, b_phq, psychosis),
        as.factor))

summary(CD_SD$psychosis)

# Visualize missingness
gg_miss_var(CD_SD, show_pct = TRUE)

# Descriptive summary statistics
tbl_summary(CD_SD, by = study_site)

# Method 1

CD_SD %>% 
  tbl_summary(by = study_site,
              # missing = "no", # Exclude missing data rows from the table
              missing_text = "No response") %>% 
    add_overall() %>% 
    #add_difference() %>%
    add_p() %>% 
    add_n() %>% 
    #add_ci() %>% 
    add_stat_label(
      label = all_continuous() ~ "Median (IQR)")

# Method 2

Xtics_Table <- CD_SD %>% 
  tbl_summary(
    by = study_site,
    # missing = "no", # Exclude missing data rows from the table
    missing_text = "No responses (n)") %>% #,
    #percent = "cell") %>% 
    add_overall() %>% 
    add_p() %>% 
    add_n() %>% 
    add_stat_label(
      label = all_continuous() ~ "Median (IQR)") %>% 
    # aesthetics helpers
    bold_p() %>% 
    bold_labels() %>% #bold_labels()
    italicize_levels() %>%  #italicize_labels()
  modify_table_body(
    ~ .x %>%
      filter_at(vars(starts_with("stat_")), any_vars(. != "0 (0%)"))
  )

Xtics_Table

################################################################################
# Standardize data sets for Bivariable and Multivariable analysis for the combined dataset
################################################################################

# Normalize numeric variables
standardized_data <- CD_SD %>%
  mutate(across(where(is.numeric), ~ (.- mean(., na.rm = TRUE)) / sd(., na.rm = TRUE)))

# Split data into community and hospital datasets
standardized_comm <- standardized_data %>% filter(study_site == "Community")
standardized_univ <- standardized_data %>% filter(study_site == "University")

# List of predictors (replace with your variable names)
predictors <- c("new_agegrp","sex","marstat","education","hhead","activitylevel",
                "socialmedia","supportive","incomesource","difficult","howoften",
                "awareness","mhservices","symptoms","seekhelp")

################################################################################
# Re-leveling categorical variables to create a Ref category
###############################################################################
CD_SD$new_agegrp = relevel(CD_SD$new_agegrp, ref = "18-24")
CD_SD$sex = relevel(CD_SD$sex, ref = "Male")
CD_SD$marstat = relevel(CD_SD$marstat, ref = "single/widowed/separated")
CD_SD$education = relevel(CD_SD$education, ref = "Degree level")
CD_SD$hhead = relevel(CD_SD$hhead, ref = "No")
CD_SD$activitylevel = relevel(CD_SD$activitylevel, ref = "Frequently")
CD_SD$socialmedia = relevel(CD_SD$socialmedia, ref = "< 1 hour")
CD_SD$supportive = relevel(CD_SD$supportive, ref = "Not suppotive")
CD_SD$incomesource = relevel(CD_SD$incomesource, ref = "Scholarship")
CD_SD$difficult = relevel(CD_SD$difficult, ref = "No")
CD_SD$howoften = relevel(CD_SD$howoften, ref = "Never")
CD_SD$awareness = relevel(CD_SD$awareness, ref = "Yes")
CD_SD$mhservices = relevel(CD_SD$mhservices, ref = "Yes")
CD_SD$symptoms = relevel(CD_SD$symptoms, ref = "No")
CD_SD$seekhelp = relevel(CD_SD$seekhelp, ref = "Yes")
CD_SD$study_site = relevel(CD_SD$study_site, ref = "University")
CD_SD$b_gad = relevel(CD_SD$b_gad, ref = "No anxiety")
CD_SD$b_phq = relevel(CD_SD$b_phq, ref = "Not depressed")
CD_SD$psychosis = relevel(CD_SD$psychosis, ref = "No psychosis")

# Fitting bivariate logistic regression models (unadjusted)
# For Gad_7
# Sub-setting MH_clean dataset to exclude other outcome carz

# Select relevant variables for Overall univariate analysis

colnames(CD_SD)
sum(is.na(CD_SD$b_gad))
CD_SD$b_gad <- as.factor(CD_SD$b_gad)
class(CD_SD$b_gad)
levels(CD_SD$study_site)

unv_overallGAD <- CD_SD %>%
  select(b_gad, new_agegrp, sex, marstat, education, hhead,
         activitylevel, socialmedia, supportive, incomesource, difficult, 
         howoften, awareness, mhservices, symptoms, seekhelp) %>% 
   tbl_uvregression(
    method       = glm,
    y            = b_gad, # Use column name as a string
    method.args  = list(family = binomial(link = "logit")),
    exponentiate = TRUE) %>% 
    add_significance_stars(
    hide_p = FALSE, hide_se = TRUE, hide_ci = FALSE) %>% 
    # aesthetics helpers
    bold_p() %>% 
    bold_labels() %>% #bold_labels()
    italicize_levels()

unv_overallGAD

# Filtering for 'Community' univariate analysis
unv_comm <- subset(CD_SD, study_site == "Community") %>%
  select(b_gad, new_agegrp, sex, marstat, education, hhead,
         activitylevel, socialmedia) %>% 
  tbl_uvregression(
    method       = glm,
    y            = b_gad, # Use column name as a string
    method.args  = list(family = binomial(link = "logit")),
    exponentiate = TRUE) %>% 
    add_significance_stars(
    hide_p = FALSE, hide_se = TRUE, hide_ci = FALSE) %>% 
    # aesthetics helpers
    bold_p() %>% 
    bold_labels() %>% #bold_labels()
    italicize_levels()

unv_comm

# Filtering for 'University'  univariate analysis
unv_univ <- subset(CD_SD, study_site == "University") %>%
  select(b_gad, new_agegrp, sex, marstat, hhead,
         activitylevel, socialmedia, supportive, incomesource, difficult, 
         howoften, awareness, mhservices, symptoms, seekhelp) %>% 
  tbl_uvregression(
    method       = glm,
    y            = b_gad, # Use column name as a string
    method.args  = list(family = binomial(link = "logit")),
    exponentiate = TRUE) %>% 
    add_significance_stars(
    hide_p = FALSE, hide_se = TRUE, hide_ci = FALSE) %>% 
    # aesthetics helpers
    bold_p() %>% 
    bold_labels() %>% #bold_labels()
    italicize_levels()

unv_univ

# Combining 'Overall table', 'Community' & 'University'
univar_table <- tbl_merge(
                  tbls = list(unv_overallGAD, unv_comm, unv_univ),
                  tab_spanner = c("Pooled Anxiety (GAD) UOR",
                                  "Community Anxiety (GAD) UOR",
                                  "University (GAD) UOR"))
univar_table

#b_phq overall

unv_overallPHQ <- CD_SD %>%
  select(b_phq, new_agegrp, sex, marstat, education, hhead,
         activitylevel, socialmedia, supportive, incomesource, difficult, 
         howoften, awareness, mhservices, symptoms, seekhelp) %>% 
  tbl_uvregression(
    method       = glm,
    y            = b_phq, # Use column name as a string
    method.args  = list(family = binomial(link = "logit")),
    exponentiate = TRUE) %>% 
  add_significance_stars(
    hide_p = FALSE, hide_se = TRUE, hide_ci = FALSE) %>% 
    # aesthetics helpers
    bold_p() %>% 
    bold_labels() %>% #bold_labels()
    italicize_levels()

unv_overallPHQ

# Filtering for 'Community' univariate analysis
unv_comm <- subset(CD_SD, study_site == "Community") %>%
  select(b_phq, new_agegrp, sex, marstat, education, hhead,
         activitylevel, socialmedia) %>% 
  tbl_uvregression(
    method       = glm,
    y            = b_phq, # Use column name as a string
    method.args  = list(family = binomial(link = "logit")),
    exponentiate = TRUE) %>% 
  add_significance_stars(
    hide_p = FALSE, hide_se = TRUE, hide_ci = FALSE) %>% 
    # aesthetics helpers
    bold_p() %>% 
    bold_labels() %>% #bold_labels()
    italicize_levels()

unv_comm

# Filtering for 'University'  univariate analysis
unv_univ <- subset(CD_SD, study_site == "University") %>%
  select(b_phq, new_agegrp, sex, marstat, hhead,
         activitylevel, socialmedia, supportive, incomesource, difficult, 
         howoften, awareness, mhservices, symptoms, seekhelp) %>% 
  tbl_uvregression(
    method       = glm,
    y            = b_phq, # Use column name as a string
    method.args  = list(family = binomial(link = "logit")),
    exponentiate = TRUE) %>% 
  add_significance_stars(
    hide_p = FALSE, hide_se = TRUE, hide_ci = FALSE) %>% 
    # aesthetics helpers
    bold_p() %>% 
    bold_labels() %>% #bold_labels()
    italicize_levels()

unv_univ

# Combining 'Overall table', 'Community' & 'University'
univar_table2 <- tbl_merge(
  tbls = list(unv_overallPHQ, unv_comm, unv_univ),
  tab_spanner = c("Pooled Depression(PHQ) UOR",
                  "Community (PHQ) UOR",
                  "University (PHQ) UOR"))
univar_table2

# Fitting Multivariable logistic regression models (adjusted)
# For Gad_7
# Sub-setting MH_clean dataset to exclude other outcome carz
unv_overallGAD <- CD_SD %>% 
  select(b_gad, new_agegrp, sex, marstat, education, hhead,
         activitylevel, socialmedia,study_site) %>% 
  tbl_uvregression(
    method       = glm,
    y            = b_gad, # Use column name as a string
    method.args  = list(family = binomial(link = "logit")),
    exponentiate = TRUE) %>% 
  add_significance_stars(
    hide_p = FALSE, hide_se = TRUE, hide_ci = FALSE) %>% 
  # aesthetics helpers
  bold_p() %>% 
  bold_labels() %>% #bold_labels()
  italicize_levels()


unv_overallGAD

# Adjusted MDD

unv_overallPHQ <- CD_SD %>%
  select(b_phq, new_agegrp, sex, marstat, education, hhead,
         activitylevel, socialmedia, study_site) %>% 
  tbl_uvregression(
    method       = glm,
    y            = b_phq, # Use column name as a string
    method.args  = list(family = binomial(link = "logit")),
    exponentiate = TRUE) %>% 
  add_significance_stars(
    hide_p = FALSE, hide_se = TRUE, hide_ci = FALSE) %>% 
  # aesthetics helpers
  bold_p() %>% 
  bold_labels() %>% #bold_labels()
  italicize_levels()

unv_overallPHQ

# Adjusted Psy

unv_overallPsy <- CD_SD %>%
  select(psychosis, new_agegrp, sex, marstat, education, hhead,
         activitylevel, socialmedia, study_site) %>% 
  tbl_uvregression(
    method       = glm,
    y            = psychosis, # Use column name as a string
    method.args  = list(family = binomial(link = "logit")),
    exponentiate = TRUE) %>% 
  add_significance_stars(
    hide_p = FALSE, hide_se = TRUE, hide_ci = FALSE) %>% 
  # aesthetics helpers
  bold_p() %>% 
  bold_labels() %>% #bold_labels()
  italicize_levels()

unv_overallPsy

# Select relevant variables for Overall univariate analysis
# Omit if GAD or PHQ == .
#CD_SD = CD_SD %>% filter(!is.na(b_gad))

#CD_SD = CD_SD %>% filter(!is.na(b_phq))

### 
mult_allGad <- glm(b_gad ~ new_agegrp+sex+marstat+education+hhead+
                     activitylevel+socialmedia+study_site,
                    data = CD_SD, family = binomial(link = "logit")
    ) %>% 
  tbl_regression(
        exponentiate = TRUE) %>% 
  # add_* helpers
  #add_n(location = "level") %>% 
  #add_nevent(location = "level") %>% 
  #add_global_p() %>% 
  #add_q() %>% 
  add_significance_stars(
    hide_p = FALSE, hide_se = TRUE, hide_ci = FALSE)%>% 
  #add_vif() %>% 
# aesthetics helpers
  bold_p() %>% 
  bold_labels() %>% #bold_labels()
  italicize_levels()

  # modify_* helpers

mult_allGad

#PHQ

mult_allPHQ <- glm(b_phq ~ new_agegrp+sex+marstat+education+hhead+
                     activitylevel+socialmedia+study_site,
                   data = CD_SD, family = binomial(link = "logit")) %>% 
  tbl_regression(
    exponentiate = TRUE) %>% 
  # add_* helpers
  #add_n(location = "level") %>% 
  #add_nevent(location = "level") %>% 
  #add_global_p() %>% 
  #add_q() %>% 
  add_significance_stars(
    hide_p = FALSE, hide_se = TRUE, hide_ci = FALSE) %>% 
#add_vif() %>% 
  # aesthetics helpers
  bold_p() %>% 
  bold_labels() %>% #bold_labels()
  italicize_levels()
  
# modify_* helpers

mult_allPHQ

# Psychosis

mult_allPsy <- glm(psychosis ~ new_agegrp+sex+marstat+education+hhead+
                     activitylevel+socialmedia+study_site,
                   data = CD_SD, family = binomial(link = "logit")) %>% 
  tbl_regression(
    exponentiate = TRUE) %>% 
  # add_* helpers
  #add_n(location = "level") %>% 
  #add_nevent(location = "level") %>% 
  #add_global_p() %>% 
  #add_q() %>% 
  add_significance_stars(
    hide_p = FALSE, hide_se = TRUE, hide_ci = FALSE) %>% 
  #add_vif() %>% 
  # aesthetics helpers
  bold_p() %>% 
  bold_labels() %>% #bold_labels()
  italicize_levels()

# modify_* helpers

mult_allPsy

# Combining 'Overall table', 'IMHDSS table' & 'Kagando table'
multvar_table_gad <- tbl_merge(
  tbls = list(unv_overallGAD, mult_allGad),
  tab_spanner = c("Generalized Anxiety Disorder(GAD) UOR",
                  "Generalized Anxiety Disorder(GAD) AOR"))
multvar_table_gad

# PHQ
multvar_table_phq <- tbl_merge(
  tbls = list(unv_overallPHQ, mult_allPHQ),
  tab_spanner = c("Major Depression Disorder(PHQ) UOR",
                  "Major Depression Disorder(PHQ) AOR"))
multvar_table_phq

# Psychosis
multvar_table_psy <- tbl_merge(
  tbls = list(unv_overallPHQ, mult_allPHQ),
  tab_spanner = c("Psychosis (UOR)",
                  "Psychosis (AOR)"))
multvar_table_psy

################################################################################
### Generating a table for 
# Sub-setting MH_clean dataset to exclude other outcome varz

# Filter out data where the study site is 'Community'
study_site_univ <- subset(CD_SD, study_site == "University")

unv_overallGAD_u <- study_site_univ %>%
  select(b_gad, supportive, incomesource, difficult, howoften, awareness, 
           mhservices, symptoms, seekhelp) %>% 
  tbl_uvregression(
    method       = glm,
    y            = b_gad, # Use column name as a string
    method.args  = list(family = binomial(link = "logit")),
    exponentiate = TRUE) %>% 
  add_significance_stars(
    hide_p = FALSE, hide_se = TRUE, hide_ci = FALSE) %>% 
  # aesthetics helpers
  bold_p() %>% 
  bold_labels() %>% #bold_labels()
  italicize_levels()


unv_overallGAD_u

# Adjusted MDD

unv_overallPHQ_u <- study_site_univ %>%
  select(b_phq, supportive, incomesource, difficult, howoften, awareness, 
         mhservices, symptoms, seekhelp) %>% 
  tbl_uvregression(
    method       = glm,
    y            = b_phq, # Use column name as a string
    method.args  = list(family = binomial(link = "logit")),
    exponentiate = TRUE) %>% 
  add_significance_stars(
    hide_p = FALSE, hide_se = TRUE, hide_ci = FALSE) %>% 
  # aesthetics helpers
  bold_p() %>% 
  bold_labels() %>% #bold_labels()
  italicize_levels()

unv_overallPHQ_u

# Adjusted Psy

unv_overallPsy_u <- study_site_univ %>%
  select(psychosis, supportive, incomesource, difficult, howoften, awareness, 
         mhservices, symptoms, seekhelp) %>% 
  tbl_uvregression(
    method       = glm,
    y            = psychosis, # Use column name as a string
    method.args  = list(family = binomial(link = "logit")),
    exponentiate = TRUE) %>% 
  add_significance_stars(
    hide_p = FALSE, hide_se = TRUE, hide_ci = FALSE) %>% 
  # aesthetics helpers
  bold_p() %>% 
  bold_labels() %>% #bold_labels()
  italicize_levels()

unv_overallPsy_u

# Select relevant variables for Overall univariate analysis
# Omit if GAD or PHQ == .
#CD_SD = CD_SD %>% filter(!is.na(b_gad))

#CD_SD = CD_SD %>% filter(!is.na(b_phq))
class(study_site_univ$b_gad)
### 
mult_allGad_u <- glm(b_gad ~ supportive + incomesource + difficult + howoften + awareness + 
                     mhservices + symptoms + seekhelp,
                   data = study_site_univ, family = binomial(link = "logit")
) %>% 
  tbl_regression(
    exponentiate = TRUE) %>% 
  # add_* helpers
  #add_n(location = "level") %>% 
  #add_nevent(location = "level") %>% 
  #add_global_p() %>% 
  #add_q() %>% 
  add_significance_stars(
    hide_p = FALSE, hide_se = TRUE, hide_ci = FALSE)%>% 
  #add_vif() %>% 
  # aesthetics helpers
  bold_p() %>% 
  bold_labels() %>% #bold_labels()
  italicize_levels()

# modify_* helpers

mult_allGad

#PHQ

mult_allPHQ <- glm(b_phq ~ new_agegrp+sex+marstat+education+hhead+
                     activitylevel+socialmedia+study_site,
                   data = CD_SD, family = binomial(link = "logit")) %>% 
  tbl_regression(
    exponentiate = TRUE) %>% 
  # add_* helpers
  #add_n(location = "level") %>% 
  #add_nevent(location = "level") %>% 
  #add_global_p() %>% 
  #add_q() %>% 
  add_significance_stars(
    hide_p = FALSE, hide_se = TRUE, hide_ci = FALSE) %>% 
  #add_vif() %>% 
  # aesthetics helpers
  bold_p() %>% 
  bold_labels() %>% #bold_labels()
  italicize_levels()

# modify_* helpers

mult_allPHQ

# Psychosis

mult_allPsy <- glm(psychosis ~ new_agegrp+sex+marstat+education+hhead+
                     activitylevel+socialmedia+study_site,
                   data = CD_SD, family = binomial(link = "logit")) %>% 
  tbl_regression(
    exponentiate = TRUE) %>% 
  # add_* helpers
  #add_n(location = "level") %>% 
  #add_nevent(location = "level") %>% 
  #add_global_p() %>% 
  #add_q() %>% 
  add_significance_stars(
    hide_p = FALSE, hide_se = TRUE, hide_ci = FALSE) %>% 
  #add_vif() %>% 
  # aesthetics helpers
  bold_p() %>% 
  bold_labels() %>% #bold_labels()
  italicize_levels()

# modify_* helpers

mult_allPsy

# Combining 'Overall table', 'Community' & 'University'
multvar_table <- tbl_merge(
  tbls = list(unv_overallGAD, mult_allGad, unv_overallPHQ, mult_allPHQ, unv_overallPsy, mult_allPsy),
  tab_spanner = c("Generalized Anxiety Disorder(GAD) UOR",
                  "Generalized Anxiety Disorder(GAD) AOR",
                  "Major Depression Disorder(PHQ) UOR",
                  "Major Depression Disorder(PHQ) AOR",
                  "Psychosis (UOR)",
                  "Psychosis (AOR)"))
multvar_table

multvar_table <- tbl_merge(
  tbls = list(unv_overallGAD, mult_allGad, unv_overallPHQ, mult_allPHQ, unv_overallPsy, mult_allPsy),
  tab_spanner = c("Generalized Anxiety Disorder(GAD) UOR",
                  "Generalized Anxiety Disorder(GAD) AOR",
                  "Major Depression Disorder(PHQ) UOR",
                  "Major Depression Disorder(PHQ) AOR",
                  "Psychosis (UOR)",
                  "Psychosis (AOR)"))
multvar_table

################################################################################
# Checking for interactions
###############################################################################
# Fit the glm model for GAD
Gad_int <- glm(b_phq ~ new_agegrp + sex + marstat + education + hhead + activitylevel + socialmedia,
               family = binomial(link = "logit"), data = CD_SD)

# Add interaction terms for all outcome variables
# Gad
model_all_inter_b_gad = glm(b_gad ~ (new_agegrp + sex + marstat + education + hhead + activitylevel + socialmedia)^2,
                 family = binomial(link = "logit"), data = CD_SD)

summary(model_all_inter_b_gad)

# Simplify the model for b_gad
model_reduced_bgad <- step(model_all_inter_b_gad)
summary(model_reduced_bgad)

# Load the necessary packages
library(broom)
library(dplyr)

# Tidy the model results, exponentiate odds ratios, and include confidence intervals
bgad_model_results <- tidy(model_reduced_bgad, conf.int = TRUE, exponentiate = TRUE)

# Filter for significant interaction terms (p-value < 0.05)
bgad_sigf_inter <- bgad_model_results %>%
  filter(p.value < 0.05 & grepl(":", term)) %>%  # Keep only interaction terms with ":" and significant p-values
  select(term, estimate, conf.low, conf.high, p.value)  # Select relevant columns

# View the significant interaction terms
print(bgad_sigf_inter)

# Format the table for presentation
bgad_sigf_inter <- bgad_sigf_inter %>%
  mutate(OR = sprintf("%.2f (%.2f–%.2f)", estimate, conf.low, conf.high),
  p.value = round(p.value, 3)) %>% # Round p-value to 3 decimal points)
  select(Variable = term, `Odds Ratio (95% CI)` = OR, `P-value` = p.value)

# Export to Word or HTML if needed
library(flextable)

tb_bgad_sigf_inter <- flextable(bgad_sigf_inter)

tb_bgad_sigf_inter <- autofit(tb_bgad_sigf_inter)

save_as_docx(tb_bgad_sigf_inter, path = "C:/Users/DIT/Desktop/BylhahMugotitsa/Cleaned Datasets/GAD_significant_interactions.docx")



# PHQ
model_all_inter_b_phq = glm(b_phq ~ (new_agegrp + sex + marstat + education + hhead + activitylevel + socialmedia)^2,
                            family = binomial(link = "logit"), data = CD_SD)

summary(model_all_inter_b_phq)

# Simplify the model for b_gad
model_reduced_phq <- step(model_all_inter_b_phq)
summary(model_reduced_phq)

# Tidy the model results, exponentiate odds ratios, and include confidence intervals
phq_model_results <- tidy(model_reduced_phq, conf.int = TRUE, exponentiate = TRUE)

# Filter for significant interaction terms (p-value < 0.05)
phq_sigf_inter <- phq_model_results %>%
  filter(p.value < 0.05 & grepl(":", term)) %>%  # Keep only interaction terms with ":" and significant p-values
  select(term, estimate, conf.low, conf.high, p.value)  # Select relevant columns

# View the significant interaction terms
print(phq_sigf_inter)

# Format the table for presentation
phq_sigf_inter <- phq_sigf_inter %>%
  mutate(OR = sprintf("%.2f (%.2f–%.2f)", estimate, conf.low, conf.high),
         p.value = round(p.value, 3)) %>% # Round p-value to 3 decimal points)
  select(Variable = term, `Odds Ratio (95% CI)` = OR, `P-value` = p.value)

# Export to Word or HTML if needed

tb_phq_sigf_inter <- flextable(phq_sigf_inter)

tb_phq_sigf_inter <- autofit(tb_phq_sigf_inter)

save_as_docx(tb_phq_sigf_inter, path = "C:/Users/DIT/Desktop/BylhahMugotitsa/Cleaned Datasets/PHQ_significant_interactions.docx")

# Psychosis
model_all_inter_psychosis = glm(psychosis ~ (new_agegrp + sex + marstat + education + hhead + activitylevel + socialmedia)^2,
                            family = binomial(link = "logit"), data = CD_SD)

summary(model_all_inter_psychosis)

# Simplify the model for b_gad
model_reduced_psycho <- step(model_all_inter_psychosis)
summary(model_reduced_psycho)

# Tidy the model results, exponentiate odds ratios, and include confidence intervals
psycho_model_results <- tidy(model_reduced_psycho, conf.int = TRUE, exponentiate = TRUE)

# Filter for significant interaction terms (p-value < 0.05)
psycho_sigf_inter <- psycho_model_results %>%
  filter(p.value < 0.05 & grepl(":", term)) %>%  # Keep only interaction terms with ":" and significant p-values
  select(term, estimate, conf.low, conf.high, p.value)  # Select relevant columns

# View the significant interaction terms
print(psycho_sigf_inter)

# Format the table for presentation
psycho_sigf_inter <- psycho_sigf_inter %>%
  mutate(OR = sprintf("%.2f (%.2f–%.2f)", estimate, conf.low, conf.high),
         p.value = round(p.value, 3)) %>% # Round p-value to 3 decimal points)
  select(Variable = term, `Odds Ratio (95% CI)` = OR, `P-value` = p.value)

# Export to Word or HTML if needed

tb_psycho_sigf_inter <- flextable(psycho_sigf_inter)

tb_psycho_sigf_inter <- autofit(tb_psycho_sigf_inter)

save_as_docx(tb_psycho_sigf_inter, path = "C:/Users/DIT/Desktop/BylhahMugotitsa/Cleaned Datasets/Psychosis_significant_interactions.docx")

################################################################################
# Generating tables to count responses from the disorder category (Anxiety, Depression, and Psychosis) for each study site
################################################################################
library(tidyverse)
library(flextable)

## GAD-Responses
# Define the target questions
target_questions <- c("nervous", "worrying", "things", "trouble", "restless", "irritable", "afraid")

# Create a long format dataframe with the target questions and study site
gad_long <- CD_SD %>%
  pivot_longer(cols = all_of(target_questions),
               names_to = "Question", values_to = "Response")

# Summarize the data by study site, question, and response, and pivot wider
gad_summary <- gad_long %>%
  count(study_site, Question, Response) %>%
  pivot_wider(names_from = Response, values_from = n, values_fill = 0) %>%
  arrange(study_site, Question)

# Format the table using flextable
gad_resp_table <- flextable(gad_summary) %>%
  autofit() %>%
  fontsize(size = 10) %>%
  font(fontname = "Times New Roman") %>%
  #border_inner(border = fp_border(width = 1, color = "gray")) %>%
  #border_outer(border = fp_border(width = 1, color = "black")) %>%
  align(align = "center", part = "all")

# Print the formatted table
gad_resp_table

save_as_docx(gad_resp_table, path = "C:/Users/DIT/Desktop/BylhahMugotitsa/Cleaned Datasets/gad_responses_summary_table.docx")

### PHQ-Responses
# Define the target questions
target_questions <- c("interest", "hopeless", "sleeping", "tired", "appetite", "family", "concentrating", "moving", "thoughts")

# Create a long format dataframe with the target questions and study site
phq_long <- CD_SD %>%
  pivot_longer(cols = all_of(target_questions),
               names_to = "Question", values_to = "Response")

# Summarize the data by study site, question, and response, and pivot wider
phq_summary <- phq_long %>%
  count(study_site, Question, Response) %>%
  pivot_wider(names_from = Response, values_from = n, values_fill = 0) %>%
  arrange(study_site, Question)

# Format the table using flextable
phq_resp_table <- flextable(phq_summary) %>%
  autofit() %>%
  fontsize(size = 10) %>%
  font(fontname = "Times New Roman") %>%
  #border_inner(border = fp_border(width = 1, color = "gray")) %>%
  #border_outer(border = fp_border(width = 1, color = "black")) %>%
  align(align = "center", part = "all")

# Print the formatted table
phq_resp_table

save_as_docx(phq_resp_table, path = "C:/Users/DIT/Desktop/BylhahMugotitsa/Cleaned Datasets/phq_responses_summary_table.docx")

### Psychosis
colnames(CD_SD)
# Define the target questions
target_questions <- c("hypomaniasymp", "insertion", "paranoiasymp", "strange", "hallucinationsymp", "psychosis")

# Create a long format dataframe with the target questions and study site
psycho_long <- CD_SD %>%
  pivot_longer(cols = all_of(target_questions),
               names_to = "Question", values_to = "Response")

# Summarize the data by study site, question, and response, and pivot wider
psycho_summary <- psycho_long %>%
  count(study_site, Question, Response) %>%
  pivot_wider(names_from = Response, values_from = n, values_fill = 0) %>%
  arrange(study_site, Question)

# Format the table using flextable
psycho_resp_table <- flextable(psycho_summary) %>%
  autofit() %>%
  fontsize(size = 10) %>%
  font(fontname = "Times New Roman") %>%
  #border_inner(border = fp_border(width = 1, color = "gray")) %>%
  #border_outer(border = fp_border(width = 1, color = "black")) %>%
  align(align = "center", part = "all")

# Print the formatted table
psycho_resp_table

save_as_docx(psycho_resp_table, path = "C:/Users/DIT/Desktop/BylhahMugotitsa/Cleaned Datasets/psycho_responses_summary_table.docx")


# Exporting tables as 'PNG' or 'WORD' formats
library(flextable)

# X-tics table
Xtics_Table %>% 
  as_flex_table() %>% 
  save_as_docx(path = "C:/Users/DIT/Desktop/BylhahMugotitsa/Cleaned Datasets/Characteristics Table.docx")
  
# Uni-variate Table - Gad-7

multvar_table %>% 
  as_flex_table() %>% 
  save_as_docx(path = "C:/Users/DIT/Desktop/BylhahMugotitsa/Cleaned Datasets/multvar_table.docx")


# Uni-variate Table - MDD-7

univar_table2 %>% 
  as_flex_table() %>% 
  save_as_docx(path = "C:/Users/DIT/Desktop/BylhahMugotitsa/Back_Pain Data/Final Round 22_Mental Health Data/Analysis/Final Mental Health Analysis/Univariate_MDD-9.docx")


Xtics_Table %>% 
  as_flex_table() %>% 
  save_as_image(path = "C:/Users/DIT/Desktop/BylhahMugotitsa/Back_Pain Data/Final Round 22_Mental Health Data/Analysis/Final Mental Health Analysis/Characteristics Table.png")

################################################################################
### 2nd Analysis of Gad-7 & PHQ-9 PCA - Variable contribution to the outcome
###############################################################################
library(FactoMineR)
library(factoextra)
library(RColorBrewer)

table(MH$phq9_apetite)

MH <- MH %>% 
      mutate(across(21:36, ~ recode(., "Not at all" = 0, "Several days" = 1, 
                                       "More than half the days" = 2, "Nearly every day" = 3)))
# Sub-setting data for PCA analysis
PCA_MH <- MH[,21:36]
PCAHDSS_MH <- MH[,c(1,21:36)]

# Data Normalization
data_normalized <- scale(PCA_MH)
head(data_normalized)

# Applying PCA
data.pca <- princomp(data_normalized)
summary(data.pca)

# Loadings
data.pca$loadings[, 1:2]

# Scree plot
fviz_eig(data.pca, addlabels = TRUE)

# Create a color vector
col_unique <- as.character(PCAHDSS_MH$HDSS)
col_unique

fviz_pca_biplot(PCA_result, geom.ind = "point",
                pointsize = 1.5, col.var = "black",
                col.ind = col_unique, pointshape = 19, )

# Contribution of each variable
q <- fviz_cos2(data.pca, choice = "var", axes = 1:2)
# Add data labels
q + geom_text(aes(label=round(..y.., 1)), vjust=-0.3)

# Contribution of each variable
p <- fviz_contrib(data.pca, choice = "var", axes = 1:2)
# Add data labels
p + geom_text(aes(label=round(..y.., 2)), vjust=-0.3)

### Both sides
# GAD - Sub-setting GAD-7 questions
Gad_varz <- MH %>% select(gad7_nervous,gad7_worrying,gad7_things,gad7_relaxing,
                          gad7_restless, gad7_annoyed,gad7_afraid)

# Data Normalization
data_norma_Gad_varz <- scale(Gad_varz)
head(data_norma_Gad_varz)

# Applying PCA
data.pca <- princomp(data_norma_Gad_varz)
summary(data.pca)

# Loadings
data.pca$loadings[, 1:2]

# Scree plot
fviz_eig(data.pca, addlabels = TRUE)

fviz_pca_biplot(data.pca, geom.ind = "point",
                pointsize = 1.5, col.var = "black",
                col.ind = col_unique, pointshape = 19, )

# Contribution of each variable
p <- fviz_contrib(data.pca, choice = "var", axes = 1:2)
# Add data labels
p + geom_text(aes(label=round(..y.., 1)), vjust=-0.3)

# MDD - Sub-setting MDD-9 questions
MDD_varz <- MH %>% select(phq9_interest,phq9_depressed,phq9_sleeping,phq9_tired,
                          phq9_apetite,phq9_feelingbad,phq9_concentrating,phq9_fidgety,
                          phq9_thoughts)

# Data Normalization
data_norma_MDD_varz <- scale(MDD_varz)
head(data_norma_MDD_varz)

# Applying PCA
data.pca <- princomp(data_norma_MDD_varz)
summary(data.pca)

# Loadings
data.pca$loadings[, 1:2]

# Scree plot
fviz_eig(data.pca, addlabels = TRUE)

fviz_pca_biplot(data.pca, geom.ind = "point",
                pointsize = 1.5, col.var = "black",
                col.ind = col_unique, pointshape = 19, )

# Contribution of each variable
p <- fviz_contrib(data.pca, choice = "var", axes = 1:2)
# Add data labels
p + geom_text(aes(label=round(..y.., 1)), vjust=-0.3)

# Saving the Graph
# Save the Word document
print(p, target = "C:/Users/DIT/Desktop/BylhahMugotitsa/Back_Pain Data/Final Round 22_Mental Health Data/Analysis/Final Mental Health Analysis/Both_GAD.docx")

