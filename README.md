# Comparative-Analysis-of-Mental-Health-Predictors
Evaluating demographics, behavioral and socioeconomic factors between Rural Community and University Population in Kilifi County to understand their influence on mental health conditions.

## Summary

This study was conducted in Kilifi County, situated along Kenya’s coast, was the primary study location. The county comprises six sub-counties: Kilifi North, Kilifi South, Ganze, Kaloleni, Malindi, and Magarini. Its population predominantly engages in informal labor, subsistence farming, and small-scale trade, with limited access to healthcare services.

Data were collected using structured surveys administered by **Community Health Workers (CHWs)**. The surveys were administered face-to-face for the rural population and electronically for university students, ensuring comprehensive coverage.

The aim of the study was to analyze and identify factors associated with mental health outcomes and compare differences and similarities in two distinct populations:

      1. Rural residents of Kilifi County
      
      2. University students at Pwani University
      
## Setup

STATA was used for cleaning while R was used for analysis.

We are assuming you have `STATA version 16 or higher`, `R Software` and `Rstudio IDE` installed. If not:

- You can purchase the license from [**STATA**](https://www.stata.com/) then download and install.

- Download and install [**R software**](https://www.r-project.org/) then followed by [**RStudio/Posit IDE**](https://posit.co/download/rstudio-desktop/).

## Data

The raw data and clean data used for analysis are available on reasonable request from the [**Study PI - Agnes Kiragga**](mailto:akiragga@aphrc.org?subject=[GitHub]%20Source%20Han%20Sans) and [**Study Program Coordinator - Bylhah Mugotitsa**](mailto:bmugotitsa@aphrc.org?subject=[GitHub]%20Source%20Han%20Sans).

- **Raw Data:** `Mental_Health_Survey_-_all_versions_-_labels_-_2024-11-05-22-48-32.xlsx` and `Uchunguzi_wa_afya_ya_akili_-_all_versions_-_labels_-_2024-09-16-10-23-32.xlsx`

- **Data used for analysis:** `community.dta` and `studentdata.dta`

## Run Analysis in R

After cloning the repository or downloading the ZIP, you also need the data files (**Data used for analysis**) in the _data_ sub-folder of [Analysis](./Analysis) folder.

Open `Rstudio` then set your working directory to the _Analysis_ folder. 

- Copy the below code to run all files at once in Rstudio

```
source("main.R")

```
- To run individual files, open the `main.R` script, and run from the beginning.
