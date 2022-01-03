## code to prepare `deaths_and_new_cases_hiv` dataset goes here

### libraries
if (!require(dplyr)) install.packages("dplyr")
library(dplyr)
if (!require(tidyr)) install.packages("data.table")
library(data.table)

### loading the dataset from csv
hiv<-read.csv("data-raw/deaths-and-new-cases-of-hiv.csv")

### changing the column names
colnames(hiv)<-c("Entity", "Code", "Year", "Deaths", "Incidence", "Prevalence")

### converting columns from numeric to integer
hiv<-hiv%>%
  mutate(Deaths=as.integer(round(Deaths)),
         Incidence=as.integer(round(Incidence)),
         Prevalence=as.integer(round(Prevalence)))

# removing geographical area without ISO codes - the app only accepts non-blank lines in the time and geographic column
hiv<-hiv[nchar(hiv$Code)>=2,]

deaths_and_new_cases_hiv<-hiv
deaths_and_new_cases_hiv <- as.data.table(deaths_and_new_cases_hiv)
usethis::use_data(deaths_and_new_cases_hiv, overwrite = TRUE, compress='xz')
