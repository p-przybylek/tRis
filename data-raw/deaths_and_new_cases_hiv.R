## code to prepare `deaths_and_new_cases_hiv` dataset goes here

### libraries
if (!require(dplyr)) install.packages(dplyr)
library(dplyr)

### loading the dataset from csv
hiv<-read.csv("data-raw/deaths-and-new-cases-of-hiv.csv")

### changing the column names
colnames(hiv)<-c("Entity", "Code", "Year", "Deaths", "Incidence", "Prevalence")

### converting columns from numeric to integer
hiv<-hiv%>%
  mutate(Deaths=as.integer(round(Deaths)),
         Incidence=as.integer(round(Incidence)),
         Prevalence=as.integer(round(Prevalence)))


deaths_and_new_cases_hiv<-hiv
save(deaths_and_new_cases_of_hiv, file = "data/deaths_and_new_cases_hiv.rda", compress='xz')

usethis::use_data(deaths_and_new_cases_hiv, overwrite = TRUE)
