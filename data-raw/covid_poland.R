## code to prepare `covid_poland` dataset goes here

### libraries
if (!require(dplyr)) install.packages("dplyr")
library(dplyr)
if (!require(tidyr)) install.packages("tidyr")
library(tidyr)
if (!require(tidyr)) install.packages("data.table")
library(data.table)

### list of file names in 'covid_poland' directory
covid_files <- list.files(path="data-raw/covid_poland", pattern="*.csv", full.names=TRUE)

### creating the dataframe by loading the first file
f<-covid_files[1]
covid_poland<-read.csv2(f, fileEncoding = "UTF-8")

### if the date column is missing, we get the date from the name of the file
if(!"stan_rekordu_na" %in% colnames(covid_poland) ){
  filename_length<-nchar(f)
  date<-substr(f,filename_length-37,filename_length-30)
  date_clean<-as.character(as.Date(paste(substr(date,1,4),"-", substr(date,5,6),"-",substr(date,7,8), sep=""))-1)
  covid_poland<-covid_poland%>%
    mutate(stan_rekordu_na=date_clean)
}

### converting columns from character to numeric and replacing created NA's with 0
covid_poland<-covid_poland%>%
  mutate(
    liczba_na_10_tys_mieszkancow=as.numeric(liczba_na_10_tys_mieszkancow),
    zgony=replace_na(as.numeric(zgony),0),
    zgony_w_wyniku_covid_bez_chorob_wspolistniejacych=replace_na(as.numeric(zgony_w_wyniku_covid_bez_chorob_wspolistniejacych),0),
    zgony_w_wyniku_covid_i_chorob_wspolistniejacych=replace_na(as.numeric(zgony_w_wyniku_covid_i_chorob_wspolistniejacych),0))


### loading the rest of the files in a loop
for (i in 1:length(covid_files)) {
  
  f<-covid_files[i]
  
  ### loading the file
  if (i %in% 1:30 || i==161){
    ### files 1-30 and 161 are in UTF-8 encoding
    temp<-read.csv2(f, fileEncoding = "UTF-8")
  }
  else{
    ### files 31-160 and >162 are in the windows-1250 encoding
    temp<-read.csv2(f, fileEncoding = "windows-1250")
  }


  ### if the date column is missing, we get the date from the name of the file
  if(!"stan_rekordu_na" %in% colnames(temp) ){
    filename_length<-nchar(f)
    date<-substr(f,filename_length-37,filename_length-30)
    date_clean<-as.character(as.Date(paste(substr(date,1,4),"-", substr(date,5,6),"-",substr(date,7,8), sep=""))-1)
    temp<-temp%>%
      mutate(stan_rekordu_na=date_clean)
  }
  ### converting columns from character to numeric and replacing created NA's with 0
  temp<-temp%>%
    mutate(
      liczba_na_10_tys_mieszkancow=as.numeric(liczba_na_10_tys_mieszkancow),
      zgony=replace_na(as.numeric(zgony),0),
      zgony_w_wyniku_covid_bez_chorob_wspolistniejacych=replace_na(as.numeric(zgony_w_wyniku_covid_bez_chorob_wspolistniejacych),0),
      zgony_w_wyniku_covid_i_chorob_wspolistniejacych=replace_na(as.numeric(zgony_w_wyniku_covid_i_chorob_wspolistniejacych),0))
  
  ### adding the data from file to dataframe
  covid_poland<-bind_rows(covid_poland,temp)
} 

### dropping an uninteresting column
covid_poland<-covid_poland%>%
  select(!liczba_pozostalych_testow)

### changing the column names to english

colnames(covid_poland)<-c('voivodeship', 'district_city', 'cases', 'cases_per_10_thousand_citizens', 'deaths',
                          'deaths_without_comorbid_diseases', 'deaths_with_comorbid_diseases', 'primary_care_physician_commisions',
                          'people_in_quarantine', 'tests', 'positive_tests', 'negative_tests', 'territory', 'date', 'convalescents')

covid_poland <- as.data.table(covid_poland)
usethis::use_data(covid_poland, overwrite = TRUE, compress='xz')
