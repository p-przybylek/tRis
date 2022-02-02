## code to prepare `poland_teryt` dataset goes here

### libraries
if (!require(dplyr)) install.packages("dplyr")
library(dplyr)

### loading the dataset from csv
teryt <- read.csv2("data-raw/TERC_Urzedowy_2021-12-08.csv")

### choosing only only needed columns and rows
teryt <- teryt[is.na(teryt$GMI),c(-4,-3,-7)]

### creating TERYT code column
teryt$WOJ1 <- ifelse(nchar(as.character(teryt$WOJ)) == 1, paste0("0", as.character(teryt$WOJ)), as.character(teryt$WOJ))
teryt$POW1 <- ifelse(nchar(as.character(teryt$POW)) == 1, paste0("0", as.character(teryt$POW)), as.character(teryt$POW))
teryt$TERYT <- ifelse(is.na(teryt$POW), teryt$WOJ1, paste0(teryt$WOJ1, teryt$POW1))

### choosing only needded columns
teryt <- teryt[,c("NAZWA", "NAZWA_DOD", "TERYT")]

### changing the column names
colnames(teryt) <- c("name", "administrative_division_level", "teryt")

poland_teryt <- teryt
usethis::use_data(poland_teryt, overwrite = TRUE, compress='xz')
save(poland_teryt, file = './data/poland_teryt.rda', compress = 'xz') # save RDA file again, because before file saved with Unicode not UTF-8
