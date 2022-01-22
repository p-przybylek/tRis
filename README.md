# tRis - time seRies visualization

<!-- badges: start -->
<!-- badges: end -->

**tRiS** is a shiny app that allows you to visualize and analyse spatio-temporal medical or biological data. This application enables visualization by means of maps and prediction of successive measurements of any (properly formatted) numerical data with spatial and temporal attributes. The user can perform all the functions of the application on the example datasets contained in the app.

The purpose of the application is to facilitate the understanding of certain dependencies in the data and to find the necessary information. Generalization of the operation on any data allows the application to be used in various projects or analyses, making it useful to a larger group of recipients.

### Installation

The application works with RStudio and the Google Chrome browser.

Installation:
```
devtools::install_github("p-przybylek/tRis")
```
To run the app locally:
```
tRis::run_app()
```

To open the application in the browser, click the "open in browser" button or copy the website address shown on the bar of the running shiny app.

### Example datasets for visualization and prediction

1. Covid Poland

This dataset contains daily coronavirus data from a Polish goverment website archive, to be found [here](https://www.gov.pl/web/koronawirus/wykaz-zarazen-koronawirusem-sars-cov-2). 
The data is available under the [Creative Commons Attribution 3.0 Poland](https://creativecommons.org/licenses/by/3.0/pl/). It allows the user to share and modify it without limitations. This dataset has been cleaned and modified using the `data-raw/covid_poland.R` script.

2. Deaths and new cases of HIV

This datasets contains number of deaths and cases of HIV from around the world from years 1990-2017, published by [Our World in Data](https://ourworldindata.org/hiv-aids), originally from Global Burden of Disease Collaborative Network.
It is under the [Open Data Commons Attributions License](https://opendatacommons.org/licenses/by/1-0/) for non-commercial use. It allows the users to freely share, modify and use the data.
This dataset has been cleaned and modified using the `data-raw/deaths_and_new_cases_hiv.R` script.

Documentation for these datasets is available in the `R/data.R` file.

### Other datasets

For data visualization from Poland, the application uses spatial data from [GADM](https://gadm.org/data.html) (the GADM license allows the use of data for academic and non-commercial purposes). The downloaded data is located in the `inst/exdata` directory. 

The geographic correctness of measurement data from Poland is carried out using the territorial codes of the register of the official territorial division of Poland (TERYT). Valid TERYT codes are in the file `poland_teryt.rda`. The raw data subjected to the study come from the website of the [Central Statistical Office](https://eteryt.stat.gov.pl/eTeryt/rejestr_teryt/udostepnianie_danych/baza_teryt/uzytkownicy_indywidualni/pobieranie/pliki_pelne.aspx?contrast=default) (Główny Urząd Statystyczny pl.), whose [license](https://stat.gov.pl/copyright) allows the use of this data for own studies.
