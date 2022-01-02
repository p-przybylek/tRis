# tRis - time seRies visualization

<!-- badges: start -->
<!-- badges: end -->

**tRiS** is a shiny app that allows you to visualize and analyse spatio-temporal medical or biological data. This application enables visualization by means of maps and prediction of successive measurements of any (properly formatted) numerical data with spatial and temporal attributes. The user can perform all the functions of the application on the example datasets contained in the app.

The purpose of the application is to facilitate the understanding of certain dependencies in the data and to find the necessary information. Generalization of the operation on any data allows the application to be used in various projects or analyses, making it useful to a larger group of recipients.

### Example datasets for visualization and prediction

1. Covid Poland

Daily coronavirus data from a Polish goverment website archive. 
Source: https://www.gov.pl/web/koronawirus/wykaz-zarazen-koronawirusem-sars-cov-2
License: Creative Commons Attribution 3.0 Poland.

2. Deaths and new cases of HIV

A datasets contains number of deaths and cases of HIV from around the world from years 1990-2017. 
Source: https://ourworldindata.org/hiv-aids
License: Open Data Commons Attributions License for non-commercial use.

### Other datasets

For data visualization only for Poland, the application uses spatial data from [GADM](https://gadm.org/data.html) (the GADM license allows the use of data for academic and non-commercial purposes). The downloaded data is located in the "inst/exdata" directory.

## Installation

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
