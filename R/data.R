#' TERYT administrative codes.
#'
#' A dataset containing the TERYT Poland codes of the geographical area. The variables description:
#'
#' @docType data
#' @usage data(poland_teryt)
#' @format A data table with 395 rows and 3 variables:
#' \describe{
#'   \item{name}{name of the district or the city on district rights or the voivodeship}
#'   \item{administrative_division_level}{district or the city on district rights or the voivodeship}
#'   \item{teryt}{code of the geographical area}
#' }
#' @source \url{https://eteryt.stat.gov.pl/eTeryt/rejestr_teryt/udostepnianie_danych/baza_teryt/uzytkownicy_indywidualni/pobieranie/pliki_pelne.aspx?contrast=default}
"poland_teryt"

#' COVID-19 statistics in Poland.
#'
#' A dataset includes daily measurements of COVID-19 virus spread in Poland. The variables description:
#'
#' @docType data
#' @usage data(covid_poland)
#' @format A data table with 132588 rows and 15 variables:
#' \describe{
#'   \item{voivodeship}{name of the voivodeship}
#'   \item{district_city}{name of the district or the city on district rights}
#'   \item{cases}{number of covid cases}
#'   \item{cases_per_10_thousand_citizens}{number of covid cases per 10 thousand citizens in the given district}
#'   \item{deaths}{number of deaths}
#'   \item{deaths_without_comorbid_diseases}{number of deaths due to covid without comorbid diseases}
#'   \item{deaths_with_comorbid_diseases}{number of deaths due to covid and comorbid diseases}
#'   \item{primary_care_physician_commisions}{number of commisions from Primary Care Physicians}
#'   \item{people_in_quarantine}{number of people in quarantine}
#'   \item{tests}{number of done tests}
#'   \item{positive_tests}{number of tests with positive results}
#'   \item{negative_tests}{number of tests with negative results}
#'   \item{territory}{territory code from original data, in tXXXX format}
#'   \item{date}{day when the data was collected}
#'   \item{convalescents}{number of convalescents}
#' }
#' @source \url{https://www.gov.pl/web/koronawirus/wykaz-zarazen-koronawirusem-sars-cov-2}
"covid_poland"

#' Statistics on the global HIV / AIDS epidemic.
#'
#' A dataset containing the number of cases and deaths caused by HIV around the world in the years 1990-2017
#' Data includes people of different ages and both genders. The variables description:
#'
#' @docType data
#' @usage data(deaths_and_new_cases_hiv)
#' @format A data table with 6468 rows and 6 variables:
#' \describe{
#'   \item{Entity}{name of the country / geographic area}
#'   \item{Code}{code of the geographical area}
#'   \item{Year}{year in which the measurements were made}
#'   \item{Deaths}{number of deaths}
#'   \item{Incidence}{number of infected}
#'   \item{Prevalence}{number of infected per 100,000 people in the studied population}
#' }
#' @source \url{https://ourworldindata.org/hiv-aids}
"deaths_and_new_cases_hiv"