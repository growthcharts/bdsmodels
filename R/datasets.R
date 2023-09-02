#' Postal code covariates
#'
#'
#' Postal codes (4-digits) of The Netherlands with covariates.
#' @format `pc4`
#' A data frame with 4052 rows and 3 columns:
#' \describe{
#'   \item{pc4}{Postal code}
#'   \item{sted}{Urbanicity, 5 levels 1 = low, 5 = high}
#'   \item{woz}{Average WOZ (house) value in postal code}
#' }
#' @source Taken from a file named `pc4_2022_v1.xlxs`.
"pc4"

#' Table 34: Country codes
#'
#' @docType data
#' @format A \code{data.frame} with 296 rows and 2 variables:
#' \describe{
#' \item{code}{Country code}
#' \item{country}{Country name}
#' }
#' @source \url{https://publicaties.rvig.nl/Landelijke_tabellen/Landelijke_tabellen_32_t_m_60_excl_tabel_35/Landelijke_Tabellen_32_t_m_60_in_csv_formaat}
"table34"

#' Mapping external - internal variable names
#' @docType data
#' @format A \code{data frame} with columns:
#' \describe{
#' \item{outcome}{Outcome name}
#' \item{version}{Model version}
#' \item{importance}{Importance score (for random forest/ranger)}
#' \item{ex_name}{Name used in external model object}
#' \item{in_name}{Name used internally by bdsmodels}
#' }
"mapping"

#' Age map to divide measurement made in continuous age into age bins
#' @docType data
#' @format A \code{data frame} with columns:
#' \describe{
#' \item{label}{Name of age bin}
#' \item{lo}{lower limit (decimal age)}
#' \item{hi}{higher limit (decimal age)}
#' }
"age_map"
