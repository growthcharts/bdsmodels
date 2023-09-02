#' Maps variable names from internal to external naming
#'
#' Use the internal table `mapping` to transform the internal data into the
#' form used by the imported model.
#'
#' @param data Target data, e.g. as produced by `collect_predictors()`
#' @param outcome  Character. Currently only `"overweight-4y"`
#' @param version Character. Version number.
#' @returns A tibble with 1 row containing person data
#' @examples
#' library(bdsreader)
#' fn <- system.file("examples/test.json", package = "bdsreader")
#' m <- bdsreader::read_bds(fn)
#' x <- collect_predictors(m)
#' ex <- map_varnames(x)
#' @export
map_varnames <- function(data = NULL,
                         outcome = "overweight-4y",
                         version = "20230905") {
  map <- bdsmodels::mapping |>
    filter(.data$outcome == !! outcome,
           .data$version == !! version) |>
    dplyr::select(all_of(c("ex_name", "in_name")))

  # Rename into external names
  lookup <- map$in_name
  names(lookup) <- map$ex_name
  z <- rename(data, any_of(lookup))

  # Add missing values (double) for any unavailable variables
  unavailable <- map$ex_name[!map$ex_name %in% names(z)]
  if (length(unavailable)) {
    m <- matrix(NA_real_, nrow = 1L, ncol = length(unavailable))
    colnames(m) <- unavailable
    z <- add_column(z, as_tibble(m))
  }

  return(z)
}
