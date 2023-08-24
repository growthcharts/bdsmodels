#' Fill risk predictor data with target person data
#'
#' @param tgt Target person data, according to JAMES internal format from `bdsreader`
#' @param outcome  Character. Currently only `"overweight-4y"`
#' @returns A tibble with 1 row containing person data
#' @export
collect_predictors <- function(tgt, outcome = "overweight-4y") {
  z <- NULL

  if (outcome == "overweight-4y") {
    lev <- c("Netherlands",
             "EU-15, other developed economies",
             "New EU-countries, economies in transition",
             "Northern Africa",
             "East Asia",
             "Other Africa, Asia, Latin America",
             "Surinam and (former) Nederlands Antilles",
             "Turkey",
             "Unknown")
    x <- tgt$xyz
    p <- tgt$psn
    z <- tibble_row(
      id = ifelse(
        hasName(p, "id"),
        p$id,
        NA_integer_),
      name = ifelse(
        hasName(p, "name"),
        p$name,
        NA_character_),
      bmi_z_4m =
        filter_z(x, zname = "bmi_z", lo = 3.5/12, hi = 6.0/12),
      bmi_z_3m =
        filter_z(x, zname = "bmi_z", lo = 2.5/12, hi = 3.5/12),
      bmi_z_8w =
        filter_z(x, zname = "bmi_z", lo = 1.5/12, hi = 2.5/12),
      bmi_z_4w =
        filter_z(x, zname = "bmi_z", lo = 0.5/12, hi = 1.5/12),
      wgt_z_4m =
        filter_z(x, zname = "wgt_z", lo = 3.5/12, hi = 6.0/12),
      wgt_z_3m =
        filter_z(x, zname = "wgt_z", lo = 2.5/12, hi = 3.5/12),
      wgt_z_8w =
        filter_z(x, zname = "wgt_z", lo = 1.5/12, hi = 2.5/12),
      wgt_z_4w =
        filter_z(x, zname = "wgt_z", lo = 0.5/12, hi = 1.5/12),
      hgt_z_4m =
        filter_z(x, zname = "hgt_z", lo = 3.5/12, hi = 6.0/12),
      hgt_z_3m =
        filter_z(x, zname = "hgt_z", lo = 2.5/12, hi = 3.5/12),
      hgt_z_8w =
        filter_z(x, zname = "hgt_z", lo = 1.5/12, hi = 2.5/12),
      hgt_z_4w =
        filter_z(x, zname = "hgt_z", lo = 0.5/12, hi = 1.5/12),
      bw = ifelse(
        hasName(p, "bw"),
        p$bw,
        NA_real_),
      woz =
        match_pc4(p, "woz"),
      agem = ifelse(
        hasName(p, "agem"),
        p$agem,
        NA_real_),
      agef = ifelse(
        hasName(p, "agef"),
        p$agef,
        NA_real_),
      ga = ifelse(
        hasName(p, "ga"),
        p$ga,
        NA_real_),
      eduf = ifelse(
        hasName(p, "eduf"),
        case_match(p$eduf, 1 ~ 1, 2 ~ 2, c(3, 4) ~ 3, 5 ~ 4, 6 ~ 5, 7 ~ 6,
                   8 ~ 7, 9 ~ 8),
        NA_real_),
      edum = ifelse(
        hasName(p, "edum"),
        case_match(p$edum, 1 ~ 1, 2 ~ 2, c(3, 4) ~ 3, 5 ~ 4, 6 ~ 5, 7 ~ 6,
                   8 ~ 7, 9 ~ 8),
        NA_real_),
      sex = ifelse(
        hasName(p, "sex"),
        case_match(as.character(p$sex), "male" ~ 1, "female" ~ 0),
        NA_real_),
      par = ifelse(
        hasName(p, "par"),
        p$par,
        NA_real_),
      urb =
        match_pc4(p, "sted"),
      ctrf =
        factor(NA_character_, levels = lev),
      ctrm =
        factor(NA_character_, levels = lev))

    req_names <- c("id", "name",
                   "bmi_z_4m", "bmi_z_3m", "bmi_z_8w", "bmi_z_4w",
                   "wgt_z_4m", "wgt_z_3m", "wgt_z_8w", "wgt_z_4w",
                   "hgt_z_4m", "hgt_z_3m", "hgt_z_8w", "hgt_z_4w",
                   "bw", "woz", "agem", "agef", "ga", "eduf", "edum",
                   "sex", "par", "urb", "ctrf", "ctrm")
    found <- hasName(z, req_names)
    if (any(!found)) stop("Could not find required name(s): ",
                          paste(req_names[!found], collapse = ", "))
  }

  return(z)
}

filter_z <- function(x, zname, lo, hi) {
  if (any(!hasName(x, c("zname", "z")))) {
    return(NA_real_)
  }
  x |>
    filter(.data$zname == !! zname & .data$age >= !! lo & .data$age < !! hi) |>
    pull("z") |>
    last(na_rm = TRUE)
}

match_pc4 <- function(p, out = c("woz", "sted")) {
  out <- match.arg(out)
  if (!hasName(p, c("pc4"))) {
    return(NA_real_)
  }
  bdsmodels::pc4 |>
    filter(.data$pc4 == !! p$pc4) |>
    pull(!! out) |>
    first(na_rm = TRUE)
}