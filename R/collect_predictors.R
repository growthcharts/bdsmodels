#' Fill risk predictor data with target person data
#'
#' @param tgt Target person data, according to JAMES internal format from `bdsreader`.
#' If `tgt = NULL` (default) the function returns an empty data with all values
#' set to missings.
#' @param outcome  Character. Currently only `"overweight-4y"`
#' @param purpose Character. Purpose for the predictor data, can be `"model"` or `"tab10"`.
#' @returns A tibble with 1 row containing person data
#' @examples
#' library(bdsreader)
#' fn <- system.file("examples/test.json", package = "bdsreader")
#' m <- bdsreader::read_bds(fn)
#' x <- collect_predictors(m)
#' @export
collect_predictors <- function(tgt = NULL, outcome = "overweight-4y", purpose = "tab10") {
  z <- NULL

    # concatenate administrative variables, time-varying and fixed covariates for model purpose
    if(purpose == "model"){

      # specify required time-varying variables
      vpred <- c("bmi_z", "hgt_z", "wgt_z")
      vtime <- c("4w", "8w", "3m", "4m")

      dummies <- list("bmi_z", vtime)
      v <- tidyr::expand_grid(vpred, vtime)

      needed <- c(paste(v$vpred, v$vtime, sep = "_"))


      # create age bins, select last observation within each bin
      # and arrange Z-scores into a tibble with one row
      tv <- tgt$xyz |>
        select(all_of(c("age", "zname", "z"))) |>
        filter(.data$zname %in% !! vpred) |>
        mutate(gp = factor(findInterval(.data$age, bdsmodels::age_map$lo),
                           levels = seq_along(bdsmodels::age_map$lo),
                           labels = bdsmodels::age_map$label)) |>
        filter(.data$gp %in% !! vtime) |>
        group_by(.data$zname, .data$gp) |>
        arrange(.data$age) |>
        slice(n()) |>
        select(-"age") |>
        pivot_wider(names_from = c("zname", "gp"), values_from = "z")


      # create dummies
      dum <- paste(dummies[[1L]], dummies[[2L]], sep = "_")
      obs <- intersect(names(tv), dum)
      mis <- setdiff(dum, obs)
      dv <- matrix(0, nrow = 1L, ncol = length(dum),
                   dimnames = list(NULL, dum))
      dv[, mis] <- 1
      dimnames(dv)[[2L]] <- paste(dum, "NA", sep = "_")

      # pad missing time-varying predictors with zeroes & dummies
      vna <- setdiff(needed, names(tv))
      tv <- bind_cols(
        tv,
        matrix(NA_real_, nrow = 1L, ncol = length(vna), dimnames = list(NULL, vna)),
        dv)

    p <- tgt$psn

    z <- tibble_row(
      id =
        if (hasName(p, "id")) {
          p$id
        } else {
          NA_integer_
        },
      name =
        if (hasName(p, "name")) {
          p$name
        } else {
          NA_character_
        },
      tv,
      bw =
        if (hasName(p, "bw")) {
          p$bw
        } else {
          NA_real_
        },
      woz =
        match_pc4(p, "woz"),
      agem =
        if (hasName(p, "agem")) {
          p$agem - 1
        } else {
          NA_real_
        },
      agef =
        if (hasName(p, "agef")) {
          p$agef - 1
        } else {
          NA_real_
        },
      ga =
        if (hasName(p, "ga")) {
          p$ga
        } else {
          NA_real_
        },
      eduf =
        if (hasName(p, "eduf")) {
          case_match(p$eduf, 1 ~ "geen", 2 ~ "basis", c(3, 4) ~ "vmbopraktijk",
                     5 ~ "vmbomavo", 6 ~ "mbo", 7 ~ "havovwo", 8 ~ "hbo",
                     9 ~ "womaster", .default = "NAdummy") |>
            factor(levels = c("geen", "basis", "vmbopraktijk", "vmbomavo",
                              "mbo", "havovwo", "hbo", "womaster", "NAdummy"),
                   ordered = TRUE)
            }
         else {
            factor("NAdummy",
                 levels = c("geen", "basis", "vmbopraktijk", "vmbomavo",
                            "mbo", "havovwo", "hbo", "womaster", "NAdummy"),
                 ordered = TRUE)
          },
      edum =
        if (hasName(p, "edum")) {
            case_match(p$edum, 1 ~ "geen", 2 ~ "basis", c(3, 4) ~ "vmbopraktijk",
                       5 ~ "vmbomavo", 6 ~ "mbo", 7 ~ "havovwo", 8 ~ "hbo",
                       9 ~ "womaster", .default = "NAdummy") |>
              factor(levels = c("geen", "basis", "vmbopraktijk", "vmbomavo",
                                "mbo", "havovwo", "hbo", "womaster", "NAdummy"),
                     ordered = TRUE)


        } else {
            factor("NAdummy",
                   levels = c("geen", "basis", "vmbopraktijk", "vmbomavo",
                              "mbo", "havovwo", "hbo", "womaster", "NAdummy"),
                   ordered = TRUE)

        },
      sex =
        if (hasName(p, "sex")) {
          case_match(p$sex, "male" ~ "mannelijk", "female" ~ "vrouwelijk") |>
            factor(levels = c("mannelijk", "vrouwelijk"))
        } else {
           factor(NA, levels = c("mannelijk", "vrouwelijk"))
        },
      par =
        if (hasName(p, "par")) {
          case_match(p$par, 0 ~ "0", 1 ~ "1", 2 ~ "2", 3 ~ "3", 4 ~ "4",
                     5 ~ "5", 6 ~ "6+", 7 ~ "6+", 8 ~ "6+", 9 ~ "6+",
                     .default = "NAdummy") |>
            factor(levels = c("0", "1", "2", "3", "4", "5", "6+", "NAdummy"),
                   ordered = TRUE)
        } else {
          factor("NA_dummy",
                 levels = c("0", "1", "2", "3", "4", "5", "6+", "NAdummy"),
                 ordered = TRUE)
        },
      urb =
        match_pc4(p, "urb"),
      ctrf =
        match_country(p, out = "etng", inp = "blbf"),
      ctrm =
        match_country(p, out = "etng", inp = "blbm"))

    req_names <- c("id", "name", needed, paste(dum, "NA", sep = "_"),
                   "bw", "woz", "agem", "agef", "ga", "eduf", "edum",
                   "sex", "par", "urb", "ctrf", "ctrm")
    found <- hasName(z, req_names)
    if (any(!found)) stop("Could not find required name(s): ",
                          paste(req_names[!found], collapse = ", "))

    z <- select(z, all_of(req_names))
  }


    if(purpose == "tab10"){


     # if (outcome == "overweight-4y") {

     # specify required time-varying variables
     vpred <- c("bmi", "hgt", "wgt")
     vtime <- c("", "4w", "8w", "3m", "4m")
     vname <- c("Geboorte", "4 wk", "8 wk", "3 mnd", "4 mnd")

     # create age bins, select last observation within each bin
     # and arrange Z-scores into a tibble with one row
     tvl <- tgt$xyz |>
       select(all_of(c("age", "yname", "y"))) |>
       filter(.data$yname %in% !! vpred) |>
       mutate(gp = factor(findInterval(.data$age, bdsmodels::age_map$lo),
                          levels = seq_along(bdsmodels::age_map$lo),
                          labels = bdsmodels::age_map$label)) |>
       filter(.data$gp %in% !! vtime) |>
       group_by(.data$yname, .data$gp) |>
       arrange(.data$age) |>
       slice(n())|>
       pivot_wider(names_from = yname, values_from = y) |>
       right_join(data.frame(vtime, vname), by = c("gp"="vtime")) |>
       ungroup()


     p <- tgt$psn

    z <- tibble_row(
      id =
        if (hasName(p, "id")) {
      p$id
    } else {
      NA_integer_
    },
  name =
    if (hasName(p, "name")) {
      p$name
    } else {
      NA_character_
    },
  dob =
    if(hasName(p, "dob")){
      as.Date(p$dob)
    } else {
      NA_character_
    },
  bw =
    if (hasName(p, "bw")) {
      p$bw
    } else {
      NA_real_
    },
  woz =
    match_pc4(p, "woz"),
  agem =
    if (hasName(p, "agem")) {
      if(!is.na(p$agem)) p$agem - 1
      if(is.na(p$agem) & hasName(p, "dobm"))  as.numeric((Sys.Date() - as.Date(p$dobm)) / 365.25)
    } else {
      if(hasName(p, "dobm")){
        as.numeric((Sys.Date() - as.Date(p$dobm)) / 365.25)
      }
      else{
      NA_real_
    }},
  agef =
    if (hasName(p, "agef")) {
      p$agef - 1
    } else {
      if(hasName(p, "dobf")){
        as.numeric((Sys.Date() - as.Date(p$dobf)) / 365.25)
      }
      else{
        NA_real_
      }},
  ga =
    if (hasName(p, "ga")) {
      p$ga
    } else {
      if(hasName(p, "gad")){
        p$gad / 7
      }else{
      NA_real_
    }},
  eduf =
    if (hasName(p, "eduf")) {
        case_match(p$eduf, c(1, 2) ~ "Geen, Basis", 3 ~ "VMBO-P",
                   c(4,5) ~ "VMBO-T, MAVO", 6 ~ "MBO", 7 ~ "HAVO, VWO", 8 ~ "HBO",
                   9 ~ "WO, MASTER", .default = "Onbekend")
    } else {
      "Onbekend"
    },
  edum =
    if (hasName(p, "edum")) {
        case_match(p$eduf, c(1, 2) ~ "Geen, Basis", 3 ~ "VMBO-P",
                   c(4,5) ~ "VMBO-T, MAVO", 6 ~ "MBO", 7 ~ "HAVO, VWO", 8 ~ "HBO",
                   9 ~ "WO, MASTER", .default = "Onbekend")

    } else {
      "Onbekend"
    },
  sex =
    if (hasName(p, "sex")) {
       case_match(p$sex, "male" ~ "Jongen", "female" ~ "Meisje")

    } else {
      "Onbekend"
    },
  par =
    if (hasName(p, "par")) {
        case_match(p$par, 0 ~ "0", 1 ~ "1", 2 ~ "2", 3 ~ "3", 4 ~ "4",
                   5 ~ "5", 6 ~ "6+", 7 ~ "6+", 8 ~ "6+", 9 ~ "6+",
                   .default = "Onbekend")
    } else {
      "Onbekend"
    },
  urb =
    match_pc4(p, "urb"),
  ctrf =
    match_country(p, out = "etng", inp = "blbf"),
  ctrm =
    match_country(p, out = "etng", inp = "blbm"))

req_names <- c("id", "name",
               "bw", "woz", "agem", "agef", "ga", "eduf", "edum",
               "sex", "par", "urb", "ctrf", "ctrm")
found <- hasName(z, req_names)
if (any(!found)) stop("Could not find required name(s): ",
                      paste(req_names[!found], collapse = ", "))

tvl <-
tvl |>
  mutate(Datum = format(z$dob + age*365.25),
         SDS = AGD::y2z(y = tvl$bmi,
                  x = tvl$age,
                  sex = ifelse(z$sex == "Meisje", "F", "M"),
                  ref = AGD::nl4.bmi),
        hgt = hgt * 10,
        mm = "mm",
        wgt = wgt * 1000,
        g = "g"
         ) |>
  rename(Bezoek = vname, Leeftijd = age, Lengte = hgt, Gewicht = wgt, BMI = bmi) |>
  select(Bezoek, Datum, Leeftijd, Lengte, mm, Gewicht, g, BMI, SDS)

z <- list(psn = z,
          tv = tvl)
}

return(z)
}



match_pc4 <- function(p, out = c("woz", "urb")) {
  out <- match.arg(out)
  if (!hasName(p, c("pc4"))) {
    return(NA_real_)
  }
  bdsmodels::pc4 |>
    filter(.data$pc4 == !! p$pc4) |>
    pull(!! out) |>
    first(na_rm = TRUE)
}

match_country <- function(p, out = c("etng"), inp = c("blbf", "blbm")) {
  out <- match.arg(out)
  inp <- match.arg(inp)
  if (!hasName(p, inp)) {
    return(factor(NA_character_,
                  levels = levels(bdsmodels::table34$achts)))
  }
  bdsmodels::table34 |>
    filter(.data$code == !! p[[inp]]) |>
    pull(!! out) |>
    first(na_rm = TRUE)
}
