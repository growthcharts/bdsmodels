# create age_map to divide measurement made in continuous age into age bins
labels <- c("","4w", "8w", "3m", "4m", "6m", "9m", "11m", "14m", "18m", "2y", "3y", "3y9m", "gp2", "gp7", "kl2")
lo <- c(0, 1, 47, 80, 112, 170, 262, 321, 406, 514, 678, 1039, 1320, 1857, 2861, 4687, 6940)
hi <- lo - 1
lo <- lo[-length(lo)]
hi <- hi[-1]
age_map <- data.frame(label = labels, lo = lo, hi = hi)
age_map$lo <- round(age_map$lo / 365.25, 4)
age_map$hi <- round(age_map$hi / 365.25, 4)

usethis::use_data(age_map, overwrite = TRUE)
