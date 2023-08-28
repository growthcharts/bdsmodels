## Official Country Table used by Dutch Government
fn <- file.path("data-raw/data/mapping.csv")
mapping <- read.table(fn, fileEncoding = "macintosh", sep = ";",
                      header = TRUE, row.names = 1L, dec = ",")
mapping$importance <- as.numeric(mapping$importance)
usethis::use_data(mapping, overwrite = TRUE)
