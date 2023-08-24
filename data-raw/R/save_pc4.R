library(dplyr)
library(readxl)
library(usethis)

# add classifcation of stedelijkheid as an predictor
fn <- file.path(getwd(), "data-raw/data/pc4_2022_v1.xlsx")
pc4 <- read_excel(fn, range = cell_rows(c(8, NA))) %>%
  rename(pc4 = "Postcode-4",
         woz = "WOZ-waarde\r\nwoning",
         sted = "...38") %>%
  filter(!pc4 == "Code") %>%
  select(pc4, sted, woz) %>%
  mutate(pc4 = as.factor(pc4),
         sted = as.factor(sted),
         woz = ifelse(woz == "-99997", NA_character_, woz),
         woz = as.numeric(woz))

usethis::use_data(pc4, overwrite = TRUE)

