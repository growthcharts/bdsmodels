## Official Country Table used by Dutch Government
fn <- file.path("data-raw/data/Tabel34_Landentabel_ASCII.txt")
table34 <- read.table(fn,
                      stringsAsFactors = FALSE, header = TRUE,
                      sep = "\t", fileEncoding = "macintosh")[, 1:2]
names(table34) <- c("code", "country")

labels <- c("Netherlands",
            "EU-15, other developed economies",
            "New EU-countries, economies in transition",
            "Northern Africa",
            "East Asia",
            "Other Africa, Asia, Latin America",
            "Surinam and (former) Nederlands Antilles",
            "Turkey",
            "Unknown")
table34$achts <- factor(NA_character_,
                        levels = 1:9,
                        labels = labels)

labels <- c("autochtoon",
            "marokko",
            "turkije",
            "suriname",
            "voormalige nederlandse antillen en aruba",
            "overige niet-westerse landen",
            "overige westerse landen",
            "onbekend",
            "overige landen")
table34$etng <- dplyr::case_match(table34$code,
           6030 ~ "autochtoon",
           5022 ~ "marokko",
           c(6043, 7094, 8019) ~ "turkije",
           5007 ~ "suriname",
           c(7011, 5095, 5106, 5107, 5108, 5109, 5110) ~ "voormalige nederlandse antillen en aruba",
           .default = "overige landen")

table34$etng <- factor(table34$etng,
                       levels = labels)



labels <- c("Nederland",
            "Marokko",
            "Turkije",
            "Suriname",
            "Antillen en Aruba",
            "Overige niet-westers",
            "Overige westers",
            "Onbekend")
table34$land <- dplyr::case_match(table34$code,
                                  6030 ~ "Nederland",
                                  5022 ~ "Marokko",
                                  c(6043, 7094, 8019) ~ "Turkije",
                                  5007 ~ "Suriname",
                                  c(7011, 5095, 5106, 5107, 5108, 5109, 5110) ~ "Antillen en Aruba",
                                  .default = "Onbekend")

table34$land <- factor(table34$land,
                       levels = labels)

usethis::use_data(table34, overwrite = TRUE)
