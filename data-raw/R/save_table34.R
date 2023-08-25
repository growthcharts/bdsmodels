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

usethis::use_data(table34, overwrite = TRUE)
