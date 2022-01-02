
library(dplyr)
library(readxl)

futpop <- read_excel("np2017-t1.xlsx", range = "A13:B22",
                     col_names = c("year", "population"))

futgdp <- read_excel("51135-2021-07-economicprojections.xlsx",
                     sheet = "2. Calendar Year", range = "I7:R9",
                     col_names = FALSE)

futgdp <- t(futgdp)
colnames(futgdp) <- c("year", "none", "gdp")
futgdp <- as.data.frame(futgdp)
futgdp <- select(futgdp, -none)
futdata <- merge(futpop, futgdp, by = "year")

usethis::use_data(futdata, overwrite = TRUE)
