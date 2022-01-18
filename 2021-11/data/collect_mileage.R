library(dplyr)
library(purrr)

files <- unique(
    dir(".", pattern = "Mileage.csv", full.names = TRUE, recursive = TRUE)
)

f1 <- function(x) {
    m <- stringr::str_extract(x, "(?<=[0-9]{2} ).*(?=\\/M)")
    t <- dplyr::as_tibble(read.table(x, sep = ","))
    t <- dplyr::mutate(t, month = m)
    t
}

f2 <- function(x) {
    m <- stringr::str_extract(x, "(?<=[0-9]{2} ).*(?=\\/M)")
    t <- dplyr::as_tibble(read.table(x, sep = "\t", fileEncoding = 'UTF-16'))
    t <- dplyr::mutate(t, month = m)
    t
}

f3 <- function(x) {
    m <- stringr::str_extract(x, "(?<=[0-9]{2} ).*(?=\\/M)")
    t <- dplyr::as_tibble(read.table(x, sep = ",", fileEncoding = 'UTF-16'))
    t <- dplyr::mutate(t, month = m)
    t
}

x1 <- map_df(files[1:7], f1)
x2 <- f2(files[8])
x3 <- map_df(files[9:length(files)], f3)

miles <- rbind(x1, x2, x3)
miles <- miles %>%
    rename("tag"=V1, "start"=V2, "end"=V3, "diff"=V4)
writexl::write_xlsx(miles, "annual_miles.xlsx")
