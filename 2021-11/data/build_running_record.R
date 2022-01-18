library(readxl)
library(dplyr)
library(purrr)

files <- unique(
    dir(".", pattern = "Inventory by LOB", full.names = TRUE, recursive = TRUE)
)


read_cols <- function(file) {
    sheets <- readxl::excel_sheets(file)
    sheet_name <- grep("inv", x = sheets, ignore.case = TRUE, value = TRUE)
    dat <- read_excel(file, sheet=sheet_name)
    select(dat, Tag, VIN, 
           `Exp Org`, Region, `Fleet Manager`, 
           `FSR Name`,`FSR Email`,
           `Contact Name`, `Contact Email`)
}

resp <- map_df(files, read_cols)
record <- distinct(resp)
writexl::write_xlsx(record, "./running_inv_record.xlsx")
