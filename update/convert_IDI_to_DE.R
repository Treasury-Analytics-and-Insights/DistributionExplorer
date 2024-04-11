#Takes IDI output and saves in .csv format to be used with DE

#Load libraries
library(data.table)
library(openxlsx)

#update to match new data

#format: YY
hes <- "21"

#e.g. "HYEFU21"
efu <- "HYEFU23"

#format YYYY-MM-DD
gen_date <- "2024-04-03"

#format: YY
years <- c("25", "26", "27", "28")

for (year in years) {

  #open file, load descriptors and values and merge into single data.table
  filename <- paste0("update/DE_results_HES", hes, "_", efu, "_TY", year, "_", gen_date, "_no_raw_info.xlsx")
  wb <- openxlsx::loadWorkbook(filename)
  descriptors <- data.table(openxlsx::read.xlsx(wb, sheet = "Descriptors"))
  values <- data.table(openxlsx::read.xlsx(wb, sheet = "Values"))
  output <- merge(descriptors, values, by = "Index")
  
  #write new data version to data folder
  fwrite(output, paste0("app/data/DE_HES", hes, "_", efu, "_TY", year, ".csv"))

}
