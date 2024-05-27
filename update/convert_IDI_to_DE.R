#Takes IDI output and saves in .csv format to be used with DE

#Load libraries
library(data.table)
library(openxlsx)
library(shinylive)
library(httpuv)

#update to match new data

#format: YY for the HES year of input data
hes_version <- "21"

#format: "<EFU>YY" e.g. HYEFU23 for the EFU version of input data
efu_version <- "HYEFU23"

#format: YYYY-MM-DD for the generation date of input data
summary_date <- "2024-04-03"

#format: c("YY", "YY" ... ) for each year of input data to be converted
tax_years <- c("25", "26", "27", "28")

#format: "SQ" or reform code
result_type <- "SQ"

#open file, load descriptors and values and merge into single data.table
filename <- paste0("update/DE_results_HES", hes_version, "_", efu_version, "_", result_type, "_", summary_date, "_no_raw_info.xlsx")
wb <- openxlsx::loadWorkbook(filename)
descriptors <- data.table(openxlsx::read.xlsx(wb, sheet = "Descriptors"))
values <- data.table(openxlsx::read.xlsx(wb, sheet = "Values"))
output <- merge(descriptors, values, by = "Index")

for (tax_year in tax_years) {
  year_output <- output[Tax_Year == tax_year, .(Index, Income_Group, Income_Type, Population_Type, Description, Income_Measure, Value_Type, Value, Population)]
  fwrite(output, paste0("app/data/DE_HES", hes_version, "_", efu_version, "_TY", tax_year, "_", result_type, ".csv"))

}
