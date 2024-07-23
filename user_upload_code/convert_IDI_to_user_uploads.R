#Takes IDI output and saves in .csv format to be used with DE

#Load libraries
library(data.table)
library(openxlsx)
library(shinylive)
library(httpuv)

#update to match user data
#NOTE: Please ensure that user data is NOT contained within the Distribution Explorer
#directory when pushing updates to GitHub. 

#format: YY for the HES year of input data
hes_version <- "21"

#format: "<EFU>YY" e.g. HYEFU23 for the EFU version of input data
efu_version <- "HYEFU23"

#format: YYYY-MM-DD for the generation date of input data
summary_date <- "2024-04-03"

#format: c("YY", "YY" ... ) for each year of input data to be converted
tax_years <- c("25", "26", "27", "28")

#format: add the reform code or other custom name for your user results
result_type <- "SQ"

#enter the file location and name of your user upload
user_filename <- "~/Projects/DE Upgrade Project/archive_data/DE_results_HES23_BEFU24_SQ_2024-06-14_no_raw_info.xlsx"

wb <- openxlsx::loadWorkbook(user_filename)
descriptors <- data.table(openxlsx::read.xlsx(wb, sheet = "Descriptors"))
values <- data.table(openxlsx::read.xlsx(wb, sheet = "Values"))
output <- merge(descriptors, values, by = "Index")

save_dir <- "user_upload_data"

for (tax_year in tax_years) {
  year_output <- output[Tax_Year == tax_year, .(Income_Group, Income_Type, Population_Type, Description, Income_Measure, Value_Type, Value, Population)]
  if (save_dir == "user_upload_data"){
    if (file.exists("user_upload_data")) {
      fwrite(year_output, paste0("user_upload_data/DE_HES", hes_version, "_", efu_version, "_TY", tax_year, "_", result_type, ".csv"))
    }
    else {
      dir.create("user_upload_data")
      fwrite(year_output, paste0("user_upload_data/DE_HES", hes_version, "_", efu_version, "_TY", tax_year, "_", result_type, ".csv"))
    }
  }
  else {
    fwrite(year_output, paste0(save_dir, "/DE_HES", hes_version, "_", efu_version, "_TY", tax_year, "_", result_type, ".csv"))
  }
}
