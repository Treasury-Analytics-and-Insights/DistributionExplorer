library(data.table)

source('app/functions.R')

files_available <- list.files('app/data/')

dt <- NULL

for (file in files_available) {
  dt_year = as.data.table(read.csv(paste0("app/data/", file)))
  dt_year[Description=="Aged 0-16", Description:="Aged 0-15"]
  dt_year[Description=="Super annuation", Description:="NZ Super"]
  dt_year[Description=="Multiple families tax credits", Description:="MFTC"]
  dt_year[Description=="Accomodation supplement", Description:="Accommodation supplement"]
  dt_year[Description=="Family tax credits", Description:="FTC"]
  dt_year[Description=="In work tax credits", Description:="IWTC"]
  dt_year[Description=="Working for families", Description:="WFF"]
  dt_year[Description=="Winter energy payment", Description:="WEP"]
  dt_year[, data_version := paste0(get_hes(file), ", ", get_efu(file))]
  dt_year[, year := paste0("20", get_year(file))]
  dt_year[, file := paste0("20", get_year(file), " (", get_hes(file), ", ", get_efu(file), ")")]
  if (is.null(dt)) {
    dt <- dt_year
  }
  else {
    dt <- rbindlist((list(dt, dt_year)))
  }
}

norm_pop <- function(dt) {
  total_pop_HH <- dt[Income.Type == "Income Quantiles" & Population.Type == "Household" & Description == "All households" & Income.Measure == "Equivalised Disposable Income",
                  sum(as.numeric(Population)), by = file]
  
  total_pop_Fam <- dt[Income.Type == "Income Quantiles" & Population.Type == "Family" & Description == "All families" & Income.Measure == "Equivalised Disposable Income",
                     sum(as.numeric(Population)), by = file]
  
  min_pop_HH <- total_pop_HH[,min(as.numeric(V1))]
  min_file <- total_pop_HH[as.numeric(V1) == min_pop_HH, file]
  min_pop_Fam <- total_pop_Fam[file == min_file, as.numeric(V1)]
  
  total_pop_HH[, factor := min_pop_HH / V1 ]
  total_pop_Fam[, factor := min_pop_Fam / V1 ]
  
  for (i in total_pop_HH[, file]) {
    factor_HH <- total_pop_HH[file == i, factor]
    factor_Fam <- total_pop_Fam[file == i, factor]
    dt[file == i & Population != "S" & Population.Type == "Household", Normalised := as.character((as.numeric(Population) * factor_HH))]
    dt[file == i & Population != "S" & Population.Type == "Family", Normalised := as.character((as.numeric(Population) * factor_Fam))]
    dt[is.na(Normalised), Normalised := "S"]
  }
  return(list(min_file, dt))
}

test <- norm_pop(dt)[[2]]
  
