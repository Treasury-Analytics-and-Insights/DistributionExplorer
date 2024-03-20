library(data.table)

data_versions_lists <- function() { 
  files_available <- list.files('data/')
  file_list <- list()
  for (file in files_available) {
    file_split <- strsplit(file, "_")[[1]]
    hes_efu <- paste0(file_split[2], ", ", file_split[3])
    year <- paste0("20", substr(file_split[4], 3, 4))
    file_list[[hes_efu]] <- c(file_list[[hes_efu]], file)
    names(file_list[[hes_efu]])[length(file_list[[hes_efu]])] <- year
  }
  return(file_list)
}



get_hes <- function(filename) {
  filename_split <- strsplit(filename, "_")[[1]]
  hes <- filename_split[2]
  return(hes)
}

get_efu <- function(filename) {
  filename_split <- strsplit(filename, "_")[[1]]
  efu <- filename_split[3]
  return(efu)
}

get_year <- function(filename) {
  filename_split <- strsplit(filename, "_")[[1]]
  year <- substr(filename_split[4], 3, 4)
  return(year)
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

