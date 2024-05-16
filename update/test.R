library(data.table)

#format: YY
hes <- "21"

#e.g. "HYEFU21"
efu <- "HYEFU23"

#format YYYY-MM-DD
gen_date <- "2024-04-03"

#format: YY
years <- c("25", "26", "27", "28")



for (year in years) {
  total_output <- data.table()
  dt <- fread(paste0("app/data/DE_HES", hes, "_", efu, "_TY", year, ".csv"))
  
  eq_inc_bands <- dt[Income.Type == "Income Bands" & Income.Measure == "Equivalised Disposable Income", unique(Income.Group)]
  oth_inc_bands <- dt[Income.Type == "Income Bands" & Income.Measure != "Equivalised Disposable Income", unique(Income.Group)]
    
  income_types <- dt[, unique(Income.Type)]
  population_types <- dt[, unique(Population.Type)]
  descriptions <- dt[, unique(Description)]
  income_measures <- dt[, unique(Income.Measure)]
  
  for (income_type in income_types) {
    for (population_type in population_types) {
      for (description in descriptions) {
        for (income_measure in income_measures) {
          if (income_type == "Income Quantiles") {
            blank_summary <- data.table(Income.Group = as.character(1:20))
          }
          else if (income_measure == "Equivalised Disposable Income") {
            blank_summary <- data.table(Income.Group = eq_inc_bands)
          }
          else {
            blank_summary <- data.table(Income.Group = oth_inc_bands)
          }
          blank_summary[, ':='(Income.Type = income_type, 
                               Population.Type = population_type,
                               Description = description,
                               Income.Measure = income_measure)]
          current_summary <- dt[Income.Type == income_type & 
                                  Population.Type == population_type & 
                                  Description == description &
                                  Income.Measure == income_measure]
          current_summary[, Index := NULL]
          output <- merge(blank_summary, current_summary, all.x = TRUE)
          output[is.na(Value), ':='(Population = "S", Value = "S")]
          total_output <- rbind(total_output, output)
        }
      }
    }
  }
  total_output[, Index := (1:.N)]
  setcolorder(total_output, c("Index", "Income.Group", "Income.Type", "Population.Type", "Description", "Income.Measure", "Value", "Population"))
  fwrite(total_output, paste0("app/data/DE_completeHES", hes, "_", efu, "_TY", year, ".csv"))
}
