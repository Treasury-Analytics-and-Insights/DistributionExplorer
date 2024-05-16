library(data.table)

#format: YY
hes <- "21"

#e.g. "HYEFU21"
efu <- "HYEFU23"

#format: YY
years <- c("25", "26", "27", "28")

eq_inc_bands <-  list("Below $0", "$0-$10k", "$10k-$20k", "$20k-$30k", "$30k-$40",   "$40k-$50k",  "$50k-$60k",  "$60k-$70k",  "$70k-$80k",  "$80k-$90k",  "$90k-$100k", "$100k-$110k", "$110k-$120k", "$120k-$130k","$130k-$140k","$140k-$150k", "Above $150k")
oth_inc_bands <- list("Below $0", "$0-$20k", "$20k-$40", "$40k-$60k",  "$60k-$80k",  "$80k-$100k",  "$100k-$120k", "$120k-$140k", "$140k-$160k", "$160k-$180k", "$180k-$200k", "$200k-$220k", "$220k-$240k","$240k-$260k","$260k-$280k","$280k-$300k", "Above $300k")
income_types <- c("Income Quantiles", "Income Bands")
population_types <- c("Household", "Family", "Individual")
descriptions <- c("All households",                    "Aged 0-15",                         "Aged 16-64",                        "Aged 65+",                         
                  "Single with children",              "Single without children",           "Couple with children",              "Couple without children",          
                  "Multiple families with children",   "Multiple families without children","With children",                     "Without children",                 
                   "Core benefits",                     "WFF",                               "FTC",                               "MFTC",                             
                   "IWTC",                              "BestStart", "NZ Super",                          "Accommodation supplement",          "WEP",                              
                   "All families", "All individuals")
income_measures <- c("Equivalised Disposable Income","Disposable Income",            "Taxable Income",               "AHC Disposable Income" )
income_components_list <- c("Equivalised Disposable Income", "Disposable Income", "Taxable Income", "AHC Disposable Income", "Housing Costs", "Wage/Salary Income", "Income Tax", "Core Benefits", "Self-Employment Income", "WFF", "FTC", "MFTC", "IWTC", "BestStart", "NZ Super", "Accomodation Supplement", "WEP")

for (year in years) {
  total_output <- data.table()
  for (population_type in population_types) {
    
    for (description in descriptions) {
      
      for (income_measure in income_measures) {
        
        for (income_component in income_components_list) {
          blank_summary_vent <- data.table(Income.Group = 1:20)
          blank_summary_vent[, ':='(Income.Type = "Income Quantiles", Population.Type = population_type, Description = description, Income.Measure = income_measure, Value.Type = income_component, Value = runif(1, min = 1, max = 200000), Population = runif(1, min = 1, max = 2000000))]
          blank_summary_bands <- data.table
          if (income_measure == "Equivalised Disposable Income") {
            blank_summary_bands <- data.table(Income.Group = eq_inc_bands)
            blank_summary_bands[, ':='(Income.Type = "Income Bands", Population.Type = population_type, Description = description, Income.Measure = income_measure, Value.Type = income_component, Value = runif(1, min = 1, max = 200000), Population = runif(1, min = 1, max = 2000000))]
          }
          else {
            blank_summary_bands <- data.table(Income.Group = oth_inc_bands)
            blank_summary_bands[, ':='(Income.Type = "Income Bands", Population.Type = population_type, Description = description, Income.Measure = income_measure, Value.Type = income_component, Value = runif(1, min = 1, max = 200000), Population = runif(1, min = 1, max = 2000000))]
          }
          total_output <- rbind(total_output, blank_summary_vent, blank_summary_bands)
        } 
      }
    }
  }
  
  test_output <- total_output
  
  test_output <- test_output[!(Population.Type == "Individual" & Income.Measure %in% c("Equivalised Disposable Income", "AHC Disposable Income"))]
  test_output <- test_output[!(Population.Type == "Individual" & Value.Type %in% c("Equivalised Disposable Income", "AHC Disposable Income", "Housing Costs"))]
  test_output <- test_output[!(Population.Type == "Family" & Income.Measure == "AHC Disposable Income")]
  test_output <- test_output[!(Population.Type == "Family" & Value.Type %in% c("AHC Disposable Income", "Housing Costs"))]
  test_output <- test_output[!(Description %in% c("Core benefits", "WFF", "FTC", "MFTC", "IWTC", "NZ Super", "Accommodation supplement", "WEP", "BestStart") & Income.Measure != Value.Type)] 
  test_output <- test_output[!(Population.Type == "Household" & Description %in% c("All families", "All individuals"))]
  test_output <- test_output[!(Population.Type == "Family" & Description %in% c("All individuals", "All households", "Multiple families with children", "Multiple families without children"))]
  test_output <- test_output[!(Population.Type == "Individual" & Description %in% c("All families", "All households", "Single with children", 
                                                                                      "Single without children", "Couple with children", "Couple without children", 
                                                                                      "Multiple families with children", "Multiple families without children", 
                                                                                      "With children", "Without children"))]
  test_output <- test_output[!(Value.Type %in% income_measures & Income.Measure != Value.Type)]
  test_output[, Index := (1:.N)]
  setcolorder(test_output, c("Index", "Income.Group", "Income.Type", "Population.Type", "Description", "Income.Measure", "Value", "Population"))
  fwrite(test_output, paste0("app/data/DE_fictionalHES", hes, "_", efu, "_TY", year, ".csv"))
}
