#' App to visualise income distributions for households and families 
#' in New Zealand by different population sub groups.

############################### 
# Librarys and packages
###############################
pkg = c("shiny", "ggplot2", "plotly", "data.table", "stringr", 
        "dplyr", "scales", "shinyWidgets", "bslib")

new.pkg = pkg[!(pkg %in% installed.packages())]
if (length(new.pkg)) {
  install.packages(new.pk)
}

library(shiny)
library(data.table)
library(ggplot2)
library(plotly)
library(dplyr)
library(stringr)
library(scales)
library(shinyWidgets)
library(bslib)
#library(shinythemes)

#' Reads in STATSNZ output data and merges the two sheets into one table
#' 
#' @param filename the name of the file of the STATSNZ output data
#' 
#' @return a table of the merged excel sheets
#' 
read_IDI_data = function(filename){
  wb = openxlsx::loadWorkbook(filename)
  descriptors = data.table(openxlsx::read.xlsx(wb, sheet = "Descriptors"))
  values = data.table(openxlsx::read.xlsx(wb, sheet = "Values"))
  results = merge(descriptors, values, by = "Index")
  
  return(results)
}

# Sub groups for HH
  groupsHH = c(
    "All households", "Aged 0-15", 'Aged 16-64', "Aged 65+", "Single with children", 
    "Single without children", "Couple with children", "Couple without children", 
    "Multiple families with children", "Multiple families without children", 
    "With children", "Without children", "Accommodation supplement", 
    "Core benefits", "FTC", "IWTC", 
    "MFTC", "NZ Super", "WEP", "WFF")
  
  # Sub groups for families
  groupsFam = c(
    "All families", "Aged 0-15", 'Aged 16-64', "Aged 65+", "Single with children", 
    "Single without children", "Couple with children", "Couple without children", 
    "With children", "Without children", "Accommodation supplement", "Core benefits", 
    "FTC", "IWTC", "NZ Super", "WEP", "WFF")
  
# groupsHH = c(
#   "All households", 
#   "Aged 0-15", 
#   'Aged 16-64', 
#   "Aged 65+", 
#   "Single with children", 
#   "Single without children", 
#   "Couple with children", 
#   "Couple without children", 
#   "Multiple families with children", 
#   "Multiple families without children", 
#   "With children", 
#   "Without children", 
#   "Accomodation supplement", 
#   "Core benefits", 
#   "FTC", 
#   "IWTC", 
#   "MFTC", 
#   "NZ Super", 
#   "Winter energy payment", 
#   "Working for families"
# )

# Sub groups for families
# groupsFam = c(
#   "All families", 
#   "Aged 0-15", 
#   'Aged 16-64', 
#   "Aged 65+", 
#   "Single with children", 
#   "Single without children", 
#   "Couple with children", 
#   "Couple without children", 
#   "With children", 
#   "Without children", 
#   "Accomodation supplement", 
#   "Core benefits", 
#   "FTC", 
#   "IWTC", 
#   "NZ Super", 
#   "Winter energy payment", 
#   "Working for families"
# )

# Color blind palette for plotting
cbPalette <- c("#56B4E9", "#E69F00",  "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")


# incomeBands = c("Below $0", "$0-$10k", "$10k-$20k", "$20k-$30k", "$30k-$40", "$40k-$50k",
#                 "$50k-$60k", "$60k-$70k", "$70k-$80k", "$80k-$90k", "$90k-$100k", "$100k-$110k",
#                 "$110k-$120k", "$120k-$130k", "$130k-$140k", "$140k-$150k", "Above $150k")

#############################################
## Server
#############################################
server = function(input, output, session) {
  
  ###############################
  #Data read in using file selector
  ###############################
  dataUpload = reactive({
    inFile = input$file1
    
    if(is.null(inFile)){
      return(NULL)
    }
    
    dt_frame = read_IDI_data(as.character(inFile[1]))
    updateSelectInput(session, "product", choices = names(dt_frame))
    dt_frame$Population = as.numeric(dt_frame$Population)
    dt_frame[is.na(data)] = 0
    dt_frame[Description=="Aged 0-16", Description:="Aged 0-15"]
    dt_frame[Description=="Super annuation", Description:="NZ Super"]
    dt_frame[Description=="Multiple families tax credits", Description:="MFTC"]
    dt_frame[Description=="Accomodation supplement", Description:="Accommodation supplement"]
    dt_frame[Description=="Family tax credits", Description:="FTC"]
    dt_frame[Description=="In work tax credits", Description:="IWTC"]
    dt_frame[Description=="Working for families", Description:="WFF"]
    dt_frame[Description=="Winter energy payment", Description:="WEP"]
    
    # dt_frame[Description=="Aged 0-16", Description:="Aged 0-15"]
    # dt_frame[Description=="Aged 0-16", Description:="Aged 0-15"]
  
    return(dt_frame)
  })
  
  ###############################
  #Ventile plot
  ###############################
  output$ventilePlot = renderPlotly({
    
    if(is.null(input$file1)){
      return(NULL)
    }
    
    dataset= dataUpload()
    
    if(length(input$show_groups) > 8){
      stop("Maximum selection is 8 population subgroups")
      return(NULL)
    }
    
    if (input$populationType == 'Household'){
      ventiles = dataset[Income.Type == "Income Quantiles" & Population.Type == input$populationType & Description %in% input$show_groups
                         & Income.Measure == input$incomeSort]
    }
    
    
    if (input$populationType == 'Family'){
      ventiles = dataset[Income.Type == "Income Quantiles" & Population.Type == input$populationType & Description %in% input$show_groups2
                         & Income.Measure == input$incomeSort]
    }
    
    p = ggplot(ventiles, aes(x = as.numeric(Income.Group), y = Population, fill = Description)) +
      labs(title = str_to_title(paste(input$populationType, input$incomeSort, "distribution by ventiles", sep = " ")), 
           x = "Ventiles", y = "Population") +
      scale_x_continuous(breaks = seq(1:20)) +
      scale_y_continuous(labels = comma,limits = c(0, NA),
                         expand = expansion(mult = c(0, 0.05))) +  # Includes 0 in y axis and has the top of graph 5% above the max value
      theme_classic() +
      theme(plot.title = element_text(hjust = 0.5, size = 15),
            axis.text.x = element_text(size = 8),
            axis.text.y = element_text(size = 8),
            axis.title = element_text(size = 10),
            legend.title.align = 0.5) 
    
    # Adds geoms, color palette and faceting
    if(input$pairPlot == TRUE) {
      p = p + facet_wrap(~Description)
      
      if(input$plotType == "Histogram"){
        p = p + geom_col(position = "dodge", color = "black", fill = "#56B4E9") 
        
      }
      else if(input$plotType == "linePlot") {
        p = p + geom_line(group = 1, color = "#56B4E9") 
        p = p + geom_point(group = 1, color = "black", fill = "#56B4E9")
        
      }
      else if(input$plotType == "smoothPlot") {
        p = p + geom_smooth(colour = "#56B4E9", aes(group = Description), se = FALSE, span = 0.5)
        
      }
    } else {
      if(input$plotType == "Histogram"){
        p = p + geom_col(position = "dodge", colour = "black") 
        p = p + scale_fill_manual(values = cbPalette)
      }
      else if(input$plotType == "linePlot") {
        p = p + geom_line(aes(color = Description), group = 1) 
        p = p + geom_point(aes(color = Description), group = 1)
        p = p + scale_colour_manual(values = cbPalette)
      }
      else if(input$plotType == "smoothPlot") {
        p = p + geom_smooth(aes(color = Description, group = Description), se = FALSE, span = 0.5)
        p = p + scale_colour_manual(values = cbPalette)
      }
    }
    
    p$labels$fill = "Subgroups"
    
    p %>%
      ggplotly()
  })
  
  ################################
  #Income band plot
  ################################
  output$incomeBandPlot = renderPlotly({
    
    if(is.null(input$file1)){
      return(NULL)
    }
    
    dataset= dataUpload()
    
    if(length(input$show_groups) > 8){
      return(NULL)
    }
    
    if (input$populationType == 'Household'){
      incomeBand = dataset[Income.Type == "Income Bands" & Population.Type == input$populationType & Description %in% input$show_groups
                           & Income.Measure == input$incomeSort]
    }
    
    if (input$populationType == 'Family'){
      incomeBand = dataset[Income.Type == "Income Bands" & Population.Type == input$populationType & Description %in% input$show_groups2
                           & Income.Measure == input$incomeSort]
    }
    
    incomeBand[, Band := as.factor(Income.Group)]
    
    # Different income bands for different income types.
    if (input$incomeSort == "Equivalised Disposable Income"){
      incomeBand$Band = factor(incomeBand$Band, levels = 
  c("Below $0", "$0-$10k", "$10k-$20k", "$20k-$30k", "$30k-$40", "$40k-$50k", "$50k-$60k",
    "$60k-$70k", "$70k-$80k", "$80k-$90k", "$90k-$100k", "$100k-$110k", "$110k-$120k", 
    "$120k-$130k", "$130k-$140k", "$140k-$150k", "Above $150k"))
    } else {
      incomeBand$Band = factor(incomeBand$Band, levels = 
  c("Below $0", "$0-$20k", "$20k-$40", "$40k-$60k", "$60k-$80k", "$80k-$100k", "$100k-$120k",
    "$120k-$140k", "$140k-$160k", "$160k-$180k", "$180k-$200k", "$200k-$220k", "$220k-$240k",
    "$240k-$260k", "$260k-$280k", "$280k-$300k", "Above $300k"))
    }

    p = ggplot(incomeBand, aes(x = Band, y=Population, fill=Description)) +
      labs(title = str_to_title(paste(input$populationType, input$incomeSort, "distribution by income bands", sep = " ")),
           x = "Income Bands", y = "Population") +
      scale_y_continuous(labels = comma, limits = c(0, NA), 
                         expand = expansion(mult = c(0, 0.05))) +  # Includes 0 in y axis and has the top of graph 5% above the max value
      theme_classic() +
      theme(plot.title = element_text(hjust = 0.5, size = 15),
            axis.text.x = element_text(angle = 45, vjust = 0.5, size = 8),
            axis.text.y = element_text(size = 8),
            axis.title = element_text(size = 10), 
            legend.title.align = 0.5) 
    
    # Adds geoms, color palette and faceting
    if(input$pairPlot == TRUE) {
      p = p + facet_wrap(~Description) 
      
      if(input$plotType == "Histogram"){
        p = p + geom_col(position = "dodge", color = "black", fill = "#56B4E9") 
        
      }
      else if(input$plotType == "linePlot") {
        p = p + geom_line(group = 1, color = "#56B4E9") 
        p = p + geom_point(group = 1, color = "black", fill = "#56B4E9")
        
      }
      else if(input$plotType == "smoothPlot") {
        p = p + geom_smooth(colour = "#56B4E9", aes(group = Description), se = FALSE, span = 0.5)
        
      }
    } else {
      if(input$plotType == "Histogram"){
        p = p + geom_col(position = "dodge", colour = "black") 
        p = p + scale_fill_manual(values = cbPalette)
      }
      else if(input$plotType == "linePlot") {
        p = p + geom_line(aes(color = Description), group = 1) 
        p = p + geom_point(aes(color = Description), group = 1)
        p = p + scale_colour_manual(values = cbPalette)
      }
      else if(input$plotType == "smoothPlot") {
        p = p + geom_smooth(aes(color = Description, group = Description), se = FALSE, span = 0.5)
        p = p + scale_colour_manual(values = cbPalette)
      }
    }
    
    p$labels$fill = "Subgroups"
    
    p %>% 
      ggplotly()
    
  })
  
  ###############################
  #Ventile table output
  ###############################
  
  # Creates table
  ventileTable = reactive ({
    
    if(is.null(input$file1)){
      return(NULL)
    }
    
    dataset = dataUpload()
    
    if (input$populationType == 'Family'){
      ventiles = dataset[Income.Type == "Income Quantiles" & Population.Type == input$populationType & Description %in% input$show_groups2
                         & Income.Measure == input$incomeSort] 
      
    } else {
      ventiles = dataset[Income.Type == "Income Quantiles" & Population.Type == input$populationType & Description %in% input$show_groups
                         & Income.Measure == input$incomeSort]
    }
    
    ventiles[, c("Index", "Income.Type", "Value") := NULL]
    colnames(ventiles) = c("Ventile", "Population Type", "Population Subgroup", "Income Type", "Population Value")
    
    return(ventiles)
    
  })
  
  #Displays table
  output$ventileTable = renderDataTable({
    
    if(is.null(input$file1)){
      return(NULL)
    }
    
    ventileTable()
  })
  
  # Download handler 
  output$downloadData1 <- downloadHandler(
    filename = function() {
      paste("ventileData", Sys.Date(), "csv", sep = ".")
    },
    content = function(file) {
      write.csv(ventileTable(), file)
    }
  )
  
  # Table title
  output$ventileDataTitle = renderText({
    str_to_title(paste(input$populationType, input$incomeSort, "distribution by ventiles", sep = " "))
  })
  
  ################################
  #Income band table output
  ################################
  
  # Creates table
  incomeBandTable = reactive ({
    if(is.null(input$file1)){
      return(NULL)
    }
    
    dataset = dataUpload()
    
    if (input$populationType == 'Household'){
      incomeBand = dataset[Income.Type == "Income Bands" & Population.Type == input$populationType & Description %in% input$show_groups
                           & Income.Measure == input$incomeSort]
    }
    
    if (input$populationType == 'Family'){
      incomeBand = dataset[Income.Type == "Income Bands" & Population.Type == input$populationType & Description %in% input$show_groups2
                           & Income.Measure == input$incomeSort]
    }
    
    incomeBand[, c("Index", "Income.Type", "Value") := NULL]
    colnames(incomeBand) = c("Income Band", "Population Type", "Population Subgroup", "Income Type", "Population Value")
    
    return(incomeBand)
  }
  )
  
  # Displays table
  output$incomeBandTable = renderDataTable({
    
    if(is.null(input$file1)){
      return(NULL)
    }
    
    incomeBandTable()
  })
  
  # Download handler 
  output$downloadData2 <- downloadHandler(
    filename = function() {
      paste("incomeBandData", Sys.Date(), "csv", sep = ".")
    },
    content = function(file) {
      write.csv(incomeBandTable(), file)
    }
  )
  
  # Table title
  output$incomeBandDataTitle = renderText({
    str_to_title(paste(input$populationType, input$incomeSort, "distribution by income bands", sep = " "))
  })
}

###############################
#User Interface
###############################
ui = fluidPage(
  
  # Theme of the UI
  theme = bslib::bs_theme(bootswatch = "superhero"),
  
  headerPanel("Income Distribution Explorer"),
  
  # Sidebar panel
  sidebarLayout(
    sidebarPanel(width = 3,
                 # Help text               
                 h6("App best run in full screen"),
                 h6(" ")
                 ,
                 
                 # For reading in the file
                 fileInput(inputId = 'file1', 
                           label = 'Select Excel File:',
                           accept=c('text/xlsx')),
                 
                 # For selecting the popultion type, HH or Fam
                 radioGroupButtons(
                   inputId = "populationType",
                   label = "Select Population Unit:",
                   choices = c(Households = 'Household',
                               Families = "Family"),
                   justified = TRUE,
                   direction = 'vertical',
                   checkIcon = list(yes = icon("ok", 
                                               lib = "glyphicon"))),
                 
                 # When households is selected
                 conditionalPanel(
                   condition = "input.populationType == 'Household'",
                   pickerInput(
                     inputId = "show_groups",
                     label = "Select Population Subgroups:",
                     choices = groupsHH,
                     selected = groupsHH[1],
                     options = list(size = 5, 
                                    `live-search` = TRUE),
                     multiple = TRUE)),
                 
                 # When households is selected
                 conditionalPanel(
                   condition = "input.populationType == 'Family'",
                   pickerInput(
                     inputId = "show_groups2",
                     label = "Select Population Subgroups:",
                     choices = groupsFam,
                     selected = groupsFam[1],
                     options = list(size = 5, 
                                    `live-search` = TRUE,
                                    "max-options" = 8),
                     multiple = TRUE)),
                 
                 # To select income sort
                 radioGroupButtons(
                   inputId = "incomeSort",
                   label = "Select Income Type:",
                   choices = c(`Equivalised Disposable Income` = "Equivalised Disposable Income",
                               `Taxable Income` = "Taxable Income",
                               `Total Income` = "Total Income",
                               `Disposable Income` = "Disposable Income"
                               ),
                   justified = TRUE,
                   direction = 'vertical',
                   checkIcon = list(
                     yes = icon("ok", 
                                lib = "glyphicon"))),
                 
                 # To select plot type
                 radioGroupButtons(
                   inputId = "plotType",
                   label = "Select Plot Type:",
                   choices = c(Histogram = "Histogram",
                               `Line Plot` = "linePlot",
                               `Smooth Line Plot` = "smoothPlot"),
                   justified = TRUE,
                   direction = 'vertical',
                   checkIcon = list(yes = icon("ok", 
                                               lib = "glyphicon"))),
                 
                 # To select pair plot
                 materialSwitch(
                   inputId = "pairPlot",
                   label = "Display As Pair Plot:", 
                   value = FALSE,
                   status = "success"),
                 
                 tags$i("Useful when multiple sub groups are selected")),  # Help text
    
    # Main panel with plots, tables and documentation
    mainPanel(
      
      tabsetPanel(
        
        # Plot tab
        tabPanel("Income Distribution Plots", 
                 br(),
                 plotlyOutput("ventilePlot"),
                 br(),
                 plotlyOutput("incomeBandPlot"),
                 br()),
        
        # Data table tab
        tabPanel("Income Distribution Tables", 
                 
                 # Ventile table
                 h6(verbatimTextOutput("ventileDataTitle"), align = "center"),
                 dataTableOutput("ventileTable"),
                 br(),
                 downloadButton(outputId = "downloadData1", 
                                label = "Download Data"),
                 br(),
                 br(),
                 
                 # Income Band table
                 h6(verbatimTextOutput("incomeBandDataTitle"), align = "center"),
                 dataTableOutput("incomeBandTable"),
                 br(),
                 downloadButton(outputId = "downloadData2", 
                                label = "Download Data"),
                 br()),
        
# Documentation Tab
tabPanel("Instructions",
h4("Overview"),
p("The income distribution explorer is a tool which can be used to understand income distribution for households and families in New Zealand."),
p("The tool allows users to explore and compare the income distribution for subgroups of interest, for example households with individuals aged over 65, households with (and without) children, and households in receipt of different welfare payments."),
h4("Instructions"),
p(strong(" 1)"), "Maximise window to full screen."),
p(strong(" 2)"), "Select the input data from the excel file provided using the Browse... button."),
p(strong(" 3)"), "Select Population Type: either Households or Families."),
p(strong(" 4)"), "Select Population Sub Groups using the drop down menu, for example, Couple with children and Couple without children. We suggest that no more than 4 subgroups are selected otherwise the plots become difficult to interpret."),
p(strong(" 5)"), "Select the Income type: either Equivalised Disposable Income, Taxable Income, Disposable Income or Total Income."),
p(strong(" 6)"), "Select Plot Type: either Histogram, Line Plot or Smooth Line Plot."),
p(strong(" 7)"), "Choose if you would like plots displayed as a Pair Plot. This is useful when multiple subgroups are selected."),

p(strong("Income Distribution Plots")),
p("This tab displays the income distribution for the subgroups and income measure selected by ventiles and fixed income bands. See Definitions for more detail on the income boundaries used in this tool."),
p("To download a plot, hover the mouse over the plot which will cause several icons to appear in the top right corner of the plot. Click on the little camera icon to download a plot."),

p(strong("Income Distribution Tables")),
p("This tab displays the data which is used to create the ventile and income band plots. A Download Data button is available to save the data as a CSV file."),
p("Note if there is an 'S' or a blank space in the tables, this means the population was too small to be released from the IDI and has been suppressed."),
h4("Disclaimer"),
p("These results are not official statistics. They have been created for research purposes from the Integrated Data Infrastructure (IDI) which is carefully managed by Stats NZ. For more information about the IDI please visit ", 
  tags$a(
  href="https://www.stats.govt.nz/integrated-data/", 
  "https://www.stats.govt.nz/integrated-data/",target="_blank"), ". The results are based in part on tax data supplied by Inland Revenue to Stats NZ under the Tax Administration Act 1994 for statistical purposes. Any discussion of data limitations or weaknesses is in the context of using the IDI for statistical purposes, and is not related to the data’s ability to support Inland Revenue’s core operational requirements."), 
br()),
        
tabPanel("Definitions",         
h5(strong("Income Type")),

p(strong("Household equivalised disposable income: "), "is calculated by dividing the total household disposable income by an equivalisation factor. TAWA uses the 'modified OECD' equivalence scale to estimate the equivalisation factor and applies a weight of 1.0 to the first adult in the household, 0.5 to other household members aged 14 and over, and 0.3 to those aged less than 14. A 'family' equivalised disposable income is also provided but results should be treated with caution as this calculation is not used in regular TAWA analysis/advice."), 

p(strong("Taxable income: "),"the sum of all core benefits (JSS, SLP, SPS), superannuation, student allowance, other taxable benefits, wage and salary income, redundancy income, self-employment income and other taxable income",strong("before"),"tax and any other deductions are removed. Note that negative income amounts are included when calculating taxable income though total taxable income is defined as greater than or equal to zero."),
  
p(strong("Total income: "),"is the sum of taxable income (see above), abated accommodation supplement, winter energy payment, other non-taxable benefits, non-taxable income, and working for family tax credits. Note working for family tax credits include FTC, IWTC, MFTC, PTC, Best Start and IETC."),

p(strong("Disposable income: "), "total income less tax and other deductions such as the ACC levy. Note negative incomes are included."),

br(),
                 
h5(strong("Population subgroups")),

p("In the population subgroups below, a child is defined using the Working for Family definition of a ‘dependent child’."),
p(" "),
p("For a selected population unit, i.e. household or family,"),

tags$ul(
tags$li(strong("Aged 0-15"),"indicates there is a least one individual aged 0-15 in the ‘population unit’"),
tags$li(strong("Aged 16-64"),"indicates there is a least one individual aged 16-64 in the ‘population unit’"),
tags$li(strong("Aged 65+"),"indicates there is a least one individual aged 16-64 in the ‘population unit’")),

tags$ul(
  tags$li(strong("Single with children"),"indicates that there is a single adult with children in the ‘population unit’"),
  tags$li(strong("Single without children"),"indicates that there is a single adult without children in the ‘population unit’"),
  tags$li(strong("Couple with children"),"indicates that there is a couple with children in the ‘population unit’"),
  tags$li(strong("Couple without children"),"indicates that there is a couple without with children in the ‘population unit’"),
  tags$li(strong("Multiple families with children"),"indicates that there are multiple families in the household with at least one family having at least one child."),
  tags$li(strong("Multiple families without children"),"indicates that there are multiple families in the household with no children.")),

tags$ul(
  tags$li(strong("With children"),"indicates that there are children in the ‘population unit’"),
  tags$li(strong("Without children"),"indicates that there are no children in the ‘population unit’")),

tags$ul(
  tags$li(strong("Accommodation supplement"),"indicates there is at least one individual receiving an Accommodation Supplement payment in the ‘population unit’"),
  tags$li(strong("Core benefits"),"indicates there is at least one individual receiving a core benefit (JSS, SLP, or SPS) in the ‘population unit’"),
  tags$li(strong("FTC"),"indicates there is at least one individual receiving a Family Tax Credit in the ‘population unit’"),
  tags$li(strong("IWTC"),"indicates there is at least one individual receiving a In Work Tax Credit in the ‘population unit’"),
tags$li(strong("MFTC"),"indicates there is at least one individual receiving a Minimum Family Tax Credit in the ‘population unit’"),
tags$li(strong("NZ Super"),"indicates there is at least one individual receiving NZ Superannuation in the ‘population unit’"),
tags$li(strong("WEP"),"indicates there is at least one individual receiving a Winter Energy Payment in the ‘population unit’"),
tags$li(strong("WFF"),"indicates there is at least one individual receiving Working for Families in the ‘population unit’")),

br(),
                 
h5(strong("Other")),
p(strong("Fixed income bands: "), "household / families are assigned an income band based on the income selected, e.g. disposable income. Each income band is of equal width (except for the  first and last income band) and has a different population size."),
p(strong("Ventiles: "), "the overall household (or family) population is separated into 20 equal groups based on the income selected, e.g. disposable income. The corresponding income bands (not currently provided) will be of different sizes with thinner income bands where the population is larger and wider income bands where the population is not as smaller.")
        )
      )
    )
  )
)

###############################
#Runs the program
###############################
shinyApp(ui = ui, server = server)
