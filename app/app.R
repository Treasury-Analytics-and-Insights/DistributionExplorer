library(shiny)
library(data.table)
library(DT)
library(ggplot2)
library(plotly)
library(dplyr)
library(stringr)
library(scales)
library(shinyWidgets)
library(bslib)
library(bsicons)

source('functions.R')

#creates a list of available data
data_versions <- data_versions_lists()

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

# Color blind palette for plotting
cbPalette <- c("#00718f", "#E69F00",  "#009E73", "#F0E442", "#56B4E9", "#D55E00", "#CC79A7", "#000000")
ui <- (navbarPage(
  "||DRAFT|| Income Distribution Explorer",
  theme = bslib::bs_theme(bootswatch = "cosmo",
                          bg = "#FFFFFF",
                          fg = "#00718f",
                          primary = "#00718f",
                          secondary = "#00718f"),
  tabPanel(
    "Tools",
    fluidRow(
      column(1),
      column(10,
             "This is a draft version of the Distribution Explorer that includes preliminary data."
      ),
      column(1)
    ),
    fluidRow(
      column(1),
      column(10,
    sidebarLayout(
      sidebarPanel(
        
        "Select Distribution Type:",
        radioGroupButtons(
          inputId = "y_type",
          label = NULL,
          choices = c("Population", "Income"),
          selected = "Population",
          checkIcon = list(yes = icon("ok", 
                                      lib = "glyphicon")),
          justified = TRUE,
          width = '100%'
        ),
        
        #For selecting the tax year from a given HES/EFU combination
        "Select Tax Year(s):",
        pickerInput(
          inputId ="chosen_file", 
          label = NULL, 
          choices = data_versions,
          selected = data_versions[1],
          multiple = TRUE,
          options = list(size = 5, 
                         `live-search` = TRUE,
                         "max-options" = 8)
          ),
        
        # For selecting the population type, HH or Fam
        "Select Population Unit:",
        radioGroupButtons(
          inputId = "populationType",
          label = NULL,
          choices = c(Households = 'Household',
                      Families = "Family"),
          justified = TRUE,
          checkIcon = list(yes = icon("ok", 
                                      lib = "glyphicon")),
          width = '100%'),

        
        # When households is selected
        conditionalPanel(
          condition = "input.populationType == 'Household' && input.chosen_file.length == 1",
          "Select Population Subgroup(s):",
          tooltip(
            bsicons::bs_icon("question-circle"),
            "Household is in a subgroup when at least one individual in the household is: of selected age | in slected family type | receiving selected benefit",
            placement = "right"
          ),
          pickerInput(
            inputId = "show_groups_HH",
            label = NULL,
            choices = groupsHH,
            selected = groupsHH[1],
            options = list(size = 5, 
                           `live-search` = TRUE),
            multiple = TRUE)
        ),
        
        # When family is selected
        conditionalPanel(
          condition = "input.populationType == 'Family' && input.chosen_file.length == 1",
          "Select Population Subgroup(s):",
          tooltip(
            bsicons::bs_icon("question-circle"),
            "Family is in a subgroup when at least one individual in the family is: of selected age | in slected family type | receiving selected benefit",
            placement = "right"
          ),
          pickerInput(
            inputId = "show_groups_Fam",
            label = NULL,         
            choices = groupsFam,
            selected = groupsFam[1],
            options = list(size = 5, 
                           `live-search` = TRUE,
                           "max-options" = 8),
            multiple = TRUE)

          ),
        
        #disable multiple selection when multiple years selected
        
        # When households is selected
        conditionalPanel(
          condition = "input.populationType == 'Household' && input.chosen_file.length > 1",
          "Select Population Subgroup(s):",
          tooltip(
            bsicons::bs_icon("question-circle"),
            "Household is in a subgroup when at least one individual in the household is: of selected age | in slected family type | receiving selected benefit",
            placement = "right"
          ),
          pickerInput(
            inputId = "show_groups_HH_multi",
            label = NULL,
            choices = groupsHH,
            selected = groupsHH[1],
            options = list(size = 5, 
                           `live-search` = TRUE)),
          p(em("*When multiple years are selected only a single subgroup can be chosen."), style = "margin-top: -10px; margin-bottom: 7px;")
        ),
        
        # When family is selected
        conditionalPanel(
          condition = "input.populationType == 'Family' && input.chosen_file.length > 1",
          "Select Population Subgroup(s):",
          tooltip(
            bsicons::bs_icon("question-circle"),
            "Family is in a subgroup when at least one individual in the family is: of selected age | in slected family type | receiving selected benefit",
            placement = "right"
          ),
          pickerInput(
            inputId = "show_groups_Fam_multi",
            label = NULL,         
            choices = groupsFam,
            selected = groupsFam[1],
            options = list(size = 5, 
                           `live-search` = TRUE)),
          p(em("*When multiple years are selected only a single subgroup can be chosen."), style = "margin-top: -10px; margin-bottom: 7px;"),
          
        ),
        
        # To select income sort
        conditionalPanel(
          condition = "input.populationType == 'Household'",
          "Select Income Type:",
        radioGroupButtons(
          inputId = "incomeSortHH",
          label = NULL,
          choices = c(`Equivalised Disposable Income` = "Equivalised Disposable Income",
                      `Taxable Income` = "Taxable Income",
                      #`Total Income` = "Total Income",
                      `Disposable Income` = "Disposable Income",
                      `After Housing Cost Disposable Income` = "AHC Disposable Income"
          ),
          selected = "Equivalised Disposable Income",
          justified = TRUE,
          direction = 'vertical',
          checkIcon = list(
            yes = icon("ok", 
                       lib = "glyphicon")),
          width = '100%')
        ),
        
        conditionalPanel(
          condition = "input.populationType == 'Family'",
        "Select Income Type:",
        radioGroupButtons(
          inputId = "incomeSortFam",
          label = NULL,
          choices = c(`Equivalised Disposable Income` = "Equivalised Disposable Income",
                      `Taxable Income` = "Taxable Income",
                      #`Total Income` = "Total Income",
                      `Disposable Income` = "Disposable Income"
          ),
          selected = "Equivalised Disposable Income",
          justified = TRUE,
          direction = 'vertical',
          checkIcon = list(
            yes = icon("ok", 
                       lib = "glyphicon")),
          width = '100%')
        ),
        
        # To select plot type
        "Select Plot Type:",
        radioGroupButtons(
          inputId = "plotType",
          label = NULL,
          choices = c(Histogram = "Histogram",
                      `Line Plot` = "linePlot",
                      `Smooth Line Plot` = "smoothPlot"),
          justified = TRUE,
          direction = 'vertical',
          checkIcon = list(yes = icon("ok", 
                                      lib = "glyphicon")),
          width = '100%'),
        
        # To select pair plot
        "Display As Pair Plot:",
        tooltip(
          bsicons::bs_icon("question-circle"),
          "Useful when multiple sub groups are selected",
          placement = "right"
        ),
        materialSwitch(
          inputId = "pairPlot",
          label = NULL, 
          value = FALSE,
          status = "primary"),
        
        conditionalPanel(
          condition = "input.chosen_file.length > 1 && input.y_type == 'Population'",
          textOutput("normalised_text", inline = TRUE),
          tooltip(
            bsicons::bs_icon("question-circle"),
            "Scales populations down to lowest population year, accounts for population growth",
            placement = "right"
          ),
          materialSwitch(
            inputId = "normalised",
            label = NULL, 
            value = FALSE,
            status = "primary")
        )
      ),
      
      mainPanel(
        tabsetPanel(
          type = "pills",
          tabPanel(
            "Plots",
            hr(),
            plotlyOutput("ventilePlot"),
            conditionalPanel(
              condition = "input.pairPlot == 1",
              p("Ventiles", align = "center",style = "color:black; margin-top: -5px; margin-bottom: -5px")
            ),
            hr(),
            plotlyOutput("incomeBandPlot"),
            conditionalPanel(
              condition = "input.pairPlot == 1",
              p("Income Bands", align = "center",style = "color:black; margin-top: -5px; margin-bottom: -5px")
            ),
            hr()
          ),
          tabPanel(
            "Tables",
            # Ventile table
            hr(),
            h6(strong(textOutput("ventileDataTitle")), align = "center"),
            h6(textOutput("ventileDataSubTitle"), align = "center"),
            hr(),
            dataTableOutput("ventileTable"),
            br(),
            downloadButton(outputId = "downloadData1",
                           label = "Download Data"),
            br(),
            br(),

            # Income Band table
            hr(),
            h6(strong(textOutput("incomeBandDataTitle")), align = "center"),
            h6(textOutput("incomeBandDataSubTitle"), align = "center"),
            hr(),
            dataTableOutput("incomeBandTable"),
            br(),
            downloadButton(outputId = "downloadData2",
                           label = "Download Data"),
            hr()
          )
        )
      )
    )
  ),
  column(1)
    )
  ),
  tabPanel("Instructions",
           fluidRow(
           column(1),
           column(10,
           h5(strong("Overview")),
           p("The income distribution explorer is a tool which can be used to understand income distribution for households and families in New Zealand."),
           p("The tool allows users to explore the income distribution for population subgroups, as well as compare income distributions for a subgroup over time or with other subgroups. The population can be sorted into subgroups based on the age of, household/family structure of, or benefit recieved by individuals in the household or family."),
           h5(strong("Instructions")),
           p(strong(" 1)"), "Maximise window to full screen."),
           p(strong(" 2)"), "Select Tax Years: using the drop down menu, choose the desired tax year or years. There may be mulitple data sources available for a single tax year."),
           p(strong(" 3)"), "Select Population Unit: either Households or Families."),
           p(strong(" 4)"), "Select Population Subgroups: Using the drop down menu, select the desired population subgroups. When multiple years are selected, then only a single subgroup can be selected. The all households/families captures the entire population. Up to 8 subgroups can be selected at a time, however we suggest that no more than 4 subgroups are selected otherwise the plots become difficult to interpret."),
           p(strong(" 5)"), "Select the Income type: either Equivalised Disposable Income, Taxable Income, or Disposable Income.  When the selected population unit is households, After Housing Cost Disposable income can be selected."),
           p(strong(" 6)"), "Select Plot Type: either Histogram, Line Plot or Smooth Line Plot."),
           p(strong(" 7)"), "Choose if you would like plots displayed as a Pair Plot. This is useful when multiple subgroups are selected."),
           p(strong(" 8)"), "If looking at multiple years, choose if you would like to normalise population values by the lowest population year (as determined by household population)."),
           
           p(strong("Income Distribution Plots")),
           p("This tab displays the income distribution for the subgroups and income measure selected by ventiles and fixed income bands. See Definitions for more detail on the income boundaries used in this tool."),
           p("To download a plot, hover the mouse over the plot which will cause several icons to appear in the top right corner of the plot. Click on the camera icon to download a plot. Note that in pair plot mode, the x-axis title is located outside of the plot and thus will not be downloaded."),
           
           p(strong("Income Distribution Tables")),
           p("This tab displays the data which is used to create the ventile and income band plots. A Download Data button is available to save the data as a CSV file."),
           p("Note if there is an 'S' or a blank space in the tables, this means the population was too small to be released from the IDI and has been suppressed."),
           h5(strong("Disclaimer")),
           p("These results are not official statistics. They have been created for research purposes from the Integrated Data Infrastructure (IDI) which is carefully managed by Stats NZ. For more information about the IDI please visit ", 
             tags$a(
               href="https://www.stats.govt.nz/integrated-data/", 
               "https://www.stats.govt.nz/integrated-data/",target="_blank"), ". The results are based in part on tax data supplied by Inland Revenue to Stats NZ under the Tax Administration Act 1994 for statistical purposes. Any discussion of data limitations or weaknesses is in the context of using the IDI for statistical purposes, and is not related to the data’s ability to support Inland Revenue’s core operational requirements."), 
           br()
           ),
           column(1)
           )
  ),
  tabPanel("Definitions",
           fluidRow(
             column(1),
             column(10,
           h5(strong("Income Type")),
           
           p(strong("Household equivalised disposable income: "), "is calculated by dividing the total household disposable income by an equivalisation factor. TAWA uses the 'modified OECD' equivalence scale to estimate the equivalisation factor and applies a weight of 1.0 to the first adult in the household, 0.5 to other household members aged 14 and over, and 0.3 to those aged less than 14. A 'family' equivalised disposable income is also provided but results should be treated with caution as this calculation is not used in regular TAWA analysis/advice."), 
           
           p(strong("Taxable income: "),"the sum of all core benefits (JSS, SLP, SPS), superannuation, student allowance, other taxable benefits, wage and salary income, redundancy income, self-employment income and other taxable income",strong("before"),"tax and any other deductions are removed. Note that negative income amounts are included when calculating taxable income though total taxable income is defined as greater than or equal to zero."),
           
           p(strong("Disposable income: "), "total income less tax and other deductions such as the ACC levy. Here, total income is the sum of taxable income (see above), abated accommodation supplement, winter energy payment, other non-taxable benefits, non-taxable income, and working for family tax credits. Note negative incomes are included."),
           
           p(strong("After Housing Cost Disposable income: "), "disposable income less housing costs. Only available for households as housing costs are reported at the household level."),
           
           
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
             tags$li(strong("Couple without children"),"indicates that there is a couple without in the ‘population unit’"),
             tags$li(strong("Multiple families with children"),"indicates that there are multiple families in the household with at least one family having at least one child"),
             tags$li(strong("Multiple families without children"),"indicates that there are multiple families in the household with no children")),
           
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
           p(strong("Ventiles: "), "the overall household (or family) population is separated into 20 equal groups based on the income selected, e.g. disposable income. The corresponding income bands (not currently provided) will be of different sizes with thinner income bands where the population is larger and wider income bands where the population is lower")
             ),
             column(1)
           )
  )
  )
)



server <- function(input, output, session) {
  
  #stops normalised selection from preventing switch to income mode
  observeEvent(input$y_type == "Income", {
    updateMaterialSwitch(session, "normalised", value = FALSE)
  })

  data_year = reactive({
    year_list <- NULL
    for (file in input$chosen_file) {
      year_list <- c(year_list, get_year(file))
    }
    return(year_list)
  })


  data_version = reactive({
    version_list <- NULL
    for (file in input$chosen_file) {
      version_list <- c(version_list, paste0(get_hes(file), ", ", get_efu(file)))
    }
    return(version_list)
  })
  
  data_version_full = reactive({
    full_version_list <- NULL
    for (file in input$chosen_file) {
      full_version_list <- c(full_version_list, paste0("20", get_year(file), " (", get_hes(file), ", ", get_efu(file), ")"))
    }
    return(full_version_list)
  })

  income_sort = reactive({
    if (input$populationType == "Household") {
      return(input$incomeSortHH)
    }
    else {
      return(input$incomeSortFam)
    }
  })
  
  show_groups = reactive({
    if (input$populationType == "Household") {
      if(length(input$chosen_file) > 1) {
        return(input$show_groups_HH_multi)
      }
      else {
        return(input$show_groups_HH)
      }
    }
    else {
      if(length(input$chosen_file) > 1) {
        return(input$show_groups_Fam_multi)
      }
      else {
        return(input$show_groups_Fam)
      }
    }
  })  
  
  dataUpload = reactive({
    dt <- NULL
    for (file in input$chosen_file) {
      dt_year = as.data.table(read.csv(paste0("data/", file)))
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
    dt <- norm_pop(dt)[[2]]
    return(dt)
  })
  
  
  output$normalised_text <- renderText(paste0("Normalise populations to ", norm_pop(dataUpload())[[1]], ":"))
  
  ###############################
  #Ventile plot
  ###############################
  output$ventilePlot = renderPlotly({
    
    if(length(show_groups()) > 8){
      stop("Maximum selection is 8 population subgroups")
      return(NULL)
    }
    
    if(length(input$chosen_file) > 8){
      stop("Maximum selection is 8 years")
      return(NULL)
    }
    
    if(length(input$chosen_file) > 1){
      fill <- "file"
      label <- "Years"
      subtitle <- show_groups()
    }
    else {
      fill <- "Description"
      label <- "Subgroups"
      subtitle <- paste0("Tax Year 20", data_year(), " from ", data_version())
    }
    
    if(input$pairPlot) {
      x_label <- NULL
    }
    else {
      x_label <- "Ventiles"
    }
    
    dataset= copy(dataUpload())
    
    if(input$normalised) {
      pop_type <- "Normalised"
      dataset = dataset[Normalised == "S", Normalised := 0L]
      dataset = dataset[,Normalised := as.numeric(Normalised)]
      y_label <- "Normalised Population"
      title_start <- "Population Distribution"
    }
    else if (input$y_type == "Population") {
      pop_type <- "Population"
      dataset = dataset[Population == "S", Population := 0L]
      dataset = dataset[,Population := as.numeric(Population)]
      y_label <- "Population"
      title_start <- "Population Distribution"
    }
    else {
      pop_type <- "Value"
      dataset = dataset[Value == "S", Value := 0L]
      dataset = dataset[,Value := as.numeric(Value)]
      y_label <- paste0("Average ", input$populationType, " \n", income_sort())
      title_start <- "Average Incomes"
    }
    
    ventiles = dataset[Income.Type == "Income Quantiles" & Population.Type == input$populationType & Description %in% show_groups()
                       & Income.Measure == income_sort()]

    p = ggplot(ventiles, aes(x = as.numeric(Income.Group), y = get(pop_type), fill = get(fill))) +
      labs(title = paste0(title_start, " by ", input$populationType, " ", income_sort(), " Ventiles", "\n", subtitle),
           x = x_label, y = y_label) +
      scale_x_continuous(breaks = seq(1:20)) +
      scale_y_continuous(labels = comma,limits = c(0, NA),
                         expand = expansion(mult = c(0, 0.05))) +  # Includes 0 in y axis and has the top of graph 5% above the max value
      theme_classic() +
      theme(plot.title = element_text(hjust = 0.5, size = 10),
            axis.text.x = element_text(size = 8),
            axis.text.y = element_text(size = 8),
            axis.title = element_text(size = 10),
            legend.title.align = 0.5,
            plot.margin = margin(t = 40),
            strip.background =element_rect(fill="#00718f", color = "#00718f"),
            strip.text = element_text(colour = 'white'))

    # Adds geoms, color palette and faceting
    if(input$pairPlot == TRUE) {
      p = p + facet_wrap(~get(fill)) + theme(panel.spacing.y = unit(2, "lines"), 
                                             axis.text.x = element_text(size = 5),
                                             axis.text.y = element_text(size = 5))
      if(input$plotType == "Histogram"){
        p = p + geom_col(position = "dodge", color = "white", fill = "#00718f") 

      }
      else if(input$plotType == "linePlot") {
        p = p + geom_line(group = 1, color = "#00718f") + theme(legend.position = "none")
        p = p + geom_point(group = 1, color = "#00718f", fill = "#00718f")

      }
      else if(input$plotType == "smoothPlot") {
        p = p + geom_smooth(colour = "#00718f", aes(group = get(fill)), se = FALSE, span = 0.5) + theme(legend.position = "none")

      }
    } else {
      if(input$plotType == "Histogram"){
        p = p + geom_col(position = "dodge", colour = "white")
        p = p + scale_fill_manual(values = cbPalette)
      }
      else if(input$plotType == "linePlot") {
        p = p + geom_line(aes(color = get(fill)), group = 1)
        p = p + geom_point(aes(color = get(fill)), group = 1)
        p = p + scale_colour_manual(values = cbPalette)
      }
      else if(input$plotType == "smoothPlot") {
        p = p + geom_smooth(aes(color = get(fill), group = get(fill)), se = FALSE, span = 0.5)
        p = p + scale_colour_manual(values = cbPalette)
      }
    }

    p = p + labs(fill = label, color = NULL)

    p %>%
      ggplotly() %>% config(modeBarButtons = list(list("toImage")))
  })

  ################################
  #Income band plot
  ################################
  output$incomeBandPlot = renderPlotly({

    if(length(show_groups()) > 8){
      return(NULL)
    }
    
    if(length(input$chosen_years) > 8){
      return(NULL)
    }
    
    if(length(input$chosen_file) > 1){
      fill <- "file"
      label <- "Years"
      subtitle <- show_groups()
    }
    else {
      fill <- "Description"
      label <- "Subgroups"
      subtitle <- paste0("Tax Year 20", data_year(), " from ", data_version())
    }
    
    if(input$pairPlot) {
      x_label <- NULL
    }
    else {
      x_label <- "Income Bands"
    }
    
    dataset= copy(dataUpload())
    
    if(input$normalised) {
      pop_type <- "Normalised"
      dataset = dataset[Normalised == "S", Normalised := 0L]
      dataset = dataset[,Normalised := as.numeric(Normalised)]
      y_label <- "Normalised Population"
      title_start <- "Population Distribution"
    }
    else if (input$y_type == "Population") {
      pop_type <- "Population"
      dataset = dataset[Population == "S", Population := 0L]
      dataset = dataset[,Population := as.numeric(Population)]
      y_label <- "Population"
      title_start <- "Population Distribution"
    }
    else {
      pop_type <- "Value"
      dataset = dataset[Value == "S", Value := 0L]
      dataset = dataset[,Value := as.numeric(Value)]
      y_label <- paste0("Average ", input$populationType, " \n", income_sort())
      title_start <- "Average Incomes"
    }

    incomeBand = dataset[Income.Type == "Income Bands" & Population.Type == input$populationType & Description %in% show_groups()
                         & Income.Measure == income_sort()]

    incomeBand[, Band := as.factor(Income.Group)]

    # Different income bands for different income types.
    if (income_sort() == "Equivalised Disposable Income"){
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

    p = ggplot(incomeBand, aes(x = Band, y=get(pop_type), fill=get(fill))) +
      labs(title = paste0(title_start, " by ", input$populationType, " ", income_sort(), " Bands", "\n", subtitle),
           x = x_label, y = y_label) +
      scale_y_continuous(labels = comma, limits = c(0, NA),
                         expand = expansion(mult = c(0, 0.05))) +  # Includes 0 in y axis and has the top of graph 5% above the max value
      theme_classic() +
      theme(plot.title = element_text(hjust = 0.5, size = 10),
            axis.text.x = element_text(angle = 45, vjust = 0.5, size = 8),
            axis.text.y = element_text(size = 8),
            axis.title = element_text(size = 10),
            legend.title.align = 0.5,
            plot.margin = margin(t = 40),
            strip.background =element_rect(fill="#00718f", color = "#00718f"),
            strip.text = element_text(colour = 'white'))

    # Adds geoms, color palette and faceting
    if(input$pairPlot == TRUE) {
      p = p + facet_wrap(~get(fill)) + theme(panel.spacing.y = unit(2, "lines"),  
                                             axis.text.x = element_text(size = 5),
                                             axis.text.y = element_text(size = 5))

      if(input$plotType == "Histogram"){
        p = p + geom_col(position = "dodge", color = "white", fill = "#00718f")

      }
      else if(input$plotType == "linePlot") {
        p = p + geom_line(group = 1, color = "#00718f") + theme(legend.position = "none")
        p = p + geom_point(group = 1, color = "#00718f", fill = "#00718f")

      }
      else if(input$plotType == "smoothPlot") {
        p = p + geom_smooth(colour = "#00718f", aes(group = get(fill)), se = FALSE, span = 0.5) + theme(legend.position = "none")

      }
      
      
    } else {
      if(input$plotType == "Histogram"){
        p = p + geom_col(position = "dodge", colour = "white")
        p = p + scale_fill_manual(values = cbPalette)
      }
      else if(input$plotType == "linePlot") {
        p = p + geom_line(aes(color = get(fill)), group = 1)
        p = p + geom_point(aes(color = get(fill)), group = 1)
        p = p + scale_colour_manual(values = cbPalette)
      }
      else if(input$plotType == "smoothPlot") {
        p = p + geom_smooth(aes(color = get(fill), group = get(fill)), se = FALSE, span = 0.5)
        p = p + scale_colour_manual(values = cbPalette)
      }
    }

    p = p + labs(fill = label, color = NULL)

    p %>%
      ggplotly() %>% config(modeBarButtons = list(list("toImage")))

  })

  ###############################
  #Ventile table output
  ###############################

  # Creates table
  ventileTable = reactive ({
    

    dataset = copy(dataUpload())

    ventiles = dataset[Income.Type == "Income Quantiles" & Population.Type == input$populationType & Description %in% show_groups()
                       & Income.Measure == income_sort() & file %in% data_version_full()]

    ventiles[, c("Index", "Income.Type", "file") := NULL]
    
    if (input$y_type == "Income") {
      ventiles[, c("Population", "Normalised") := NULL]
      if (length(input$chosen_file) == 1) {
        ventiles[, c("data_version", "year") := NULL]
        colnames(ventiles) = c("Ventile", "Population Type", "Population Subgroup", "Income Type", paste0("Average ", input$populationType, " ", income_sort()))
      }
      else {
        colnames(ventiles) = c("Ventile", "Population Type", "Population Subgroup", "Income Type", paste0("Average ", input$populationType, " ", income_sort()), "Version", "Year")
        setcolorder(ventiles, c("Ventile", "Population Type", "Population Subgroup", "Income Type", "Version", "Year", paste0("Average ", input$populationType, " ", income_sort())))
      }
    }
    else {
      ventiles[, c("Value") := NULL]
      if (length(input$chosen_file) == 1) {
        ventiles[, c("data_version", "year", "Normalised") := NULL]
        colnames(ventiles) = c("Ventile", "Population Type", "Population Subgroup", "Income Type", "Population Value")
      }
      else if (input$normalised) {
        ventiles[, c("Population") := NULL]
        ventiles[Normalised != "S", Normalised := round(as.numeric(Normalised))]
        colnames(ventiles) = c("Ventile", "Population Type", "Population Subgroup", "Income Type", "Version", "Year", "Normalised Population Value")
      }
      else {
        ventiles[, c("Normalised") := NULL]
        colnames(ventiles) = c("Ventile", "Population Type", "Population Subgroup", "Income Type", "Population Value", "Version", "Year")
        setcolorder(ventiles, c("Ventile", "Population Type", "Population Subgroup", "Income Type", "Version", "Year", "Population Value"))
      }
    }
    return(ventiles)

  })

  #Displays table
  output$ventileTable = renderDataTable({
    
    
    datatable(ventileTable(), rownames = FALSE)
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
    if (input$y_type == "Income") {
      title_start <- "Average Incomes"
    }
    else{
      title_start <- "Population Distribution"
    }
    paste(title_start, "by", input$populationType, income_sort(), "Ventiles", sep = " ")
  })

  output$ventileDataSubTitle = renderText({
    if(length(input$chosen_file) > 1){
      show_groups()
    }
    else {
      paste0("Tax Year 20", data_year(), " from ", data_version())
    }
  })

  ################################
  #Income band table output
  ################################

  # Creates table
  incomeBandTable = reactive ({
    

    dataset = copy(dataUpload())

    incomeBand = dataset[Income.Type == "Income Bands" & Population.Type == input$populationType & Description %in% show_groups()
                         & Income.Measure == income_sort() & file %in% data_version_full()]

    incomeBand[, c("Index", "Income.Type", "file") := NULL]
    
    if (input$y_type == "Income") {
      incomeBand[, c("Population", "Normalised") := NULL]
      if (length(input$chosen_file) == 1) {
        incomeBand[, c("data_version", "year") := NULL]
        colnames(incomeBand) = c("Income Band", "Population Type", "Population Subgroup", "Income Type", paste0("Average ", input$populationType, " ", income_sort()))
      }
      else {
        colnames(incomeBand) = c("Income Band", "Population Type", "Population Subgroup", "Income Type", paste0("Average ", input$populationType, " ", income_sort()), "Version", "Year")
        setcolorder(incomeBand, c("Income Band", "Population Type", "Population Subgroup", "Income Type", "Version", "Year", paste0("Average ", input$populationType, " ", income_sort())))
      }
    }
    else{
      incomeBand[, c("Value") := NULL] 
      if (length(input$chosen_file) == 1) {
        incomeBand[, c("data_version", "year", "Normalised") := NULL]
        colnames(incomeBand) = c("Income Band", "Population Type", "Population Subgroup", "Income Type", "Population Value")
      }
      else if (input$normalised) {
        incomeBand[, c("Population") := NULL]
        incomeBand[Normalised != "S", Normalised := round(as.numeric(Normalised))]
        colnames(incomeBand) = c("Income Band", "Population Type", "Population Subgroup", "Income Type", "Version", "Year", "Normalised Population Value")
        
      }
      else {
        incomeBand[, c("Normalised") := NULL]
        colnames(incomeBand) = c("Income Band", "Population Type", "Population Subgroup", "Income Type", "Population Value", "Version", "Year")
        setcolorder(incomeBand, c("Income Band", "Population Type", "Population Subgroup", "Income Type", "Version", "Year", "Population Value"))
      }
    }
    
    return(incomeBand)
  }
  )

  # Displays table
  output$incomeBandTable = renderDataTable({
    
    
    datatable(incomeBandTable(), rownames = FALSE)
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
    if (input$y_type == "Income") {
      title_start <- "Average Incomes"
    }
    else{
      title_start <- "Population Distribution"
    }
    paste(title_start, "by", input$populationType, income_sort(), "Bands", sep = " ")
  })

  output$incomeBandDataSubTitle = renderText({
    
    if(length(input$chosen_file) > 1){
      show_groups()
    }
    else {
    paste0("Tax Year 20", data_year(), " from ", data_version())
    }
  })
}

shinyApp(ui, server)