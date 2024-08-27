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

HH_pop_groups <- c(
    "All households", "Aged 0-15", 'Aged 16-35', 'Aged 36-50', 'Aged 51-64', 'Aged 16-64', "Aged 65+", "Single with children", 
    "Single without children", "Couple with children", "Couple without children", 
    "Multiple families with children", "Multiple families without children", 
    "With children", "Without children", "Core Benefit recipients", "Core Benefit non-recipients",
    "Working for Families recipients", "Working for Families non-recipients", "Family Tax Credit recipients",
    "In-Work Tax Credit recipients", "BestStart recipients", "FamilyBoost recipients", "NZ Super recipients", "NZ Super non-recipients",
    "Accommodation Supplement recipients", "Accommodation Supplement non-recipients", "Winter Energy Payment recipients")

Fam_pop_groups <- c(
  "All families", "Aged 0-15", 'Aged 16-35', 'Aged 36-50', 'Aged 51-64', 'Aged 16-64', "Aged 65+", "Single with children", 
  "Single without children", "Couple with children", "Couple without children",
  "With children", "Without children", "Core Benefit recipients", "Core Benefit non-recipients",
  "Working for Families recipients", "Working for Families non-recipients", "Family Tax Credit recipients",
  "In-Work Tax Credit recipients", "BestStart recipients", "FamilyBoost recipients", "NZ Super recipients", "NZ Super non-recipients",
  "Accommodation Supplement recipients", "Winter Energy Payment recipients")

I_pop_groups <- c(
  "All individuals", "Aged 0-15", 'Aged 16-35', 'Aged 36-50', 'Aged 51-64', 'Aged 16-64', "Aged 65+", "Single (not living with partner)", 
  "Living with partner",
  "NZ Super recipients", "NZ Super non-recipients",
  "Accommodation Supplement recipients", "Accommodation Supplement non-recipients", "Winter Energy Payment recipients")

HH_IC <- c(
  "Wage/Salary Income", "Income Tax", "ACC Levy", "Core Benefits", "Self-Employment Income", "WFF", "FTC", "IWTC", "BestStart", "FamilyBoost", "NZ Super", "Accommodation Supplement", "WEP", "Housing Costs")

Fam_IC <- c(
  "Wage/Salary Income", "Income Tax", "ACC Levy", "Core Benefits", "Self-Employment Income", "WFF", "FTC", "IWTC", "BestStart", "FamilyBoost", "NZ Super", "Accommodation Supplement", "WEP")

I_IC <- c(
  "Wage/Salary Income", "Income Tax", "ACC Levy", "Core Benefits", "Self-Employment Income", "WFF", "NZ Super", "Accommodation Supplement", "WEP")

# Color blind palette for plotting
cbPalette <- c("#00718f", "#E69F00",  "#009E73", "#F0E442", "#56B4E9", "#D55E00", "#CC79A7", "#000000")
ui <- (
  navbarPage(
  windowTitle = "Distribution Explorer - The Treasury New Zealand",
  title = div(a(img(src = "logo.png", height = "30px"),
                href = "https://www.treasury.govt.nz/", target = "_blank", style = "text-decoration:none;"),
              "Distribution Explorer", 
              style = "float:left;"),
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
    sidebarLayout(
      sidebarPanel(
        "Select Distribution Type:",
        radioGroupButtons(
          inputId = "y_type",
          label = NULL,
          choices = c("Population", "Income", "Income Components"),
          selected = "Population",
          checkIcon = list(yes = icon("ok", 
                                      lib = "glyphicon")),
          justified = TRUE,
          direction = "vertical",
          width = '100%'
        ),
        
        "Select Tax Year(s):",
        fluidRow(
          column(10,
            #For selecting the tax year from a given HES/EFU combination
            conditionalPanel(
              condition = "input.y_type != 'Income Components'",
              pickerInput(
                inputId ="chosen_file", 
                label = NULL, 
                choices = data_versions,
                selected = data_versions[[1]][1],
                multiple = TRUE,
                options = list(size = 5, 
                               `live-search` = TRUE)
              )),
            
            conditionalPanel(
              condition = "input.y_type == 'Income Components'",
              pickerInput(
                inputId ="chosen_file_components", 
                label = NULL, 
                choices = data_versions,
                selected = data_versions[[1]][1],
                multiple = FALSE,
                options = list(size = 5, 
                               `live-search` = TRUE)
              ),
              p(em("*When income components is selected, only a single tax year can be chosen."), style = "margin-top: -10px; margin-bottom: 7px;")
              )),
          column(2,
                 fileInputButton(
                   "user_file",
                   buttonLabel = NULL,
                   icon = icon("upload", lib = "glyphicon"),
                   multiple = TRUE,
                   accept = c(".csv")
                 )
                 )
        ),
        
        # For selecting the population type, HH or Fam
        "Select Population Unit:",
        radioGroupButtons(
          inputId = "populationType",
          label = NULL,
          choices = c(Households = 'Household',
                      Families = "Family",
                      Individuals = "Individual"),
          justified = TRUE,
          direction = "vertical",
          checkIcon = list(yes = icon("ok", 
                                      lib = "glyphicon")),
          width = '100%'),

        
        # When households is selected
        conditionalPanel(
          condition = "input.populationType == 'Household' && input.chosen_file.length < 2 && input.y_type != 'Income Components'",
          "Select Population Subgroup(s):",
          tooltip(
            bsicons::bs_icon("question-circle"),
            "Household is in a subgroup when at least one individual in the household is: of selected age | in selected family type | receiving selected benefit",
            placement = "right"
          ),
          pickerInput(
            inputId = "show_groups_HH",
            label = NULL,
            choices = HH_pop_groups,
            selected = HH_pop_groups[1],
            options = list(size = 5, 
                           `live-search` = TRUE),
            multiple = TRUE)
        ),
        
        # When family is selected
        conditionalPanel(
          condition = "input.populationType == 'Family' && input.chosen_file.length < 2 && input.y_type != 'Income Components'",
          "Select Population Subgroup(s):",
          tooltip(
            bsicons::bs_icon("question-circle"),
            "Family is in a subgroup when at least one individual in the family is: of selected age | in selected family type | receiving selected benefit",
            placement = "right"
          ),
          pickerInput(
            inputId = "show_groups_Fam",
            label = NULL,         
            choices = Fam_pop_groups,
            selected = Fam_pop_groups[1],
            options = list(size = 5, 
                           `live-search` = TRUE,
                           "max-options" = 8),
            multiple = TRUE)
          
        ),
        
        # When individual is selected
        conditionalPanel(
          condition = "input.populationType == 'Individual' && input.chosen_file.length < 2 && input.y_type != 'Income Components'",
          "Select Population Subgroup(s):",
          tooltip(
            bsicons::bs_icon("question-circle"),
            "Individual is in a subgroup when they are: of selected age | receiving selected benefit",
            placement = "right"
          ),
          pickerInput(
            inputId = "show_groups_I",
            label = NULL,         
            choices = I_pop_groups,
            selected = I_pop_groups[1],
            options = list(size = 5, 
                           `live-search` = TRUE,
                           "max-options" = 8),
            multiple = TRUE)
          
        ),
        
        #disable multiple selection when multiple years selected
        
        # When households is selected
        conditionalPanel(
          condition = "input.populationType == 'Household' && (input.chosen_file.length > 1 && input.y_type != 'Income Components')",
          "Select Population Subgroup(s):",
          tooltip(
            bsicons::bs_icon("question-circle"),
            "Household is in a subgroup when at least one individual in the household is: of selected age | in selected family type | receiving selected benefit",
            placement = "right"
          ),
          pickerInput(
            inputId = "show_groups_HH_multi",
            label = NULL,
            choices = HH_pop_groups,
            selected = HH_pop_groups[1],
            options = list(size = 5, 
                           `live-search` = TRUE)),
          p(em("*When multiple years are selected only a single subgroup can be chosen."), style = "margin-top: -10px; margin-bottom: 7px;")
        ),
        
        # When family is selected
        conditionalPanel(
          condition = "input.populationType == 'Family' && (input.chosen_file.length > 1 && input.y_type != 'Income Components')",
          "Select Population Subgroup(s):",
          tooltip(
            bsicons::bs_icon("question-circle"),
            "Family is in a subgroup when at least one individual in the family is: of selected age | in selected family type | receiving selected benefit",
            placement = "right"
          ),
          pickerInput(
            inputId = "show_groups_Fam_multi",
            label = NULL,         
            choices = Fam_pop_groups,
            selected = Fam_pop_groups[1],
            options = list(size = 5, 
                           `live-search` = TRUE)),
          p(em("*When multiple years are selected only a single subgroup can be chosen."), style = "margin-top: -10px; margin-bottom: 7px;"),
          
        ),
        
        # When individual is selected
        conditionalPanel(
          condition = "input.populationType == 'Individual' && (input.chosen_file.length > 1 && input.y_type != 'Income Components')",
          "Select Population Subgroup(s):",
          tooltip(
            bsicons::bs_icon("question-circle"),
            "Individual is in a subgroup when they are: of selected age | receiving selected benefit",
            placement = "right"
          ),
          pickerInput(
            inputId = "show_groups_I_multi",
            label = NULL,         
            choices = I_pop_groups,
            selected = I_pop_groups[1],
            options = list(size = 5, 
                           `live-search` = TRUE)),
          p(em("*When multiple years are selected only a single subgroup can be chosen."), style = "margin-top: -10px; margin-bottom: 7px;"),
          
        ),
        
        #show reduced subgroups in IC mode
        
        # When households is selected
        conditionalPanel(
          condition = "input.populationType == 'Household' && input.y_type == 'Income Components'",
          "Select Population Subgroup(s):",
          tooltip(
            bsicons::bs_icon("question-circle"),
            "Household is in a subgroup when at least one individual in the household is: of selected age | in selected family type | receiving selected benefit",
            placement = "right"
          ),
          pickerInput(
            inputId = "show_groups_HH_IC",
            label = NULL,
            choices = HH_pop_groups,
            selected = HH_pop_groups[1],
            options = list(size = 5, 
                           `live-search` = TRUE),
            multiple = FALSE),
          p(em("*When income components is selected, only a single population subgroup can be chosen."), style = "margin-top: -10px; margin-bottom: 7px;")
        ),
        
        # When family is selected
        conditionalPanel(
          condition = "input.populationType == 'Family' && input.y_type == 'Income Components'",
          "Select Population Subgroup(s):",
          tooltip(
            bsicons::bs_icon("question-circle"),
            "Family is in a subgroup when at least one individual in the family is: of selected age | in selected family type | receiving selected benefit",
            placement = "right"
          ),
          pickerInput(
            inputId = "show_groups_Fam_IC",
            label = NULL,         
            choices = Fam_pop_groups,
            selected = Fam_pop_groups[1],
            options = list(size = 5, 
                           `live-search` = TRUE,
                           "max-options" = 8),
            multiple = FALSE),
          p(em("*When income components is selected, only a single population subgroup can be chosen."), style = "margin-top: -10px; margin-bottom: 7px;")
          
        ),
        
        # When individual is selected
        conditionalPanel(
          condition = "input.populationType == 'Individual' && input.y_type == 'Income Components'",
          "Select Population Subgroup(s):",
          tooltip(
            bsicons::bs_icon("question-circle"),
            "Individual is in a subgroup when they are: of selected age | receiving selected benefit",
            placement = "right"
          ),
          pickerInput(
            inputId = "show_groups_I_IC",
            label = NULL,         
            choices = I_pop_groups,
            selected = I_pop_groups[1],
            options = list(size = 5, 
                           `live-search` = TRUE,
                           "max-options" = 8),
            multiple = FALSE),
          p(em("*When income components is selected, only a single population subgroup can be chosen."), style = "margin-top: -10px; margin-bottom: 7px;")
          
        ),
        
        #title of select income sort
        conditionalPanel(
          condition = "input.y_type != 'Income Components'",
          "Select Income Type:"
        ),
        
        
        # To select income sort
        conditionalPanel(
          condition = "input.populationType == 'Household' && input.y_type != 'Income Components'",
          radioGroupButtons(
            inputId = "incomeSortHH",
            label = NULL,
            choices = c(`Equivalised Disposable Income` = "Equivalised Disposable Income",
                        `Taxable Income` = "Taxable Income",
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
          condition = "input.populationType == 'Family' && input.y_type != 'Income Components'",
          radioGroupButtons(
            inputId = "incomeSortFam",
            label = NULL,
            choices = c(`Equivalised Disposable Income` = "Equivalised Disposable Income",
                        `Taxable Income` = "Taxable Income",
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
        
        conditionalPanel(
          condition = "input.populationType == 'Individual' && input.y_type != 'Income Components'",
          radioGroupButtons(
            inputId = "incomeSortI",
            label = NULL,
            choices = c(`Taxable Income` = "Taxable Income",
                        `Disposable Income` = "Disposable Income"
            ),
            selected = "Taxable Income",
            justified = TRUE,
            direction = 'vertical',
            checkIcon = list(
              yes = icon("ok", 
                         lib = "glyphicon")),
            width = '100%')
        ),
        
        # To select plot type
        conditionalPanel(
          condition = "input.y_type != 'Income Components'", 
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
          p(textOutput("suppressedCheck"), style = "margin-top: -10px; margin-bottom: 7px;")
        ),
        
        conditionalPanel(
          condition = "input.y_type != 'Income Components'",
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
            status = "primary")
        ),
        
        conditionalPanel(
          condition = "(input.chosen_file.length > 1 && input.y_type != 'Income Components') && input.y_type == 'Population'",
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
        ),
        
        # When households is selected
        conditionalPanel(
          condition = "input.populationType == 'Household' && input.y_type == 'Income Components'",
          "Select Income Component(s):",
          pickerInput(
            inputId = "inc_comp_HH",
            label = NULL,
            choices = HH_IC,
            selected = HH_IC[1],
            options = list(size = 5, 
                           `live-search` = TRUE),
            multiple = TRUE)
        ),
        
        # When family is selected
        conditionalPanel(
          condition = "input.populationType == 'Family' && input.y_type == 'Income Components'",
          "Select Income Component(s):",
          pickerInput(
            inputId = "inc_comp_Fam",
            label = NULL,         
            choices = Fam_IC,
            selected = Fam_IC[1],
            options = list(size = 5, 
                           `live-search` = TRUE,
                           "max-options" = 8),
            multiple = TRUE)
          
        ),
        
        #select income components
        
        # When individual is selected
        conditionalPanel(
          condition = "input.populationType == 'Individual' && input.y_type == 'Income Components'",
          "Select Income Component(s):",
          pickerInput(
            inputId = "inc_comp_I",
            label = NULL,         
            choices = I_IC,
            selected = I_IC[1],
            options = list(size = 5, 
                           `live-search` = TRUE,
                           "max-options" = 8), 
            multiple = TRUE)
          
        ),
        conditionalPanel(
          condition = "input.y_type == 'Income Components'",
          p(em("*Income components represent the average amount per population unit in each quantile/band, and are rounded to the nearest $1000. Thus, if the amount received on average by population units in each ventile/income band is too small, the income component will not be displayed. It may be useful to select a more targeted population subgroup to explore the contribution of this component to incomes."))
        )
      ),
      
      mainPanel(
        tabsetPanel(
          id = "tabChange",
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
            conditionalPanel(
              condition = "input.y_type != 'Income Components'",
              # Ventile table
              hr(),
              h6(strong(textOutput("ventileDataTitle")), align = "center"),
              h6(textOutput("ventileDataSubTitle"), align = "center"),
              hr(),
              DT::dataTableOutput("ventileTable"),
              br(),
  
              # Income Band table
              hr(),
              h6(strong(textOutput("incomeBandDataTitle")), align = "center"),
              h6(textOutput("incomeBandDataSubTitle"), align = "center"),
              hr(),
              DT::dataTableOutput("incomeBandTable"),
              br(),
              hr(),
              fluidRow(
                column(12,
                   actionButton("downloadData",
                                "Download Tables" )
                  ),
                align = "center"
              ),
              hr()
            ),
            conditionalPanel(
              condition = "input.y_type == 'Income Components'",
              hr(),
              p("Tables not available in Income Components mode."),
              hr()
            )
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
           p("The Distribution Explorer is a tool which can be used to understand the income distribution of households, families, and individuals in New Zealand."),
           p("The tool allows users to explore the income distribution for population subgroups, as well as compare income distributions between subgroups or over time. The population can be sorted into subgroups based on factors including age, family structure and relationship status, as well as government transfer status."),
           p("The population and income distributions in this tool are modelled using the Treasury’s Tax and Welfare Analysis (TAWA) model. TAWA uses survey and administrative data, policy settings, and Treasury economic projections to model incomes in New Zealand. More information on the TAWA model can be found on ", a("the Treasury website.",
             href="https://www.treasury.govt.nz/information-and-services/financial-management-and-advice/revenue-expenditure/tax-and-welfare-analysis-tawa-model", target="_blank")),
           h5(strong("Instructions")),
           p(strong(" 1)"), "Maximise the browser window to full screen."),
           p(strong(" 2)"), "Select Distribution Type: Choose whether you would like to investigate population, income, or income component distributions."),
           p(strong(" 3a)"), "Select Tax Years: Using the drop down menu, choose the desired tax year or years. There may be multiple data sources available for a single tax year. Only a single year can be investigated at a time in income components mode."),
           p(strong(" 3b)"), "Upload Data Files: Using the file upload button, you may also choose to upload unique TAWA outputs at this point. This can be useful to compare policy changes to the status quo. Acceptable files are Distribution Explorer outputs that have been converted to .csv files using the convert_IDI_to_user_uploads.R script. Once uploaded, select the desired Tax Years/Scenarios from the drop down menu. Ensure that each uploaded file has a unique and descriptive name."),
           p(strong(" 4)"), "Select Population Unit: Either Households, Families, or Individuals."),
           p(strong(" 5)"), "Select Population Subgroups: Using the drop down menu, select the desired population subgroups. When multiple years or income components mode are chosen, then only a single subgroup can be selected. The all households/families/individuals option captures the entire population. Up to 8 subgroups can be selected at a time, however we suggest that no more than 4 subgroups are selected otherwise the plots become difficult to interpret."),
           p(strong(" 6)"), "Select Income Type: Either Equivalised Disposable Income, Taxable Income, or Disposable Income.  When the selected population unit is households, After Housing Cost Disposable income can be selected. Individuals do not have an Equivalised Disposable Income option. In income components mode, households and families are always categorized by Equivalised Disposable Income, while individuals are always categorized by Disposable Income."),
           p(strong(" 7)"), "Select Income Components: If using income components mode, select which components of income/expenditure you would like to be displayed. Please note that some components may overlap with other components (e.g. FTC is a component of WFF, and thus choosing WFF and FTC at the same time will give a misleading impression of total WFF income). See Definitions for more information about each income component."),
           p(strong(" 8)"), "Select Plot Type: Either Histogram, Line Plot or Smooth Line Plot. The Smooth Line Plot is not compatible with suppressed data points. Only Histograms are available in income components mode."),
           p(strong(" 9)"), "Choose if you would like plots displayed as a Pair Plot. This is useful when multiple subgroups or multiple years are selected. This mode is not available in income components mode."),
           p(strong("10)"), "If looking at population distributions over multiple years, choose if you would like to normalise population values by the lowest population year (as determined by household population). This can make comparing population distributions across years more straightforward."),

           p(strong("Income Distribution Plots")),
           p("This tab displays the desired distribution both over income ventiles (upper panel) and over income bands (lower panel). See Definitions for more detail on the income boundaries used in this tool."),
           p("To download a plot, hover the mouse over the plot which will cause an icon to appear in the top right corner of the plot. Click on the camera icon to download a plot. Note that in pair plot mode, the x-axis title is located outside of the plot and thus will not be downloaded."),
           
           p(strong("Income Distribution Tables")),
           p("This tab displays the data which is used to create the ventile and income band plots. A Download Table button is available to save the data in both tables as a CSV file."),
           p("Note that tables are not available for income components mode."),
           
           p(strong("Suppression")),
           p("Note if there is an 'S' in a table or graph, this means the underlying population on which the data point is calculated is too small to be released from the IDI. This value has thus been suppressed."),
           
           h5(strong("Disclaimers")),
           p("The Distribution Explorer has been developed by the Analytics & Insights team in the New Zealand Treasury's Office of the Chief Economic Adviser."),
           p("The app provides insights into the income distributions of households, families, and individuals in New Zealand. The app is provided as-is and for research purposes only. Despite reasonable measures taken to ensure quality and accuracy, the Treasury makes no warranty, or guarantee, express or implied, nor assumes any legal liability or responsibility for the accuracy, correctness, completeness or use of any information that is provided through the app."),
           p("These results are not official statistics. They have been created for research purposes from the Integrated Data Infrastructure (IDI) which is carefully managed by Stats NZ. For more information about the IDI please visit ", 
             a("https://www.stats.govt.nz/integrated-data/",
               href="https://www.stats.govt.nz/integrated-data/", target="_blank"), ". The results are based in part on tax data supplied by Inland Revenue to Stats NZ under the Tax Administration Act 1994 for statistical purposes. Any discussion of data limitations or weaknesses is in the context of using the IDI for statistical purposes, and is not related to the data’s ability to support Inland Revenue’s core operational requirements."), 
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
           
           p(strong("Household Equivalised Disposable Income: "), "is calculated by dividing the total household disposable income by an equivalisation factor. TAWA uses the 'modified OECD' equivalence scale to estimate the equivalisation factor and applies a weight of 1.0 to the first adult in the household, 0.5 to other household members aged 14 and over, and 0.3 to those aged less than 14. A 'family' equivalised disposable income is also provided but results should be treated with caution as this calculation is not used in regular TAWA analysis or advice. This measure is not available for individuals."), 
           
           p(strong("Taxable Income: "),"the sum of all core benefits (JSS, SLP, SPS), superannuation, student allowance, other taxable benefits, wage and salary income, redundancy income, self-employment income and other taxable income",strong("before"),"tax and any other deductions are removed. Note that negative income amounts are included when calculating taxable income though total taxable income is defined as greater than or equal to zero."),
           
           p(strong("Disposable Income: "), "total income less tax and other deductions such as the ACC levy. Here, total income is the sum of taxable income (see above), abated accommodation supplement, winter energy payment, other non-taxable benefits, non-taxable income, and working for family tax credits. Note negative incomes are included."),
           
           p(strong("After Housing Cost Disposable Income: "), "disposable income less housing costs. Only available for households as housing costs are reported at the household level."),
           
           
           br(),
           
           h5(strong("Population subgroups")),
           
           p("Not all of the population subgroups listed are available for every population unit."),
           p(" "),
           p("In the population subgroups below, a child is defined using the Working for Family definition of a ‘dependent child’."),
           p(" "),
           p("For a selected population unit, i.e. household, family, or individual,"),
           
           tags$ul(
             tags$li(strong("Aged 0-15"),"indicates there is a least one individual aged 0-15 in the ‘population unit’."),
             tags$li(strong("Aged 16-35"),"indicates there is a least one individual aged 16-35 in the ‘population unit’."),
             tags$li(strong("Aged 36-50"),"indicates there is a least one individual aged 36-50 in the ‘population unit’."),
             tags$li(strong("Aged 51-64"),"indicates there is a least one individual aged 51-64 in the ‘population unit’."),
             tags$li(strong("Aged 16-64"),"indicates there is a least one individual aged 16-64 in the ‘population unit’."),
             tags$li(strong("Aged 65+"),"indicates there is a least one individual aged 16-64 in the ‘population unit’.")),
           
           p("Note that the following household/family structure groups are not available for individuals,"),
           tags$ul(
             tags$li(strong("Single with children"),"indicates that there is a single adult (an adult not living with a partner) with children in the ‘population unit’."),
             tags$li(strong("Single without children"),"indicates that there is a single adult without children in the ‘population unit’."),
             tags$li(strong("Couple with children"),"indicates that there is a couple with children in the ‘population unit’."),
             tags$li(strong("Couple without children"),"indicates that there is a couple without in the ‘population unit’."),
             tags$li(strong("Multiple families with children"),"indicates that there are multiple families in the household with at least one family having at least one child. Households only."),
             tags$li(strong("Multiple families without children"),"indicates that there are multiple families in the household with no children. Households only."),
             tags$li(strong("With children"),"indicates that there are children in the ‘population unit’."),
             tags$li(strong("Without children"),"indicates that there are no children in the ‘population unit’.")),
           
           p("The following subgroups are only available for individuals,"),
           tags$ul(
             tags$li(strong("Single (not living with partner)"),"indicates that an individual is single, or is in a relationship but not living in a household with their partner. Individuals only."),
             tags$li(strong("Living with partner"),"indicates that an individual is living in a household with their partner. Individuals only.")),
           

           tags$ul(
             tags$li(strong("Accommodation Supplement recipients"),"indicates there is at least one individual receiving an Accommodation Supplement payment in the ‘population unit’."),
             tags$li(strong("Accommodation Supplement non-recipients"),"indicates that no individuals receiving an Accommodation Supplement payment are in the ‘population unit’."),
             
             tags$li(strong("Core Benefit recipients"),"indicates there is at least one individual receiving a core benefit (JSS, SLP, or SPS) in the ‘population unit’."),
             tags$li(strong("Core Benefit non-recipients"),"indicates that no individuals receiving a core benefit (JSS, SLP, or SPS) are in the ‘population unit’."),
             
             tags$li(strong("NZ Super recipients"),"indicates there is at least one individual receiving NZ Superannuation in the ‘population unit’."),
             tags$li(strong("NZ Super non-recipients"),"indicates there are no individuals receiving NZ Superannuation in the ‘population unit’."),
             
             tags$li(strong("Winter Energy Payment recipients"),"indicates there is at least one individual receiving a Winter Energy Payment in the ‘population unit’.")),
            
           p("Note that the following Working for Families benefits are received at the family level and thus are not available for individuals."),
           tags$ul(  
             tags$li(strong("Working for Families recipients"),"indicates there is at least one individual receiving Working for Families in the ‘population unit’."),
             tags$li(strong("Working for Families non-recipients"),"indicates there are no individuals receiving Working for Families in the ‘population unit’."),
             tags$li(strong("Family Tax Credit recipients"),"indicates there is at least one individual receiving a Family Tax Credit in the ‘population unit’."),
             tags$li(strong("In-Work Tax Credit recipients"),"indicates there is at least one individual receiving a In-Work Tax Credit in the ‘population unit’."),
             tags$li(strong("BestStart recipients"),"indicates there is at least one individual receiving a BestStart in the ‘population unit’."),
             tags$li(strong("FamilyBoost recipients"),"indicates there is at least one individual receiving a FamilyBoost in the ‘population unit’.")),
           
           br(),
           
           h5(strong("Income components")),
           
           p("Not all of the income components listed are available for every population unit. For example, housing costs are only available over households, and WFF tax credits are not available over individuals."),
           
           p("For a selected population unit, "),
           
           tags$ul(
             tags$li(strong("Housing Costs"),"indicates the average total of housing costs paid by the 'population unit'"),
             tags$li(strong("Wage/Salary Income"),"indicates the average total of wage and salary income that is earned by the ‘population unit’"),
             tags$li(strong("Income Tax"),"indicates the average total of income tax that is paid by the ‘population unit’"),
             tags$li(strong("Core Benefits"),"indicates the average total of core benefits (e.g. JSS, SLP, and SPS) received by the ‘population unit’"),
             tags$li(strong("Self-Employment Income"),"indicates the average total of self-employment income that is earned by the ‘population unit’"),
             tags$li(strong("WFF"),"indicates the average total of Working For Families tax credits received by the ‘population unit’. Please note that this includes the totals of FTC, MFTC, IWTC, and Best Start which are also listed separately as income components."),
             tags$li(strong("FTC"),"indicates the average total of Family Tax Credit that is received by the ‘population unit’. Note this is included in the WFF total."),
             tags$li(strong("MFTC"),"indicates the average total of Minimum Family Tax Credit that is received by the ‘population unit’. Note this is included in the WFF total."),
             tags$li(strong("IWTC"),"indicates the average total of In-Work Tax Credit that is received by the ‘population unit’. Note this is included in the WFF total."),
             tags$li(strong("BestStart"),"indicates the average total of BestStart Tax Credit that is received by the ‘population unit’. Note this is included in the WFF total."),
             tags$li(strong("FamilyBoost"),"indicates the average total of FamilyBoost rebate that is received by the ‘population unit’. Note this is included in the WFF total."),
             tags$li(strong("NZ Super"),"indicates the average total of New Zealand Superannuation that is received by the ‘population unit’"),
             tags$li(strong("Accommodation Supplement"),"indicates the average total of Accommodation Supplement that is received by the ‘population unit’"),
             tags$li(strong("WEP"),"indicates the average total of Winter Energy Payment that is received by the ‘population unit’")),
           
           br(),
           
           h5(strong("Other")),
           p(strong("Ventiles: "), "the overall population is separated into 20 equal groups based on the income selected, e.g. disposable income. The corresponding income bands (not currently provided) will be of different sizes with thinner income bands where the population is larger and wider income bands where the population is lower."),
           p(strong("Fixed income bands: "), "household, families, and individuals are assigned an income band based on the income selected, e.g. disposable income. Each income band is of equal width (except for the  first and last income band) and has a different population size.")
             ),
             column(1)
           )
  ),
  footer = div(
    style = "color:white;background-color:#00718f;margin:0;",
    div(class = "foot", style = "text-align:center",
    p(HTML('&emsp;'),
      "© 2024",
      HTML('&emsp;'),
    a("Analytics & Insights", href = "https://github.com/Treasury-Analytics-and-Insights/", target = "_blank", style=("text-decoration:none;")),
    HTML('&emsp;'),
    a("Te Tai Ōhanga - The Treasury New Zealand", href = "https://treasury.govt.nz/", target = "_blank", style=("text-decoration:none;")),
    HTML('&emsp;'),
    a("Source", href = "https://github.com/Treasury-Analytics-and-Insights/DistributionExplorer", target = "_blank", style=("text-decoration:none;")),
    HTML('&emsp;'),
    a("License", href = "https://github.com/Treasury-Analytics-and-Insights/DistributionExplorer/blob/main/LICENSE.md",  target = "_blank", style=("text-decoration:none;")),
    style="padding:2px;margin:0;")
    ),
    tags$head(
      tags$link(rel = "shortcut icon", 
                href = "favicon.ico"
      ),
      tags$style(HTML("
              .navbar {
                padding-left: 15px;
                padding-right: 15px;
                --bslib-navbar-default-bg: #00718F;
                  --bslib-navbar-inverse-bg: #FFFFFF;
                  --bs-navbar-color: rgba(255, 255, 255, 1);
                  --bs-navbar-hover-color: rgba(255, 255, 255, 0.8);
              }
                     "))
    ),
    tags$body(
      tags$style(HTML("
            .container-fluid {
      padding: 0;
      }
              .foot {
              --bs-link-color-rgb:255,255,255;
              --bs-link-hover-color-rgb:204,227,233;
              }
                     "))
    ),
  tags$script(HTML("
    function downloadCSV(csv, filename) {
      var csvFile;
      var downloadLink;
      
      // CSV file
      csvFile = new Blob([csv], {type: 'text/csv'});
      
      // Download link
      downloadLink = document.createElement('a');
      
      // File name
      downloadLink.download = filename;
      
      // Create a link to the file
      downloadLink.href = window.URL.createObjectURL(csvFile);
      
      // Hide download link
      downloadLink.style.display = 'none';
      
      // Add the link to DOM
      document.body.appendChild(downloadLink);
      
      // Click download link
      downloadLink.click();
    }

    function exportTableToCSV(filename) {
      var csv = [];
      var rows = document.querySelectorAll('table tr');
      
      for (var i = 0; i < rows.length; i++) {
        var row = [], cols = rows[i].querySelectorAll('td, th');
        
        for (var j = 0; j < cols.length; j++) 
          row.push(cols[j].innerText);
        
        csv.push(row.join(','));
      }

      // Download CSV
      downloadCSV(csv.join('\\n'), filename);
    }
    
    document.getElementById('downloadData').onclick = function () {
      exportTableToCSV('Distribution-Explorer-User-Tables-' + new Date().toISOString().slice(0,10) + '.csv');
    };
  ")))
  )
)



server <- function(input, output, session) {
  
  #increase allowed upload size
  options(shiny.maxRequestSize=500*1024^2)
  
  #stops normalised selection from preventing switch to income mode
  observeEvent(input$y_type %in% c("Income", "Income Components"), {
    updateMaterialSwitch(session, "normalised", value = FALSE)
  })
  
  showModal(modalDialog(
    p("The Distribution Explorer has been developed by the Analytics & Insights team in the New Zealand Treasury's Office of the Chief Economic Adviser."),
    p("The app provides insights into the income distributions of households, families, and individuals in New Zealand. The app is provided as-is and for research purposes only. Despite reasonable measures taken to ensure quality and accuracy, the Treasury makes no warranty, or guarantee, express or implied, nor assumes any legal liability or responsibility for the accuracy, correctness, completeness or use of any information that is provided through the app."),
    hr(),
    p("These results are not official statistics. They have been created for research purposes from the Integrated Data Infrastructure (IDI) 
          which is carefully managed by Stats NZ. For more information about the IDI please visit ", 
      a("https://www.stats.govt.nz/integrated-data/", href = "https://www.stats.govt.nz/integrated-data/", target="_blank"), 
      "."),
    p("The results are based in part on tax data supplied by Inland Revenue to Stats NZ under the Tax Administration Act 1994 for statistical purposes. 
          Any discussion of data limitations or weaknesses is in the context of using the IDI for statistical purposes, 
            and is not related to the data’s ability to support Inland Revenue’s core operational requirements."),
    title = "Disclaimer",
    size = "l",
    footer = actionButton("closeModal", "Acknowledge")
    
  ))
  
  observeEvent(input$closeModal, {
    removeModal()
  })
  
  refresh_trigger <- reactive({
    list(input$closeModal, input$tabChange)
  })
  
  #refreshes data so that all plots/graphs appear without input from user
  observeEvent(refresh_trigger(), {
    y_type <- input$y_type
    if (y_type != "Population") {
      updateRadioGroupButtons(session, "y_type", selected = "Population")
      updateRadioGroupButtons(session, "y_type", selected = y_type)
    }
    else {
      updateRadioGroupButtons(session, "y_type", selected = "Income")
      updateRadioGroupButtons(session, "y_type", selected = "Population")
    }
  })
  
  chosen_file = reactive({
    if (input$y_type == "Income Components") {
      return(input$chosen_file_components)
    }
    else {
      return(input$chosen_file)
    }
  })

  data_year = reactive({
    year_list <- NULL
    for (file in chosen_file()) {
      if (substr(file, 1, 5) == 'data/') {
        year_list <- c(year_list, get_year(file))
      }
      else {
        year_list <- c(year_list, NA)
      }
    }
    return(year_list)
  })


  data_version = reactive({
    version_list <- NULL
    for (file in chosen_file()) {
      if (substr(file, 1, 5) == 'data/') {
        version_list <- c(version_list, paste0(get_hes(file), ", ", get_efu(file)))
      }
      else {
        version_list <- c(version_list, file_lookup_reactive()[[file]])
      }
    }
    return(version_list)
  })
  
  data_version_full = reactive({
    full_version_list <- NULL
    for (file in chosen_file()) {
      if (substr(file, 1, 5) == 'data/') {
        full_version_list <- c(full_version_list, paste0("20", get_year(file), " (", get_hes(file), ", ", get_efu(file), ")"))
      }
      else {
        full_version_list <- c(full_version_list, file_lookup_reactive()[[file]])
      }
    }
    return(full_version_list)
  })

  income_sort = reactive({
    if (input$populationType == "Household") {
      if (input$y_type != "Income Components") {
        return(input$incomeSortHH)
      }
      else {
        return("Equivalised Disposable Income")
      }
    }
    else if (input$populationType == "Family") {
      if (input$y_type != "Income Components") {
        return(input$incomeSortFam)
      }
      else {
        return("Equivalised Disposable Income")
      }
    }
    else {
      if (input$y_type != "Income Components") {
        return(input$incomeSortI)
      }
      else {
        return("Disposable Income")
      }
    }
  })
  
  inc_comp = reactive({
    if (input$populationType == "Household") {
      return(input$inc_comp_HH)
    }
    else if (input$populationType == "Family") {
      return(input$inc_comp_Fam)
    }
    else {
      return(input$inc_comp_I)
    }
  })
  
  
  show_groups = reactive({
    if (input$y_type != 'Income Components') {
      if (input$populationType == "Household") {
        if(length(chosen_file()) > 1) {
          return(input$show_groups_HH_multi)
        }
        else {
          return(input$show_groups_HH)
        }
      }
      else if (input$populationType == "Family") {
        if(length(chosen_file()) > 1) {
          return(input$show_groups_Fam_multi)
        }
        else {
          return(input$show_groups_Fam)
        }
      }
      else {
        if(length(chosen_file()) > 1) {
          return(input$show_groups_I_multi)
        }
        else {
          return(input$show_groups_I)
        }
      }
    }
    else {
      if (input$populationType == "Household") {
        return(input$show_groups_HH_IC)
      }
      else if (input$populationType == "Family") {
        return(input$show_groups_Fam_IC)
      }
      else {
          return(input$show_groups_I_IC)
      }
    }
  }) 
  
  data_version_reactive <- reactiveVal(data_versions_lists())
  file_lookup_reactive <- reactiveVal(list())

  observeEvent(input$user_file, {
    new_files <- input$user_file
    lister_return <- file_lister(data_version_reactive(), file_lookup_reactive(), new_files)
    data_version_reactive(lister_return[[1]])
    file_lookup_reactive(lister_return[[2]])
    updatePickerInput(
      inputId ="chosen_file",
      choices = data_version_reactive(),
      selected = data_version_reactive()[["User Uploads"]][1]
    )
    updatePickerInput(
      inputId ="chosen_file_components",
      choices = data_version_reactive(),
      selected = data_version_reactive()[["User Uploads"]][1]
    )
  })

  dataUpload = reactive({
    dt <- data.table()
    for (file in chosen_file()) {
      dt_year = as.data.table(read.csv(file))
      if (substr(file, 1, 5) == 'data/') {
        dt_year[, data_version := paste0(get_hes(file), ", ", get_efu(file))]
        dt_year[, year := paste0("20", get_year(file))]
        dt_year[, file := paste0("20", get_year(file), " (", get_hes(file), ", ", get_efu(file), ")")]
      }
      else {
        dt_year[, data_version := file_lookup_reactive()[[file]]]
        dt_year[, year := "N/A"]
        dt_year[, file := file_lookup_reactive()[[file]]]
      }
      dt <- rbind(dt, dt_year)
    }
    dt <- norm_pop(dt)[[2]]
    return(dt)
  })
  
  
  output$normalised_text <- renderText(paste0("Normalise populations to ", norm_pop(dataUpload())[[1]], ":"))
  
  ###############################
  #Ventile plot
  ###############################
  output$ventilePlot = renderPlotly({
    
    if(length(show_groups()) == 0){
      stop("Please select a population subgroup.")
      return(NULL)
    }

    if(length(chosen_file()) == 0){
      stop("Please select a tax year.")
      return(NULL)
    }

    if(input$y_type == "Income Components" & length(inc_comp()) == 0){
      stop("Please select an income component.")
      return(NULL)
    }

    if(length(show_groups()) > 8){
      stop("Maximum selection is 8 population subgroups.")
      return(NULL)
    }

    if(length(chosen_file()) > 8){
      stop("Maximum selection is 8 tax years.")
      return(NULL)
    }

    if(length(inc_comp()) > 8){
      stop("Maximum selection is 8 income components.")
      return(NULL)
    }

    for (file in chosen_file()){
      file_split <- strsplit(file, "\\.")
      extension <- file_split[[1]][length(file_split[[1]])]
      if (extension != "csv") {
        stop("An uploaded file has the wrong file type (must be .csv)")
        return(NULL)
      }
    }
    
    if(length(chosen_file()) > 1){
      fill <- "file"
      label <- "Years"
      subtitle <- show_groups()
    }
    else if (input$y_type != "Income Components") {
      fill <- "Description"
      label <- "Subgroups"
      subtitle <- paste0(ifelse(!(is.na(data_year())), paste0("Tax Year 20", data_year(), " from "), ""), data_version())
    }
    else {
      fill <- "Value_Type"
      label <- "Income Components"
      subtitle <- paste0(ifelse(!(is.na(data_year())), paste0(show_groups(), ", Tax Year 20", data_year(), " from "), ""), data_version())
    }
    
    if(input$pairPlot) {
      x_label <- NULL
    }
    else {
      x_label <- "Ventiles"
    }
    
    dataset= copy(dataUpload())

    dataset[, Suppressed := ""]
    if(input$normalised) {
      pop_type <- "Normalised"
      dataset[Normalised == "S", Suppressed := "S"]
      dataset[Normalised == "S", Normalised := 0L]
      dataset[,Normalised := as.numeric(Normalised)]
      y_label <- "Normalised Population"
      title_start <- "Population Distribution"
    }
    else if (input$y_type == "Population") {
      pop_type <- "Population"
      dataset[Population == "S", Suppressed := "S"]
      dataset[Population == "S", Population := 0L]
      dataset[,Population := as.numeric(Population)]
      y_label <- "Population"
      title_start <- "Population Distribution"
    }
    else if (input$y_type == "Income") {
      pop_type <- "Value"
      dataset[Value == "S", Suppressed := "S"]
      dataset[Value == "S", Value := 0L]
      dataset[,Value := as.numeric(Value)]
      y_label <- paste0("Average ", input$populationType, " \n", income_sort())
      title_start <- "Average Incomes"
    }
    else {
      pop_type <- "Value"
      dataset[Value == "S", Suppressed := "S"]
      dataset[Value == "S", Value := 0L]
      dataset[,Value := as.numeric(Value)]
      y_label <-"Average Amount"
      title_start <- "Average Income Components"
    }
    
    nudge_distance_y <- pmax(0.1, dataset[Description %in% show_groups(), max(get(pop_type))] / 100)

    if (input$y_type != "Income Components") {
      ventiles = dataset[Income_Type == "Income Quantiles" & Population_Type == input$populationType & Description %in% show_groups()
                         & Income_Measure == income_sort() & Value_Type == income_sort()]
    }
    else {
      ventiles = dataset[Income_Type == "Income Quantiles" & Population_Type == input$populationType & Description %in% show_groups()
                         & Income_Measure == income_sort() & Value_Type %in% inc_comp()]
      ventiles[Value_Type %in% c("Income Tax", "ACC Levy", "Housing Costs"), Value := -1 * Value]
    }

    p = ggplot(ventiles, aes(x = as.numeric(Income_Group), y = get(pop_type), label = Suppressed, fill = get(fill), text = paste0("Ventile: ", as.numeric(Income_Group),
                                                                                                                                  "\n", y_label, ": ", ifelse(Suppressed == "S", "Suppressed", get(pop_type)),
                                                                                                                                  "\n", substr(label, 1, (nchar(label) - 1)),": ", get(fill)))) +
      labs(title = paste0(title_start, " by ", input$populationType, " ", income_sort(), " Ventiles", "\n", subtitle),
           x = x_label, y = y_label) + 
      scale_x_continuous(breaks = seq(1:20)) +
      scale_y_continuous(labels = comma,
                         expand = expansion(mult = c(0, 0.05))) +  # Includes 0 in y axis and has the top of graph 5% above the max value
      geom_hline(yintercept = 0) + 
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
        p = p + geom_text(aes(x = as.numeric(Income_Group), y = get(pop_type) + nudge_distance_y, group = get(fill)), color = "#00718f", position = position_dodge(width = 0.9)) + theme(legend.position = "none")

      }
      else if(input$plotType == "linePlot") {
        p = p + geom_line(group = 1, color = "#00718f") + theme(legend.position = "none")
        p = p + geom_point(group = 1, color = "#00718f", fill = "#00718f")
        p = p + geom_text(aes(x = as.numeric(Income_Group), y = get(pop_type) + nudge_distance_y, group = get(fill)), color = "#00718f", position = position_dodge(width = 0.9))

      }
      else if(input$plotType == "smoothPlot") {
        p = p + geom_smooth(colour = "#00718f", aes(group = get(fill)), se = FALSE, span = 0.5) + theme(legend.position = "none")

      }
    } else if (input$y_type != "Income Components") {
      if(input$plotType == "Histogram"){
        p = p + geom_col(position = "dodge", colour = "white")
        p = p + scale_fill_manual(values = cbPalette)
        p = p + geom_text(aes(x = as.numeric(Income_Group), y = get(pop_type)+ nudge_distance_y, group = get(fill), color = get(fill)), position = position_dodge(width = 0.9))
        p = p + scale_color_manual(values = cbPalette) + guides(color = "none")
      }
      else if(input$plotType == "linePlot") {
        p = p + geom_line(aes(color = get(fill)), group = 1)
        p = p + geom_point(aes(color = get(fill)), group = 1)
        p = p + geom_text(aes(x = as.numeric(Income_Group), y = get(pop_type)+ nudge_distance_y, group = get(fill), color = get(fill)), position = position_dodge(width = 0))
        p = p + scale_colour_manual(values = cbPalette) + guides(color = "none")
      }
      else if(input$plotType == "smoothPlot") {
        p = p + geom_smooth(aes(color = get(fill), group = get(fill)), se = FALSE, span = 0.5)
        p = p + scale_colour_manual(values = cbPalette)
      }
    }
    else {
      p = p + geom_col(colour = "white")
      p = p + scale_fill_manual(values = cbPalette)
      p = p + geom_text(aes(x = as.numeric(Income_Group), y = get(pop_type)+ nudge_distance_y, group = get(fill), color = get(fill)))
      p = p + scale_color_manual(values = cbPalette) + guides(color = "none")
    }

    p = p + labs(fill = label, color = NULL)

    p %>%
      ggplotly(tooltip = "text") %>% config(modeBarButtons = list(list("toImage")))
  })

  ################################
  #Income band plot
  ################################
  output$incomeBandPlot = renderPlotly({
    
    if(length(show_groups()) == 0){
      stop("Please select a population subgroup.")
      return(NULL)
    }

    if(length(chosen_file()) == 0){
      stop("Please select a tax year.")
      return(NULL)
    }

    if(input$y_type == "Income Components" & length(inc_comp()) == 0){
      stop("Please select an income component.")
      return(NULL)
    }

    if(length(show_groups()) > 8){
      stop("Maximum selection is 8 population subgroups.")
      return(NULL)
    }

    if(length(chosen_file()) > 8){
      stop("Maximum selection is 8 tax years.")
      return(NULL)
    }

    if(length(inc_comp()) > 8){
      stop("Maximum selection is 8 income components.")
      return(NULL)
    }

    for (file in chosen_file()){
      file_split <- strsplit(file, "\\.")
      extension <- file_split[[1]][length(file_split[[1]])]
      if (extension != "csv") {
        stop("An uploaded file has the wrong file type (must be .csv)")
        return(NULL)
      }
    }
    
    if(length(chosen_file()) > 1){
      fill <- "file"
      label <- "Years"
      subtitle <- show_groups()
    }
    else if (input$y_type != "Income Components"){
      fill <- "Description"
      label <- "Subgroups"
      subtitle <- paste0(ifelse(!(is.na(data_year())), paste0("Tax Year 20", data_year(), " from "), ""), data_version())
    }
    else {
      fill <- "Value_Type"
      label <- "Income Components"
      subtitle <- paste0(ifelse(!(is.na(data_year())), paste0(show_groups(), ", Tax Year 20", data_year(), " from "), ""), data_version())
    }
    
    if(input$pairPlot) {
      x_label <- NULL
    }
    else {
      x_label <- "Income Bands"
    }
    
    dataset= copy(dataUpload())
    
    dataset[, Suppressed := ""]
    
    if(input$normalised) {
      pop_type <- "Normalised"
      dataset[Normalised == "S", Suppressed := "S"]
      dataset[Normalised == "S", Normalised := 0L]
      dataset[,Normalised := as.numeric(Normalised)]
      y_label <- "Normalised Population"
      title_start <- "Population Distribution"
    }
    else if (input$y_type == "Population") {
      pop_type <- "Population"
      dataset[Population == "S", Suppressed := "S"]
      dataset[Population == "S", Population := 0L]
      dataset[,Population := as.numeric(Population)]
      y_label <- "Population"
      title_start <- "Population Distribution"
    }
    else if (input$y_type == "Income") {
      pop_type <- "Value"
      dataset[Value == "S", Suppressed := "S"]
      dataset[Value == "S", Value := 0L]
      dataset[,Value := as.numeric(Value)]
      y_label <- paste0("Average ", input$populationType, " \n", income_sort())
      title_start <- "Average Incomes"
    }
    else {
      pop_type <- "Value"
      dataset[Value == "S", Suppressed := "S"]
      dataset[Value == "S", Value := 0L]
      dataset[,Value := as.numeric(Value)]
      y_label <- paste0("Average Amoount")
      title_start <- "Average Income Components"
    }
    
    nudge_distance_y <- pmax(0.1, dataset[Description %in% show_groups(), max(get(pop_type))] / 50)
    
    if (input$y_type != "Income Components") {
      incomeBand = dataset[Income_Type == "Income Bands" & Population_Type == input$populationType & Description %in% show_groups()
                           & Income_Measure == income_sort() & Value_Type == income_sort()]
    }
    else {
      incomeBand = dataset[Income_Type == "Income Bands" & Population_Type == input$populationType & Description %in% show_groups()
                           & Income_Measure == income_sort() & Value_Type %in% inc_comp()]
      incomeBand[Value_Type %in% c("Income Tax", "Housing Costs"), Value := -1 * Value]
    }

    incomeBand[, Band := as.factor(Income_Group)]

    # Different income bands for different income types.
    if (income_sort() == "Equivalised Disposable Income"){
      incomeBand$Band = factor(incomeBand$Band, levels =
                                 c("Below $0", "$0-$10k", "$10k-$20k", "$20k-$30k", "$30k-$40k", "$40k-$50k", "$50k-$60k",
                                   "$60k-$70k", "$70k-$80k", "$80k-$90k", "$90k-$100k", "$100k-$110k", "$110k-$120k",
                                   "$120k-$130k", "$130k-$140k", "$140k-$150k", "Above $150k"))
    } else {
      incomeBand$Band = factor(incomeBand$Band, levels =
                                 c("Below $0", "$0-$20k", "$20k-$40k", "$40k-$60k", "$60k-$80k", "$80k-$100k", "$100k-$120k",
                                   "$120k-$140k", "$140k-$160k", "$160k-$180k", "$180k-$200k", "$200k-$220k", "$220k-$240k",
                                   "$240k-$260k", "$260k-$280k", "$280k-$300k", "Above $300k"))
    }

    p = ggplot(incomeBand, aes(x = Band, y=get(pop_type), label = Suppressed, fill=get(fill), text = paste0("Income Band: ", Band,
                                                                                                            "\n", y_label, ": ", ifelse(Suppressed == "S", "Suppressed", get(pop_type)),
                                                                                                            "\n", substr(label, 1, (nchar(label) - 1)),": ", get(fill)))) +
      labs(title = paste0(title_start, " by ", input$populationType, " ", income_sort(), " Bands", "\n", subtitle),
           x = x_label, y = y_label) + 
      scale_y_continuous(labels = comma, 
                         expand = expansion(mult = c(0, 0.05))) +  # Includes 0 in y axis and has the top of graph 5% above the max value
      geom_hline(yintercept = 0) + 
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
        p = p + geom_text(aes(x = Band, y = get(pop_type) + nudge_distance_y, group = get(fill)), color = "#00718f",position = position_dodge(width = 0.9)) + theme(legend.position = "none")

      }
      else if(input$plotType == "linePlot") {
        p = p + geom_line(group = 1, color = "#00718f") + theme(legend.position = "none")
        p = p + geom_point(group = 1, color = "#00718f", fill = "#00718f")
        p = p + geom_text(aes(x = Band, y = get(pop_type) + nudge_distance_y, group = get(fill)), color = "#00718f",position = position_dodge(width = 0.9))

      }
      else if(input$plotType == "smoothPlot") {
        p = p + geom_smooth(colour = "#00718f", aes(group = get(fill)), se = FALSE, span = 0.5) + theme(legend.position = "none")

      }
      
      
    } else if (input$y_type != "Income Components") {
      if(input$plotType == "Histogram"){
        p = p + geom_col(position = "dodge", colour = "white")
        p = p + scale_fill_manual(values = cbPalette)
        p = p + geom_text(aes(x = Band, y = get(pop_type) + nudge_distance_y, group = get(fill), color = get(fill)),position = position_dodge(width = 0.9))
        p = p + scale_color_manual(values = cbPalette) + guides(color = "none")
      }
      else if(input$plotType == "linePlot") {
        p = p + geom_line(aes(color = get(fill)), group = 1)
        p = p + geom_point(aes(color = get(fill)), group = 1)
        p = p + geom_text(aes(x = Band, y = get(pop_type) + nudge_distance_y, group = get(fill), color = get(fill)),position = position_dodge(width = 0))
        p = p + scale_colour_manual(values = cbPalette) + guides(color = "none")
      }
      else if(input$plotType == "smoothPlot") {
        p = p + geom_smooth(aes(color = get(fill), group = get(fill)), se = FALSE, span = 0.5)
        p = p + scale_colour_manual(values = cbPalette)
      }
    } else {
      p = p + geom_col(colour = "white")
      p = p + scale_fill_manual(values = cbPalette)
      p = p + geom_text(aes(x = Band, y = get(pop_type) + nudge_distance_y, group = get(fill), color = get(fill)))
      p = p + scale_color_manual(values = cbPalette) + guides(color = "none")
    }

    p = p + labs(fill = label, color = NULL)

    p %>%
      ggplotly(tooltip = "text") %>% config(modeBarButtons = list(list("toImage")))

  })
  ###############################
  #Check for suppressed output
  ###############################
  
  output$suppressedCheck = reactive ({
    dataset = copy(dataUpload())
    
    
    ds = dataset[Population_Type == input$populationType & Description %in% show_groups() & Income_Measure == income_sort()]
    if ("S" %in% ds[, Value]) {
      updateRadioGroupButtons(session, 
                              "plotType", 
                              choices = c(Histogram = "Histogram",
                                          `Line Plot` = "linePlot",
                                          `Smooth Line Plot` = "smoothPlot"), 
                              disabledChoices = c(`Smooth Line Plot` = "smoothPlot"),
                              checkIcon = list(yes = icon("ok", 
                                                          lib = "glyphicon")))
      return("*Smooth line plot cannot be used with suppressed values")
    }
    else {
      updateRadioGroupButtons(session, 
                              "plotType", 
                              choices = c(Histogram = "Histogram",
                                          `Line Plot` = "linePlot",
                                          `Smooth Line Plot` = "smoothPlot"), 
                              disabledChoices = NULL,
                              checkIcon = list(yes = icon("ok", 
                                                          lib = "glyphicon")))
      return("")
    }
  }) 

  ###############################
  #Ventile table output
  ###############################

  # Creates table
  ventileTable = reactive ({
    
    if(length(show_groups()) == 0){
      stop("Please select a population subgroup.")
      return(NULL)
    }

    if(length(chosen_file()) == 0){
      stop("Please select a tax year.")
      return(NULL)
    }

    if(input$y_type == "Income Components" & length(inc_comp()) == 0){
      stop("Please select an income component.")
      return(NULL)
    }

    if(length(show_groups()) > 8){
      stop("Maximum selection is 8 population subgroups.")
      return(NULL)
    }

    if(length(chosen_file()) > 8){
      stop("Maximum selection is 8 tax years.")
      return(NULL)
    }

    if(length(inc_comp()) > 8){
      stop("Maximum selection is 8 income components.")
      return(NULL)
    }

    for (file in chosen_file()){
      file_split <- strsplit(file, "\\.")
      extension <- file_split[[1]][length(file_split[[1]])]
      if (extension != "csv") {
        stop("An uploaded file has the wrong file type (must be .csv)")
        return(NULL)
      }
    }

    dataset = copy(dataUpload())
    
    ventiles = dataset[Income_Type == "Income Quantiles" & Population_Type == input$populationType & Description %in% show_groups()
                       & Income_Measure == income_sort() & file %in% data_version_full() & Value_Type == income_sort()]
    
    if (input$y_type == "Income") {
      if (length(chosen_file()) == 1) {
        ventiles <- ventiles[, .(Income_Group, Population_Type, Description, Income_Type, Value)]
        colnames(ventiles) = c("Ventile", "Population Type", "Population Subgroup", "Income Type", paste0("Average ", input$populationType, " ", income_sort()))
      }
      else {
        ventiles <- ventiles[, .(Income_Group, Population_Type, Description, Income_Type, data_version, year, Value)]
        colnames(ventiles) = c("Ventile", "Population Type", "Population Subgroup", "Income Type", "Version", "Year", paste0("Average ", input$populationType, " ", income_sort()))
      }
    }
    else {
      if (length(chosen_file()) == 1) {
        ventiles <- ventiles[, .(Income_Group, Population_Type, Description, Income_Type, Population)]
        colnames(ventiles) = c("Ventile", "Population Type", "Population Subgroup", "Income Type", "Population Value")
      }
      else if (input$normalised) {
        ventiles[Normalised != "S", Normalised := round(as.numeric(Normalised))]
        ventiles <- ventiles[, .(Income_Group, Population_Type, Description, Income_Type, data_version, year, Normalised)]
        colnames(ventiles) = c("Ventile", "Population Type", "Population Subgroup", "Income Type", "Version", "Year", "Normalised Population Value")
      }
      else {
        ventiles <- ventiles[, .(Income_Group, Population_Type, Description, Income_Type, data_version, year, Population)]
        colnames(ventiles) = c("Ventile", "Population Type", "Population Subgroup", "Income Type", "Version", "Year", "Population Value")
      }
    }
    return(ventiles)

  })

  #Displays table
  output$ventileTable = renderDataTable({
    
    
    datatable(ventileTable(), rownames = FALSE, options = list(paging = FALSE))
  })

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
    if(length(chosen_file()) > 1){
      show_groups()
    }
    else {
      paste0(ifelse(!(is.na(data_year())), paste0("Tax Year 20", data_year(), " from "), ""), data_version())
    }
  })

  ################################
  #Income band table output
  ################################

  # Creates table
  incomeBandTable = reactive ({
    
    if(length(show_groups()) == 0){
      stop("Please select a population subgroup.")
      return(NULL)
    }

    if(length(chosen_file()) == 0){
      stop("Please select a tax year.")
      return(NULL)
    }

    if(input$y_type == "Income Components" & length(inc_comp()) == 0){
      stop("Please select an income component.")
      return(NULL)
    }

    if(length(show_groups()) > 8){
      stop("Maximum selection is 8 population subgroups.")
      return(NULL)
    }

    if(length(chosen_file()) > 8){
      stop("Maximum selection is 8 tax years.")
      return(NULL)
    }

    if(length(inc_comp()) > 8){
      stop("Maximum selection is 8 income components.")
      return(NULL)
    }

    for (file in chosen_file()){
      file_split <- strsplit(file, "\\.")
      extension <- file_split[[1]][length(file_split[[1]])]
      if (extension != "csv") {
        stop("An uploaded file has the wrong file type (must be .csv)")
        return(NULL)
      }
    }
    
    dataset = copy(dataUpload())
    
    incomeBand = dataset[Income_Type == "Income Bands" & Population_Type == input$populationType & Description %in% show_groups()
                         & Income_Measure == income_sort() & file %in% data_version_full() & Value_Type == income_sort()]

    
    if (input$y_type == "Income") {
      if (length(chosen_file()) == 1) {
        incomeBand <- incomeBand[, .(Income_Group, Population_Type, Description, Income_Type, Value)]
        colnames(incomeBand) = c("Income Band", "Population Type", "Population Subgroup", "Income Type", paste0("Average ", input$populationType, " ", income_sort()))
      }
      else {
        incomeBand <- incomeBand[, .(Income_Group, Population_Type, Description, Income_Type, data_version, year, Value)]
        colnames(incomeBand) = c("Income Band", "Population Type", "Population Subgroup", "Income Type", "Version", "Year", paste0("Average ", input$populationType, " ", income_sort()))
      }
    }
    else{
      incomeBand[, c("Value") := NULL] 
      if (length(chosen_file()) == 1) {
        incomeBand <- incomeBand[, .(Income_Group, Population_Type, Description, Income_Type, Population)]
        colnames(incomeBand) = c("Income Band", "Population Type", "Population Subgroup", "Income Type", "Population Value")
      }
      else if (input$normalised) {
        incomeBand[Normalised != "S", Normalised := round(as.numeric(Normalised))]
        incomeBand <- incomeBand[, .(Income_Group, Population_Type, Description, Income_Type, data_version, year, Normalised)]
        colnames(incomeBand) = c("Income Band", "Population Type", "Population Subgroup", "Income Type", "Version", "Year", "Normalised Population Value")
        
      }
      else {
        incomeBand <- incomeBand[, .(Income_Group, Population_Type, Description, Income_Type, data_version, year, Population)]
        colnames(incomeBand) = c("Income Band", "Population Type", "Population Subgroup", "Income Type", "Version", "Year", "Population Value")
      }
    }
    
    return(incomeBand)
  }
  )

  # Displays table
  output$incomeBandTable = renderDataTable({
    
    
    datatable(incomeBandTable(), rownames = FALSE, options = list(paging = FALSE))
  })


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
    
    if(length(chosen_file()) > 1){
      show_groups()
    }
    else {
      paste0(ifelse(!(is.na(data_year())), paste0("Tax Year 20", data_year(), " from "), ""), data_version())
    }
  })
}

shinyApp(ui, server)