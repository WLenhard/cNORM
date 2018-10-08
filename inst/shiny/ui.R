library(shiny)
library(shinythemes)

title <- "cNORM-Shiny"

# Define UI for cNORM-Application
shinyUI(fluidPage(

  # Set tab title
  title = "cNORM - Shiny",
  # shinythemes::themeSelector(),

  # Tabsetpanel for single tabs
  tabsetPanel(
    #Tab for data input
        tabPanel("Data Input", sidebarLayout(sidebarPanel(img(src='logo.png', align = "right") ,tags$h3("Data Input"), tags$h5("Please choose a data set for your calculations"), selectizeInput("Example", label = "Choose an example file",
                                                                         choices = c("", "elfe", "ppvt", "CDC"), selected = character(0),
                                                                         multiple = FALSE),helpText("Choose one of the included data examples"), hr(),
                                                          fileInput("file", "Choose a file", multiple = FALSE, accept = c(".csv",".xlsx", ".xls", ".rda", ".sav")),
                                                          tags$h5(tags$b("HINT: If you choose a file from your own directory, the chosen example will not be used during the session anymore!"))),
                                             mainPanel(dataTableOutput("table")))),

  # Panel for Data Preparation (choosing and visualizing)
    tabPanel("Data Preperation",
              # Define layout for sidebar
             sidebarLayout(

              # Define sidebar panel for choosing grouping, raw values and scale
               sidebarPanel(
                 tags$h3("Choose"),
                 uiOutput("GroupingVariable"),
                 tags$h5("Variable which is used to divide the observations into groups"),
                 tags$h5("For example, variable age by rounding to a half or a full year"),
                 tags$br(),
                 uiOutput("RawValues"),
                 tags$h5("Raw values, for example testscores or physiological data "),
                 tags$br(),
                 selectInput("Scale", label = "Scale", choices = c("T", "IQ", "z"), selected = "T"),
                 tags$h5("Choose the scale to which the raw values shall be transformed"),
                 tags$h5("You can choose between between T-, IQ-scaled and z-values"),
                 actionButton(inputId = "DoDataPreparation", label = "Prepare Data"),

                 tags$br(),
                 tags$br(),
                 # Additional options (explanatory variable with default groupoing, ranking method, number of powers)
                 tags$h3("Additional options"),
                 uiOutput("ExplanatoryVariable"),
                 tags$h5("As default explanatory variable is set to grouping variable"),
                 tags$br(),
                 selectInput(inputId = "NumberOfPowers",
                             label = "Choose number of powers",
                             choices = c(1:5),
                             selected = 4),
                 tags$h5("As default number of power is set to 4"),
                 tags$h5("Powers higher than 4 leads to overfitting"),
                 tags$h5("God solution for powers equal 4 in general"),
                 tags$br(),
                 selectInput("Method", label = "Ranking method", choices = c(1:7), selected = 4),
                 tags$h5("Ranking methods"),
                 tags$ol(
                   tags$li("Blom (1985"),
                   tags$li("Tukey (1949)"),
                   tags$li("Van der Warden (1952)"),
                   tags$li("Rankit"),
                   tags$li("Levenbach (1953)"),
                   tags$li("Filliben (1975)"),
                   tags$li("Yu & Huang (2001)")
                 )
             ),
            # Main panel for showing prepared data
             mainPanel(dataTableOutput("preparedData")))),

    # Defines panel for best model output
    # Tab returns bestModel with information function and plot of real and predicted raw values
    tabPanel("Best Model",
             sidebarLayout(sidebarPanel(tags$h3("Calc best model"), tags$h5("Button starts calculation of the best model and returns its coefficients as well as the information function"),  actionButton(inputId = "CalcBestModel",
                                                     label = "Calc best model"), tags$h5("HINT: If no file is chosen and prepared, no calculation is done!")),
                           mainPanel(verbatimTextOutput("BestModel"), plotOutput("PlotWL"), plotOutput("PlotValues")))),

    # Defines panel for plots
     tabPanel("Percentiles", column(6, plotOutput("PlotPercentiles"))),
     tabPanel("Norm curves", column(6, plotOutput("NormCurves"))),
  # NavbarMenu for predicting norm and raw values and raw/norm tables
  navbarMenu("Prediction",
                tabPanel("Norm value prediction", tags$h3("Prediction of singel norm values"), uiOutput("InputNormValue"), verbatimTextOutput("NormValue")),
                tabPanel("Raw value prediction", tags$h3("Prediction of single raw values"), uiOutput("InputRawValue"), verbatimTextOutput("RawValue")),
                tabPanel("Norm table", tags$h3("Generating norm tables"), uiOutput("InputNormTable"),

                         dataTableOutput("NormTable")),
                tabPanel("Raw table", tags$h3("Generating raw tables"), uiOutput("InputRawTable"), dataTableOutput("RawTable"))))
  )
  )


