library(shiny)
#library(shinythemes)
library(markdown)

title <- "cNORM-Shiny"

# Define UI for cNORM-Application
shinyUI(fluidPage(

  # Set tab title
  title = "cNORM - Shiny",
  # Shiny theme selector for testing different style themes
  # shinythemes::themeSelector(),

  # Tabsetpanel for single tabs
  tabsetPanel(
    # Tab for data input
    tabPanel("Data Input", sidebarLayout(
      sidebarPanel(
        img(src = "logo.png", align = "right"), tags$h3("Data Input"), tags$h5("Please choose a data set for your calculations. You can use an inbuilt example or load your own file:"), selectizeInput("Example",
                                                                                                                                                                                                        label = "Example:",
                                                                                                                                                                                                        choices = c("", "elfe", "ppvt", "CDC"), selected = character(0),
                                                                                                                                                                                                        multiple = FALSE
        ), hr(),
        fileInput("file", "Choose a file", multiple = FALSE, accept = c(".csv", ".xlsx", ".xls", ".rda", ".sav")),
        tags$h5(tags$b("HINT: If you choose a file from your own directory, the chosen example will not be used during the session anymore!"))
      ),
      mainPanel(htmlOutput("introduction"), dataTableOutput("table"))
    )),

    # Panel for Data Preparation (choosing and visualizing)
    tabPanel(
      "Data Preparation",
      # Define layout for sidebar
      sidebarLayout(

        # Define sidebar panel for choosing grouping, raw values and scale
        sidebarPanel(
          tags$h3("Choose"),
          uiOutput("GroupingVariable"),
          tags$h5("Variable which is used to divide the observations into groups, for example variable age by rounding to a half or a full year"),
          tags$br(),
          uiOutput("RawValues"),
          tags$h5("Raw values, for example test scores or physiological data "),
          tags$br(),
          selectInput("Scale", label = "Scale", choices = c("T", "IQ", "z"), selected = "T"),
          tags$h5("Choose the scale to which the raw values shall be transformed"),
          tags$h5("You can choose between between T scores, IQ scores and z scores."),
          actionButton(inputId = "DoDataPreparation", label = "Prepare Data"),

          tags$br(),
          tags$br(),
          # Additional options (explanatory variable with default grouping, ranking method, number of powers)
          tags$h3("Additional options"),
          uiOutput("ExplanatoryVariable"),
          tags$h5("As default, the explanatory variable is set to grouping variable. In case, a continuous explanatory variable like age is available, please use it."),
          tags$br(),
          selectInput(
            inputId = "NumberOfPowers",
            label = "Choose number of powers",
            choices = c(1:5),
            selected = 4
          ),
          tags$h5("This variable specifies the power parameter for the Taylor polynomial. As default number of power is set to 4. Higher values might lead to a closer fit, but yield the danger of overfitting."),
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
        mainPanel(dataTableOutput("preparedData"))
      )
    ),

    # Defines panel for best model output
    # Tab returns bestModel with information function and plot of real and predicted raw values
    tabPanel(
      "Best Model",
      sidebarLayout(
        sidebarPanel(
          tags$h3("Model Data"),
          tags$h5("Button starts calculation of the best model and returns its coefficients as well as the information function"),
          actionButton(
            inputId = "CalcBestModel",

            label = "Model Data"
          ), tags$h5("HINT: Please ensure that the data is loaded and prepared, before starting the modeling. The calculation might take a few seconds."),
          tags$br(),
          tags$br(),
          # Additional options (R^2, terms, type for printSubset)
          tags$h3("Additional options"),
          numericInput(
            inputId = "ChosenDetCoeff",
            label = "Coefficient of determination", value = 0.99, min = 0, max = 1, step = 0.01
          ),
          uiOutput("NumberOfTerms"),
          selectInput(inputId = "chosenTypePlotSubset", "Type of plot", choices = c(0:2), selected = 1)
        ),
        mainPanel(
          tags$h4("Best model"),
          tags$h5("cNORM determines the best fitting model for a given number of terms or R2."),
          verbatimTextOutput("BestModel"),
          tags$br(),
          tags$br(),
          tags$h4("Information Function, Subset Specifics and Fitted Values"),
          tags$h5("The plot shows the informationcriteria for the different models, beginning with the model with one terms up to the maximum. The model should have a high R2 with as few terms as possible. The information of the plot is again displayed as a table below the chart"),
          tags$h5("The information of the plot is again displayed as a table below the chart."),
          tags$h5("On the bottom of the page, you can see, how well the manifest data are fitted by the model."),
          plotOutput("PlotWL"),
          tags$br(),
          dataTableOutput("PrintSubset"),
          tags$br(),
          plotOutput("PlotValues")
        )
      )
    ),

    navbarMenu(
      "Percentiles and Norm Curves",
      tabPanel("Percentiles", sidebarLayout(sidebarPanel(tags$h3("Percentiles"), tags$h5("The charts shows how well the model generally fits the manifest data. The manifest percentiles are represented as dots, the continuous norm curves as lines. In case of intersecting norm curves the model is inconsistent. Please change the number of terms in order to find a consistent model.")), mainPanel(plotOutput("PlotPercentiles")))),
      tabPanel("Norm Curves", sidebarLayout(sidebarPanel(tags$h3("Norm Curves"), tags$h5("The chart is comparable to the percentile plot. It only shows the norm curves for some selected norm scores.")), mainPanel(plotOutput("NormCurves")))),
      tabPanel("Density Plot", sidebarLayout(sidebarPanel(tags$h3("Density Plot"), tags$h5("The plot shows the probability density function of the raw scores based on the regression model. Like the 'Derivative Plot', it can be used to identify violations of model validity or to better visualize deviations of the test results from the normal distribution. As a default, the lowest, highest and a medium group is shown.")), mainPanel(plotOutput("PlotDensity")))),
      tabPanel("Derivative Plot", sidebarLayout(sidebarPanel(tags$h3("Derivative Plot"), tags$h5("To check whether the mapping between latent person variables and test scores is biunique, the regression function can be searched numerically within each group for bijectivity violations using the 'checkConsistency' function. In addition, it is also possible to plot the first partial derivative of the regression function to l and search for negative values. Look out for values lower than 0. These indicate violations of the model.")), mainPanel(plotOutput("PlotDerivatives"))))
    ),


    # NavbarMenu for predicting norm and raw values and raw/norm tables
    navbarMenu(
      "Prediction",
      tabPanel("Norm value prediction", sidebarLayout(sidebarPanel(tags$h3("Prediction of singel norm values"), uiOutput("InputNormValue")), mainPanel(verbatimTextOutput("NormValue")))),
      tabPanel("Raw value prediction", sidebarLayout(sidebarPanel(tags$h3("Prediction of single raw values"), uiOutput("InputRawValue")), mainPanel(verbatimTextOutput("RawValue")))),
      tabPanel("Norm table", sidebarLayout(sidebarPanel(tags$h3("Generating norm tables"), uiOutput("InputNormTable")), mainPanel(dataTableOutput("NormTable")))),
      tabPanel("Raw table", sidebarLayout(sidebarPanel(tags$h3("Generating raw tables"), uiOutput("InputRawTable")), mainPanel(dataTableOutput("RawTable"))))
    )
  )
))
