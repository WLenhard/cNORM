if(!require(shiny)){
  install.packages("shiny")
  require(shiny)
}

if(!require(shinycssloaders)){
  install.packages("shinycssloaders")
  require(shinycssloaders)
}

if(!require(cNORM)){
  install.packages("cNORM")
  require(cNORM)
}

title <- "cNORM-Shiny"

# Define UI for cNORM-Application
shinyUI(fluidPage(

  # Set tab title
  title = "cNORM - Shiny",

  # Tabsetpanel for single tabs
  tabsetPanel(
    # Tab for data input
    tabPanel("Data Input", sidebarLayout(
      sidebarPanel(
        img(src = "logo.png", align = "right"), tags$h3("Data Input"), tags$p("Please choose a data set for your calculations. You can use an inbuilt example or load your own file:"), selectizeInput("Example",
          label = "Example:",
          choices = c("", "elfe", "ppvt", "CDC"), selected = character(0),
          multiple = FALSE
        ), hr(),
        fileInput("file", "Choose a file", multiple = FALSE, accept = c(".csv", ".xlsx", ".xls", ".rda", ".sav")),
        tags$p(tags$b("HINT: If you choose a file from your own directory, the chosen example will not be used during the session anymore!"))
      ),
      mainPanel(htmlOutput("introduction"), dataTableOutput("table"))
    )),

    # Panel for Data Preparation (choosing and visualizing)
    tabPanel(
      "Preparation",
      # Define layout for sidebar
      sidebarLayout(

        # Define sidebar panel for choosing grouping, raw values and scale
        sidebarPanel(
          tags$h3("Choose"),
          uiOutput("GroupingVariable"),
          tags$p("Variable which is used to divide the observations into groups, for example variable age by rounding to a half or a full year"),
          tags$br(),
          uiOutput("RawValues"),
          tags$p("Raw values, for example test scores or physiological data "),
          tags$br(),
          selectInput("Scale", label = "Scale", choices = c("T", "IQ", "z"), selected = "T"),
          tags$p("Choose the scale to which the raw values shall be transformed"),
          tags$p("You can choose between between T scores, IQ scores and z scores."),
          actionButton(inputId = "DoDataPreparation", label = "Prepare Data"),

          tags$br(),
          tags$br(),
          # Additional options (explanatory variable with default grouping, ranking method, number of powers)
          tags$h3("Additional options"),
          uiOutput("ExplanatoryVariable"),
          tags$p("As default, the explanatory variable is set to grouping variable. In case, a continuous explanatory variable like age is available, please use it."),
          tags$br(),
          selectInput(
            inputId = "NumberOfPowers",
            label = "Choose number of powers",
            choices = c(1:5),
            selected = 4
          ),
          tags$p("This variable specifies the power parameter for the Taylor polynomial. As default number of power is set to 4. Higher values might lead to a closer fit, but yield the danger of overfitting."),
          tags$br(),
          selectInput("Method", label = "Ranking method", choices = c(1:7), selected = 4),
          tags$p("Ranking methods"),
          tags$ol(
            tags$li("Blom (1985)"),
            tags$li("Tukey (1949)"),
            tags$li("Van der Warden (1952)"),
            tags$li("Rankit (Bliss, 1967)"),
            tags$li("Levenbach (1953)"),
            tags$li("Filliben (1975)"),
            tags$li("Yu & Huang (2001)")
          )
        ),
        # Main panel for showing prepared data
        mainPanel(withSpinner(dataTableOutput("preparedData"), type = 5))
      )
    ),

    # Defines panel for best model output
    # Tab returns bestModel with information function and plot of real and predicted raw values
    tabPanel(
      "Modeling & Validation",
      sidebarLayout(
        sidebarPanel(
          tags$h3("Model Data"),
          tags$p("Here, you can calculate a regression model that models the original data as close as possible, while smoothing the curves and eliminating noise. After hitting the button, the regression function for a possible model is shown. The plot displays the information criteria for the different models, beginning with the model with one terms up to the maximum. The model should have a high R2 with as few terms as possible. The information of the plot is again displayed as a table below the chart. To display plots of observed vs. fitted raw and norm scores, please use the plotting options on the next tab. There, you can check the percentile curves as well and inspect the curves of the different plausible models."),
          tags$p("HINT: Please ensure that the data is loaded and prepared, before starting the modeling. In case of k > 4, the calculation will take a few seconds."),
          actionButton(
            inputId = "CalcBestModel",

            label = "Model Data"
          ),
          tags$br(),
          # Additional options (R^2, terms, type for printSubset)
          tags$h3("Additional options"),
          numericInput(
            inputId = "ChosenDetCoeff",
            label = "Coefficient of determination", value = 0.99, min = 0, max = 1, step = 0.01
          ),
          uiOutput("NumberOfTerms"),
          tags$br(),
          selectInput(inputId = "chosenTypePlotSubset", "Type of plot", choices = c(0:3), selected = 1),
          tags$p("Please select the type of chart for plotting the information function, with"),
          tags$ol(
            start = 0,
            tags$li("adjusted R2 by number of predictors"),
            tags$li("log transformed Mallow's Cp by adjusted R2"),
            tags$li("Bayesian Information Criterion (BIC) by adjusted R2"),
            tags$li("RMSE by number of predictors")
          )
        ),
        mainPanel(
          withSpinner(verbatimTextOutput("BestModel1"), type = 5),
          verbatimTextOutput("BestModel2"),
          verbatimTextOutput("BestModel3"),
          verbatimTextOutput("BestModel4"),
          verbatimTextOutput("BestModel5"),
          verbatimTextOutput("BestModel6"),
          tags$br(),
          tags$br(),
          # tags$h4("Information Function, Subset Specifics and Fitted Values"),
          # tags$p("The plot shows the informationcriteria for the different models, beginning with the model with one terms up to the maximum. The model should have a high R2 with as few terms as possible. The information of the plot is again displayed as a table below the chart. On the bottom of the page, you can see, how well the observed data are fitted by the model."),
          withSpinner(plotOutput("PlotWL", width = "100%", height = "600px"), type = 5),
          tags$br(),
          withSpinner(dataTableOutput("PrintSubset"), type = 5)
          # , tags$br(),
          # withSpinner(plotOutput("PlotValues", width = "100%", height = "600px"), type=5)
        )
      )
    ),

    navbarMenu(
      "Visualization",
      tabPanel("Percentiles", sidebarLayout(
        sidebarPanel(
          tags$h3("Percentiles"), tags$p("The chart shows how well the model generally fits the observed data. The observed percentiles are represented as dots, the continuous norm curves as lines. In case of intersecting norm curves the model is inconsistent. Please change the number of terms in the 'Best Model' tab in order to find a consistent model. You can use the 'Series' option to look out for suitable parameters."),
          tags$br(),
          textInput(inputId = "PercentilesForPercentiles", "Choose percentiles"),
          tags$p("Please seperate the values by a comma or space.")
        ),
        mainPanel(plotOutput("PlotPercentiles", width = "100%", height = "800px"))
      )),

      tabPanel("Series", sidebarLayout(
        sidebarPanel(
          tags$h3("Percentile Series"),
          tags$p("In oder to facilitate model selection, the chart displays percentile curves of the different models."),
          tags$br(), sliderInput("terms", "Number of terms:",
            min = 1, max = 24, value = 5
          ), tags$br(), tags$br(),
          tags$p("Please use the slider to change the number of terms in the model. Please select a model with non-intersecting percentile curves. Avoid undulating curves, as these indicate model overfit."),
          tags$br(), tags$b("After choosing the best fitting number of terms, please rerun the model calculation before generating norm tables.")
        ),
        mainPanel(plotOutput("Series", width = "100%", height = "800px"))
      )),


      tabPanel("Norm Curves", sidebarLayout(
        sidebarPanel(
          tags$h3("Norm Curves"), tags$p("The chart is comparable to the percentile plot. It only shows the norm curves for some selected norm scores."),
          textInput(inputId = "PercentilesForNormCurves", label = "Choose percentiles for norm curves", value = ""),
          tags$p("Please seperate the values by a comma or space. The percentile values are automatically transformed to the norm scale used in the data preparation. In order to get curves specific z values, you can use the following percentiles:"),
          tags$div(
            HTML("<div align=center><table width=100%><tr><td align = right><b>z</b></td><td align = right>-2</td><td align = right>-1</td><td align = right>0</td><td align = right>1</td><td align = right>2</td></tr><tr><td align = right><b>percentile</b></td><td align = right> 2.276</td><td align = right> 15.87</td><td align = right> 50.00</td><td align = right> 84.13</td><td align = right> 97.724</td></tr></table></div>")
          )
        ),
        mainPanel(plotOutput("NormCurves", width = "100%", height = "800px"))
      )),
      tabPanel("Density", sidebarLayout(sidebarPanel(
        tags$h3("Density"), tags$p("The plot shows the probability density function of the raw scores based on the regression model. Like the 'Derivative Plot', it can be used to identify violations of model validity or to better visualize deviations of the test results from the normal distribution. As a default, the lowest, highest and a medium group is shown."),
        tags$br(),
        textInput(inputId = "densities", "Choose groups"),
        tags$p("Please seperate the values by a comma or space.")
      ), mainPanel(
        withSpinner(
          plotOutput("PlotDensity", width = "100%", height = "800px"),
          type = 5
        )
      ))),
      tabPanel("Derivative Plot", sidebarLayout(sidebarPanel(tags$h3("Derivative Plot"), tags$p("To check whether the mapping between latent person variables and test scores is biunique, the regression function can be searched numerically within each group for bijectivity violations using the 'checkConsistency' function. In addition, it is also possible to plot the first partial derivative of the regression function to l and search for negative values. Look out for values lower than 0. These indicate violations of the model.")), mainPanel(
        withSpinner(plotOutput("PlotDerivatives", width = "100%", height = "600px"), type = 5)
      ))),

      tabPanel("Norm Scores", sidebarLayout(sidebarPanel(
        tags$h3("Norm Scores Plot"), tags$p("The plot shows the observed and predicted norm scores. You can identify, how well the model is able to predict the norm scores of the dataset. The duration of the computation increases with the size of the dataset."),
        tags$br(),
        checkboxInput("grouping", "Plot by group", FALSE),
        checkboxInput("differences", "Plot differences", FALSE)
      ), mainPanel(
        withSpinner(
          plotOutput("PlotNormScores", width = "100%", height = "800px"),
          type = 5
        )
      ))),

      tabPanel("Raw Scores", sidebarLayout(sidebarPanel(
        tags$h3("Raw Scores Plot"), tags$p("The plot shows the observed and predicted raw scores. You can identify, how well the model is able to predict the raw scores of the original dataset."),
        tags$br(),
        checkboxInput("grouping1", "Plot by group", FALSE),
        checkboxInput("differences1", "Plot differences", FALSE)
      ), mainPanel(
        withSpinner(
          plotOutput("PlotRawScores", width = "100%", height = "800px"),
          type = 5
        )
      )))
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
