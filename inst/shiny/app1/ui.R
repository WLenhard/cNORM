library(shiny)
library(shinycssloaders)
library(cNORM)
library(DT)
library(foreign)
library(readxl)

# Value of the "(none)" choice in the grouping-variable selector.
# Must match NO_GROUP in server.R.
NO_GROUP <- "__none__"

shinyUI(fluidPage(
  title = "cNORM - Shiny",
  tags$head(
    tags$script(HTML("
      var keepAliveInterval;
      $(document).on('shiny:connected', function(event) {
        keepAliveInterval = setInterval(function() {
          Shiny.setInputValue('keep_alive', Math.random());
        }, 25000); // Send ping every 25 seconds
      });
      $(document).on('shiny:disconnected', function(event) {
        clearInterval(keepAliveInterval);
      });
    "))
  ),
  tabsetPanel(
    # ------------------------------------------------------------------
    # Tab 0: Introduction
    # ------------------------------------------------------------------
    tabPanel(
      "Introduction",
      fluidRow(
        column(
          width = 10, offset = 1,
          includeHTML("www/introduction.html")
        )
      )
    ),

    # ---------------------------------------------------------------- DATA ----
    tabPanel("Data",
             sidebarLayout(
               sidebarPanel(
                 img(src = "logo.png", align = "right", style = "max-width:100px;"),
                 h3("Data input"),
                 selectInput("Example", "Example dataset:",
                             choices = c("(none)" = "", "elfe", "ppvt", "CDC"),
                             selected = ""),
                 fileInput("file", "Or upload your own file:",
                           accept = c(".csv", ".xlsx", ".xls", ".rda", ".RData", ".rdata", ".sav")),
                 helpText("Accepted: CSV (semicolon or comma), Excel, SPSS, R data.",
                          "Uploaded files take precedence over the example.")
               ),
               mainPanel(
                 h3("Data preview"),
                 p("Review your data below, then continue with the 'Modeling' tab."),
                 DT::DTOutput("table")
               )
             )
    ),

    # ------------------------------------------------------------ MODELING ----
    tabPanel("Modeling",
             sidebarLayout(
               sidebarPanel(
                 h3("Model settings"),
                 uiOutput("GroupingVariable"),
                 helpText("Choose '(none)' if the raw scores do not depend on age or grade. ",
                          "The whole sample is then treated as one group and the raw score is ",
                          "modelled as a function of the norm score only."),
                 uiOutput("RawValues"),
                 selectInput("Scale", "Norm scale:",
                             choices = c("T (M=50, SD=10)"   = "T",
                                         "IQ (M=100, SD=15)" = "IQ",
                                         "z (M=0, SD=1)"     = "z"),
                             selected = "T"),
                 selectInput("NumberOfPowers",    "Power degree of location (k):",
                             choices = 1:6, selected = 5),
                 # t is only relevant if a grouping variable is selected
                 conditionalPanel(
                   condition = paste0("input.InputGroupingVariable != '", NO_GROUP, "'"),
                   selectInput("NumberOfPowersAge",
                               "Power degree of explanatory variable (t), e. g. age:",
                               choices = 1:6, selected = 3)
                 ),
                 numericInput("InputNumberOfTerms",
                              "Number of terms (leave blank for auto selection):",
                              value = NA, min = 1, step = 1),
                 actionButton("RunModel", "Run model", class = "btn-primary"),
                 downloadButton("downloadModel", "Download model"),
                 hr(),
                 helpText(
                   "k controls the polynomial complexity for the location term, ",
                   "t for the explanatory variable (typically age). ",
                   "Leaving the number of terms blank autoselects the highest consistent model."
                 )
               ),
               mainPanel(
                 tabsetPanel(
                   id = "ModelingResults",
                   type = "tabs",

                   tabPanel("Percentile Plot",
                            br(),
                            withSpinner(plotOutput("modelPlot", height = "600px"), type = 5)
                   ),

                   tabPanel("Model Summary",
                            br(),
                            withSpinner(uiOutput("ModelReportUI"), type = 5),
                            verbatimTextOutput("ConsistencyCheck"),
                            verbatimTextOutput("RangeCheck")
                   ),

                   tabPanel("Model Fit",
                            br(),
                            selectInput("chosenTypePlotSubset", "Fit criterion:",
                                        choices = c("R2 by Number of Predictors",
                                                    "Mallow's (log) Cp by R2",
                                                    "Bayesian Information Criterion (BIC) by R2",
                                                    "RMSE by Number of Predictors"),
                                        selected = "RMSE by Number of Predictors"),
                            withSpinner(plotOutput("PlotWL", height = "500px"), type = 5)
                   ),

                   tabPanel("Model Fit Table",
                            br(),
                            DT::DTOutput("PrintSubset")
                   )
                 )
               )
             )
    ),

    # ------------------------------------------------------- VISUALIZATION ----
    navbarMenu("Visualization",

               tabPanel("Percentiles", sidebarLayout(
                 sidebarPanel(
                   h3("Percentile curves"),
                   helpText("Dots: observed percentiles. Lines: model. ",
                            "Intersecting curves suggest an inconsistent model — try adjusting ",
                            "the number of terms in the Modeling tab. ",
                            "Without a grouping variable, the raw score is plotted against ",
                            "the norm score instead."),
                   textInput("PercentilesForPercentiles",
                             "Custom percentiles (comma/space separated):", value = "2.5, 10, 25, 50, 75, 90, 97.5")
                 ),
                 mainPanel(plotOutput("PlotPercentiles", height = "700px"))
               )),

               tabPanel("Series", sidebarLayout(
                 sidebarPanel(
                   h3("Percentile series"),
                   helpText("Choose the number of terms with the slider to compare models. ",
                            "Avoid intersecting or wavy curves. ",
                            "Requires a grouping variable."),
                   sliderInput("terms", "Number of terms:",
                               min = 1, max = 24, value = 5)
                 ),
                 mainPanel(plotOutput("Series", height = "700px"))
               )),

               tabPanel("Norm curves", sidebarLayout(
                 sidebarPanel(
                   h3("Norm curves"),
                   textInput("PercentilesForNormCurves",
                             "Percentiles (comma/space separated):", value = "2.5, 10, 25, 50, 75, 90, 97.5"),
                   helpText("z = -2, -1, 0, 1, 2 corresponds to ",
                            "2.276, 15.87, 50.00, 84.13, 97.724. ",
                            "Requires a grouping variable.")
                 ),
                 mainPanel(plotOutput("NormCurves", height = "700px"))
               )),

               tabPanel("Density", sidebarLayout(
                 sidebarPanel(
                   h3("Density"),
                   helpText("Probability density of raw scores by group. Helps detect ",
                            "non-normality or model violations. Without a grouping variable, ",
                            "a histogram of the raw scores is shown."),
                   textInput("densities", "Groups (comma/space separated):", "")
                 ),
                 mainPanel(withSpinner(plotOutput("PlotDensity", height = "700px"), type = 5))
               )),

               tabPanel("Derivative", sidebarLayout(
                 sidebarPanel(
                   h3("First derivative"),
                   helpText("Negative regions indicate violations of the bijective ",
                            "mapping between raw and norm scores. Without a grouping ",
                            "variable, the slope of the raw score over the norm score is shown.")
                 ),
                 mainPanel(withSpinner(plotOutput("PlotDerivatives", height = "600px"), type = 5))
               )),

               tabPanel("Norm scores", sidebarLayout(
                 sidebarPanel(
                   h3("Predicted vs. observed norm scores"),
                   checkboxInput("grouping",   "Plot by group", FALSE),
                   checkboxInput("differences","Plot differences", FALSE),
                   actionButton("normScoreButton", "Update")
                 ),
                 mainPanel(withSpinner(plotOutput("PlotNormScores", height = "700px"), type = 5))
               )),

               tabPanel("Raw scores", sidebarLayout(
                 sidebarPanel(
                   h3("Predicted vs. observed raw scores"),
                   checkboxInput("grouping1",   "Plot by group", FALSE),
                   checkboxInput("differences1","Plot differences", FALSE),
                   actionButton("rawScoreButton", "Update")
                 ),
                 mainPanel(withSpinner(plotOutput("PlotRawScores", height = "700px"), type = 5))
               ))
    ),

    # ---------------------------------------------------- CROSS-VALIDATION ----
    tabPanel("Cross-validation",
             sidebarLayout(
               sidebarPanel(
                 h3("Cross-validation"),
                 helpText("Stratified 80/20 splits, repeated. Compares RMSE and norm-score ",
                          "reliability across model sizes. Computationally heavy."),
                 sliderInput("MaxTermsCV",    "Max number of terms:", min = 1, max = 24, value = 10),
                 sliderInput("RepetitionsCV", "Repetitions:",         min = 1, max = 20, value = 1),
                 checkboxInput("NormsCV", "Evaluate norm scores", TRUE),
                 actionButton("CrossValidation", "Run cross-validation", class = "btn-primary")
               ),
               mainPanel(
                 withSpinner(plotOutput("PlotCV", height = "800px"), type = 5),
                 DT::DTOutput("TableCV")
               )
             )
    ),

    # ---------------------------------------------------------- PREDICTION ---
    navbarMenu("Prediction",
               tabPanel("Norm score",  sidebarLayout(
                 sidebarPanel(uiOutput("InputNormValue")),
                 mainPanel(verbatimTextOutput("NormValue")))),
               tabPanel("Raw score",   sidebarLayout(
                 sidebarPanel(uiOutput("InputRawValue")),
                 mainPanel(verbatimTextOutput("RawValue")))),
               tabPanel("Norm table",  sidebarLayout(
                 sidebarPanel(uiOutput("InputNormTable")),
                 mainPanel(DT::DTOutput("NormTable")))),
               tabPanel("Raw table",   sidebarLayout(
                 sidebarPanel(uiOutput("InputRawTable")),
                 mainPanel(DT::DTOutput("RawTable"))))
    )
  )
))
