library(shiny)
library(shinycssloaders)
library(cNORM)
library(DT)
library(ggplot2)

# Define UI for Parametric Modeling Application
shinyUI(fluidPage(

  # Set tab title
  title = "cNORM - Parametric Modeling",

  # Tabsetpanel for main workflow
  tabsetPanel(

    # Tab 1: Data Input
    tabPanel(
      "Data Input",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          tags$h3("Load Data"),
          tags$p("Please choose a data set for parametric modeling. You can use a built-in example or load your own file:"),

          selectizeInput(
            "Example",
            label = "Example:",
            choices = c("", "elfe", "ppvt", "CDC"),
            selected = character(0),
            multiple = FALSE
          ),

          hr(),

          fileInput(
            "file",
            "Choose a file",
            multiple = FALSE,
            accept = c(".csv", ".xlsx", ".xls", ".rda", ".sav")
          ),

          tags$p(tags$b("Note: If you upload a file, the selected example will be overridden.")),

          hr(),

          tags$h4("Built-in Examples:"),
          tags$ul(
            tags$li(tags$b("elfe:"), "Reading comprehension test (ages 6-10, scores 0-28)"),
            tags$li(tags$b("ppvt:"), "Peabody Picture Vocabulary Test (ages 2.5-17.5, scores 0-228)"),
            tags$li(tags$b("CDC:"), "Growth data from CDC (ages 2-18, BMI/weight/height)")
          )
        ),

        mainPanel(
          width = 9,
          tags$h3("Data Preview"),
          tags$p("After loading data, review it below before proceeding to modeling. You can apply betabinomial distributions (especially suitable for IRT-based test scales) and SinH-ArcsinH (shash) distributions, which are extremely versatile and can approximate a wide range of distribution forms."),
          withSpinner(DT::DTOutput("dataPreview"), type = 5)
        )
      )
    ),

    # Tab 2: Modeling
    tabPanel(
      "Modeling",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          tags$h3("Model Configuration"),

          # Variable selection
          uiOutput("ageVariable"),
          uiOutput("scoreVariable"),
          uiOutput("weightVariable"),

          hr(),

          # Distribution type
          selectInput(
            "distributionType",
            "Distribution Type:",
            choices = c("SinH-ArcSinH (SHASH)" = "shash",
                        "Beta-Binomial" = "betabinomial"),
            selected = "shash"
          ),

          hr(),

          # Conditional UI for SHASH parameters
          conditionalPanel(
            condition = "input.distributionType == 'shash'",
            tags$h4("SHASH Parameters"),
            tags$p("Specify polynomial degrees for each distribution parameter:"),

            sliderInput(
              "mu_degree",
              "Degree of μ (location):",
              min = 0, max = 5, value = 3, step = 1
            ),

            sliderInput(
              "sigma_degree",
              "Degree of σ (scale):",
              min = 0, max = 5, value = 2, step = 1
            ),

            sliderInput(
              "epsilon_degree",
              "Degree of ε (skewness):",
              min = 0, max = 5, value = 2, step = 1
            ),

            sliderInput(
              "delta_degree",
              "Degree of δ (tail weight):",
              min = 0, max = 5, value = 1, step = 1
            ),

            checkboxInput(
              "fix_delta",
              "Fix delta (instead of polynomial)?",
              value = FALSE
            ),

            conditionalPanel(
              condition = "input.fix_delta == true",
              numericInput(
                "delta_value",
                "Fixed delta value:",
                value = 1,
                min = 0.1,
                max = 5,
                step = 0.1
              )
            )
          ),

          # Conditional UI for Beta-Binomial parameters
          conditionalPanel(
            condition = "input.distributionType == 'betabinomial'",
            tags$h4("Beta-Binomial Parameters"),
            tags$p("Specify polynomial degrees for alpha and beta:"),

            sliderInput(
              "alpha_degree",
              "Degree of α:",
              min = 0, max = 5, value = 3, step = 1
            ),

            sliderInput(
              "beta_degree",
              "Degree of β:",
              min = 0, max = 5, value = 3, step = 1
            )
          ),

          hr(),

          # Scale selection
          selectInput(
            "scale",
            "Norm Scale:",
            choices = c("T-scores (M=50, SD=10)" = "T",
                        "IQ-scores (M=100, SD=15)" = "IQ",
                        "z-scores (M=0, SD=1)" = "z"),
            selected = "T"
          ),

          hr(),

          # Action buttons
          actionButton(
            "computeModel",
            "Compute Model",
            class = "btn-primary",
            style = "width: 100%;"
          ),

          br(), br(),

          downloadButton(
            "downloadModel",
            "Download Model",
            style = "width: 100%;"
          )
        ),

        mainPanel(
          width = 9,

          # Model summary output
          tags$h3("Model Results"),

          conditionalPanel(
            condition = "input.computeModel == 0",
            tags$div(
              style = "padding: 20px; background-color: #f8f9fa; border-radius: 5px;",
              tags$p(
                style = "font-size: 16px;",
                "Configure the model parameters in the sidebar and click 'Compute Model' to fit the parametric distribution."
              )
            )
          ),

          conditionalPanel(
            condition = "input.computeModel > 0",
            tabsetPanel(
              id = "modelTabs",
              # Percentile plot
              tabPanel(
                "Percentile Plot",
                tags$br(),
                tags$p("Observed percentiles (points) vs. model predictions (lines). Wavy lines indicate overfit. Reduce polynomial degrees in this case."),
                tags$p(""),
                withSpinner(
                  plotOutput("percentilePlot", width = "100%", height = "700px"),
                  type = 5
                )
              ),
              # Summary tab
              tabPanel(
                "Summary",
                tags$br(),
                withSpinner(verbatimTextOutput("modelSummary"), type = 5)
              )
            )
          )
        )
      )
    ),

    # Tab 3: Norm Tables
    tabPanel(
      "Norm Tables",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          tags$h3("Norm Table Configuration"),

          tags$p("Generate norm tables with confidence intervals based on the fitted model."),

          # Age specification
          textInput(
            "norm_ages",
            "Ages (comma or space separated):",
            value = ""
          ),

          tags$p(tags$small("Example: 7, 8, 9, 10 or 7 8 9 10")),

          hr(),

          # Score range
          numericInput(
            "norm_start",
            "Start raw score:",
            value = NULL,
            step = 1
          ),

          numericInput(
            "norm_end",
            "End raw score:",
            value = NULL,
            step = 1
          ),

          numericInput(
            "norm_step",
            "Step size:",
            value = 1,
            min = 0.1,
            step = 0.1
          ),

          hr(),

          # Confidence intervals
          checkboxInput(
            "include_ci",
            "Include confidence intervals",
            value = FALSE
          ),

          conditionalPanel(
            condition = "input.include_ci == true",
            sliderInput(
              "ci_level",
              "Confidence level:",
              min = 0.80,
              max = 0.99,
              value = 0.90,
              step = 0.01
            ),

            numericInput(
              "reliability",
              "Reliability coefficient:",
              value = 0.90,
              min = 0.01,
              max = 0.99,
              step = 0.01
            )
          ),

          hr(),

          actionButton(
            "generateTables",
            "Generate Tables",
            class = "btn-primary",
            style = "width: 100%;"
          ),

          br(), br(),

          downloadButton(
            "downloadTables",
            "Download Tables (CSV)",
            style = "width: 100%;"
          )
        ),

        mainPanel(
          width = 9,
          tags$h3("Norm Tables"),

          conditionalPanel(
            condition = "input.generateTables == 0",
            tags$div(
              style = "padding: 20px; background-color: #f8f9fa; border-radius: 5px;",
              tags$p(
                style = "font-size: 16px;",
                "Ensure a model has been computed, then configure the table parameters and click 'Generate Tables'."
              )
            )
          ),

          conditionalPanel(
            condition = "input.generateTables > 0",
            uiOutput("normTablesUI")
          )
        )
      )
    )
  )
))
