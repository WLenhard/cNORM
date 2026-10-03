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

    # ------------------------------------------------------------------
    # Tab 1: Data Input
    # ------------------------------------------------------------------
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
            choices = c("", "speed", "elfe", "ppvt", "CDC"),
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
            tags$li(tags$b("speed:"), "Reading fluency / speeded test (ages 7-11, scores 0-75)"),
            tags$li(tags$b("elfe:"), "Reading comprehension test (ages 6-10, scores 0-28)"),
            tags$li(tags$b("ppvt:"), "Peabody Picture Vocabulary Test (ages 2.5-17.5, scores 0-228)"),
            tags$li(tags$b("CDC:"), "Growth data from CDC (ages 2-18, BMI/weight/height)")
          )
        ),

        mainPanel(
          width = 9,
          tags$h3("Data Preview"),
          tags$p("With this app, you can model norm scores like percentiles, T scores and IQ scores for psychometric, biometric or
                 other data. It provides parametric continuous norming based on three flexible distribution families:",
                 tags$ul(
                   tags$li(tags$b("SinH-ArcSinH (SHASH):"), " For continuous, unbounded raw scores, response times, or difference scores with flexible skewness and tail weights."),
                   tags$li(tags$b("Beta-Binomial:"), " For accuracy and power tests with a fixed number of items (k items, scores 0 to k). It is especially suitable for 1PL IRT / Rasch scales. Fpr these use cases, it fits perfectly."),
                   tags$li(tags$b("Conway-Maxwell-Poisson (CMP):"), " For speeded tests and rate/count data, accounting for under- and over-dispersion and optional test ceilings.")
                 ),
                 "After loading data, review it below before proceeding to modeling. To fit models, select the 'Modeling' tab. To generate norm tables, select 'Norm Tables'.",
                 tags$br(), tags$br(),
                 "Further information on the methodology can be found at",
                 tags$a(href = "https://www.psychometrica.de/cNorm_betabinomial_en.html", "Psychometrica", target = "_blank"),
                 "and in the package vignettes on CRAN."),
          withSpinner(DT::DTOutput("dataPreview"), type = 5)
        )
      )
    ),

    # ------------------------------------------------------------------
    # Tab 2: Modeling
    # ------------------------------------------------------------------
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
            choices = c("SinH-ArcSinH (SHASH)"         = "shash",
                        "Beta-Binomial"                 = "betabinomial",
                        "Conway-Maxwell-Poisson (CMP)"  = "cmp"),
            selected = "shash"
          ),

          hr(),

          # Conditional UI for SHASH parameters
          conditionalPanel(
            condition = "input.distributionType == 'shash'",
            tags$h4("SHASH Parameters"),
            tags$p("Specify polynomial degrees for each distribution parameter:"),

            sliderInput("mu_degree",      "Degree of \u03BC (location):",
                        min = 0, max = 5, value = 3, step = 1),
            sliderInput("sigma_degree",   "Degree of \u03C3 (scale):",
                        min = 0, max = 5, value = 2, step = 1),
            sliderInput("epsilon_degree", "Degree of \u03B5 (skewness):",
                        min = 0, max = 5, value = 2, step = 1),
            sliderInput("delta_degree",   "Degree of \u03B4 (tail weight):",
                        min = 0, max = 5, value = 1, step = 1),

            checkboxInput("fix_delta", "Fix delta (instead of polynomial)?", value = FALSE),

            conditionalPanel(
              condition = "input.fix_delta == true",
              numericInput("delta_value", "Fixed delta value:",
                           value = 1, min = 0.1, max = 5, step = 0.1)
            )
          ),

          # Conditional UI for Beta-Binomial parameters
          conditionalPanel(
            condition = "input.distributionType == 'betabinomial'",
            tags$h4("Beta-Binomial Parameters"),
            tags$p("Specify polynomial degrees for alpha and beta:"),

            sliderInput("alpha_degree", "Degree of \u03B1:",
                        min = 0, max = 5, value = 3, step = 1),
            sliderInput("beta_degree",  "Degree of \u03B2:",
                        min = 0, max = 5, value = 3, step = 1)
          ),

          # Conditional UI for CMP parameters
          conditionalPanel(
            condition = "input.distributionType == 'cmp'",
            tags$h4("CMP Parameters"),
            tags$p("Specify polynomial degrees for location and dispersion:"),

            sliderInput("cmp_mu_degree", "Degree of \u03BC (location):",
                        min = 1, max = 5, value = 3, step = 1),
            sliderInput("cmp_nu_degree", "Degree of \u03BD (dispersion):",
                        min = 0, max = 5, value = 2, step = 1),
            tags$small("Note: Degree 0 fits a constant estimated dispersion."),

            tags$br(), tags$br(),

            checkboxInput("cmp_fix_nu", "Fix dispersion \u03BD (e.g. Poisson)?", value = FALSE),

            conditionalPanel(
              condition = "input.cmp_fix_nu == true",
              numericInput("cmp_nu_value", "Fixed \u03BD value (1 = Poisson):",
                           value = 1, min = 0.05, max = 20, step = 0.1)
            ),

            hr(),

            checkboxInput("cmp_use_max_score", "Right-truncate distribution (test ceiling)?", value = FALSE),

            conditionalPanel(
              condition = "input.cmp_use_max_score == true",
              numericInput("cmp_max_score", "Maximum attainable score (ceiling):",
                           value = NULL, min = 1, step = 1),
              tags$small("Items on the test sheet. Prevents probability mass beyond the ceiling.")
            )
          ),

          hr(),

          # Scale selection
          selectInput(
            "scale",
            "Norm Scale:",
            choices = c("T-scores (M=50, SD=10)"  = "T",
                        "IQ-scores (M=100, SD=15)" = "IQ",
                        "z-scores (M=0, SD=1)"    = "z"),
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
              tabPanel(
                "Percentile Plot",
                tags$br(),
                tags$p("Observed percentiles (points) vs. model predictions (lines). Wavy lines indicate overfit. Reduce polynomial degrees in this case."),
                withSpinner(
                  plotOutput("percentilePlot", width = "100%", height = "700px"),
                  type = 5
                )
              ),
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

    # ------------------------------------------------------------------
    # Tab 3: Norm Tables
    # ------------------------------------------------------------------
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

          # Score range for SHASH
          conditionalPanel(
            condition = "input.distributionType == 'shash'",
            numericInput("norm_start_shash", "Start raw score:", value = NULL, step = 1),
            numericInput("norm_end_shash",   "End raw score:",   value = NULL, step = 1),
            numericInput("norm_step_shash",  "Step size:",       value = 1, min = 0.1, step = 0.1)
          ),

          # Score range for CMP (count data: integer steps)
          conditionalPanel(
            condition = "input.distributionType == 'cmp'",
            tags$div(
              style = "padding: 10px; background-color: #f8f9fa; border-left: 4px solid #1b9e77; border-radius: 3px;",
              tags$p(tags$small(
                tags$b("Count Data:"), " Raw scores must be non-negative integers. Percentiles are evaluated via mid-p adjustment."
              ))
            ),
            tags$br(),
            numericInput("norm_start_cmp", "Start raw score:", value = 0, min = 0, step = 1),
            numericInput("norm_end_cmp",   "End raw score:",   value = NULL, min = 1, step = 1),
            numericInput("norm_step_cmp",  "Step size (integer):", value = 1, min = 1, step = 1)
          ),

          # Note and filter for Beta-Binomial
          conditionalPanel(
            condition = "input.distributionType == 'betabinomial'",
            tags$div(
              style = "padding: 10px; background-color: #f8f9fa; border-left: 4px solid #0275d8; border-radius: 3px;",
              tags$p(tags$small(
                tags$b("Note:"), " For the Beta-Binomial distribution, the norm table is generated automatically
                across the entire item range (0 to n). Start/End can optionally be used to restrict display."
              ))
            ),
            tags$br(),
            numericInput("norm_start_bb", "Optional: restrict table start:", value = NULL, step = 1),
            numericInput("norm_end_bb",   "Optional: restrict table end:",   value = NULL, step = 1)
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
