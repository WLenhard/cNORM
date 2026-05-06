# Required packages
library(shiny)
library(shinycssloaders)
library(cNORM)
library(DT)
library(markdown)
library(foreign)
library(readxl)

data(elfe, package = "cNORM")
data(ppvt, package = "cNORM")
data(CDC,  package = "cNORM")

# Helper: split user-entered numbers separated by space/comma
parseNums <- function(s) {
  if (is.null(s) || !nzchar(trimws(s))) return(NULL)
  as.numeric(unlist(strsplit(s, "[,\\s]+", perl = TRUE)))
}

shinyServer(function(input, output, session) {

  session$onSessionEnded(function() stopApp())

  # ---- Data loading ----------------------------------------------------------
  currentFile <- reactive({
    if (is.null(input$file)) {
      ex <- input$Example
      if (is.null(ex) || ex == "") return(NULL)
      return(switch(ex,
                    "elfe" = as.data.frame(elfe),
                    "ppvt" = as.data.frame(ppvt),
                    "CDC"  = as.data.frame(CDC),
                    NULL))
    }
    path <- input$file$datapath
    ext  <- tolower(tools::file_ext(path))
    switch(ext,
           "csv"  = as.data.frame(read.csv2(path)),
           "sav"  = as.data.frame(foreign::read.spss(path, to.data.frame = TRUE)),
           "xlsx" = as.data.frame(readxl::read_xlsx(path)),
           "xls"  = as.data.frame(readxl::read_xls(path)),
           "rda"  = { e <- new.env(); load(path, e); get(ls(e)[1], envir = e) },
           NULL)
  })

  variableNames <- reactive({
    req(currentFile())
    names(currentFile())
  })

  # ---- Variable selectors ----------------------------------------------------
  output$GroupingVariable <- renderUI({
    nm  <- variableNames()
    sel <- if ("group" %in% nm) "group" else if ("Group" %in% nm) "Group" else NULL
    selectInput("InputGroupingVariable", "Grouping variable", choices = nm, selected = sel)
  })

  output$RawValues <- renderUI({
    nm  <- variableNames()
    sel <- if ("raw" %in% nm) "raw" else if ("Raw" %in% nm) "Raw" else NULL
    selectInput("InputRawValues", "Raw-score variable", choices = nm, selected = sel)
  })

  # ---- Intro / data preview --------------------------------------------------
  output$introduction <- renderUI({
    if (is.null(currentFile())) includeHTML("www/introduction.html") else NULL
  })

  output$table <- DT::renderDT({
    req(currentFile())
    currentFile()
  }, options = list(pageLength = 10, scrollX = TRUE))

  # ---- Combined preparation + modeling --------------------------------------
  # Holds the current model + prepared data; NULL means "no model yet".
  cnormResult <- reactiveVal(NULL)

  # Whenever a new dataset is loaded (file or example), drop the old model.
  observeEvent(currentFile(), {
    cnormResult(NULL)
  }, ignoreNULL = FALSE)

  # Also clear if the user changes a key model setting after running once,
  # so stale plots can't be shown against new parameters.
  observeEvent(list(input$InputGroupingVariable,
                    input$InputRawValues,
                    input$Scale,
                    input$NumberOfPowers,
                    input$NumberOfPowersAge,
                    input$InputNumberOfTerms), {
                      cnormResult(NULL)
                    }, ignoreInit = TRUE)

  # Build the model when the user clicks "Run model".
  observeEvent(input$RunModel, {
    req(currentFile(), input$InputGroupingVariable, input$InputRawValues)

    df    <- currentFile()
    grp   <- input$InputGroupingVariable
    raw   <- input$InputRawValues
    scale <- input$Scale
    k     <- as.numeric(input$NumberOfPowers)
    t     <- as.numeric(input$NumberOfPowersAge)
    nT    <- input$InputNumberOfTerms

    ranked   <- cNORM::rankByGroup(df, group = grp, raw = raw, scale = scale)
    prepared <- cNORM::computePowers(ranked, k = k, t = t, norm = "normValue")

    model <- if (is.null(nT) || is.na(nT) || nT < 1) {
      cNORM::bestModel(prepared, raw = raw, k = k, t = t,
                       terms = 0, plot = FALSE)
    } else {
      cNORM::bestModel(prepared, raw = raw, k = k, t = t,
                       terms = as.integer(nT), plot = FALSE)
    }

    cnormResult(list(data = prepared, model = model,
                     group = grp, raw = raw, scale = scale))
  })

  preparedData   <- reactive({ req(cnormResult()); cnormResult()$data })
  bestModel      <- reactive({ req(cnormResult()); cnormResult()$model })
  chosenGrouping <- reactive({ req(cnormResult()); cnormResult()$group })
  chosenRaw      <- reactive({ req(cnormResult()); cnormResult()$raw })
  chosenScale    <- reactive({ req(cnormResult()); cnormResult()$scale })
  cnormObj       <- reactive({
    req(cnormResult())
    cNORM::buildCnormObject(preparedData(), bestModel())
  })

  ageRange <- reactive({
    req(currentFile(), chosenGrouping())
    range(currentFile()[[chosenGrouping()]], na.rm = TRUE)
  })

  # ---- Model report ----------------------------------------------------------
  output$ModelReport <- renderPrint({
    req(bestModel())
    cat(bestModel()$report, sep = "\n")
  })

  output$ModelReportUI <- renderUI({
    if (is.null(cnormResult())) {
      tags$div(class = "alert alert-info",
               "No model yet. Choose your variables and click ",
               tags$b("Run model"), ".")
    } else {
      verbatimTextOutput("ModelReport")
    }
  })

  output$ConsistencyCheck <- renderText({
    req(bestModel())
    if (cNORM::checkConsistency(bestModel()))
      "⚠ Model may be inconsistent. Check the percentile plot for intersecting curves and try a different number of terms."
    else
      "✓ No consistency violations within the data range."
  })

  output$RangeCheck <- renderText({
    req(bestModel())
    cNORM::rangeCheck(bestModel())
  })

  # ---- Modeling diagnostics --------------------------------------------------
  output$modelPlot <- renderPlot({
    rng <- ageRange()
    cNORM::plotPercentiles(cnormObj(), raw = chosenRaw(), group = chosenGrouping(),
                           minAge = rng[1], maxAge = rng[2])
  })

  chosenTypeOfPlotSubset <- reactive({
    m <- c("R2 by Number of Predictors",
           "Mallow's (log) Cp by R2",
           "Bayesian Information Criterion (BIC) by R2",
           "RMSE by Number of Predictors")
    match(input$chosenTypePlotSubset, m) - 1
  })

  output$PlotWL <- renderPlot({
    cNORM::plotSubset(bestModel(), type = chosenTypeOfPlotSubset())
  })

  output$PrintSubset <- DT::renderDT({
    cNORM::printSubset(bestModel())
  }, options = list(pageLength = 10, scrollX = TRUE))

  # ---- Visualization ---------------------------------------------------------
  output$PlotPercentiles <- renderPlot({
    rng <- ageRange()
    pct <- parseNums(input$PercentilesForPercentiles) / 100
    if (is.null(pct))
      cNORM::plotPercentiles(cnormObj(), raw = chosenRaw(), group = chosenGrouping(),
                             minAge = rng[1], maxAge = rng[2])
    else
      cNORM::plotPercentiles(cnormObj(), raw = chosenRaw(), group = chosenGrouping(),
                             minAge = rng[1], maxAge = rng[2], percentiles = pct)
  })

  output$Series <- renderPlot({
    cNORM::plotPercentileSeries(cnormObj(), start = input$terms, end = input$terms)
  })

  output$NormCurves <- renderPlot({
    rng <- ageRange()
    pct <- parseNums(input$PercentilesForNormCurves) / 100
    if (is.null(pct)) pct <- c(.02276, .1587, .5, .8413, .97724)
    norms <- switch(chosenScale(),
                    "T"  = qnorm(pct, 50, 10),
                    "z"  = qnorm(pct),
                    "IQ" = qnorm(pct, 100, 15))
    cNORM::plotNormCurves(bestModel(), minAge = rng[1], maxAge = rng[2],
                          normList = round(norms, 2))
  })

  output$PlotDensity <- renderPlot({
    grps <- parseNums(input$densities)
    if (is.null(grps)) cNORM::plotDensity(bestModel())
    else cNORM::plotDensity(bestModel(), group = grps)
  })

  output$PlotDerivatives <- renderPlot({
    rng <- ageRange()
    cNORM::plotDerivative(bestModel(), minAge = rng[1], maxAge = rng[2])
  })

  output$PlotNormScores <- renderPlot({
    input$normScoreButton
    type <- if (isTRUE(input$differences)) 1 else 0
    cNORM::plotNorm(cnormObj(), group = isTRUE(input$grouping), type = type)
  }) |> bindEvent(input$normScoreButton)

  output$PlotRawScores <- renderPlot({
    input$rawScoreButton
    type <- if (isTRUE(input$differences1)) 1 else 0
    if (isTRUE(input$grouping1))
      cNORM::plotRaw(cnormObj(), group = TRUE, type = type)
    else
      cNORM::plotRaw(cnormObj(), type = type)
  }) |> bindEvent(input$rawScoreButton)

  # ---- Cross validation ------------------------------------------------------
  cvResult <- eventReactive(input$CrossValidation, {
    cNORM::cnorm.cv(preparedData(),
                    repetitions = input$RepetitionsCV,
                    norms = input$NormsCV,
                    min = 1, max = input$MaxTermsCV,
                    group = chosenGrouping(),
                    raw   = chosenRaw(),
                    age   = chosenGrouping())
  })
  output$PlotCV  <- renderPlot({ cvResult() })
  output$TableCV <- DT::renderDT({ cvResult() })

  # ---- Predictions -----------------------------------------------------------
  output$InputNormValue <- renderUI({
    req(bestModel())
    tagList(
      numericInput("NormValueInputAge", "Age",  value = NULL,
                   min = bestModel()$minA1,  max = bestModel()$maxA1),
      numericInput("NormValueInputRaw", "Raw score", value = NULL,
                   min = bestModel()$minRaw, max = bestModel()$maxRaw),
      actionButton("CalcNormValue", "Compute norm score")
    )
  })

  output$NormValue <- renderText({
    input$CalcNormValue
    isolate({
      req(input$NormValueInputAge, input$NormValueInputRaw)
      m <- bestModel()
      v <- cNORM::predictNorm(input$NormValueInputRaw, input$NormValueInputAge,
                              model = m, minNorm = m$minL1, maxNorm = m$maxL1)
      paste("Predicted norm score:", round(v, 2))
    })
  }) |> bindEvent(input$CalcNormValue)

  output$InputRawValue <- renderUI({
    req(bestModel())
    tagList(
      numericInput("RawValueInputAge", "Age", value = NULL,
                   min = bestModel()$minA1, max = bestModel()$maxA1),
      numericInput("RawValueInputNorm", "Norm score", value = NULL,
                   min = bestModel()$minL1, max = bestModel()$maxL1),
      actionButton("CalcRawValue", "Compute raw score")
    )
  })

  output$RawValue <- renderText({
    input$CalcRawValue
    isolate({
      req(input$RawValueInputAge, input$RawValueInputNorm)
      m <- bestModel()
      v <- cNORM::predictRaw(input$RawValueInputNorm, input$RawValueInputAge,
                             coefficients = m$coefficients,
                             minRaw = m$minRaw, maxRaw = m$maxRaw)
      paste("Predicted raw score:", round(v, 2))
    })
  }) |> bindEvent(input$CalcRawValue)

  output$InputNormTable <- renderUI({
    req(bestModel())
    m <- bestModel()
    tagList(
      numericInput("NormTableInput",         "Age",            value = NULL,
                   min = 0, max = m$maxA1),
      numericInput("NormTableInputStart",    "Norm start",     value = m$scaleM - 2.5 * m$scaleSD),
      numericInput("NormTableInputEnd",      "Norm end",       value = m$scaleM + 2.5 * m$scaleSD),
      numericInput("NormTableInputStepping", "Step width",     value = NULL),
      numericInput("NormTableCI",            "Confidence",     value = 0.9),
      numericInput("NormTableRel",           "Reliability",    value = NULL),
      actionButton("CalcNormTables", "Generate norm table"),
      downloadButton("DownloadNormTable", "Download")
    )
  })

  normTable <- eventReactive(input$CalcNormTables, {
    req(input$NormTableInput, input$NormTableInputStart,
        input$NormTableInputEnd, input$NormTableInputStepping)
    m <- bestModel()
    tab <- cNORM::normTable(input$NormTableInput, m,
                            minNorm = input$NormTableInputStart,
                            maxNorm = input$NormTableInputEnd,
                            minRaw  = m$minRaw, maxRaw = m$maxRaw,
                            step    = input$NormTableInputStepping,
                            CI          = if (is.na(input$NormTableCI))  NULL else input$NormTableCI,
                            reliability = if (is.na(input$NormTableRel)) NULL else input$NormTableRel,
                            pretty = TRUE)
    tab$raw        <- round(tab$raw, 2)
    tab$percentile <- round(tab$percentile, 2)
    tab
  })

  output$NormTable         <- DT::renderDT({ normTable() })
  output$DownloadNormTable <- downloadHandler(
    filename = function() "NormTable.csv",
    content  = function(file) write.csv(normTable(), file, row.names = FALSE)
  )

  output$InputRawTable <- renderUI({
    req(bestModel())
    m <- bestModel()
    tagList(
      numericInput("RawTableInput",         "Age",         value = NULL,
                   min = 0, max = m$maxA1),
      numericInput("RawTableInputStart",    "Raw start",   value = m$minRaw),
      numericInput("RawTableInputEnd",      "Raw end",     value = m$maxRaw),
      numericInput("RawTableInputStepping", "Step width",  value = NULL),
      numericInput("RawTableCI",            "Confidence",  value = 0.9),
      numericInput("RawTableRel",           "Reliability", value = NULL),
      actionButton("CalcRawTables", "Generate raw table"),
      downloadButton("DownloadRawTable", "Download")
    )
  })

  rawTable <- eventReactive(input$CalcRawTables, {
    req(input$RawTableInput, input$RawTableInputStart,
        input$RawTableInputEnd, input$RawTableInputStepping)
    m <- bestModel()
    tab <- cNORM::rawTable(input$RawTableInput, m,
                           minNorm = m$minL1, maxNorm = m$maxL1,
                           minRaw  = input$RawTableInputStart,
                           maxRaw  = input$RawTableInputEnd,
                           step    = input$RawTableInputStepping,
                           CI          = if (is.na(input$RawTableCI))  NULL else input$RawTableCI,
                           reliability = if (is.na(input$RawTableRel)) NULL else input$RawTableRel,
                           pretty = TRUE)
    tab$norm       <- round(tab$norm, 3)
    tab$percentile <- round(tab$percentile, 2)
    tab
  })

  output$RawTable         <- DT::renderDT({ rawTable() })
  output$DownloadRawTable <- downloadHandler(
    filename = function() "RawTable.csv",
    content  = function(file) write.csv(rawTable(), file, row.names = FALSE)
  )

  # ---- Model export ----------------------------------------------------------
  output$downloadModel <- downloadHandler(
    filename = function() "cnorm_model.RData",
    content  = function(file) {
      model.cnorm <- bestModel()
      data.cnorm  <- preparedData()
      save(model.cnorm, data.cnorm, file = file)
    }
  )
})
