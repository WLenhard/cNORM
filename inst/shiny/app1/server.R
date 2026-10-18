# Required packages
library(shiny)
library(shinycssloaders)
library(cNORM)
library(DT)
library(foreign)
library(readxl)

data(elfe, package = "cNORM")
data(ppvt, package = "cNORM")
data(CDC,  package = "cNORM")

# Value of the "(none)" choice in the grouping-variable selector (see ui.R).
NO_GROUP <- "__none__"

# ---- Helpers -----------------------------------------------------------------

# Split user-entered numbers separated by comma, semicolon or whitespace.
parseNums <- function(s) {
  if (is.null(s) || !nzchar(trimws(s))) return(NULL)
  v <- suppressWarnings(as.numeric(strsplit(trimws(s), "[,;[:space:]]+")[[1]]))
  v <- v[is.finite(v)]
  if (length(v) == 0) NULL else v
}

# Percentiles entered as 0-100, returned as proportions (or NULL if none valid).
parsePercentiles <- function(s) {
  v <- parseNums(s)
  if (is.null(v)) return(NULL)
  v <- sort(unique(v[v > 0 & v < 100]))
  if (length(v) == 0) NULL else v / 100
}

# Read an uploaded file into a data frame.
readUserFile <- function(path, name) {
  ext <- tolower(tools::file_ext(name))
  df <- switch(ext,
               "csv" = {
                 d <- utils::read.csv2(path)                    # semicolon, decimal comma
                 if (ncol(d) == 1) d <- utils::read.csv(path)   # fall back to comma-separated
                 d
               },
               "sav" = suppressWarnings(
                 foreign::read.spss(path, to.data.frame = TRUE, use.value.labels = FALSE)
               ),
               "xlsx" = readxl::read_xlsx(path),
               "xls"  = readxl::read_xls(path),
               "rda"  = ,
               "rdata" = {
                 e <- new.env()
                 load(path, envir = e)
                 dfs <- Filter(is.data.frame, mget(ls(e), envir = e))
                 if (length(dfs) == 0) stop("The R data file contains no data frame.")
                 dfs[[1]]
               },
               stop("Unsupported file type: .", ext)
  )
  as.data.frame(df)
}

# Raw-score curve of a model WITHOUT age/group terms (raw = f(norm score)).
modelRawCurve <- function(m, n = 200) {
  L <- seq(m$minL1, m$maxL1, length.out = n)
  r <- vapply(L, function(x)
    cNORM::predictRaw(x, 1, coefficients = m$coefficients,
                      minRaw = -Inf, maxRaw = Inf), numeric(1))
  data.frame(norm = L, raw = r)
}

shinyServer(function(input, output, session) {
  session$allowReconnect(TRUE)
  # Only terminate the R process when running locally (cNORM.GUI());
  # on a server this would stop the app for all users.
  if (interactive()) session$onSessionEnded(function() stopApp())

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
    tryCatch(
      readUserFile(input$file$datapath, input$file$name),
      error = function(e) {
        showNotification(paste("File could not be read:", conditionMessage(e)),
                         type = "error", duration = 10)
        NULL
      }
    )
  })

  variableNames <- reactive({
    req(currentFile())
    names(currentFile())
  })

  # ---- Variable selectors ----------------------------------------------------
  output$GroupingVariable <- renderUI({
    nm  <- variableNames()
    sel <- if ("group" %in% nm) "group" else if ("Group" %in% nm) "Group" else NO_GROUP
    selectInput("InputGroupingVariable", "Grouping variable (e. g. age group, grade)",
                choices  = c("(none: no modelling over age/group)" = NO_GROUP, nm),
                selected = sel)
  })

  output$RawValues <- renderUI({
    nm  <- variableNames()
    sel <- if ("raw" %in% nm) "raw" else if ("Raw" %in% nm) "Raw" else NULL
    selectInput("InputRawValues", "Raw-score variable", choices = nm, selected = sel)
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
    req(currentFile(), input$InputRawValues, input$InputGroupingVariable)

    fail <- function(msg) showNotification(msg, type = "error", duration = 10)

    df       <- currentFile()
    raw      <- input$InputRawValues
    grp      <- input$InputGroupingVariable
    useGroup <- !identical(grp, NO_GROUP)
    scale    <- input$Scale
    k        <- as.numeric(input$NumberOfPowers)
    tAge     <- if (useGroup) as.numeric(input$NumberOfPowersAge) else 0
    nT       <- input$InputNumberOfTerms
    nTerms   <- if (is.null(nT) || is.na(nT) || nT < 1) 0L else as.integer(nT)

    # --- input checks ---
    if (!is.numeric(df[[raw]]))
      return(fail(paste0("The raw-score variable '", raw, "' is not numeric.")))
    if (useGroup) {
      if (identical(grp, raw))
        return(fail("Grouping variable and raw-score variable must be different."))
      if (!is.numeric(df[[grp]]))
        return(fail(paste0("The grouping variable '", grp, "' is not numeric.")))
    }

    # --- remove cases with missing values in the selected variables ---
    vars    <- if (useGroup) c(raw, grp) else raw
    keep    <- stats::complete.cases(df[, vars, drop = FALSE])
    nRemove <- sum(!keep)
    df      <- df[keep, , drop = FALSE]

    if (nrow(df) < 20)
      return(fail("Too few complete cases to fit a model (at least 20 needed)."))
    if (useGroup && length(unique(df[[grp]])) < 2)
      return(fail("The grouping variable has only one value. Select '(none)' instead."))

    res <- withProgress(message = "Fitting model ...", value = 0.5, {
      tryCatch({
        if (useGroup) {
          ranked   <- cNORM::rankByGroup(df, group = grp, raw = raw, scale = scale)
          prepared <- cNORM::computePowers(ranked, k = k, t = tAge, norm = "normValue")
          model    <- cNORM::bestModel(prepared, raw = raw, k = k, t = tAge,
                                       terms = nTerms, plot = FALSE)
        } else {
          # No grouping variable: ranks are computed in the total sample and
          # the model contains no age/group terms. 'group' and 't' are not provided.
          ranked   <- cNORM::rankByGroup(df, group = FALSE, raw = raw, scale = scale)
          prepared <- cNORM::computePowers(ranked, k = k, norm = "normValue")
          model    <- cNORM::bestModel(prepared, raw = raw, k = k,
                                       terms = nTerms, plot = FALSE)
        }
        list(data = prepared, model = model,
             group = if (useGroup) grp else NULL,
             raw = raw, scale = scale,
             hasGroup = useGroup, k = k, t = tAge,   # t = 0 without group
             n = nrow(df), nRemoved = nRemove)
      }, error = function(e) {
        fail(paste("Model could not be fitted:", conditionMessage(e)))
        NULL
      })
    })

    if (!is.null(res)) cnormResult(res)
  })

  preparedData   <- reactive({ req(cnormResult()); cnormResult()$data })
  bestModel      <- reactive({ req(cnormResult()); cnormResult()$model })
  chosenGrouping <- reactive({ req(cnormResult()); cnormResult()$group })
  chosenRaw      <- reactive({ req(cnormResult()); cnormResult()$raw })
  chosenScale    <- reactive({ req(cnormResult()); cnormResult()$scale })
  hasGroup       <- reactive({ req(cnormResult()); isTRUE(cnormResult()$hasGroup) })
  cnormObj       <- reactive({
    req(cnormResult())
    cNORM::buildCnormObject(preparedData(), bestModel())
  })

  # Observed range of the grouping variable (only meaningful with a group).
  ageRange <- reactive({
    req(cnormResult(), hasGroup())
    range(preparedData()[[chosenGrouping()]], na.rm = TRUE)
  })

  # Raw-score curve over the norm scores (only for models without group).
  rawCurve <- reactive({
    req(bestModel())
    modelRawCurve(bestModel())
  })

  # Keep the term sliders within the number of terms that is possible.
  observeEvent(cnormResult(), {
    res <- cnormResult()
    req(res)
    nmax <- max(1, (res$k + 1) * (res$t + 1) - 1)
    updateSliderInput(session, "terms",
                      max = nmax, value = min(isolate(input$terms), nmax))
    updateSliderInput(session, "MaxTermsCV",
                      max = nmax, value = min(isolate(input$MaxTermsCV), nmax))
  })

  # ---- Model report ----------------------------------------------------------
  output$ModelReport <- renderPrint({
    req(bestModel())
    res <- cnormResult()
    cat(sprintf("Cases used: %d (%d removed due to missing values)\n",
                res$n, res$nRemoved))
    if (res$hasGroup) {
      cat(sprintf("Grouping variable: %s (k = %d, t = %d)\n",
                  res$group, as.integer(res$k), as.integer(res$t)))
    } else {
      cat(sprintf("No grouping variable: modelled without age (k = %d)\n",
                  as.integer(res$k)))
    }
    cat("\n")
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
    inconsistent <- if (hasGroup()) {
      cNORM::checkConsistency(bestModel())
    } else {
      # no age dimension: raw score must increase monotonically with the norm score
      any(diff(rawCurve()$raw) < 0)
    }
    if (isTRUE(inconsistent))
      "⚠ Model may be inconsistent. Check the plot for intersecting curves / decreasing raw scores and try a different number of terms."
    else
      "✓ No consistency violations within the data range."
  })

  output$RangeCheck <- renderText({
    req(bestModel())
    m <- bestModel()
    if (hasGroup()) {
      cNORM::rangeCheck(m)
    } else {
      sprintf("Valid range: norm scores %.2f to %.2f, raw scores %s to %s. Do not extrapolate beyond this range.",
              m$minL1, m$maxL1, format(m$minRaw), format(m$maxRaw))
    }
  })

  # ---- Modeling diagnostics --------------------------------------------------
  # Raw score vs. norm score: observed data and model curve (no-group case).
  plotNoGroupFit <- function() {
    d  <- preparedData()
    cv <- rawCurve()
    plot(d$normValue, d[[chosenRaw()]], pch = 16, col = grDevices::rgb(0, 0, 0, .25),
         xlab = "Norm score", ylab = "Raw score",
         main = "Raw score by norm score (no grouping variable)")
    lines(cv$norm, cv$raw, col = "#d95f02", lwd = 3)
    legend("topleft", legend = c("Observed", "Model"),
           pch = c(16, NA), lty = c(NA, 1), lwd = c(NA, 3),
           col = c(grDevices::rgb(0, 0, 0, .5), "#d95f02"), bty = "n")
  }

  output$modelPlot <- renderPlot({
    req(cnormResult())
    if (!hasGroup()) {
      plotNoGroupFit()
    } else {
      rng <- ageRange()
      cNORM::plotPercentiles(cnormObj(), raw = chosenRaw(), group = chosenGrouping(),
                             minAge = rng[1], maxAge = rng[2])
    }
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
    req(cnormResult())
    if (!hasGroup()) {
      plotNoGroupFit()
    } else {
      rng <- ageRange()
      pct <- parsePercentiles(input$PercentilesForPercentiles)
      if (is.null(pct))
        cNORM::plotPercentiles(cnormObj(), raw = chosenRaw(), group = chosenGrouping(),
                               minAge = rng[1], maxAge = rng[2])
      else
        cNORM::plotPercentiles(cnormObj(), raw = chosenRaw(), group = chosenGrouping(),
                               minAge = rng[1], maxAge = rng[2], percentiles = pct)
    }
  })

  output$Series <- renderPlot({
    req(cnormObj())
    validate(need(hasGroup(), "This plot requires a grouping variable."))
    cNORM::plotPercentileSeries(cnormObj(), start = input$terms, end = input$terms)
  })

  output$NormCurves <- renderPlot({
    req(bestModel())
    validate(need(hasGroup(), "This plot requires a grouping variable."))
    rng <- ageRange()
    pct <- parsePercentiles(input$PercentilesForNormCurves)
    if (is.null(pct)) pct <- c(.02276, .1587, .5, .8413, .97724)
    norms <- switch(chosenScale(),
                    "T"  = qnorm(pct, 50, 10),
                    "z"  = qnorm(pct),
                    "IQ" = qnorm(pct, 100, 15))
    cNORM::plotNormCurves(bestModel(), minAge = rng[1], maxAge = rng[2],
                          normList = round(norms, 2))
  })

  output$PlotDensity <- renderPlot({
    req(bestModel())
    if (!hasGroup()) {
      x <- preparedData()[[chosenRaw()]]
      hist(x, breaks = "Sturges", col = "grey80", border = "white",
           xlab = "Raw score", main = "Distribution of raw scores")
    } else {
      grps <- parseNums(input$densities)
      if (is.null(grps)) cNORM::plotDensity(bestModel())
      else cNORM::plotDensity(bestModel(), group = grps)
    }
  })

  output$PlotDerivatives <- renderPlot({
    req(bestModel())
    if (!hasGroup()) {
      cv <- rawCurve()
      d  <- diff(cv$raw) / diff(cv$norm)
      x  <- cv$norm[-1] - diff(cv$norm) / 2
      plot(x, d, type = "l", lwd = 3, col = "#2c7fb8",
           xlab = "Norm score", ylab = "d raw / d norm",
           main = "First derivative of the raw score over the norm score")
      abline(h = 0, lty = 2, col = "red")
    } else {
      rng <- ageRange()
      cNORM::plotDerivative(bestModel(), minAge = rng[1], maxAge = rng[2])
    }
  })

  output$PlotNormScores <- renderPlot({
    req(cnormObj())
    type <- if (isTRUE(input$differences)) 1 else 0
    cNORM::plotNorm(cnormObj(), group = hasGroup() && isTRUE(input$grouping), type = type)
  }) |> bindEvent(input$normScoreButton, ignoreNULL = FALSE)

  output$PlotRawScores <- renderPlot({
    req(cnormObj())
    type <- if (isTRUE(input$differences1)) 1 else 0
    if (hasGroup() && isTRUE(input$grouping1))
      cNORM::plotRaw(cnormObj(), group = TRUE, type = type)
    else
      cNORM::plotRaw(cnormObj(), type = type)
  }) |> bindEvent(input$rawScoreButton, ignoreNULL = FALSE)

  # ---- Cross validation ------------------------------------------------------
  # Note: cnorm.cv() draws its plot while it is computing. The plot output is
  # defined (and displayed) before the table, so it triggers the computation
  # first and the plot ends up in the right place.
  cvResult <- eventReactive(input$CrossValidation, {
    req(cnormResult())
    res <- cnormResult()
    args <- list(repetitions = input$RepetitionsCV,
                 norms       = input$NormsCV,
                 min         = 1,
                 max         = input$MaxTermsCV,
                 raw         = res$raw,
                 k           = res$k)    # pass the user's settings, if supported
    if (res$hasGroup) {
      args$group <- res$group
      args$age   <- res$group
      args$t     <- res$t
    }
    fm <- names(formals(cNORM::cnorm.cv))
    if (!"..." %in% fm) args <- args[names(args) %in% fm]

    withProgress(message = "Running cross-validation ...", value = 0.5, {
      tryCatch(
        do.call(cNORM::cnorm.cv, c(list(preparedData()), args)),
        error = function(e) {
          showNotification(paste("Cross-validation failed:", conditionMessage(e)),
                           type = "error", duration = 10)
          NULL
        }
      )
    })
  })
  output$PlotCV  <- renderPlot({ req(cvResult()); invisible(cvResult()) })
  output$TableCV <- DT::renderDT({ req(cvResult()); cvResult() })

  # ---- Predictions -----------------------------------------------------------
  # Without a grouping variable, no age is required: the dummy group value 1 is used.
  output$InputNormValue <- renderUI({
    req(bestModel())
    m <- bestModel()
    tagList(
      if (hasGroup())
        numericInput("NormValueInputAge", "Age / group", value = NULL,
                     min = m$minA1, max = m$maxA1),
      numericInput("NormValueInputRaw", "Raw score", value = NULL,
                   min = m$minRaw, max = m$maxRaw),
      actionButton("CalcNormValue", "Compute norm score")
    )
  })

  output$NormValue <- renderText({
    m   <- bestModel()
    age <- if (hasGroup()) input$NormValueInputAge else 1
    req(age, input$NormValueInputRaw)
    v <- cNORM::predictNorm(input$NormValueInputRaw, age,
                            model = m, minNorm = m$minL1, maxNorm = m$maxL1)
    out <- paste("Predicted norm score:", round(v, 2))
    if (hasGroup() && (age < m$minA1 || age > m$maxA1))
      out <- paste0(out, "\nNote: age/group is outside the observed range (",
                    m$minA1, " to ", m$maxA1, "); this is an extrapolation.")
    out
  }) |> bindEvent(input$CalcNormValue)

  output$InputRawValue <- renderUI({
    req(bestModel())
    m <- bestModel()
    tagList(
      if (hasGroup())
        numericInput("RawValueInputAge", "Age / group", value = NULL,
                     min = m$minA1, max = m$maxA1),
      numericInput("RawValueInputNorm", "Norm score", value = NULL,
                   min = m$minL1, max = m$maxL1),
      actionButton("CalcRawValue", "Compute raw score")
    )
  })

  output$RawValue <- renderText({
    m   <- bestModel()
    age <- if (hasGroup()) input$RawValueInputAge else 1
    req(age, input$RawValueInputNorm)
    v <- cNORM::predictRaw(input$RawValueInputNorm, age,
                           coefficients = m$coefficients,
                           minRaw = m$minRaw, maxRaw = m$maxRaw)
    out <- paste("Predicted raw score:", round(v, 2))
    if (hasGroup() && (age < m$minA1 || age > m$maxA1))
      out <- paste0(out, "\nNote: age/group is outside the observed range (",
                    m$minA1, " to ", m$maxA1, "); this is an extrapolation.")
    out
  }) |> bindEvent(input$CalcRawValue)

  output$InputNormTable <- renderUI({
    req(bestModel())
    m <- bestModel()
    tagList(
      if (hasGroup())
        numericInput("NormTableInput", "Age / group", value = NULL,
                     min = m$minA1, max = m$maxA1),
      numericInput("NormTableInputStart",    "Norm start",  value = m$scaleM - 2.5 * m$scaleSD),
      numericInput("NormTableInputEnd",      "Norm end",    value = m$scaleM + 2.5 * m$scaleSD),
      numericInput("NormTableInputStepping", "Step width",
                   value = if (identical(chosenScale(), "z")) 0.1 else 1),
      numericInput("NormTableCI",            "Confidence",  value = 0.9),
      numericInput("NormTableRel",           "Reliability", value = NULL),
      actionButton("CalcNormTables", "Generate norm table"),
      downloadButton("DownloadNormTable", "Download")
    )
  })

  normTable <- eventReactive(input$CalcNormTables, {
    m   <- bestModel()
    age <- if (hasGroup()) input$NormTableInput else 1
    req(age, input$NormTableInputStart, input$NormTableInputEnd,
        input$NormTableInputStepping)
    tab <- cNORM::normTable(age, m,
                            minNorm = input$NormTableInputStart,
                            maxNorm = input$NormTableInputEnd,
                            minRaw  = m$minRaw, maxRaw = m$maxRaw,
                            step    = input$NormTableInputStepping,
                            CI          = if (is.na(input$NormTableCI))  NULL else input$NormTableCI,
                            reliability = if (is.na(input$NormTableRel)) NULL else input$NormTableRel,
                            pretty = TRUE)
    if ("raw"        %in% names(tab)) tab$raw        <- round(tab$raw, 2)
    if ("percentile" %in% names(tab)) tab$percentile <- round(tab$percentile, 2)
    tab
  })

  output$NormTable         <- DT::renderDT({ normTable() })
  output$DownloadNormTable <- downloadHandler(
    filename = function() "NormTable.csv",
    content  = function(file) utils::write.csv(normTable(), file, row.names = FALSE)
  )

  output$InputRawTable <- renderUI({
    req(bestModel())
    m <- bestModel()
    tagList(
      if (hasGroup())
        numericInput("RawTableInput", "Age / group", value = NULL,
                     min = m$minA1, max = m$maxA1),
      numericInput("RawTableInputStart",    "Raw start",   value = m$minRaw),
      numericInput("RawTableInputEnd",      "Raw end",     value = m$maxRaw),
      numericInput("RawTableInputStepping", "Step width",  value = 1),
      numericInput("RawTableCI",            "Confidence",  value = 0.9),
      numericInput("RawTableRel",           "Reliability", value = NULL),
      actionButton("CalcRawTables", "Generate raw table"),
      downloadButton("DownloadRawTable", "Download")
    )
  })

  rawTable <- eventReactive(input$CalcRawTables, {
    m   <- bestModel()
    age <- if (hasGroup()) input$RawTableInput else 1
    req(age, input$RawTableInputStart, input$RawTableInputEnd,
        input$RawTableInputStepping)
    tab <- cNORM::rawTable(age, m,
                           minNorm = m$minL1, maxNorm = m$maxL1,
                           minRaw  = input$RawTableInputStart,
                           maxRaw  = input$RawTableInputEnd,
                           step    = input$RawTableInputStepping,
                           CI          = if (is.na(input$RawTableCI))  NULL else input$RawTableCI,
                           reliability = if (is.na(input$RawTableRel)) NULL else input$RawTableRel,
                           pretty = TRUE)
    if ("norm"       %in% names(tab)) tab$norm       <- round(tab$norm, 3)
    if ("percentile" %in% names(tab)) tab$percentile <- round(tab$percentile, 2)
    tab
  })

  output$RawTable         <- DT::renderDT({ rawTable() })
  output$DownloadRawTable <- downloadHandler(
    filename = function() "RawTable.csv",
    content  = function(file) utils::write.csv(rawTable(), file, row.names = FALSE)
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
