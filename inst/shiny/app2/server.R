library(shiny)
library(cNORM)
library(DT)
library(ggplot2)

shinyServer(function(input, output, session) {

  # Reactive values to store data and model
  rv <- reactiveValues(
    data = NULL,
    model = NULL,
    normTables = NULL
  )

  # ============================================================================
  # DATA INPUT TAB
  # ============================================================================

  # Load data based on selection
  observeEvent(input$Example, {
    req(input$Example)

    if (input$Example == "elfe") {
      rv$data <- elfe
    } else if (input$Example == "ppvt") {
      rv$data <- ppvt
    } else if (input$Example == "CDC") {
      rv$data <- CDC
    }

    # Invalidate model when new data is loaded
    rv$model <- NULL
    rv$normTables <- NULL
  })

  # Load data from file
  observeEvent(input$file, {
    req(input$file)

    tryCatch({
      ext <- tools::file_ext(input$file$name)

      if (ext == "csv") {
        rv$data <- read.csv(input$file$datapath)
      } else if (ext %in% c("xlsx", "xls")) {
        rv$data <- readxl::read_excel(input$file$datapath)
      } else if (ext == "rda" || ext == "RData") {
        load(input$file$datapath)
        rv$data <- get(ls()[1])
      } else if (ext == "sav") {
        rv$data <- haven::read_sav(input$file$datapath)
      }

      # Invalidate model when new data is loaded
      rv$model <- NULL
      rv$normTables <- NULL

      showNotification("Data loaded successfully!", type = "message")
    }, error = function(e) {
      showNotification(paste("Error loading file:", e$message), type = "error")
    })
  })

  # Display data preview
  output$dataPreview <- DT::renderDT({
    req(rv$data)

    DT::datatable(
      rv$data,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        dom = 'Bfrtip'
      ),
      rownames = FALSE
    )
  })

  # ============================================================================
  # MODELING TAB - Dynamic UI
  # ============================================================================

  # Age variable selector
  output$ageVariable <- renderUI({
    req(rv$data)

    selectInput(
      "age_var",
      "Age Variable:",
      choices = names(rv$data),
      selected = names(rv$data)[1]
    )
  })

  # Score variable selector
  output$scoreVariable <- renderUI({
    req(rv$data)

    selectInput(
      "score_var",
      "Raw Score Variable:",
      choices = names(rv$data),
      selected = if(ncol(rv$data) > 1) names(rv$data)[2] else names(rv$data)[1]
    )
  })

  # Weight variable selector
  output$weightVariable <- renderUI({
    req(rv$data)

    selectInput(
      "weight_var",
      "Weight Variable (optional):",
      choices = c("None" = "", names(rv$data)),
      selected = ""
    )
  })

  # ============================================================================
  # MODELING TAB - Compute Model
  # ============================================================================

  observeEvent(input$computeModel, {
    req(rv$data, input$age_var, input$score_var)

    # Show progress
    withProgress(message = 'Fitting model...', value = 0, {

      tryCatch({
        # Extract variables
        age <- rv$data[[input$age_var]]
        score <- rv$data[[input$score_var]]

        # Handle weights
        weights <- NULL
        if (!is.null(input$weight_var) && input$weight_var != "") {
          weights <- rv$data[[input$weight_var]]
        }

        incProgress(0.2, detail = "Preparing data...")

        # Fit model based on distribution type
        if (input$distributionType == "shash") {

          # Determine delta parameter
          delta_deg <- if (input$fix_delta) NULL else input$delta_degree
          delta_val <- if (input$fix_delta) input$delta_value else 1

          incProgress(0.3, detail = "Fitting SHASH model...")

          rv$model <- cnorm.shash(
            age = age,
            score = score,
            weights = weights,
            mu_degree = input$mu_degree,
            sigma_degree = input$sigma_degree,
            epsilon_degree = input$epsilon_degree,
            delta_degree = delta_deg,
            delta = delta_val,
            scale = input$scale,
            plot = FALSE
          )

        } else if (input$distributionType == "betabinomial") {

          incProgress(0.3, detail = "Fitting Beta-Binomial model...")

          rv$model <- cnorm.betabinomial(
            age = age,
            score = score,
            weights = weights,
            alpha = input$alpha_degree,
            beta = input$beta_degree,
            scale = input$scale,
            plot = FALSE
          )
        }

        incProgress(1, detail = "Model fitted!")

        showNotification("Model computed successfully!", type = "message")

      }, error = function(e) {
        showNotification(
          paste("Error fitting model:", e$message),
          type = "error",
          duration = 10
        )
      })
    })
  })

  # ============================================================================
  # MODELING TAB - Outputs
  # ============================================================================

  # Model summary
  output$modelSummary <- renderPrint({
    req(rv$model)
    summary(rv$model, age = rv$data[[input$age_var]], score = rv$data[[input$score_var]])
  })

  # Percentile plot
  output$percentilePlot <- renderPlot({
    req(rv$model, rv$data, input$age_var, input$score_var)

    weights <- NULL
    if (!is.null(input$weight_var) && input$weight_var != "") {
      weights <- rv$data[[input$weight_var]]
    }

    plot(rv$model,
         age = rv$data[[input$age_var]],
         score = rv$data[[input$score_var]],
         weights = weights,
         points = TRUE)
  })

  # Model diagnostics
  output$modelDiagnostics <- renderPrint({
    req(rv$model)

    if (input$distributionType == "shash") {
      diag <- diagnostics.shash(rv$model,
                                age = rv$data[[input$age_var]],
                                score = rv$data[[input$score_var]])
    } else {
      diag <- diagnostics.betabinomial(rv$model,
                                       age = rv$data[[input$age_var]],
                                       score = rv$data[[input$score_var]])
    }

    print(diag)
  })

  # Derivative plot (SHASH only)
  output$derivativePlot <- renderPlot({
    req(rv$model, input$distributionType == "shash")

    plotDerivative(rv$model, age = rv$data[[input$age_var]])
  })

  # Download model
  output$downloadModel <- downloadHandler(
    filename = function() {
      paste0("cnorm_model_", input$distributionType, "_", Sys.Date(), ".rds")
    },
    content = function(file) {
      saveRDS(rv$model, file)
    }
  )

  # ============================================================================
  # NORM TABLES TAB
  # ============================================================================

  # Auto-populate score range when data/score variable changes
  observe({
    req(rv$data, input$score_var)

    score <- rv$data[[input$score_var]]

    updateNumericInput(
      session,
      "norm_start",
      value = floor(min(score, na.rm = TRUE))
    )

    updateNumericInput(
      session,
      "norm_end",
      value = ceiling(max(score, na.rm = TRUE))
    )
  })

  observeEvent(input$generateTables, {
    req(rv$model, input$norm_ages)

    withProgress(message = 'Generating norm tables...', value = 0, {

      tryCatch({
        # Parse ages
        ages_text <- gsub(",", " ", input$norm_ages)
        ages <- as.numeric(unlist(strsplit(ages_text, "\\s+")))
        ages <- ages[!is.na(ages)]

        if (length(ages) == 0) {
          showNotification("Please enter valid ages", type = "error")
          return(NULL)
        }

        incProgress(0.3, detail = "Computing percentiles...")

        # Set score range
        start_score <- input$norm_start
        end_score <- input$norm_end

        if (is.null(start_score)) start_score <- attr(rv$model$result, "min")
        if (is.null(end_score)) end_score <- attr(rv$model$result, "max")

        # Generate tables
        if (input$distributionType == "shash") {

          ci_val <- if (input$include_ci) input$ci_level else NULL
          rel_val <- if (input$include_ci) input$reliability else NULL

          rv$normTables <- normTable.shash(
            rv$model,
            ages = ages,
            start = start_score,
            end = end_score,
            step = input$norm_step,
            CI = ci_val,
            reliability = rel_val
          )

        } else if (input$distributionType == "betabinomial") {

          ci_val <- if (input$include_ci) input$ci_level else NULL
          rel_val <- if (input$include_ci) input$reliability else NULL

          rv$normTables <- normTable.betabinomial(
            rv$model,
            ages = ages,
            start = start_score,
            end = end_score,
            step = input$norm_step,
            CI = ci_val,
            reliability = rel_val
          )
        }

        incProgress(1, detail = "Done!")

        showNotification("Norm tables generated!", type = "message")

      }, error = function(e) {
        showNotification(
          paste("Error generating tables:", e$message),
          type = "error",
          duration = 10
        )
      })
    })
  })

  # Display norm tables
  output$normTablesUI <- renderUI({
    req(rv$normTables)

    table_tabs <- lapply(names(rv$normTables), function(age_name) {
      tabPanel(
        paste("Age", age_name),
        tags$br(),
        DT::DTOutput(paste0("normTable_", age_name))
      )
    })

    do.call(tabsetPanel, table_tabs)
  })

  # Render individual norm tables
  # Render individual norm tables
  observe({
    req(rv$normTables)

    lapply(names(rv$normTables), function(age_name) {
      output[[paste0("normTable_", age_name)]] <- DT::renderDT({

        # Identify columns to round
        cols_to_round <- c("Px", "Pcum", "Percentile", "z", "norm")

        # Add CI columns if they exist
        ci_cols <- c("lowerCI", "upperCI", "lowerCI_PR", "upperCI_PR")
        existing_ci_cols <- ci_cols[ci_cols %in% names(rv$normTables[[age_name]])]
        cols_to_round <- c(cols_to_round, existing_ci_cols)

        DT::datatable(
          rv$normTables[[age_name]],
          options = list(
            pageLength = 25,
            scrollX = TRUE,
            dom = 'Bfrtip'
          ),
          rownames = FALSE
        ) %>%
          DT::formatRound(columns = cols_to_round, digits = 2)
      })
    })
  })

  # Download norm tables
  output$downloadTables <- downloadHandler(
    filename = function() {
      paste0("norm_tables_", input$distributionType, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(rv$normTables)

      # Combine all tables with age column
      combined <- do.call(rbind, lapply(names(rv$normTables), function(age_name) {
        df <- rv$normTables[[age_name]]
        df$age <- as.numeric(age_name)
        df
      }))

      write.csv(combined, file, row.names = FALSE)
    }
  )
})
