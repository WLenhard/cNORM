library(shiny)
library(cNORM)

# Define server logic required for cNORM-Application
shinyServer(function(input, output, session) {

  # Stops program by closing browser tab
  session$onSessionEnded(function() {
    stopApp()
  })


# Returnss currently chosen file; returns null,if no file was chosen
  currentFile <- reactive({

    inFile <- input$file
    exData <- input$Example

    # Check for input file; if no file is chosen, check for chosen example data
    if(is.null(input$file)){

      if(is.null(exData)){
        return(NULL)
      }
      if(!is.null(exData)){

        if(exData == ""){
          return(NULL)
        }
        if(exData == "elfe"){
          currentData <- as.data.frame(elfe)
          return(currentData)
        }
        if(exData == "ppvt"){
          currentData <- as.data.frame(ppvt)
          return(currentData)
        }
        if(exData == "CDC"){
          currentData <- as.data.frame(CDC)
          return(currentData)
        }
      }
    }

    # If a file other than the examples is chosen, the file will be loaded depending on the file type
    currentData <- NULL
    currentFilePath <- input$file$datapath
    fileEnding <- strsplit(currentFilePath, ".", fixed = TRUE)[[1]][2]

    if(fileEnding == "csv"){
      currentData <- as.data.frame(read.csv2(currentFilePath))
    }
    if(fileEnding == "sav"){
      currentData <- as.data.frame(foreign::read.spss(currentFilePath))
    }
    if(fileEnding == "xlsx"){
      currentData <- as.data.frame(readxl::read_xlsx(currentFilePath))
    }
    if (fileEnding == "xls"){
      currentData <- as.data.frame(readxl::read_xls(currentFilePath))
    }
    if(fileEnding == "rda"){

      load_first_object <- function(fname){
        e <- new.env(parent = parent.frame())
        load(fname, e)
        return(e[[ls(e)[1]]])
      }
      currentData <- load_first_object(currentFilePath)

    }
    return(currentData)
  })

# Returns column names of chosen data file
  variableNames <- reactive({

    if (is.null(currentFile())) {
      return()
    }
    return(names(currentFile()))

  })

# Generates selectInput for choosing grouping variable for data analysis
  output$GroupingVariable <- renderUI({

    currentNames <- names(currentFile())
    if(is.element("group", currentNames)|| is.element("Group", currentNames))
    {
      if(is.element("group", currentNames)){
      selectInput(inputId = "InputGroupingVariable", "Grouping variable", choices = variableNames(), selected = "group")
      }
      else{
        selectInput(inputId = "InputGroupingVariable", "Grouping variable", choices = variableNames(), selected = "group")

      }
      }
    else{    selectInput(inputId = "InputGroupingVariable", "Grouping variable", choices = variableNames(), selected = NULL)
}
  })

# Generates selectINput for choosing explanatory variable for data analysis
  output$ExplanatoryVariable <- renderUI({

    selectInput(inputId = "InputExplanatoryVariable", "Explanatory variable", choices = variableNames(), selected = input$InputGroupingVariable)
  })

# Generates selectInput for choosing raw values for data analysis
  output$RawValues <- renderUI({

    currentNames <- names(currentFile())
    if(is.element("raw", currentNames)||is.element("Raw", currentNames)){
      if(is.element("raw", currentNames)){
        selectInput(inputId = "InputRawValues", "Choose raw values", choices = variableNames(), selected = "raw")
      }
      else{
        selectInput(inputId = "InputRawValues", "Choose raw values", choices = variableNames(), selected = "Raw")

      }
    }
    else{
      selectInput(inputId = "InputRawValues", "Choose raw values", choices = variableNames(), selected = NULL)
    }

  })

# Shows the current data file
  output$table <- renderDataTable({
    if(is.null(currentFile())){ return(NULL)}
    else{return(currentFile())}

  })

# Updating chosen variables for group, raw, explanatory and powers

  # Retruns chosen variable for grouping
  chosenGrouping <- reactive({
    return(input$InputGroupingVariable)
  })

  # Returns chosen raw value
  chosenRaw <- reactive({
    return(input$InputRawValues)
  })

  # Returns chosen explanatory variable
  chosenExplanatory <- reactive({

    return(input$InputExplanatoryVariable)
  })

  chosenMethod <- reactive({
    return(as.numeric(input$Method))
  })

  chosenScale <- reactive({
    return(toString(input$Scale))
  })

  chosenNumberOfPowers <- reactive({
    return(as.numeric(input$NumberOfPowers))
  })


  # Calculates prepared data by pressing action button
  preparedData <- eventReactive(input$DoDataPreparation, {



    # Data preperation for data analysis with cNORM
    # Returns prepared data, which can be used for calculating best model
    preparedData <- reactive({

      if(is.null(currentFile())){
        print("CurrentFile is null")
        return()}

      if(is.null(chosenGrouping())){
        print("GroupingVariable is null")
        return()}

      # Ranky by chosen group
      data_to_output <- cNORM::rankByGroup(currentFile(),
                                           group = chosenGrouping(),
                                           raw = chosenRaw(),
                                           method = chosenMethod(),
                                           scale = chosenScale())
      # Computation of powers and linear combinations
      data_to_output <- cNORM::computePowers(data_to_output,
                                             k = chosenNumberOfPowers(), age = chosenGrouping(),
                                             norm = "normValue")

      return(data_to_output)
    })


    return(preparedData())
  })

  # Shows prepared data for data analysis with cNORM
  output$preparedData <- renderDataTable({
    if(is.null(preparedData())){return()}

    return(preparedData())
  })


  # Calculates best model using cNORM
  bestModel <- eventReactive(input$CalcBestModel, {

    currentBestModel <- cNORM::bestModel(data = preparedData(),
                                         raw = chosenRaw(),
                                         k = chosenNumberOfPowers(),
                                         predictors = NULL,
                                         terms = 0)

    return(currentBestModel)
  })

  modelDerivatives <- eventReactive(input$CalcBestModel, {
    MIN_AGE <- min((currentFile())[chosenGrouping()])
    MAX_AGE <- max((currentFile())[chosenGrouping()])

    cNORM::plotDerivative(bestModel(),
                          minAge = MIN_AGE,
                          maxAge = MAX_AGE)

  })

  wlPlot <- eventReactive(input$CalcBestModel, {

    cNORM::plotSubset(bestModel())

  })

  valuesPlot <- eventReactive(input$CalcBestModel, {

    cNORM::plotRaw(preparedData(), bestModel(), group = chosenGrouping(), raw = chosenRaw())
  })
  # Prints best model
  output$BestModel <- renderPrint({
    return(bestModel())
  })

  # Plots model derivation
  output$PlotDerivatives <- renderPlot({
    return(modelDerivatives())
  })


  output$PlotWL <- renderPlot({
    return(wlPlot())
  })

  output$PlotValues <- renderPlot({

    return(valuesPlot())
  })

  normCurves <- reactive({

    MIN_AGE <- min((currentFile())[chosenGrouping()])
    MAX_AGE <- max((currentFile())[chosenGrouping()])

    currentBestModel <- cNORM::plotNormCurves(bestModel(),
                          minAge = MIN_AGE,
                          maxAge = MAX_AGE)
    return(currentBestModel)
  })

  # Generates and prints norm curves
  output$NormCurves <- renderPlot({

    return(normCurves())
  })

  # Generates and plots percentile curves
  output$PlotPercentiles <- renderPlot({

    MIN_AGE <- min((currentFile())[chosenGrouping()])
    MAX_AGE <- max((currentFile())[chosenGrouping()])

    cNORM::plotPercentiles(currentFile(), bestModel(), raw = chosenRaw(),
                           group = chosenGrouping(),
                           minAge = MIN_AGE,
                           maxAge = MAX_AGE)
  })


  output$InputNormValue <- renderUI({
    tagList(numericInput(inputId = "NormValueInputAge", label = "Choose age", value = NULL, min = bestModel()$minA1, max = bestModel()$maxA1),
            numericInput(inputId = "NormValueInputRaw", label = "Choose raw value", value = NULL, min = bestModel()$minRaw, max = bestModel()$maxRaw),
            actionButton(inputId = "CalcNormValue",label = "Calc norm value"))
  })

  normValue <- eventReactive(input$CalcNormValue,{

    if(is.null(input$NormValueInputRaw) || is.null(input$NormValueInputRaw)){return()}
    else{
      currentAgeForNormValue <- input$NormValueInputAge
      currentRawForNormValue <- input$NormValueInputRaw
      currentBestModel <- bestModel()
      MIN_NORM <- currentBestModel$minL1
      MAX_NORM <- currentBestModel$maxL1
      MIN_RAW <- currentBestModel$minRaw
      MAX_RAW <- currentBestModel$maxRaw

      currentNormValue <- cNORM::predictNormValue(currentRawForNormValue, currentAgeForNormValue, model =currentBestModel, minNorm = MIN_NORM, maxNorm = MAX_NORM)
      return(currentNormValue)
    }
  })


  output$NormValue <- renderText({
    if(is.null(normValue())){return()}

    return(paste("Predicted norm value:",toString(normValue())))
  })

  output$InputRawValue <- renderUI({
    tagList(numericInput(inputId = "RawValueInputAge", label = "Choose age", value = NULL, min = bestModel()$minA1, max = bestModel()$maxA1),
            numericInput(inputId = "RawValueInputNorm", label = "Choose norm value", value = NULL, min = bestModel()$minL1, max = bestModel()$maxL1),
            actionButton(inputId = "CalcRawValue",label = "Calc raw value"))
  })

  rawValue <- eventReactive(input$CalcRawValue,{

    if(is.null(input$RawValueInputAge) || is.null(input$RawValueInputNorm)){return()}
    else{
      currentAgeForRawValue <- input$RawValueInputAge
      currentNormForRawValue <- input$RawValueInputNorm
      currentBestModel <- bestModel()
      MIN_NORM <- currentBestModel$minL1
      MAX_NORM <- currentBestModel$maxL1
      MIN_RAW <- currentBestModel$minRaw
      MAX_RAW <- currentBestModel$maxRaw

      currentRawValue <- cNORM::predictRaw(currentNormForRawValue, currentAgeForRawValue, coefficients = currentBestModel$coefficients,
                                                  minRaw = MIN_RAW, maxRaw = MAX_RAW)
      return(currentRawValue)
    }
  })

  output$RawValue <- renderText({
    if(is.null(rawValue())){return()}

    return(paste("Predicted raw value:",toString(rawValue())))
  })


  output$InputNormTable <- renderUI({
      tagList(numericInput(inputId = "NormTableInput", label = "Choose age for prediction of norm values", value = NULL,min = 0, max = bestModel()$maxA1),
              numericInput(inputId = "NormTableInputStart", label = "Choose norm start value", value = NULL),
              numericInput(inputId = "NormTableInputEnd", label = "Choose norm end value", value = NULL),
              numericInput(inputId = "NormTableInputStepping", label = "Choose stepping value", value = NULL),
              actionButton(inputId = "CalcNormTables",label = "Generate norm table", value = NULL))
      })


   normTable <- eventReactive(input$CalcNormTables, {

        check_inputs <- is.null(input$NormTableInput) || is.null(input$NormTableInputStart) || is.null(input$NormTableInputEnd) || is.null(input$NormTableInputStepping)
       if(check_inputs){return()}
       else{
         currentAgeForNormTable <- input$NormTableInput
         currentBestModel <- bestModel()
         MIN_NORM <- as.numeric(input$NormTableInputStart)
         MAX_NORM <- as.numeric(input$NormTableInputEnd)
         STEPPING <- as.numeric(input$NormTableInputStepping)
         MIN_RAW <- currentBestModel$minRaw
         MAX_RAW <- currentBestModel$maxRaw


         currentNormTable <- cNORM::normTable(currentAgeForNormTable, bestModel(), minNorm = MIN_NORM, maxNorm = MAX_NORM,
                                              minRaw = MIN_RAW, maxRaw = MAX_RAW, step = STEPPING)

         currentNormTable$raw <- round(currentNormTable$raw, digits = 2)
         return(currentNormTable)}
        })



  output$NormTable <- renderDataTable({
    if(is.null(normTable())){return()}

    return(normTable())
  })


  output$InputRawTable <- renderUI({
    tagList(numericInput(inputId = "RawTableInput", label = "Choose age for prediction of raw values", value = NULL,min = 0, max = bestModel()$maxA1),
            numericInput(inputId = "RawTableInputStart", label = "Choose raw start value", value = NULL),
            numericInput(inputId = "RawTableInputEnd", label = "Choose raw end value", value = NULL),
            numericInput(inputId = "RawTableInputStepping", label = "Choose stepping value", value = NULL),
            actionButton(inputId = "CalcRawTables",label = "Generate raw table"))
  })


  rawTable <- eventReactive(input$CalcRawTables, {

    checking_input <- is.null(input$RawTableInputStart) || is.null(input$RawTableInputStart) || is.null(input$RawTableInputEnd) || is.null(input$RawTableInputStepping)
    if(checking_input){return()}
    else{
      currentAgeForRawTable <- input$RawTableInput
      currentBestModel <- bestModel()
      MIN_NORM <- currentBestModel$minL1
      MAX_NORM <- currentBestModel$maxL1
      MIN_RAW <- input$RawTableInputStart
      MAX_RAW <- input$RawTableInputEnd
      STEPPING <- as.numeric(input$RawTableInputStepping)


      currentRawTable <- cNORM::rawTable(currentAgeForRawTable, currentBestModel, minNorm = MIN_NORM, maxNorm = MAX_NORM,
                                           minRaw = MIN_RAW, maxRaw = MAX_RAW, step = STEPPING)
      currentRawTable$norm <- round(currentRawTable$norm, digits = 3)

      return(currentRawTable)}
  })

  output$RawTable <- renderDataTable({
    if(is.null(rawTable())){return()}

    return(rawTable())
  })



  # Generates and plots modelled against actual data values
  output$PlotValues <- renderPlot({

    cNORM::plotRaw(currentFile(), bestModel(), group = chosenGrouping(), raw = chosenRaw())
  })

  # Generates and plots contour plot of first derivation
  output$PlotDerivatives <- renderPlot({


    MIN_AGE <- min((currentFile())[chosenGrouping()])
    MAX_AGE <- max((currentFile())[chosenGrouping()])

    cNORM::plotDerivative(bestModel(),
                          minAge = MIN_AGE,
                          maxAge = MAX_AGE)
  })








})


