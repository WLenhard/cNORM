if(!require(shiny)){
  install.packages("shiny")
  require(shiny)
}

if(!require(markdown)){
  install.packages("markdown")
  require(markdown)
}

if(!require(foreign)){
  install.packages("foreign")
  require(foreign)
}

if(!require(readxl)){
  install.packages("readxl")
  require(readxl)
}

# Define server logic required for cNORM-Application
shinyServer(function(input, output, session) {

  # Stops program by closing browser tab
  session$onSessionEnded(function() {
    stopApp()
  })


  # Returns currently chosen file; returns null,if no file was chosen
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
        selectInput(inputId = "InputGroupingVariable", "Grouping Variable", choices = variableNames(), selected = "group")
      }
      else{
        selectInput(inputId = "InputGroupingVariable", "Grouping Variable", choices = variableNames(), selected = "group")

      }
    }
    else{    selectInput(inputId = "InputGroupingVariable", "Grouping Variable", choices = variableNames(), selected = NULL)
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
        selectInput(inputId = "InputRawValues", "Raw Score Variable", choices = variableNames(), selected = "raw")
      }
      else{
        selectInput(inputId = "InputRawValues", "Raw Score Variable", choices = variableNames(), selected = "Raw")

      }
    }
    else{
      selectInput(inputId = "InputRawValues", "Raw Score Variable", choices = variableNames(), selected = NULL)
    }

  })

  # Shows introduction text until a file was chosen
  output$introduction <- renderText({

    if(is.null(currentFile())){

      output <- readChar("www/introduction.html", file.info("www/introduction.html")$size)
      #output <- textreadr::read_html(file = "www/introduction.html")
      includeHTML("www/introduction.html")
    }
    else{
      return(NULL)
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

  chosenDescend <- reactive({
    if(input$RankingOrder == "Descending")
      return(TRUE)
    else
      return(FALSE)
  })

  # Returns chosen explanatory variable
  chosenExplanatory <- reactive({

    return(input$InputExplanatoryVariable)
  })

  chosenMethod <- reactive({
    methods <- c("Blom (1985)", "Tukey (1949)", "Van der Warden (1952)", "Rankit (Bliss, 1967)", "Levenbach (1953)", "Filliben (1975)", "Yu & Huang (2001)")
    return(match(input$Method, methods))
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
                                           scale = chosenScale(),
                                           descend = chosenDescend())
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

  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, "data.RData", sep = "")
    },
    content = function(file) {
      data.cnorm <- prepareData()
      attr(data.cnorm, "descend") <- chosenDescend()

      save(data.cnorm, file = file)
    }
  )

  output$downloadModel <- downloadHandler(
    filename = function() {
      paste(input$dataset, "model.RData", sep = "")
    },
    content = function(file) {
      model.cnorm <- bestModel()
      save(model.cnorm, file = file)
    }
  )



  output$NumberOfTerms <- renderUI({

    k <- as.numeric(chosenNumberOfPowers())
    terms_max <- k*k + 2*k
    numericInput("InputNumberOfTerms", "Number of terms", value = NULL, min = 1, max = terms_max, step = 1)
  })

  chosenCoeffOfDet <- reactive({

    return(as.numeric(input$ChosenDetCoeff))
  })

  chosenNumberOfTerms <- reactive({

    if(!is.null(input$InputNumberOfTerms)){
      return(as.numeric(input$InputNumberOfTerms))

    }
    else{
      return(0)
    }
  })

  # Calculates best model using cNORM
  bestModel <- eventReactive(input$CalcBestModel, {

    # CAUTION: The value of a non-specified numericInput-value is NA and not NULL!
    # Therefore use is.na() to check, whether a specific number of terms were chosen or not
    if(!is.na(chosenNumberOfTerms()))
    {
      currentBestModel <- cNORM::bestModel(data = preparedData(),
                                           raw = chosenRaw(),
                                           k = chosenNumberOfPowers(),
                                           predictors = NULL,
                                           terms = chosenNumberOfTerms(),
                                           plot=FALSE)
    }
    else{
      currentBestModel <- cNORM::bestModel(data = preparedData(),
                                           raw = chosenRaw(),
                                           k = chosenNumberOfPowers(),
                                           predictors = NULL,
                                           R2 = chosenCoeffOfDet(),
                                           terms = 0,
                                           plot=FALSE)
    }



    return(currentBestModel)
  })

  modelDerivatives <- eventReactive(input$CalcBestModel, {
    cNORM::plotDerivative(bestModel())

  })


  modelDensity <- reactive({
    if(input$densities == ""){
      cNORM::plotDensity(bestModel())
    }
    else{
      densityList <- as.numeric(unlist(strsplit(input$densities, "\\, |\\,| ")))
      cNORM::plotDensity(bestModel(), group = densityList)
    }
  })



  chosenTypeOfPlotSubset <- reactive({
    method <- c("Adjusted R2 by Number of Predictors", "Log Transformed Mallow's Cp by Adjusted R2", "Bayesian Information Criterion (BIC) by Adjusted R2", "RMSE by Number of Predictors")
    return(match(input$chosenTypePlotSubset, method) - 1)
  })

  changeObject <- reactive({
    paste(input$CalcBestModel, input$chosenTypePlotSubset)
  })


  wlPlot <- eventReactive(changeObject(), {
    cNORM::plotSubset(bestModel(), type = chosenTypeOfPlotSubset())
  })

  # Generates and prints norm curves
  output$NormCurves <- renderPlot({
    return(normCurves())
  })

  # Generates and plots percentile curves
  output$modelPlot <- renderPlot({

    MIN_AGE <- min((currentFile())[chosenGrouping()])
    MAX_AGE <- max((currentFile())[chosenGrouping()])

    # data <- currentFile()
    # attr(data, "descend") <- chosenDescend()

    cNORM::plotPercentiles(preparedData(), bestModel(), raw = chosenRaw(),
                             group = chosenGrouping(),
                             minAge = MIN_AGE,
                             maxAge = MAX_AGE)

  })

  valuesPlot <- eventReactive(input$CalcBestModel, {

    cNORM::plotRaw(preparedData(), bestModel(), group = chosenGrouping(), raw = chosenRaw())
  })

  normScorePlot <- eventReactive(c(input$grouping, input$differences, input$normScoreButton), {
    type <- 0
    if(input$differences)
      type <- 1

    if(input$grouping){
      cNORM::plotNorm(preparedData(), bestModel(), group = bestModel()$group, type = type)
    }else{
      cNORM::plotNorm(preparedData(), bestModel(), type = type)
    }
  })


  crossValidation <- eventReactive(input$CrossValidation, {
    rep <- input$RepetitionsCV
    norm <- input$NormsCV
    maxT <- input$MaxTermsCV
    g <- chosenGrouping()
    r <- chosenRaw()
    e <- chosenExplanatory()

        table <- cNORM::cnorm.cv(preparedData(), repetitions = rep, norms = norm, min = 1, max = maxT, group = g, raw = r, age = e)
        output$TableCV <- renderDataTable({table})
  })


  rawScorePlot <- eventReactive(c(input$grouping1, input$differences1, input$rawScoreButton), {
    type <- 0
    if(input$differences1)
      type <- 1

    if(input$grouping1){
      cNORM::plotRaw(preparedData(), bestModel(), group = bestModel()$group, type = type)
    }else{
      cNORM::plotRaw(preparedData(), bestModel(), type = type)
    }
  })

  # Prints best model
  output$BestModel1 <- renderText({
    return(bestModel()$report[1])
  })
  output$BestModel2 <- renderText({
    return(bestModel()$report[2])
  })
  output$BestModel3 <- renderText({
    return(bestModel()$report[3])
  })
  output$BestModel4 <- renderText({
    return(bestModel()$report[4])
  })
  output$BestModel5 <- renderText({
    return(bestModel()$report[5])
  })
  output$BestModel6 <- renderText({
    if(checkConsistency(bestModel())){
      return("WARNING! The model seems to be inconsistent. Please check the percentile plot for intersecting percentile curves and change the number of terms for a different solution.")
    }else{
      return("No violations of model consistency found within the boundaries of the original data.")
    }
  })
  output$BestModel7 <- renderText({
    return(rangeCheck(bestModel()))
  })

  # Plots model derivation
  output$PlotDerivatives <- renderPlot({
    return(modelDerivatives())
  })

  output$PlotDensity <- renderPlot({
    return(modelDensity())
  })

  output$PlotCV <- renderPlot({
    return(crossValidation())
  })

  output$PlotWL <- renderPlot({
    return(wlPlot())
  })

  output$PlotValues <- renderPlot({

    return(valuesPlot())
  })

  output$PlotNormScores <- renderPlot({

    return(normScorePlot())
  })

  output$PlotRawScores <- renderPlot({

    return(rawScorePlot())
  })

  output$PrintSubset <- renderDataTable({
    return(cNORM::printSubset(bestModel()))
  })

  output$Series <- renderPlot({

    return(cNORM::plotPercentileSeries(preparedData(), bestModel(), start = input$terms, end = input$terms))

  })


  chosenPercentilesForNormCurves <- reactive({


    if(input$PercentilesForNormCurves == ""){
      return(NULL)
  }
    else{
    (input$PercentilesForNormCurves)
  }

  })

  chosenPercentilesForPercentiles <- reactive({
    if(input$PercentilesForPercentiles == ""){
      return(NULL)
    }
    else{
      return(input$PercentilesForPercentiles)
    }
  })

  normCurves <- reactive({

    MIN_AGE <- min((currentFile())[chosenGrouping()])
    MAX_AGE <- max((currentFile())[chosenGrouping()])


    percentileList <- chosenPercentilesForNormCurves()

    if(is.null(chosenPercentilesForNormCurves())){
      percentileList <- c(.02276, 0.1587, 0.5000, 0.8413, 0.97724)
    }
    else{
      percentileList <- as.numeric(unlist(strsplit(chosenPercentilesForNormCurves(), "\\, |\\,| ")))/100
    }



    # Set different norm lists for different scales
      currentScale <- chosenScale()
      if(currentScale == "T"){
        normList <- qnorm(percentileList, 50, 10)
      }
      if(currentScale == "z")
      {
        normList <- normList <- qnorm(percentileList, 0, 1)
      }
      if(currentScale == "IQ"){
        normList <- normList <- qnorm(percentileList, 100, 15)
      }

      normList <- round(normList, digits = 2)

    currentNormCurves <- cNORM::plotNormCurves(bestModel(),
                                              minAge = MIN_AGE,
                                              maxAge = MAX_AGE,
                                              normList = normList)
    return(currentNormCurves)
  })

  # Generates and prints norm curves
  output$NormCurves <- renderPlot({

    return(normCurves())
  })

  # Generates and plots percentile curves
  output$PlotPercentiles <- renderPlot({

    MIN_AGE <- min((currentFile())[chosenGrouping()])
    MAX_AGE <- max((currentFile())[chosenGrouping()])
    # data <- currentFile()
    # attr(data, "descend") <- chosenDescend()

    percentileList <- chosenPercentilesForPercentiles()

    if(is.null(percentileList)){
      cNORM::plotPercentiles(preparedData(), bestModel(), raw = chosenRaw(),
                             group = chosenGrouping(),
                             minAge = MIN_AGE,
                             maxAge = MAX_AGE)
    }
    else{
      percentileList <- as.numeric(unlist(strsplit(chosenPercentilesForPercentiles(), "\\, |\\,| ")))/100

      cNORM::plotPercentiles(preparedData(), bestModel(), raw = chosenRaw(),
                             group = chosenGrouping(),
                             minAge = MIN_AGE,
                             maxAge = MAX_AGE,
                             percentiles = percentileList)
    }

  })


  output$InputNormValue <- renderUI({
    tagList(numericInput(inputId = "NormValueInputAge", label = "Choose age", value = NULL, min = bestModel()$minA1, max = bestModel()$maxA1),
            numericInput(inputId = "NormValueInputRaw", label = "Choose raw value", value = NULL, min = bestModel()$minRaw, max = bestModel()$maxRaw),
            actionButton(inputId = "CalcNormValue",label = "Norm Score"))
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

      currentNormValue <- cNORM::predictNorm(currentRawForNormValue, currentAgeForNormValue, model =currentBestModel, minNorm = MIN_NORM, maxNorm = MAX_NORM)
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
            actionButton(inputId = "CalcRawValue",label = "Raw Score"))
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
            numericInput(inputId = "NormTableInputStart", label = "Choose norm start value", value = bestModel()$scaleM - bestModel()$scaleSD*2.5),
            numericInput(inputId = "NormTableInputEnd", label = "Choose norm end value", value = bestModel()$scaleM + bestModel()$scaleSD*2.5),
            numericInput(inputId = "NormTableInputStepping", label = "Choose stepping value", value = NULL),
            numericInput(inputId = "NormTableCI", label = "Confidence Coefficient", value = .9),
            numericInput(inputId = "NormTableRel", label = "Reliability Coefficient", value = NULL),
            actionButton(inputId = "CalcNormTables",label = "Generate norm table", value = NULL),
            downloadButton("DownloadNormTable", "Download norm table"))

  })


  normTable <- eventReactive(input$CalcNormTables, {

    check_inputs <- is.null(input$NormTableInput) || is.null(input$NormTableInputStart) || is.null(input$NormTableInputEnd) || is.null(input$NormTableInputStepping)
    if(check_inputs){return()}
    else{
      currentAgeForNormTable <- input$NormTableInput
      currentBestModel <- bestModel()
      MIN_NORM <- as.numeric(input$NormTableInputStart)
      MAX_NORM <- as.numeric(input$NormTableInputEnd)

      if(is.null(input$NormTableRel)){
        REL <- NULL
      }else{
        REL <- as.numeric(input$NormTableRel)
      }

      if(is.na(REL)){
        REL <- NULL
      }

      CI <- as.numeric(input$NormTableCI)
      if(is.na(CI)){
        CI <- NULL
      }

      STEPPING <- as.numeric(input$NormTableInputStepping)
      MIN_RAW <- currentBestModel$minRaw
      MAX_RAW <- currentBestModel$maxRaw

      currentNormTable <- cNORM::normTable(currentAgeForNormTable, bestModel(), minNorm = MIN_NORM, maxNorm = MAX_NORM,
                                           minRaw = MIN_RAW, maxRaw = MAX_RAW, step = STEPPING, CI = CI, reliability = REL)

      currentNormTable$raw <- round(currentNormTable$raw, digits = 2)
      currentNormTable$percentile <- round(currentNormTable$percentile, digits = 2)

      return(currentNormTable)}
  })



  output$NormTable <- renderDataTable({
    if(is.null(normTable())){return()}

    return(normTable())
  })

  output$DownloadNormTable <- downloadHandler(
    filename = function(){
      "NormTable.csv"
    },
    content = function(file){
      output <- as.data.frame(normTable())
      write.csv(output, file)
    }
  )


  output$InputRawTable <- renderUI({
    tagList(numericInput(inputId = "RawTableInput", label = "Choose age for prediction of raw values", value = NULL,min = 0, max = bestModel()$maxA1),
            numericInput(inputId = "RawTableInputStart", label = "Choose raw start value", value = bestModel()$minRaw),
            numericInput(inputId = "RawTableInputEnd", label = "Choose raw end value", value = bestModel()$maxRaw),
            numericInput(inputId = "RawTableInputStepping", label = "Choose stepping value", value = NULL),
            numericInput(inputId = "RawTableCI", label = "Confidence Coefficient", value = .9),
            numericInput(inputId = "RawTableRel", label = "Reliability Coefficient", value = NULL),
            actionButton(inputId = "CalcRawTables",label = "Generate raw table"),
            downloadButton("DownloadRawTable", label = "Download raw table"))
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

      if(is.null(input$RawTableRel)){
        REL <- NULL
      }else{
        REL <- as.numeric(input$RawTableRel)
      }

      if(is.na(REL)){
        REL <- NULL
      }

      CI <- as.numeric(input$RawTableCI)
      if(is.na(CI)){
        CI <- NULL
      }

      currentRawTable <- cNORM::rawTable(currentAgeForRawTable, currentBestModel, minNorm = MIN_NORM, maxNorm = MAX_NORM,
                                         minRaw = MIN_RAW, maxRaw = MAX_RAW, step = STEPPING, CI = CI, reliability = REL)
      currentRawTable$norm <- round(currentRawTable$norm, digits = 3)
      currentRawTable$percentile <- round(currentRawTable$percentile, digits = 2)


      return(currentRawTable)}
  })

  output$RawTable <- renderDataTable({
    if(is.null(rawTable())){return()}

    return(rawTable())
  })

  output$DownloadRawTable <- downloadHandler(
    filename = function(){
      "RawTable.csv"
    },
    content = function(file){
      output <- as.data.frame(rawTable())
      write.csv(output, file)
    }
  )

  output$exportData <- downloadHandler(
    filename = function() {
      paste("model.RData", sep = "")
    },
    content = function(file) {
      save(preparedData(), filename = file)
    }
  )

  # Generates and plots modelled against actual data values
  output$PlotValues <- renderPlot({

    cNORM::plotRaw(preparedData(), bestModel(), group = chosenGrouping(), raw = chosenRaw())
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
