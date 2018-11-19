crossval <- function(repetitions) {
  library(gamlss)
  library(openxlsx)
  library(psych)
  load("c:/temp/cross.rda")

  terms <- c(4, 4, 4, 4, 4, 4, 4)
  lowerT <- c(20, 20, 20, 20, 20, 20, 20)
  upperT <- c(80, 80, 80, 80, 80, 80, 80)
  sample <- list(50, 75, 100, 150, 250, 500, 1000)
  difficulty <- list(theta1, theta2, theta3)

  # data frame for die Ergebnisse der Simulationsrechnung
  results <- data.frame(
    repetition = integer(),
    sample = integer(),
    theta = integer(),
    t = integer(),
    R2adj = double(),
    gamlssModel0 = integer(),
    gamlssModel1 = integer(),
    gamlssModel2 = integer(),
    timeCNORM = double(),
    timeGAMLSS0 = double(),
    timeGAMLSS1 = double(),
    timeGAMLSS2 = double(),
    train_raw_z = double(),
    train_raw_tcNORM = double(),
    train_raw_tGAMLSS0 = double(),
    train_raw_tGAMLSS1 = double(),
    train_raw_tGAMLSS2 = double(),
    train_z_tcNORM = double(),
    train_z_tGAMLSS0 = double(),
    train_z_tGAMLSS1 = double(),
    train_z_tGAMLSS2 = double(),
    train_z_T = double(),
    cross_raw_z = double(),
    cross_raw_tcNORM = double(),
    cross_raw_tGAMLSS0 = double(),
    cross_raw_tGAMLSS1 = double(),
    cross_raw_tGAMLSS2 = double(),
    cross_z_tcNORM = double(),
    cross_z_tGAMLSS0 = double(),
    cross_z_tGAMLSS1 = double(),
    cross_z_tGAMLSS2 = double(),
    cross_z_T = double(),
    vote0 = double(),
    vote1 = double(),
    vote3 = double()
  )

  descriptives <- data.frame(
    totalMeanTrain = double(),
    totalMedianTrain = double(),
    totalSDTrain = double(),
    totalSkewTrain = double(),
    totalKurtosisTrain = double(),
    totalMeanTest = double(),
    totalMedianTest = double(),
    totalSDTest = double(),
    totalSkewTest = double(),
    totalKurtosisTest = double(),
    meanTrain1 = double(),
    meanTrain2 = double(),
    meanTrain3 = double(),
    meanTrain4 = double(),
    meanTrain5 = double(),
    meanTrain6 = double(),
    meanTrain7 = double(),
    medianTrain1 = double(),
    medianTrain2 = double(),
    medianTrain3 = double(),
    medianTrain4 = double(),
    medianTrain5 = double(),
    medianTrain6 = double(),
    medianTrain7 = double(),
    sDTrain1 = double(),
    sDTrain2 = double(),
    sDTrain3 = double(),
    sDTrain4 = double(),
    sDTrain5 = double(),
    sDTrain6 = double(),
    sDTrain7 = double(),
    skewTrain1 = double(),
    skewTrain2 = double(),
    skewTrain3 = double(),
    skewTrain4 = double(),
    skewTrain5 = double(),
    skewTrain6 = double(),
    skewTrain7 = double(),
    kurtosisTrain1 = double(),
    kurtosisTrain2 = double(),
    kurtosisTrain3 = double(),
    kurtosisTrain4 = double(),
    kurtosisTrain5 = double(),
    kurtosisTrain6 = double(),
    kurtosisTrain7 = double(),
    meanTest1 = double(),
    meanTest2 = double(),
    meanTest3 = double(),
    meanTest4 = double(),
    meanTest5 = double(),
    meanTest6 = double(),
    meanTest7 = double(),
    medianTest1 = double(),
    medianTest2 = double(),
    medianTest3 = double(),
    medianTest4 = double(),
    medianTest5 = double(),
    medianTest6 = double(),
    medianTest7 = double(),
    sDTest1 = double(),
    sDTest2 = double(),
    sDTest3 = double(),
    sDTest4 = double(),
    sDTest5 = double(),
    sDTest6 = double(),
    sDTest7 = double(),
    skewTest1 = double(),
    skewTest2 = double(),
    skewTest3 = double(),
    skewTest4 = double(),
    skewTest5 = double(),
    skewTest6 = double(),
    skewTest7 = double(),
    kurtosisTest1 = double(),
    kurtosisTest2 = double(),
    kurtosisTest3 = double(),
    kurtosisTest4 = double(),
    kurtosisTest5 = double(),
    kurtosisTest6 = double(),
    kurtosisTest7 = double()
  )

  for (k in 1:repetitions) {
    for (i in 1:length(sample)) {
      for (j in 1:length(difficulty)) {
        # preparation
        tmp <- simulateRasch(n = sample[[i]], Theta = difficulty[[j]])
        tmp <- data.frame(age = tmp$sim$age, group = tmp$sim$group, z = tmp$sim$z, m = tmp$sim$m, sd = tmp$sim$sd, latent = tmp$sim$latent, zOverall = tmp$sim$zOverall)
        tmp$zOverall <- (tmp$latent - 5.0973533676) / 3.1268747761
        training.data <- simulateRasch(data = tmp, Theta = difficulty[[j]])
        training.data$data$raw1 <- training.data$data$raw + 1
        test.data <- simulateRasch(data = crossA, Theta = difficulty[[j]])
        test.data$data$raw1 <- test.data$data$raw + 1



        # cNORM
        startC <- proc.time()[[1]]
        training.data$data <- rankBySlidingWindow(training.data$data, age = "age", width = 1, raw = "raw")
        training.data$data <- computePowers(training.data$data, age = "age")
        test.data$data <- rankByGroup(test.data$data)

        t <- terms[i]
        cnorm.model <- bestModel(training.data$data, terms = t)

        if (checkConsistency(cnorm.model, minRaw = 0, maxRaw = length(difficulty[[j]]))) {
          t <- 3
          end <- 24
          cnorm.model <- bestModel(training.data$data, terms = t)

          while (checkConsistency(cnorm.model, minRaw = 0, maxRaw = length(difficulty[[j]])) && t < end) {
            t <- t + 1
            print("Searching for alternative cNorm model")
            cnorm.model <- bestModel(training.data$data, terms = t)
          }
        }
        training.data$data$tCNORM <- predictNorm(training.data$data$raw, training.data$data$age, cnorm.model, lowerT[[i]], upperT[[i]])
        test.data$data$tCNORM <- predictNorm(test.data$data$raw, test.data$data$age, cnorm.model, lowerT[[i]], upperT[[i]])
        timeCNORM <- proc.time()[[1]] - startC
        cross_z_tcNORM <- cor(test.data$sim$z, test.data$data$tCNORM, use = "complete.obs")
        cross_z_T <- cor(test.data$sim$z, test.data$data$normValue, use = "complete.obs")

        # GAMLS 0
        gamlssModel0 <- "Not converged"
        vote0 <- 1
        cross_z_tGAMLSS0 <- 0
        train_raw_tGAMLSS0 <- 0
        train_z_tGAMLSS0 <- 0
        cross_raw_tGAMLSS0 <- 0
        time0 <- proc.time()[[1]]

        tryCatch({
          gamlss.model0 <- lms(raw, age, data = training.data$data, families = c("NO"), pb.method = "GAIC", k = 3)
        }, error = function(e) {
        })

        tryCatch({
          training.data$data$tGAMLSS0 <- z.scores(gamlss.model0, y = training.data$data$raw, x = training.data$data$age) * 10 + 50
          training.data$data$tGAMLSS0[training.data$data$tGAMLSS0 < lowerT[[i]]] <- lowerT[[i]]
          training.data$data$tGAMLSS0[training.data$data$tGAMLSS0 > upperT[[i]]] <- upperT[[i]]
          test.data$data$tGAMLSS0 <- z.scores(gamlss.model0, y = test.data$data$raw, x = test.data$data$age) * 10 + 50
          test.data$data$tGAMLSS0[test.data$data$tGAMLSS0 < lowerT[[i]]] <- lowerT[[i]]
          test.data$data$tGAMLSS0[test.data$data$tGAMLSS0 > upperT[[i]]] <- upperT[[i]]
          gamlssModel0 <- gamlss.model0$family[1]
          train_raw_tGAMLSS0 <- cor(training.data$data$raw, training.data$data$tGAMLSS0, use = "complete.obs")
          cross_z_tGAMLSS0 <- cor(test.data$sim$z, test.data$data$tGAMLSS0, use = "complete.obs")
          train_z_tGAMLSS0 <- cor(training.data$sim$z, training.data$data$tGAMLSS0, use = "complete.obs")
          cross_raw_tGAMLSS0 <- cor(test.data$data$raw, test.data$data$tGAMLSS0, use = "complete.obs")

          if (cross_z_tcNORM < cross_z_tGAMLSS0) {
            vote0 <- 0
          }
        }, error = function(e) {
          print("catch1 reached")
        })
        timeGAMLSS0 <- proc.time()[[1]] - time0

        # GAMLS 1
        gamlssModel1 <- "Not converged"
        vote1 <- 1
        cross_z_tGAMLSS1 <- 0
        train_raw_tGAMLSS1 <- 0
        train_z_tGAMLSS1 <- 0
        cross_raw_tGAMLSS1 <- 0
        time1 <- proc.time()[[1]]

        tryCatch({
          gamlss.model1 <- lms(raw1, age, data = training.data$data, c("BCCGo", "BCPEo", "BCTo"), pb.method = "GAIC", k = 3)
        }, error = function(e) {
        })

        tryCatch({
          training.data$data$tGAMLSS1 <- z.scores(gamlss.model1, y = training.data$data$raw1, x = training.data$data$age) * 10 + 50
          training.data$data$tGAMLSS1[training.data$data$tGAMLSS1 < lowerT[[i]]] <- lowerT[[i]]
          training.data$data$tGAMLSS1[training.data$data$tGAMLSS1 > upperT[[i]]] <- upperT[[i]]
          test.data$data$tGAMLSS1 <- z.scores(gamlss.model1, y = test.data$data$raw1, x = test.data$data$age) * 10 + 50
          test.data$data$tGAMLSS1[test.data$data$tGAMLSS1 < lowerT[[i]]] <- lowerT[[i]]
          test.data$data$tGAMLSS1[test.data$data$tGAMLSS1 > upperT[[i]]] <- upperT[[i]]
          gamlssModel1 <- gamlss.model1$family[1]
          train_raw_tGAMLSS1 <- cor(training.data$data$raw1, training.data$data$tGAMLSS1, use = "complete.obs")
          cross_z_tGAMLSS1 <- cor(test.data$sim$z, test.data$data$tGAMLSS1, use = "complete.obs")
          train_z_tGAMLSS1 <- cor(training.data$sim$z, training.data$data$tGAMLSS1, use = "complete.obs")
          cross_raw_tGAMLSS1 <- cor(test.data$data$raw1, test.data$data$tGAMLSS1, use = "complete.obs")
          if (cross_z_tcNORM < cross_z_tGAMLSS1) {
            vote1 <- 0
          }
        }, error = function(e) {
          print("catch1 reached")
        })
        timeGAMLSS1 <- proc.time()[[1]] - time1

        # GAMLS 2
        gamlssModel2 <- "Not converged"
        vote2 <- 1
        cross_z_tGAMLSS2 <- 0
        train_raw_tGAMLSS2 <- 0
        train_z_tGAMLSS2 <- 0
        cross_raw_tGAMLSS2 <- 0
        time2 <- proc.time()[[1]]

        tryCatch({
          gamlss.model2 <- lms(raw, age, data = training.data$data, families = c("SHASHo"), pb.method = "GAIC", k = 3)
        }, error = function(e) {
        })

        tryCatch({
          training.data$data$tGAMLSS2 <- z.scores(gamlss.model2, y = training.data$data$raw, x = training.data$data$age) * 10 + 50
          training.data$data$tGAMLSS2[training.data$data$tGAMLSS2 < lowerT[[i]]] <- lowerT[[i]]
          training.data$data$tGAMLSS2[training.data$data$tGAMLSS2 > upperT[[i]]] <- upperT[[i]]
          test.data$data$tGAMLSS2 <- z.scores(gamlss.model2, y = test.data$data$raw, x = test.data$data$age) * 10 + 50
          test.data$data$tGAMLSS2[test.data$data$tGAMLSS2 < lowerT[[i]]] <- lowerT[[i]]
          test.data$data$tGAMLSS2[test.data$data$tGAMLSS2 > upperT[[i]]] <- upperT[[i]]
          gamlssModel2 <- gamlss.model2$family[1]
          train_raw_tGAMLSS2 <- cor(training.data$data$raw, training.data$data$tGAMLSS2, use = "complete.obs")
          train_z_tGAMLSS2 <- cor(training.data$sim$z, training.data$data$tGAMLSS2, use = "complete.obs")
          cross_raw_tGAMLSS2 <- cor(test.data$data$raw, test.data$data$tGAMLSS2, use = "complete.obs")
          cross_z_tGAMLSS2 <- cor(test.data$sim$z, test.data$data$tGAMLSS2, use = "complete.obs")
          if (cross_z_tcNORM < cross_z_tGAMLSS2) {
            vote2 <- 0
          }
        }, error = function(e) {
        })

        timeGAMLSS2 <- proc.time()[[1]] - time2

        print(paste0("Saving cycle ", k, " ", i, " ", j))

        r <- data.frame(
          repetition = k,
          sample = length(training.data$data$raw),
          theta = j,
          terms = t,
          R2adj = cnorm.model$subsets$adjr2[[cnorm.model$ideal.model]],
          gamlssModel0 = gamlssModel0,
          gamlssModel1 = gamlssModel1,
          gamlssModel2 = gamlssModel2,
          timeCNORM = timeCNORM,
          timeGAMLSS0 = timeGAMLSS0,
          timeGAMLSS1 = timeGAMLSS1,
          timeGAMLSS2 = timeGAMLSS2,
          train_raw_z = cor(training.data$data$raw, training.data$sim$z, use = "complete.obs"),
          train_raw_tcNORM = cor(training.data$data$raw, training.data$data$tCNORM, use = "complete.obs"),
          train_raw_tGAMLSS0 = train_raw_tGAMLSS0,
          train_raw_tGAMLSS1 = train_raw_tGAMLSS1,
          train_raw_tGAMLSS2 = train_raw_tGAMLSS2,
          train_z_tcNORM = cor(training.data$sim$z, training.data$data$tCNORM, use = "complete.obs"),
          train_z_tGAMLSS0 = train_z_tGAMLSS0,
          train_z_tGAMLSS1 = train_z_tGAMLSS1,
          train_z_tGAMLSS2 = train_z_tGAMLSS2,
          train_z_T = cor(training.data$sim$z, training.data$data$normValue, use = "complete.obs"),
          cross_raw_z = cor(test.data$data$raw, test.data$sim$z, use = "complete.obs"),
          cross_raw_tcNORM = cor(test.data$data$raw, test.data$data$tCNORM, use = "complete.obs"),
          cross_raw_tGAMLSS0 = cross_raw_tGAMLSS0,
          cross_raw_tGAMLSS1 = cross_raw_tGAMLSS1,
          cross_raw_tGAMLSS2 = cross_raw_tGAMLSS2,
          cross_z_tcNORM = cross_z_tcNORM,
          cross_z_tGAMLSS0 = cross_z_tGAMLSS0,
          cross_z_tGAMLSS1 = cross_z_tGAMLSS1,
          cross_z_tGAMLSS2 = cross_z_tGAMLSS2,
          cross_z_T = cross_z_T,
          vote0 = vote0,
          vote1 = vote1,
          vote2 = vote2
        )
        results <- rbind(results, r)

        totalTrain <- describe(training.data$data$raw)
        totalTest <- describe(test.data$data$raw)
        descTrain <- describeBy(training.data$data$raw, group = training.data$data$group)
        descTest <- describeBy(test.data$data$raw, group = test.data$data$group)


        d <- data.frame(
          totalMeanTrain = totalTrain$mean,
          totalMedianTrain = totalTrain$median,
          totalSDTrain = totalTrain$sd,
          totalSkewTrain = totalTrain$skew,
          totalKurtosisTrain = totalTrain$kurtosis,
          totalMeanTest = totalTest$mean,
          totalMedianTest = totalTest$median,
          totalSDTest = totalTest$sd,
          totalSkewTest = totalTest$skew,
          totalKurtosisTest = totalTest$kurtosis,
          meanTrain1 = descTrain[[1]]$mean,
          meanTrain2 = descTrain[[2]]$mean,
          meanTrain3 = descTrain[[3]]$mean,
          meanTrain4 = descTrain[[4]]$mean,
          meanTrain5 = descTrain[[5]]$mean,
          meanTrain6 = descTrain[[6]]$mean,
          meanTrain7 = descTrain[[7]]$mean,
          medianTrain1 = descTrain[[1]]$median,
          medianTrain2 = descTrain[[2]]$median,
          medianTrain3 = descTrain[[3]]$median,
          medianTrain4 = descTrain[[4]]$median,
          medianTrain5 = descTrain[[5]]$median,
          medianTrain6 = descTrain[[6]]$median,
          medianTrain7 = descTrain[[7]]$median,
          sDTrain1 = descTrain[[1]]$sd,
          sDTrain2 = descTrain[[2]]$sd,
          sDTrain3 = descTrain[[3]]$sd,
          sDTrain4 = descTrain[[4]]$sd,
          sDTrain5 = descTrain[[5]]$sd,
          sDTrain6 = descTrain[[6]]$sd,
          sDTrain7 = descTrain[[7]]$sd,
          skewTrain1 = descTrain[[1]]$skew,
          skewTrain2 = descTrain[[2]]$skew,
          skewTrain3 = descTrain[[3]]$skew,
          skewTrain4 = descTrain[[4]]$skew,
          skewTrain5 = descTrain[[5]]$skew,
          skewTrain6 = descTrain[[6]]$skew,
          skewTrain7 = descTrain[[7]]$skew,
          kurtosisTrain1 = descTrain[[1]]$kurtosis,
          kurtosisTrain2 = descTrain[[2]]$kurtosis,
          kurtosisTrain3 = descTrain[[3]]$kurtosis,
          kurtosisTrain4 = descTrain[[4]]$kurtosis,
          kurtosisTrain5 = descTrain[[5]]$kurtosis,
          kurtosisTrain6 = descTrain[[6]]$kurtosis,
          kurtosisTrain7 = descTrain[[7]]$kurtosis,
          meanTest1 = descTest[[1]]$mean,
          meanTest2 = descTest[[2]]$mean,
          meanTest3 = descTest[[3]]$mean,
          meanTest4 = descTest[[4]]$mean,
          meanTest5 = descTest[[5]]$mean,
          meanTest6 = descTest[[6]]$mean,
          meanTest7 = descTest[[7]]$mean,
          medianTest1 = descTest[[1]]$median,
          medianTest2 = descTest[[2]]$median,
          medianTest3 = descTest[[3]]$median,
          medianTest4 = descTest[[4]]$median,
          medianTest5 = descTest[[5]]$median,
          medianTest6 = descTest[[6]]$median,
          medianTest7 = descTest[[7]]$median,
          sDTest1 = descTest[[1]]$sd,
          sDTest2 = descTest[[2]]$sd,
          sDTest3 = descTest[[3]]$sd,
          sDTest4 = descTest[[4]]$sd,
          sDTest5 = descTest[[5]]$sd,
          sDTest6 = descTest[[6]]$sd,
          sDTest7 = descTest[[7]]$sd,
          skewTest1 = descTest[[1]]$skew,
          skewTest2 = descTest[[2]]$skew,
          skewTest3 = descTest[[3]]$skew,
          skewTest4 = descTest[[4]]$skew,
          skewTest5 = descTest[[5]]$skew,
          skewTest6 = descTest[[6]]$skew,
          skewTest7 = descTest[[7]]$skew,
          kurtosisTest1 = descTest[[1]]$kurtosis,
          kurtosisTest2 = descTest[[2]]$kurtosis,
          kurtosisTest3 = descTest[[3]]$kurtosis,
          kurtosisTest4 = descTest[[4]]$kurtosis,
          kurtosisTest5 = descTest[[5]]$kurtosis,
          kurtosisTest6 = descTest[[6]]$kurtosis,
          kurtosisTest7 = descTest[[7]]$kurtosis
        )
        descriptives <- rbind(descriptives, d)

        # save
        write.table(results, "c:/temp/results.txt", sep = "\t")
        write.table(descriptives, "c:/temp/descriptives.txt", sep = "\t")
        write.table(cbind(results, descriptives), "c:/temp/complete.txt", sep = "\t")

        # save(results, file = "../../Continuous Norming 2/Results/results.rda")
        # write.xlsx(results, file = paste0("../../Continuous Norming 2/Results/results.xlsx"))
        # write.xlsx(training.data, file = paste0("../../Continuous Norming 2/Results/training_", k, "_", i, "_", j, ".xlsx"))
        # write.xlsx(test.data, file = paste0("../../Continuous Norming 2/Results/test_", k, "_", i, "_", j, ".xlsx"))
      }
    }
  }
}



crossvalGLM <- function(repetitions) {
  library(glmnet)
    library(gamlss)
  library(openxlsx)
  library(psych)
  load("c:/temp/cross.rda")

  terms <- c(4, 4, 4, 4, 4, 4, 4)
  lowerT <- c(20, 20, 20, 20, 20, 20, 20)
  upperT <- c(80, 80, 80, 80, 80, 80, 80)
  sample <- list(50, 75, 100, 150, 250, 500, 1000)
  difficulty <- list(theta1, theta2, theta3)

  results <- data.frame(
    repetition = integer(),
    sample = integer(),
    theta = integer(),
    t = integer(),
    R2adj = double(),
    gamlssModel0 = integer(),
    gamlssModel1 = integer(),
    gamlssModel2 = integer(),
    timeCNORM = double(),
    timeGAMLSS0 = double(),
    timeGAMLSS1 = double(),
    timeGAMLSS2 = double(),
    train_raw_z = double(),
    train_raw_tcNORM = double(),
    train_raw_tGLM = double(),
    train_raw_tGAMLSS0 = double(),
    train_raw_tGAMLSS1 = double(),
    train_raw_tGAMLSS2 = double(),
    train_z_tcNORM = double(),
    train_z_tGLM = double(),
    train_z_tGAMLSS0 = double(),
    train_z_tGAMLSS1 = double(),
    train_z_tGAMLSS2 = double(),
    train_z_T = double(),
    cross_raw_z = double(),
    cross_raw_tcNORM = double(),
    cross_raw_tGLM = double(),
    cross_raw_tGAMLSS0 = double(),
    cross_raw_tGAMLSS1 = double(),
    cross_raw_tGAMLSS2 = double(),
    cross_z_tcNORM = double(),
    cross_z_tGLM = double(),
    cross_z_tGAMLSS0 = double(),
    cross_z_tGAMLSS1 = double(),
    cross_z_tGAMLSS2 = double(),
    cross_z_T = double(),
    vote0 = double(),
    vote1 = double(),
    vote3 = double()
  )

  descriptives <- data.frame(
    totalMeanTrain = double(),
    totalMedianTrain = double(),
    totalSDTrain = double(),
    totalSkewTrain = double(),
    totalKurtosisTrain = double(),
    totalMeanTest = double(),
    totalMedianTest = double(),
    totalSDTest = double(),
    totalSkewTest = double(),
    totalKurtosisTest = double(),
    meanTrain1 = double(),
    meanTrain2 = double(),
    meanTrain3 = double(),
    meanTrain4 = double(),
    meanTrain5 = double(),
    meanTrain6 = double(),
    meanTrain7 = double(),
    medianTrain1 = double(),
    medianTrain2 = double(),
    medianTrain3 = double(),
    medianTrain4 = double(),
    medianTrain5 = double(),
    medianTrain6 = double(),
    medianTrain7 = double(),
    sDTrain1 = double(),
    sDTrain2 = double(),
    sDTrain3 = double(),
    sDTrain4 = double(),
    sDTrain5 = double(),
    sDTrain6 = double(),
    sDTrain7 = double(),
    skewTrain1 = double(),
    skewTrain2 = double(),
    skewTrain3 = double(),
    skewTrain4 = double(),
    skewTrain5 = double(),
    skewTrain6 = double(),
    skewTrain7 = double(),
    kurtosisTrain1 = double(),
    kurtosisTrain2 = double(),
    kurtosisTrain3 = double(),
    kurtosisTrain4 = double(),
    kurtosisTrain5 = double(),
    kurtosisTrain6 = double(),
    kurtosisTrain7 = double(),
    meanTest1 = double(),
    meanTest2 = double(),
    meanTest3 = double(),
    meanTest4 = double(),
    meanTest5 = double(),
    meanTest6 = double(),
    meanTest7 = double(),
    medianTest1 = double(),
    medianTest2 = double(),
    medianTest3 = double(),
    medianTest4 = double(),
    medianTest5 = double(),
    medianTest6 = double(),
    medianTest7 = double(),
    sDTest1 = double(),
    sDTest2 = double(),
    sDTest3 = double(),
    sDTest4 = double(),
    sDTest5 = double(),
    sDTest6 = double(),
    sDTest7 = double(),
    skewTest1 = double(),
    skewTest2 = double(),
    skewTest3 = double(),
    skewTest4 = double(),
    skewTest5 = double(),
    skewTest6 = double(),
    skewTest7 = double(),
    kurtosisTest1 = double(),
    kurtosisTest2 = double(),
    kurtosisTest3 = double(),
    kurtosisTest4 = double(),
    kurtosisTest5 = double(),
    kurtosisTest6 = double(),
    kurtosisTest7 = double()
  )

  for (k in 1:repetitions) {
    for (i in 1:length(sample)) {
      for (j in 1:length(difficulty)) {
        # preparation
        tmp <- simulateRasch(n = sample[[i]], Theta = difficulty[[j]])
        tmp <- data.frame(age = tmp$sim$age, group = tmp$sim$group, z = tmp$sim$z, m = tmp$sim$m, sd = tmp$sim$sd, latent = tmp$sim$latent, zOverall = tmp$sim$zOverall)
        tmp$zOverall <- (tmp$latent - 5.0973533676) / 3.1268747761
        training.data <- simulateRasch(data = tmp, Theta = difficulty[[j]])
        training.data$data$raw1 <- training.data$data$raw + 1
        test.data <- simulateRasch(data = crossA, Theta = difficulty[[j]])
        test.data$data$raw1 <- test.data$data$raw + 1



        # cNORM
        startC <- proc.time()[[1]]
        training.data$data <- rankBySlidingWindow(training.data$data, age = "age", width = 1, raw = "raw")
        training.data$data <- computePowers(training.data$data, age = "age")
        test.data$data <- rankByGroup(test.data$data)

        t <- terms[i]
        cnorm.model <- bestModel(training.data$data, terms = t)

        if (checkConsistency(cnorm.model, minRaw = 0, maxRaw = length(difficulty[[j]]))) {
          t <- 3
          end <- 24
          cnorm.model <- bestModel(training.data$data, terms = t)

          while (checkConsistency(cnorm.model, minRaw = 0, maxRaw = length(difficulty[[j]])) && t < end) {
            t <- t + 1
            print("Searching for alternative cNorm model")
            cnorm.model <- bestModel(training.data$data, terms = t)
          }
        }
        training.data$data$tCNORM <- predictNorm(training.data$data$raw, training.data$data$age, cnorm.model, lowerT[[i]], upperT[[i]])
        test.data$data$tCNORM <- predictNorm(test.data$data$raw, test.data$data$age, cnorm.model, lowerT[[i]], upperT[[i]])
        timeCNORM <- proc.time()[[1]] - startC
        cross_z_tcNORM <- cor(test.data$sim$z, test.data$data$tCNORM, use = "pairwise.complete.obs")
        cross_z_T <- cor(test.data$sim$z, test.data$data$normValue, use = "pairwise.complete.obs")

        # GLM
        glm.model <- cnorm.model
        x <- as.matrix(training.data$data[,11:34]) # auch hier wieder: Power und Interaktionen von L und A
        y <- training.data$data$raw
        cvfit <- cv.glmnet(x, y, type.measure = "mse", nfolds = 100, alpha=1) # Kreuzvalidiert mit MSE als Kriterium, 100 Teilstichproben für Kreuzvalidierung
        c <- coef(cvfit, s = "lambda.min") # glmnet-Koeffizienten ermitteln
        values <- as.numeric(c@x)
        cols <- c@i + 1
        names <- c@Dimnames[[1]][cols]
        names(values) <- names
        glm.model$coefficients <- values
        training.data$data$tGLM <- predictNorm(training.data$data$raw, training.data$data$age, glm.model, lowerT[[i]], upperT[[i]])
        test.data$data$tGLM <- predictNorm(test.data$data$raw, test.data$data$age, glm.model, lowerT[[i]], upperT[[i]])
        cross_z_tGLM <- cor(test.data$sim$z, test.data$data$tGLM, use = "pairwise.complete.obs")


        # GAMLS 0
        gamlssModel0 <- "Not converged"
        vote0 <- 1
        cross_z_tGAMLSS0 <- 0
        train_raw_tGAMLSS0 <- 0
        train_z_tGAMLSS0 <- 0
        cross_raw_tGAMLSS0 <- 0
        time0 <- proc.time()[[1]]

        tryCatch({
          gamlss.model0 <- lms(raw, age, data = training.data$data, families = c("NO"), pb.method = "GAIC", k = 3)
        }, error = function(e) {
        })

        tryCatch({
          training.data$data$tGAMLSS0 <- z.scores(gamlss.model0, y = training.data$data$raw, x = training.data$data$age) * 10 + 50
          training.data$data$tGAMLSS0[training.data$data$tGAMLSS0 < lowerT[[i]]] <- lowerT[[i]]
          training.data$data$tGAMLSS0[training.data$data$tGAMLSS0 > upperT[[i]]] <- upperT[[i]]
          test.data$data$tGAMLSS0 <- z.scores(gamlss.model0, y = test.data$data$raw, x = test.data$data$age) * 10 + 50
          test.data$data$tGAMLSS0[test.data$data$tGAMLSS0 < lowerT[[i]]] <- lowerT[[i]]
          test.data$data$tGAMLSS0[test.data$data$tGAMLSS0 > upperT[[i]]] <- upperT[[i]]
          gamlssModel0 <- gamlss.model0$family[1]
          train_raw_tGAMLSS0 <- cor(training.data$data$raw, training.data$data$tGAMLSS0, use = "pairwise.complete.obs")
          cross_z_tGAMLSS0 <- cor(test.data$sim$z, test.data$data$tGAMLSS0, use = "pairwise.complete.obs")
          train_z_tGAMLSS0 <- cor(training.data$sim$z, training.data$data$tGAMLSS0, use = "pairwise.complete.obs")
          cross_raw_tGAMLSS0 <- cor(test.data$data$raw, test.data$data$tGAMLSS0, use = "pairwise.complete.obs")

          if (cross_z_tcNORM < cross_z_tGAMLSS0) {
            vote0 <- 0
          }
        }, error = function(e) {
          print("catch1 reached")
        })
        timeGAMLSS0 <- proc.time()[[1]] - time0

        # GAMLS 1
        gamlssModel1 <- "Not converged"
        vote1 <- 1
        cross_z_tGAMLSS1 <- 0
        train_raw_tGAMLSS1 <- 0
        train_z_tGAMLSS1 <- 0
        cross_raw_tGAMLSS1 <- 0
        time1 <- proc.time()[[1]]

        tryCatch({
          gamlss.model1 <- lms(raw1, age, data = training.data$data, c("BCCGo", "BCPEo", "BCTo"), pb.method = "GAIC", k = 3)
        }, error = function(e) {
        })

        tryCatch({
          training.data$data$tGAMLSS1 <- z.scores(gamlss.model1, y = training.data$data$raw1, x = training.data$data$age) * 10 + 50
          training.data$data$tGAMLSS1[training.data$data$tGAMLSS1 < lowerT[[i]]] <- lowerT[[i]]
          training.data$data$tGAMLSS1[training.data$data$tGAMLSS1 > upperT[[i]]] <- upperT[[i]]
          test.data$data$tGAMLSS1 <- z.scores(gamlss.model1, y = test.data$data$raw1, x = test.data$data$age) * 10 + 50
          test.data$data$tGAMLSS1[test.data$data$tGAMLSS1 < lowerT[[i]]] <- lowerT[[i]]
          test.data$data$tGAMLSS1[test.data$data$tGAMLSS1 > upperT[[i]]] <- upperT[[i]]
          gamlssModel1 <- gamlss.model1$family[1]
          train_raw_tGAMLSS1 <- cor(training.data$data$raw1, training.data$data$tGAMLSS1, use = "pairwise.complete.obs")
          cross_z_tGAMLSS1 <- cor(test.data$sim$z, test.data$data$tGAMLSS1, use = "pairwise.complete.obs")
          train_z_tGAMLSS1 <- cor(training.data$sim$z, training.data$data$tGAMLSS1, use = "pairwise.complete.obs")
          cross_raw_tGAMLSS1 <- cor(test.data$data$raw1, test.data$data$tGAMLSS1, use = "pairwise.complete.obs")
          if (cross_z_tcNORM < cross_z_tGAMLSS1) {
            vote1 <- 0
          }
        }, error = function(e) {
          print("catch1 reached")
        })
        timeGAMLSS1 <- proc.time()[[1]] - time1

        # GAMLS 2
        gamlssModel2 <- "Not converged"
        vote2 <- 1
        cross_z_tGAMLSS2 <- 0
        train_raw_tGAMLSS2 <- 0
        train_z_tGAMLSS2 <- 0
        cross_raw_tGAMLSS2 <- 0
        time2 <- proc.time()[[1]]

        tryCatch({
          gamlss.model2 <- lms(raw, age, data = training.data$data, families = c("SHASHo"), pb.method = "GAIC", k = 3)
        }, error = function(e) {
        })

        tryCatch({
          training.data$data$tGAMLSS2 <- z.scores(gamlss.model2, y = training.data$data$raw, x = training.data$data$age) * 10 + 50
          training.data$data$tGAMLSS2[training.data$data$tGAMLSS2 < lowerT[[i]]] <- lowerT[[i]]
          training.data$data$tGAMLSS2[training.data$data$tGAMLSS2 > upperT[[i]]] <- upperT[[i]]
          test.data$data$tGAMLSS2 <- z.scores(gamlss.model2, y = test.data$data$raw, x = test.data$data$age) * 10 + 50
          test.data$data$tGAMLSS2[test.data$data$tGAMLSS2 < lowerT[[i]]] <- lowerT[[i]]
          test.data$data$tGAMLSS2[test.data$data$tGAMLSS2 > upperT[[i]]] <- upperT[[i]]
          gamlssModel2 <- gamlss.model2$family[1]
          train_raw_tGAMLSS2 <- cor(training.data$data$raw, training.data$data$tGAMLSS2, use = "pairwise.complete.obs")
          train_z_tGAMLSS2 <- cor(training.data$sim$z, training.data$data$tGAMLSS2, use = "pairwise.complete.obs")
          cross_raw_tGAMLSS2 <- cor(test.data$data$raw, test.data$data$tGAMLSS2, use = "pairwise.complete.obs")
          cross_z_tGAMLSS2 <- cor(test.data$sim$z, test.data$data$tGAMLSS2, use = "pairwise.complete.obs")
          if (cross_z_tcNORM < cross_z_tGAMLSS2) {
            vote2 <- 0
          }
        }, error = function(e) {
        })

        timeGAMLSS2 <- proc.time()[[1]] - time2

        print(paste0("Saving cycle ", k, " ", i, " ", j))

        r <- data.frame(
          repetition = k,
          sample = length(training.data$data$raw),
          theta = j,
          terms = t,
          R2adj = cnorm.model$subsets$adjr2[[cnorm.model$ideal.model]],
          gamlssModel0 = gamlssModel0,
          gamlssModel1 = gamlssModel1,
          gamlssModel2 = gamlssModel2,
          timeCNORM = timeCNORM,
          timeGAMLSS0 = timeGAMLSS0,
          timeGAMLSS1 = timeGAMLSS1,
          timeGAMLSS2 = timeGAMLSS2,
          train_raw_z = cor(training.data$data$raw, training.data$sim$z, use = "pairwise.complete.obs"),
          train_raw_tcNORM = cor(training.data$data$raw, training.data$data$tCNORM, use = "pairwise.complete.obs"),
          train_raw_tGLM = cor(training.data$data$raw, training.data$data$tGLM, use = "pairwise.complete.obs"),
          train_raw_tGAMLSS0 = train_raw_tGAMLSS0,
          train_raw_tGAMLSS1 = train_raw_tGAMLSS1,
          train_raw_tGAMLSS2 = train_raw_tGAMLSS2,
          train_z_tcNORM = cor(training.data$sim$z, training.data$data$tCNORM, use = "pairwise.complete.obs"),
          train_z_tGLM = cor(training.data$sim$z, training.data$data$tGLM, use = "pairwise.complete.obs"),
          train_z_tGAMLSS0 = train_z_tGAMLSS0,
          train_z_tGAMLSS1 = train_z_tGAMLSS1,
          train_z_tGAMLSS2 = train_z_tGAMLSS2,
          train_z_T = cor(training.data$sim$z, training.data$data$normValue, use = "pairwise.complete.obs"),
          cross_raw_z = cor(test.data$data$raw, test.data$sim$z, use = "pairwise.complete.obs"),
          cross_raw_tcNORM = cor(test.data$data$raw, test.data$data$tCNORM, use = "pairwise.complete.obs"),
          cross_raw_tGLM = cor(test.data$data$raw, test.data$data$tGLM, use = "pairwise.complete.obs"),
          cross_raw_tGAMLSS0 = cross_raw_tGAMLSS0,
          cross_raw_tGAMLSS1 = cross_raw_tGAMLSS1,
          cross_raw_tGAMLSS2 = cross_raw_tGAMLSS2,
          cross_z_tcNORM = cross_z_tcNORM,
          cross_z_tGLM = cross_z_tGLM,
          cross_z_tGAMLSS0 = cross_z_tGAMLSS0,
          cross_z_tGAMLSS1 = cross_z_tGAMLSS1,
          cross_z_tGAMLSS2 = cross_z_tGAMLSS2,
          cross_z_T = cross_z_T,
          vote0 = vote0,
          vote1 = vote1,
          vote2 = vote2
        )
        results <- rbind(results, r)

        totalTrain <- describe(training.data$data$raw)
        totalTest <- describe(test.data$data$raw)
        descTrain <- describeBy(training.data$data$raw, group = training.data$data$group)
        descTest <- describeBy(test.data$data$raw, group = test.data$data$group)


        d <- data.frame(
          totalMeanTrain = totalTrain$mean,
          totalMedianTrain = totalTrain$median,
          totalSDTrain = totalTrain$sd,
          totalSkewTrain = totalTrain$skew,
          totalKurtosisTrain = totalTrain$kurtosis,
          totalMeanTest = totalTest$mean,
          totalMedianTest = totalTest$median,
          totalSDTest = totalTest$sd,
          totalSkewTest = totalTest$skew,
          totalKurtosisTest = totalTest$kurtosis,
          meanTrain1 = descTrain[[1]]$mean,
          meanTrain2 = descTrain[[2]]$mean,
          meanTrain3 = descTrain[[3]]$mean,
          meanTrain4 = descTrain[[4]]$mean,
          meanTrain5 = descTrain[[5]]$mean,
          meanTrain6 = descTrain[[6]]$mean,
          meanTrain7 = descTrain[[7]]$mean,
          medianTrain1 = descTrain[[1]]$median,
          medianTrain2 = descTrain[[2]]$median,
          medianTrain3 = descTrain[[3]]$median,
          medianTrain4 = descTrain[[4]]$median,
          medianTrain5 = descTrain[[5]]$median,
          medianTrain6 = descTrain[[6]]$median,
          medianTrain7 = descTrain[[7]]$median,
          sDTrain1 = descTrain[[1]]$sd,
          sDTrain2 = descTrain[[2]]$sd,
          sDTrain3 = descTrain[[3]]$sd,
          sDTrain4 = descTrain[[4]]$sd,
          sDTrain5 = descTrain[[5]]$sd,
          sDTrain6 = descTrain[[6]]$sd,
          sDTrain7 = descTrain[[7]]$sd,
          skewTrain1 = descTrain[[1]]$skew,
          skewTrain2 = descTrain[[2]]$skew,
          skewTrain3 = descTrain[[3]]$skew,
          skewTrain4 = descTrain[[4]]$skew,
          skewTrain5 = descTrain[[5]]$skew,
          skewTrain6 = descTrain[[6]]$skew,
          skewTrain7 = descTrain[[7]]$skew,
          kurtosisTrain1 = descTrain[[1]]$kurtosis,
          kurtosisTrain2 = descTrain[[2]]$kurtosis,
          kurtosisTrain3 = descTrain[[3]]$kurtosis,
          kurtosisTrain4 = descTrain[[4]]$kurtosis,
          kurtosisTrain5 = descTrain[[5]]$kurtosis,
          kurtosisTrain6 = descTrain[[6]]$kurtosis,
          kurtosisTrain7 = descTrain[[7]]$kurtosis,
          meanTest1 = descTest[[1]]$mean,
          meanTest2 = descTest[[2]]$mean,
          meanTest3 = descTest[[3]]$mean,
          meanTest4 = descTest[[4]]$mean,
          meanTest5 = descTest[[5]]$mean,
          meanTest6 = descTest[[6]]$mean,
          meanTest7 = descTest[[7]]$mean,
          medianTest1 = descTest[[1]]$median,
          medianTest2 = descTest[[2]]$median,
          medianTest3 = descTest[[3]]$median,
          medianTest4 = descTest[[4]]$median,
          medianTest5 = descTest[[5]]$median,
          medianTest6 = descTest[[6]]$median,
          medianTest7 = descTest[[7]]$median,
          sDTest1 = descTest[[1]]$sd,
          sDTest2 = descTest[[2]]$sd,
          sDTest3 = descTest[[3]]$sd,
          sDTest4 = descTest[[4]]$sd,
          sDTest5 = descTest[[5]]$sd,
          sDTest6 = descTest[[6]]$sd,
          sDTest7 = descTest[[7]]$sd,
          skewTest1 = descTest[[1]]$skew,
          skewTest2 = descTest[[2]]$skew,
          skewTest3 = descTest[[3]]$skew,
          skewTest4 = descTest[[4]]$skew,
          skewTest5 = descTest[[5]]$skew,
          skewTest6 = descTest[[6]]$skew,
          skewTest7 = descTest[[7]]$skew,
          kurtosisTest1 = descTest[[1]]$kurtosis,
          kurtosisTest2 = descTest[[2]]$kurtosis,
          kurtosisTest3 = descTest[[3]]$kurtosis,
          kurtosisTest4 = descTest[[4]]$kurtosis,
          kurtosisTest5 = descTest[[5]]$kurtosis,
          kurtosisTest6 = descTest[[6]]$kurtosis,
          kurtosisTest7 = descTest[[7]]$kurtosis
        )
        descriptives <- rbind(descriptives, d)

        # save
        write.table(results, "c:/temp/results.txt", sep = "\t")
        write.table(descriptives, "c:/temp/descriptives.txt", sep = "\t")
        write.table(cbind(results, descriptives), "c:/temp/complete.txt", sep = "\t")

        # save(results, file = "../../Continuous Norming 2/Results/results.rda")
        # write.xlsx(results, file = paste0("../../Continuous Norming 2/Results/results.xlsx"))
        # write.xlsx(training.data, file = paste0("../../Continuous Norming 2/Results/training_", k, "_", i, "_", j, ".xlsx"))
        # write.xlsx(test.data, file = paste0("../../Continuous Norming 2/Results/test_", k, "_", i, "_", j, ".xlsx"))
      }
    }
  }
}

crossvalIntervals <- function(repetitions) {
  library(glmnet)
  library(gamlss)
  library(openxlsx)
  library(psych)
  load("c:/temp/cross.rda")

  terms <- c(4, 4, 4, 4, 4, 4, 4)
  lowerT <- c(20, 20, 20, 20, 20, 20, 20)
  upperT <- c(80, 80, 80, 80, 80, 80, 80)
  sample <- list(100, 150, 200, 250)
  difficulty <- list(theta1, theta2, theta3)

  segments <- data.frame(
    sample = integer(),
    difficulty = integer(),
    leaps1 = double(),
    leaps2 = double(),
    leaps3 = double(),
    leaps4 = double(),
    leaps5 = double(),
    leaps6 = double(),
    leaps7 = double(),
    leaps8 = double(),
    leaps9 = double(),
    leaps10 = double(),
    leaps11 = double(),
    leaps12 = double(),
    leaps13 = double(),
    leaps14 = double(),
    glmnet1 = double(),
    glmnet2 = double(),
    glmnet3 = double(),
    glmnet4 = double(),
    glmnet5 = double(),
    glmnet6 = double(),
    glmnet7 = double(),
    glmnet8 = double(),
    glmnet9 = double(),
    glmnet10 = double(),
    glmnet11 = double(),
    glmnet12 = double(),
    glmnet13 = double(),
    glmnet14 = double(),
    no1 = double(),
    no2 = double(),
    no3 = double(),
    no4 = double(),
    no5 = double(),
    no6 = double(),
    no7 = double(),
    no8 = double(),
    no9 = double(),
    no10 = double(),
    no11 = double(),
    no12 = double(),
    no13 = double(),
    no14 = double(),
    bc1 = double(),
    bc2 = double(),
    bc3 = double(),
    bc4 = double(),
    bc5 = double(),
    bc6 = double(),
    bc7 = double(),
    bc8 = double(),
    bc9 = double(),
    bc10 = double(),
    bc11 = double(),
    bc12 = double(),
    bc13 = double(),
    bc14 = double(),
    shasho1 = double(),
    shasho2 = double(),
    shasho3 = double(),
    shasho4 = double(),
    shasho5 = double(),
    shasho6 = double(),
    shasho7 = double(),
    shasho8 = double(),
    shasho9 = double(),
    shasho10 = double(),
    shasho11 = double(),
    shasho12 = double(),
    shasho13 = double(),
    shasho14 = double()
  )

  results <- data.frame(
    repetition = integer(),
    sample = integer(),
    theta = integer(),
    t = integer(),
    R2adj = double(),
    gamlssModel0 = integer(),
    gamlssModel1 = integer(),
    gamlssModel2 = integer(),
    timeCNORM = double(),
    timeGAMLSS0 = double(),
    timeGAMLSS1 = double(),
    timeGAMLSS2 = double(),
    train_raw_z = double(),
    train_raw_tcNORM = double(),
    train_raw_tGLM = double(),
    train_raw_tGAMLSS0 = double(),
    train_raw_tGAMLSS1 = double(),
    train_raw_tGAMLSS2 = double(),
    train_z_tcNORM = double(),
    train_z_tGLM = double(),
    train_z_tGAMLSS0 = double(),
    train_z_tGAMLSS1 = double(),
    train_z_tGAMLSS2 = double(),
    train_z_T = double(),
    cross_raw_z = double(),
    cross_raw_tcNORM = double(),
    cross_raw_tGLM = double(),
    cross_raw_tGAMLSS0 = double(),
    cross_raw_tGAMLSS1 = double(),
    cross_raw_tGAMLSS2 = double(),
    cross_z_tcNORM = double(),
    cross_z_tGLM = double(),
    cross_z_tGAMLSS0 = double(),
    cross_z_tGAMLSS1 = double(),
    cross_z_tGAMLSS2 = double(),
    cross_z_T = double(),
    vote0 = double(),
    vote1 = double(),
    vote3 = double()
  )

  descriptives <- data.frame(
    totalMeanTrain = double(),
    totalMedianTrain = double(),
    totalSDTrain = double(),
    totalSkewTrain = double(),
    totalKurtosisTrain = double(),
    totalMeanTest = double(),
    totalMedianTest = double(),
    totalSDTest = double(),
    totalSkewTest = double(),
    totalKurtosisTest = double(),
    meanTrain1 = double(),
    meanTrain2 = double(),
    meanTrain3 = double(),
    meanTrain4 = double(),
    meanTrain5 = double(),
    meanTrain6 = double(),
    meanTrain7 = double(),
    medianTrain1 = double(),
    medianTrain2 = double(),
    medianTrain3 = double(),
    medianTrain4 = double(),
    medianTrain5 = double(),
    medianTrain6 = double(),
    medianTrain7 = double(),
    sDTrain1 = double(),
    sDTrain2 = double(),
    sDTrain3 = double(),
    sDTrain4 = double(),
    sDTrain5 = double(),
    sDTrain6 = double(),
    sDTrain7 = double(),
    skewTrain1 = double(),
    skewTrain2 = double(),
    skewTrain3 = double(),
    skewTrain4 = double(),
    skewTrain5 = double(),
    skewTrain6 = double(),
    skewTrain7 = double(),
    kurtosisTrain1 = double(),
    kurtosisTrain2 = double(),
    kurtosisTrain3 = double(),
    kurtosisTrain4 = double(),
    kurtosisTrain5 = double(),
    kurtosisTrain6 = double(),
    kurtosisTrain7 = double(),
    meanTest1 = double(),
    meanTest2 = double(),
    meanTest3 = double(),
    meanTest4 = double(),
    meanTest5 = double(),
    meanTest6 = double(),
    meanTest7 = double(),
    medianTest1 = double(),
    medianTest2 = double(),
    medianTest3 = double(),
    medianTest4 = double(),
    medianTest5 = double(),
    medianTest6 = double(),
    medianTest7 = double(),
    sDTest1 = double(),
    sDTest2 = double(),
    sDTest3 = double(),
    sDTest4 = double(),
    sDTest5 = double(),
    sDTest6 = double(),
    sDTest7 = double(),
    skewTest1 = double(),
    skewTest2 = double(),
    skewTest3 = double(),
    skewTest4 = double(),
    skewTest5 = double(),
    skewTest6 = double(),
    skewTest7 = double(),
    kurtosisTest1 = double(),
    kurtosisTest2 = double(),
    kurtosisTest3 = double(),
    kurtosisTest4 = double(),
    kurtosisTest5 = double(),
    kurtosisTest6 = double(),
    kurtosisTest7 = double()
  )

  for (k in 1:repetitions) {
    for (i in 1:length(sample)) {
      for (j in 1:length(difficulty)) {
        rm(gamlssModel0)
        rm(gamlssModel1)
        rm(gamlssModel2)
        rm(cnorm.model)
        rm(glm.model)

        # preparation
        tmp <- simulateRasch(n = sample[[i]], Theta = difficulty[[j]])
        tmp <- data.frame(age = tmp$sim$age, group = tmp$sim$group, z = tmp$sim$z, m = tmp$sim$m, sd = tmp$sim$sd, latent = tmp$sim$latent, zOverall = tmp$sim$zOverall)
        tmp$zOverall <- (tmp$latent - 5.0973533676) / 3.1268747761
        training.data <- simulateRasch(data = tmp, Theta = difficulty[[j]])
        training.data$data$raw1 <- training.data$data$raw + 1
        test.data <- simulateRasch(data = crossA, Theta = difficulty[[j]])
        test.data$data$raw1 <- test.data$data$raw + 1



        # cNORM
        startC <- proc.time()[[1]]
        training.data$data <- rankBySlidingWindow(training.data$data, age = "age", width = 1, raw = "raw")
        training.data$data <- computePowers(training.data$data, age = "age")
        test.data$data <- rankByGroup(test.data$data)

        t <- terms[i]
        cnorm.model <- bestModel(training.data$data, terms = t)

        if (checkConsistency(cnorm.model, minRaw = 0, maxRaw = length(difficulty[[j]]))) {
          t <- 3
          end <- 24
          cnorm.model <- bestModel(training.data$data, terms = t)

          while (checkConsistency(cnorm.model, minRaw = 0, maxRaw = length(difficulty[[j]])) && t < end) {
            t <- t + 1
            print("Searching for alternative cNorm model")
            cnorm.model <- bestModel(training.data$data, terms = t)
          }
        }

        # GLM
        glm.model <- cnorm.model
        x <- as.matrix(training.data$data[,11:34]) # auch hier wieder: Power und Interaktionen von L und A
        y <- training.data$data$raw
        cvfit <- cv.glmnet(x, y, type.measure = "mse", nfolds = 100, alpha=1) # Kreuzvalidiert mit MSE als Kriterium, 100 Teilstichproben für Kreuzvalidierung
        c <- coef(cvfit, s = "lambda.min") # glmnet-Koeffizienten ermitteln
        values <- as.numeric(c@x)
        cols <- c@i + 1
        names <- c@Dimnames[[1]][cols]
        names(values) <- names
        glm.model$coefficients <- values



        # GAMLS 0
        gamlssModel0 <- "Not converged"
        vote0 <- 1
        cross_z_tGAMLSS0 <- 0
        train_raw_tGAMLSS0 <- 0
        train_z_tGAMLSS0 <- 0
        cross_raw_tGAMLSS0 <- 0
        time0 <- proc.time()[[1]]

        tryCatch({
          gamlss.model0 <- lms(raw, age, data = training.data$data, families = c("NO"), pb.method = "GAIC", k = 3)
        }, error = function(e) {

        })

        tryCatch({
          training.data$data$tGAMLSS0 <- z.scores(gamlss.model0, y = training.data$data$raw, x = training.data$data$age) * 10 + 50
          training.data$data$tGAMLSS0[training.data$data$tGAMLSS0 < lowerT[[i]]] <- lowerT[[i]]
          training.data$data$tGAMLSS0[training.data$data$tGAMLSS0 > upperT[[i]]] <- upperT[[i]]
          test.data$data$tGAMLSS0 <- z.scores(gamlss.model0, y = test.data$data$raw, x = test.data$data$age) * 10 + 50
          test.data$data$tGAMLSS0[test.data$data$tGAMLSS0 < lowerT[[i]]] <- lowerT[[i]]
          test.data$data$tGAMLSS0[test.data$data$tGAMLSS0 > upperT[[i]]] <- upperT[[i]]
          gamlssModel0 <- gamlss.model0$family[1]
          train_raw_tGAMLSS0 <- cor(training.data$data$raw, training.data$data$tGAMLSS0, use = "pairwise.complete.obs")
          cross_z_tGAMLSS0 <- cor(test.data$sim$z, test.data$data$tGAMLSS0, use = "pairwise.complete.obs")
          train_z_tGAMLSS0 <- cor(training.data$sim$z, training.data$data$tGAMLSS0, use = "pairwise.complete.obs")
          cross_raw_tGAMLSS0 <- cor(test.data$data$raw, test.data$data$tGAMLSS0, use = "pairwise.complete.obs")

          if (cross_z_tcNORM < cross_z_tGAMLSS0) {
            vote0 <- 0
          }
        }, error = function(e) {
          print("catch1 reached")
        })
        timeGAMLSS0 <- proc.time()[[1]] - time0

        # GAMLS 1
        gamlssModel1 <- "Not converged"
        vote1 <- 1
        cross_z_tGAMLSS1 <- 0
        train_raw_tGAMLSS1 <- 0
        train_z_tGAMLSS1 <- 0
        cross_raw_tGAMLSS1 <- 0
        time1 <- proc.time()[[1]]

        tryCatch({
          gamlss.model1 <- lms(raw1, age, data = training.data$data, c("BCCGo", "BCPEo", "BCTo"), pb.method = "GAIC", k = 3)
        }, error = function(e) {

        })

        tryCatch({
          training.data$data$tGAMLSS1 <- z.scores(gamlss.model1, y = training.data$data$raw1, x = training.data$data$age) * 10 + 50
          training.data$data$tGAMLSS1[training.data$data$tGAMLSS1 < lowerT[[i]]] <- lowerT[[i]]
          training.data$data$tGAMLSS1[training.data$data$tGAMLSS1 > upperT[[i]]] <- upperT[[i]]
          test.data$data$tGAMLSS1 <- z.scores(gamlss.model1, y = test.data$data$raw1, x = test.data$data$age) * 10 + 50
          test.data$data$tGAMLSS1[test.data$data$tGAMLSS1 < lowerT[[i]]] <- lowerT[[i]]
          test.data$data$tGAMLSS1[test.data$data$tGAMLSS1 > upperT[[i]]] <- upperT[[i]]
          gamlssModel1 <- gamlss.model1$family[1]
          train_raw_tGAMLSS1 <- cor(training.data$data$raw1, training.data$data$tGAMLSS1, use = "pairwise.complete.obs")
          cross_z_tGAMLSS1 <- cor(test.data$sim$z, test.data$data$tGAMLSS1, use = "pairwise.complete.obs")
          train_z_tGAMLSS1 <- cor(training.data$sim$z, training.data$data$tGAMLSS1, use = "pairwise.complete.obs")
          cross_raw_tGAMLSS1 <- cor(test.data$data$raw1, test.data$data$tGAMLSS1, use = "pairwise.complete.obs")
          if (cross_z_tcNORM < cross_z_tGAMLSS1) {
            vote1 <- 0
          }
        }, error = function(e) {

          print("catch1 reached")
        })
        timeGAMLSS1 <- proc.time()[[1]] - time1

        # GAMLS 2
        gamlssModel2 <- "Not converged"
        vote2 <- 1
        cross_z_tGAMLSS2 <- 0
        train_raw_tGAMLSS2 <- 0
        train_z_tGAMLSS2 <- 0
        cross_raw_tGAMLSS2 <- 0
        time2 <- proc.time()[[1]]

        tryCatch({
          gamlss.model2 <- lms(raw, age, data = training.data$data, families = c("SHASHo"), pb.method = "GAIC", k = 3)
        }, error = function(e) {
        })

        tryCatch({
          training.data$data$tGAMLSS2 <- z.scores(gamlss.model2, y = training.data$data$raw, x = training.data$data$age) * 10 + 50
          training.data$data$tGAMLSS2[training.data$data$tGAMLSS2 < lowerT[[i]]] <- lowerT[[i]]
          training.data$data$tGAMLSS2[training.data$data$tGAMLSS2 > upperT[[i]]] <- upperT[[i]]
          test.data$data$tGAMLSS2 <- z.scores(gamlss.model2, y = test.data$data$raw, x = test.data$data$age) * 10 + 50
          test.data$data$tGAMLSS2[test.data$data$tGAMLSS2 < lowerT[[i]]] <- lowerT[[i]]
          test.data$data$tGAMLSS2[test.data$data$tGAMLSS2 > upperT[[i]]] <- upperT[[i]]
          gamlssModel2 <- gamlss.model2$family[1]
          train_raw_tGAMLSS2 <- cor(training.data$data$raw, training.data$data$tGAMLSS2, use = "pairwise.complete.obs")
          train_z_tGAMLSS2 <- cor(training.data$sim$z, training.data$data$tGAMLSS2, use = "pairwise.complete.obs")
          cross_raw_tGAMLSS2 <- cor(test.data$data$raw, test.data$data$tGAMLSS2, use = "pairwise.complete.obs")
          cross_z_tGAMLSS2 <- cor(test.data$sim$z, test.data$data$tGAMLSS2, use = "pairwise.complete.obs")
          if (cross_z_tcNORM < cross_z_tGAMLSS2) {
            vote2 <- 0
          }
        }, error = function(e) {
        })

        timeGAMLSS2 <- proc.time()[[1]] - time2


        if(is.null(test.data$data$tGAMLSS0)||is.null(test.data$data$tGAMLSS1)||is.null(test.data$data$tGAMLSS2)){
          message("Partly not converged, skipping trial")
          next
        }

        training.data$data$tGLM <- predictNorm(training.data$data$raw, training.data$data$age, glm.model, lowerT[[i]], upperT[[i]])
        test.data$data$tGLM <- predictNorm(test.data$data$raw, test.data$data$age, glm.model, lowerT[[i]], upperT[[i]])
        cross_z_tGLM <- cor(test.data$sim$z, test.data$data$tGLM, use = "pairwise.complete.obs")

        training.data$data$tCNORM <- predictNorm(training.data$data$raw, training.data$data$age, cnorm.model, lowerT[[i]], upperT[[i]])
        test.data$data$tCNORM <- predictNorm(test.data$data$raw, test.data$data$age, cnorm.model, lowerT[[i]], upperT[[i]])
        timeCNORM <- proc.time()[[1]] - startC
        cross_z_tcNORM <- cor(test.data$sim$z, test.data$data$tCNORM, use = "pairwise.complete.obs")
        cross_z_T <- cor(test.data$sim$z, test.data$data$normValue, use = "pairwise.complete.obs")



        r <- data.frame(
          repetition = k,
          sample = length(training.data$data$raw),
          theta = j,
          terms = t,
          R2adj = cnorm.model$subsets$adjr2[[cnorm.model$ideal.model]],
          gamlssModel0 = gamlssModel0,
          gamlssModel1 = gamlssModel1,
          gamlssModel2 = gamlssModel2,
          timeCNORM = timeCNORM,
          timeGAMLSS0 = timeGAMLSS0,
          timeGAMLSS1 = timeGAMLSS1,
          timeGAMLSS2 = timeGAMLSS2,
          train_raw_z = cor(training.data$data$raw, training.data$sim$z, use = "pairwise.complete.obs"),
          train_raw_tcNORM = cor(training.data$data$raw, training.data$data$tCNORM, use = "pairwise.complete.obs"),
          train_raw_tGLM = cor(training.data$data$raw, training.data$data$tGLM, use = "pairwise.complete.obs"),
          train_raw_tGAMLSS0 = train_raw_tGAMLSS0,
          train_raw_tGAMLSS1 = train_raw_tGAMLSS1,
          train_raw_tGAMLSS2 = train_raw_tGAMLSS2,
          train_z_tcNORM = cor(training.data$sim$z, training.data$data$tCNORM, use = "pairwise.complete.obs"),
          train_z_tGLM = cor(training.data$sim$z, training.data$data$tGLM, use = "pairwise.complete.obs"),
          train_z_tGAMLSS0 = train_z_tGAMLSS0,
          train_z_tGAMLSS1 = train_z_tGAMLSS1,
          train_z_tGAMLSS2 = train_z_tGAMLSS2,
          train_z_T = cor(training.data$sim$z, training.data$data$normValue, use = "pairwise.complete.obs"),
          cross_raw_z = cor(test.data$data$raw, test.data$sim$z, use = "pairwise.complete.obs"),
          cross_raw_tcNORM = cor(test.data$data$raw, test.data$data$tCNORM, use = "pairwise.complete.obs"),
          cross_raw_tGLM = cor(test.data$data$raw, test.data$data$tGLM, use = "pairwise.complete.obs"),
          cross_raw_tGAMLSS0 = cross_raw_tGAMLSS0,
          cross_raw_tGAMLSS1 = cross_raw_tGAMLSS1,
          cross_raw_tGAMLSS2 = cross_raw_tGAMLSS2,
          cross_z_tcNORM = cross_z_tcNORM,
          cross_z_tGLM = cross_z_tGLM,
          cross_z_tGAMLSS0 = cross_z_tGAMLSS0,
          cross_z_tGAMLSS1 = cross_z_tGAMLSS1,
          cross_z_tGAMLSS2 = cross_z_tGAMLSS2,
          cross_z_T = cross_z_T,
          vote0 = vote0,
          vote1 = vote1,
          vote2 = vote2
        )
        results <- rbind(results, r)

        totalTrain <- describe(training.data$data$raw)
        totalTest <- describe(test.data$data$raw)
        descTrain <- describeBy(training.data$data$raw, group = training.data$data$group)
        descTest <- describeBy(test.data$data$raw, group = test.data$data$group)


        d <- data.frame(
          totalMeanTrain = totalTrain$mean,
          totalMedianTrain = totalTrain$median,
          totalSDTrain = totalTrain$sd,
          totalSkewTrain = totalTrain$skew,
          totalKurtosisTrain = totalTrain$kurtosis,
          totalMeanTest = totalTest$mean,
          totalMedianTest = totalTest$median,
          totalSDTest = totalTest$sd,
          totalSkewTest = totalTest$skew,
          totalKurtosisTest = totalTest$kurtosis,
          meanTrain1 = descTrain[[1]]$mean,
          meanTrain2 = descTrain[[2]]$mean,
          meanTrain3 = descTrain[[3]]$mean,
          meanTrain4 = descTrain[[4]]$mean,
          meanTrain5 = descTrain[[5]]$mean,
          meanTrain6 = descTrain[[6]]$mean,
          meanTrain7 = descTrain[[7]]$mean,
          medianTrain1 = descTrain[[1]]$median,
          medianTrain2 = descTrain[[2]]$median,
          medianTrain3 = descTrain[[3]]$median,
          medianTrain4 = descTrain[[4]]$median,
          medianTrain5 = descTrain[[5]]$median,
          medianTrain6 = descTrain[[6]]$median,
          medianTrain7 = descTrain[[7]]$median,
          sDTrain1 = descTrain[[1]]$sd,
          sDTrain2 = descTrain[[2]]$sd,
          sDTrain3 = descTrain[[3]]$sd,
          sDTrain4 = descTrain[[4]]$sd,
          sDTrain5 = descTrain[[5]]$sd,
          sDTrain6 = descTrain[[6]]$sd,
          sDTrain7 = descTrain[[7]]$sd,
          skewTrain1 = descTrain[[1]]$skew,
          skewTrain2 = descTrain[[2]]$skew,
          skewTrain3 = descTrain[[3]]$skew,
          skewTrain4 = descTrain[[4]]$skew,
          skewTrain5 = descTrain[[5]]$skew,
          skewTrain6 = descTrain[[6]]$skew,
          skewTrain7 = descTrain[[7]]$skew,
          kurtosisTrain1 = descTrain[[1]]$kurtosis,
          kurtosisTrain2 = descTrain[[2]]$kurtosis,
          kurtosisTrain3 = descTrain[[3]]$kurtosis,
          kurtosisTrain4 = descTrain[[4]]$kurtosis,
          kurtosisTrain5 = descTrain[[5]]$kurtosis,
          kurtosisTrain6 = descTrain[[6]]$kurtosis,
          kurtosisTrain7 = descTrain[[7]]$kurtosis,
          meanTest1 = descTest[[1]]$mean,
          meanTest2 = descTest[[2]]$mean,
          meanTest3 = descTest[[3]]$mean,
          meanTest4 = descTest[[4]]$mean,
          meanTest5 = descTest[[5]]$mean,
          meanTest6 = descTest[[6]]$mean,
          meanTest7 = descTest[[7]]$mean,
          medianTest1 = descTest[[1]]$median,
          medianTest2 = descTest[[2]]$median,
          medianTest3 = descTest[[3]]$median,
          medianTest4 = descTest[[4]]$median,
          medianTest5 = descTest[[5]]$median,
          medianTest6 = descTest[[6]]$median,
          medianTest7 = descTest[[7]]$median,
          sDTest1 = descTest[[1]]$sd,
          sDTest2 = descTest[[2]]$sd,
          sDTest3 = descTest[[3]]$sd,
          sDTest4 = descTest[[4]]$sd,
          sDTest5 = descTest[[5]]$sd,
          sDTest6 = descTest[[6]]$sd,
          sDTest7 = descTest[[7]]$sd,
          skewTest1 = descTest[[1]]$skew,
          skewTest2 = descTest[[2]]$skew,
          skewTest3 = descTest[[3]]$skew,
          skewTest4 = descTest[[4]]$skew,
          skewTest5 = descTest[[5]]$skew,
          skewTest6 = descTest[[6]]$skew,
          skewTest7 = descTest[[7]]$skew,
          kurtosisTest1 = descTest[[1]]$kurtosis,
          kurtosisTest2 = descTest[[2]]$kurtosis,
          kurtosisTest3 = descTest[[3]]$kurtosis,
          kurtosisTest4 = descTest[[4]]$kurtosis,
          kurtosisTest5 = descTest[[5]]$kurtosis,
          kurtosisTest6 = descTest[[6]]$kurtosis,
          kurtosisTest7 = descTest[[7]]$kurtosis
        )
        descriptives <- rbind(descriptives, d)

        parts <-data.frame(
          z = test.data$sim$z,
          t = test.data$data$normValue,
          leaps = test.data$data$tCNORM,
          glm = test.data$data$tGLM,
          no = test.data$data$tGAMLSS0,
          bc = test.data$data$tGAMLSS1,
          shasho = test.data$data$tGAMLSS2
        )


        parts$interval <- findInterval(parts$t, c(20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80))
        means <- data.frame(leaps = aggregate(abs(parts$t-parts$leaps), list(parts$interval), mean, na.rm=T)$x,
                            glmnet = aggregate(abs(parts$t-parts$glm), list(parts$interval), mean, na.rm=T)$x,
                            no = aggregate(abs(parts$t-parts$no), list(parts$interval), mean, na.rm=T)$x,
                            bc = aggregate(abs(parts$t-parts$bc), list(parts$interval), mean, na.rm=T)$x,
                            shasho = aggregate(abs(parts$t-parts$shasho), list(parts$interval), mean, na.rm=T)$x)
        s <- data.frame(
          sample = sample[[i]],
          difficulty = j,
          leaps1 = means[1, 1],
          leaps2 = means[2, 1],
          leaps3 = means[3, 1],
          leaps4 = means[4, 1],
          leaps5 = means[5, 1],
          leaps6 = means[6, 1],
          leaps7 = means[7, 1],
          leaps8 = means[8, 1],
          leaps9 = means[9, 1],
          leaps10 = means[10, 1],
          leaps11 = means[11, 1],
          leaps12 = means[12, 1],
          leaps13 = means[13, 1],
          leaps14 = means[14, 1],
          glmnet1 = means[1, 2],
          glmnet2 = means[2, 2],
          glmnet3 = means[3, 2],
          glmnet4 = means[4, 2],
          glmnet5 = means[5, 2],
          glmnet6 = means[6, 2],
          glmnet7 = means[7, 2],
          glmnet8 = means[8, 2],
          glmnet9 = means[9, 2],
          glmnet10 = means[10, 2],
          glmnet11 = means[11, 2],
          glmnet12 = means[12, 2],
          glmnet13 = means[13, 2],
          glmnet14 = means[14, 2],
          no1 = means[1, 3],
          no2 = means[2, 3],
          no3 = means[3, 3],
          no4 = means[4, 3],
          no5 = means[5, 3],
          no6 = means[6, 3],
          no7 = means[7, 3],
          no8 = means[8, 3],
          no9 = means[9, 3],
          no10 = means[10, 3],
          no11 = means[11, 3],
          no12 = means[12, 3],
          no13 = means[13, 3],
          no14 = means[14, 3],
          bc1 = means[1, 4],
          bc2 = means[2, 4],
          bc3 = means[3, 4],
          bc4 = means[4, 4],
          bc5 = means[5, 4],
          bc6 = means[6, 4],
          bc7 = means[7, 4],
          bc8 = means[8, 4],
          bc9 = means[9, 4],
          bc10 = means[10, 4],
          bc11 = means[11, 4],
          bc12 = means[12, 4],
          bc13 = means[13, 4],
          bc14 = means[14, 4],
          shasho1 = means[1, 5],
          shasho2 = means[2, 5],
          shasho3 = means[3, 5],
          shasho4 = means[4, 5],
          shasho5 = means[5, 5],
          shasho6 = means[6, 5],
          shasho7 = means[7, 5],
          shasho8 = means[8, 5],
          shasho9 = means[9, 5],
          shasho10 = means[10, 5],
          shasho11 = means[11, 5],
          shasho12 = means[12, 5],
          shasho13 = means[13, 5],
          shasho14 = means[14, 5]
        )

        segments <- rbind(segments, s)

        # save
        write.table(results, "c:/temp/results3.txt", sep = "\t")
        write.table(descriptives, "c:/temp/descriptives3.txt", sep = "\t")
        write.table(cbind(results, descriptives), "c:/temp/complete3.txt", sep = "\t")

        save(segments, file = "c:/temp/segments3.rda")
        write.xlsx(segments, file = "c:/temp/segments3.xlsx")
        write.table(segments, "c:/temp/segments3.txt", sep = "\t")
        #write.xlsx(parts, file = paste0("c:/temp/parts", k, "_", i, "_", j,".xlsx"))
        #write.xlsx(means, file = paste0("c:/temp/means", k, "_", i, "_", j,".xlsx"))

        # write.xlsx(training.data, file = paste0("../../Continuous Norming 2/Results/training_", k, "_", i, "_", j, ".xlsx"))
        # write.xlsx(test.data, file = paste0("../../Continuous Norming 2/Results/test_", k, "_", i, "_", j, ".xlsx"))
      }
    }
  }
}
