  # Simulate conventional versus regression based norming (SPCN)
  #
  # The function conducts the simulation, writes the results in a text file.
  # Training and validation samples and scales differing in length are drawn and an 1PL IRT-based
  # simulation is applied to both in order to generate a raw score distribution. These raw score
  # distributions are then converted to norm scores via SPCN or through conventional norming with
  # age brackets of 1 month, 3 month, 6 month oder 12 month.
  #
  # Example:
  # simulate(10, "results.txt")
simulate <- function(repetitions, file, append = TRUE, header = TRUE) {
  require(cNORM)
  require(gamlss)
  require(gamlss.tr)

  if(is.null(file)){
    warning("Please specify the filename")
    stop()

  }

  if(is.null(repetitions)){
    warning("Please specify the number of repetitions")
    stop()
  }

  start <- Sys.time()

  # define basic variables
  # upper and lower T score boundary
  lowerT <- 20
  upperT <- 80

  # sample sizes of the total sample for the span of 7 years
  sample <- list(700, 1400, 2800)

  # number of items per scale
  nItems <- list(30)

  # mean item difficulties, with -1 denoting easy and +1 denoting difficult scales
  meanDelta <- list(-1, 0, 1)

  # open file for writing and write header
  if(header)
    cat("Cycle\tSample\tItems\tTerms\tMeanDelta\tnaTraincNORM\tnaTrainBeta\trTrainRawZ\trTrainRawcNORM\trTrainRawBeta\trTrainRawT1\trTrainRawT3\trTrainRawT6\trTrainRawT12\trTrainZcNORM\trTrainZBeta\trTrainZT1\trTrainZT3\trTrainZT6\trTrainZT12\trTraincNORM_T1\trTraincNORM_T3\trTraincNORM_T6\trTraincNORM_T12\trTrainBeta_T1\trTrainBeta_T3\trTrainBeta_T6\trTrainBeta_T12\tnaCrosscNORM\tnaCrossBeta\tnaCrossT1\tnaCrossT3\tnaCrossT6\tnaCrossT12\trCrossRawZ\trCrossRawT\trCrossZ_T\trCrossRawcNORM\trCrossRawBeta\trCrossRawT1\trCrossRawT3\trCrossRawT6\trCrossRawT12\trCrossZ_cNORM\trCrossZ_Beta\trCrossZ_T1\trCrossZ_T3\trCrossZ_T6\trCrossZ_T12\trCrossT_cNORM\trCrossT_Beta\trCrossT_T1\trCrossT_T3\trCrossT_T6\trCrossT_T12\trCrosscNORM_T1\trCrosscNORM_T3\trCrosscNORM_T6\trCrosscNORM_T12\trCrossBeta_T1\trCrossBeta_T3\trCrossBeta_T6\trCrossBeta_T12\tMSE_cNORM\tMSE_Beta\tMSE_T1\tMSE_T3\tMSE_T6\tMSE_T12\tBiascNORM\tBiasBeta\tBiasT1\tBiasT3\tBiasT6\tBiasT12\tMAD_cNORM\tMAD_Beta\tMAD_T1\tMAD_T3\tMAD_T6\tMAD_T12\tmeanIdealT\tmeanT_cNORM\tmeanT_Beta\tmeanT1\tmeanT3\tmeanT6\tmeanT12\tsdIdealT\tsdT_cNORM\tsdT_Beta\tsdT1\tsdT3\tsdT6\tsdT12", file=file, append = append)


  # start loop for all combinations of scales, scale difficulties, sample size and repetitions as desired
  for (k in 1:repetitions) {
    for (i in 1:length(sample)) {
      for (m in 1:length(meanDelta)) {
        for (j in 1:length(nItems)) {

          # prepare population and scales
          difficulty <- rnorm(nItems[[j]], 0 + meanDelta[[m]], 1) # randomly draw item difficulties
          cross <- generateCrossValidationSample(500) # cross validation sample with 10 000 cases each month, resulting 840 000 cases
          train <- generateData(0.5, 7.5, sample[[i]])  # norm sample from age 0.5 to 7.5 with sample size i

          # simulate test results for training and cross validation
          cross.data <- simulateRasch(data = cross, Theta = difficulty)
          train.data <- simulateRasch(data = train, Theta = difficulty)

          # grouping variable for conventional norming
          group1 <- recodeGroup(train.data$data$age, 1)
          group3 <- recodeGroup(train.data$data$age, 3)
          group6 <- recodeGroup(train.data$data$age, 6)
          group12 <- recodeGroup(train.data$data$age, 12)

          # prepare  cNORM model; first rank data
          #cnorm.data <- rankBySlidingWindow(train.data$data, age = "age", width = 1, raw = "raw")
          cnorm.data <- rankByGroup(train.data$data, group=group12, raw = "raw")
          cnorm.data <- computePowers(cnorm.data, age = "age")

          # prepare conventional norm score lookup table
          # repeat ranking with rankByGroup and delete duplicate entries
          normTable1 <- data.frame(group = group1, raw = train.data$data$raw)
          normTable3 <- data.frame(group = group3, raw = train.data$data$raw)
          normTable6 <- data.frame(group = group6, raw = train.data$data$raw)
          normTable12 <- data.frame(group = group12, raw = train.data$data$raw)

          normTable1 <- rankByGroup(normTable1)
          normTable3 <- rankByGroup(normTable3)
          normTable6 <- rankByGroup(normTable6)
          normTable12 <- rankByGroup(normTable12)

          train.data$data$T1train <- normTable1$normValue
          train.data$data$T3train <- normTable3$normValue
          train.data$data$T6train <- normTable6$normValue
          train.data$data$T12train <- normTable12$normValue

          normTable1$hash <- normTable1$group * 10000 + normTable1$raw
          normTable3$hash <- normTable3$group * 10000 + normTable3$raw
          normTable6$hash <- normTable6$group * 10000 + normTable6$raw
          normTable12$hash <- normTable12$group * 10000 + normTable12$raw

          #normTable1 <- normTable1[!duplicated(normTable1[, c("hash")]), ]
          normTable3 <- normTable3[!duplicated(normTable3[, c("hash")]), ]
          normTable6 <- normTable6[!duplicated(normTable6[, c("hash")]), ]
          normTable12 <- normTable12[!duplicated(normTable12[, c("hash")]), ]

          # prepare ideal norm
          crossX <- cross.data$data
          crossX$z <- cross.data$sim$z

          # rank cross validation data
          cross.data$data <- rankByGroup(cross.data$data)
          cross.data$data$normValue[cross.data$data$normValue < lowerT] <- lowerT
          cross.data$data$normValue[cross.data$data$normValue > upperT] <- upperT

          cross.data$data$group1 <- recodeGroup(cross.data$data$age, 1)
          cross.data$data$group3 <- recodeGroup(cross.data$data$age, 3)
          cross.data$data$group6 <- recodeGroup(cross.data$data$age, 6)
          cross.data$data$group12 <- recodeGroup(cross.data$data$age, 12)

          cross.data$data$hash1 <- cross.data$data$group1 * 10000 + cross.data$data$raw
          cross.data$data$hash3 <- cross.data$data$group3 * 10000 + cross.data$data$raw
          cross.data$data$hash6 <- cross.data$data$group6 * 10000 + cross.data$data$raw
          cross.data$data$hash12 <- cross.data$data$group12 * 10000 + cross.data$data$raw


          # build cNORM model, assume t = 4 as default solution
          t <- 5
          cnorm.model <- bestModel(cnorm.data, terms = t, plot=FALSE)

          # in case of inconsistencies, search for consistent model
          # start with three terms and continue up to maximum number of 24 in k = 4
          if (checkConsistency(cnorm.model, minRaw = 0, maxRaw = nItems[[j]], minNorm=30, maxNorm= 70, silent=TRUE)) {
            t <- 5
            end <- 12
            cnorm.model <- bestModel(cnorm.data, terms = t, plot=FALSE)

            while (checkConsistency(cnorm.model, minRaw = 0, maxRaw = nItems[[j]], stepNorm = 5, silent = TRUE) && t < end) {
              t <- t + 1
              cnorm.model <- bestModel(cnorm.data, terms = t, plot=FALSE)
            }
          }

          # predict training and validation norm scores based on cNORM model
          tCNORM.train <- predictNorm(train.data$data$raw, train.data$data$age, cnorm.model, lowerT, upperT)
          tCNORM.cross <- predictNorm(cross.data$data$raw, cross.data$data$age, cnorm.model, lowerT, upperT)


          # apply conventional norming to cross validation data via lookup data
          crossX <- cross.data$data
          crossX$T <- cross.data$data$normValue

          indices1 <- match(crossX$hash1, normTable1$hash)
          indices3 <- match(crossX$hash3, normTable3$hash)
          indices6 <- match(crossX$hash6, normTable6$hash)
          indices12 <- match(crossX$hash12, normTable12$hash)


          # apply beta binomial modelling
          param <- betaByGroup(train.data$data$raw, group12, nItems[[j]])
          beta.model <- betaContinuous(param, 4, 4)
          TBetaTrain <- predictBeta(beta.model, train.data$data$raw, train.data$data$age)$z*10 + 50
          TBetaCross <- predictBeta(beta.model, cross.data$data$raw, cross.data$data$age)$z*10 + 50

          TBetaTrain[TBetaTrain<lowerT] <- lowerT
          TBetaTrain[TBetaTrain>upperT] <- upperT
          TBetaTrain[is.infinite(TBetaTrain)] <- NA

          TBetaCross[TBetaCross<lowerT] <- lowerT
          TBetaCross[TBetaCross>upperT] <- upperT
          TBetaCross[is.infinite(TBetaCross)] <- NA

          ############### RESULTS #############################
          # manifest and conventional norms in training
          rawTrain <- train.data$data$raw
          zTrain <- train.data$data$z
          T1Train <- train.data$data$T1train
          T3Train <- train.data$data$T3train
          T6Train <- train.data$data$T6train
          T12Train <- train.data$data$T12train

          T1Train[T1Train<lowerT] <- lowerT
          T1Train[T1Train>upperT] <- upperT

          T3Train[T3Train<lowerT] <- lowerT
          T3Train[T3Train>upperT] <- upperT

          T6Train[T6Train<lowerT] <- lowerT
          T6Train[T6Train>upperT] <- upperT

          T12Train[T12Train<lowerT] <- lowerT
          T12Train[T12Train>upperT] <- upperT


          # manifest and conventional norms in cross validation
          raw <- cross.data$data$raw
          z <- cross.data$sim$z
          T <- cross.data$data$normValue
          T1 <- normTable1$normValue[indices1]
          T3 <- normTable3$normValue[indices3]
          T6 <- normTable6$normValue[indices6]
          T12 <- normTable12$normValue[indices12]

          T[T<lowerT] <- lowerT
          T[T>upperT] <- upperT

          T1[T1<lowerT] <- lowerT
          T1[T1>upperT] <- upperT

          T3[T3<lowerT] <- lowerT
          T3[T3>upperT] <- upperT

          T6[T6<lowerT] <- lowerT
          T6[T6>upperT] <- upperT

          T12[T12<lowerT] <- lowerT
          T12[T12>upperT] <- upperT

          # mse, accuracy, precision, bias
          mseCNORM <- mean((T - tCNORM.cross)^2, na.rm = TRUE)
          mseBeta <- mean((T - TBetaCross)^2, na.rm = TRUE)
          mseConv1 <- mean((T - T1)^2, na.rm = TRUE)
          mseConv3 <- mean((T - T3)^2, na.rm = TRUE)
          mseConv6 <- mean((T - T6)^2, na.rm = TRUE)
          mseConv12 <- mean((T - T12)^2, na.rm = TRUE)

          varCNORM <- mean((tCNORM.cross - mean(tCNORM.cross, na.rm = TRUE))^2, na.rm = TRUE)
          varBeta <- mean((tCNORM.cross - mean(TBetaCross, na.rm = TRUE))^2, na.rm = TRUE)
          varT1 <- mean((T1 - mean(T1, na.rm = TRUE))^2, na.rm = TRUE)
          varT3 <- mean((T3 - mean(T3, na.rm = TRUE))^2, na.rm = TRUE)
          varT6 <- mean((T6 - mean(T6, na.rm = TRUE))^2, na.rm = TRUE)
          varT12 <- mean((T12 - mean(T12, na.rm = TRUE))^2, na.rm = TRUE)

          biasCNORM <- mean(tCNORM.cross - T, na.rm = TRUE)
          biasBeta <- mean(TBetaCross - T, na.rm = TRUE)
          biasT1 <- mean(T1 - T, na.rm = TRUE)
          biasT3 <- mean(T3 - T, na.rm = TRUE)
          biasT6 <- mean(T6 - T, na.rm = TRUE)
          biasT12 <- mean(T12 - T, na.rm = TRUE)

          MAD_CNORM <- mean(abs(tCNORM.cross - T), na.rm = TRUE)
          MAD_Beta <- mean(abs(TBetaCross - T), na.rm = TRUE)
          MAD_T1 <- mean(abs(T1 - T), na.rm = TRUE)
          MAD_T3 <- mean(abs(T3 - T), na.rm = TRUE)
          MAD_T6 <- mean(abs(T6 - T), na.rm = TRUE)
          MAD_T12 <- mean(abs(T12 - T), na.rm = TRUE)

          text <- paste0(c(k,
                          sample[[i]],
                          nItems[[j]],
                          t,
                          meanDelta[[m]],
                          sum(is.na(tCNORM.train)),
                          sum(is.na(TBetaTrain)),
                          cor(zTrain, rawTrain, use = "pairwise.complete.obs"),

                          cor(tCNORM.train, rawTrain, use = "pairwise.complete.obs"),
                          cor(TBetaTrain, rawTrain, use = "pairwise.complete.obs"),
                          cor(T3Train, rawTrain, use = "pairwise.complete.obs"),
                          cor(T1Train, rawTrain, use = "pairwise.complete.obs"),
                          cor(T6Train, rawTrain, use = "pairwise.complete.obs"),
                          cor(T12Train, rawTrain, use = "pairwise.complete.obs"),

                          cor(tCNORM.train, zTrain, use = "pairwise.complete.obs"),
                          cor(TBetaTrain, zTrain, use = "pairwise.complete.obs"),
                          cor(T1Train, zTrain, use = "pairwise.complete.obs"),
                          cor(T3Train, zTrain, use = "pairwise.complete.obs"),
                          cor(T6Train, zTrain, use = "pairwise.complete.obs"),
                          cor(T12Train, zTrain, use = "pairwise.complete.obs"),

                          cor(T1Train, tCNORM.train, use = "pairwise.complete.obs"),
                          cor(T3Train, tCNORM.train, use = "pairwise.complete.obs"),
                          cor(T6Train, tCNORM.train, use = "pairwise.complete.obs"),
                          cor(T12Train, tCNORM.train, use = "pairwise.complete.obs"),

                          cor(T1Train, TBetaTrain, use = "pairwise.complete.obs"),
                          cor(T3Train, TBetaTrain, use = "pairwise.complete.obs"),
                          cor(T6Train, TBetaTrain, use = "pairwise.complete.obs"),
                          cor(T12Train, TBetaTrain, use = "pairwise.complete.obs"),

                          sum(is.na(tCNORM.cross)),
                          sum(is.na(TBetaCross)),
                          sum(is.na(T1)),
                          sum(is.na(T3)),
                          sum(is.na(T6)),
                          sum(is.na(T12)),

                          cor(raw, z, use = "pairwise.complete.obs"),
                          cor(raw, T, use = "pairwise.complete.obs"),
                          cor(T, z, use = "pairwise.complete.obs"),

                          cor(tCNORM.cross, raw, use = "pairwise.complete.obs"),
                          cor(TBetaCross, raw, use = "pairwise.complete.obs"),
                          cor(T1, raw, use = "pairwise.complete.obs"),
                          cor(T3, raw, use = "pairwise.complete.obs"),
                          cor(T6, raw, use = "pairwise.complete.obs"),
                          cor(T12, raw, use = "pairwise.complete.obs"),

                          cor(tCNORM.cross, z, use = "pairwise.complete.obs"),
                          cor(TBetaCross, z, use = "pairwise.complete.obs"),
                          cor(T1, z, use = "pairwise.complete.obs"),
                          cor(T3, z, use = "pairwise.complete.obs"),
                          cor(T6, z, use = "pairwise.complete.obs"),
                          cor(T12, z, use = "pairwise.complete.obs"),

                          cor(tCNORM.cross, T, use = "pairwise.complete.obs"),
                          cor(TBetaCross, T, use = "pairwise.complete.obs"),
                          cor(T1, T, use = "pairwise.complete.obs"),
                          cor(T3, T, use = "pairwise.complete.obs"),
                          cor(T6, T, use = "pairwise.complete.obs"),
                          cor(T12, T, use = "pairwise.complete.obs"),

                          cor(T1, tCNORM.cross, use = "pairwise.complete.obs"),
                          cor(T3, tCNORM.cross, use = "pairwise.complete.obs"),
                          cor(T6, tCNORM.cross, use = "pairwise.complete.obs"),
                          cor(T12, tCNORM.cross, use = "pairwise.complete.obs"),

                          cor(T1, TBetaCross, use = "pairwise.complete.obs"),
                          cor(T3, TBetaCross, use = "pairwise.complete.obs"),
                          cor(T6, TBetaCross, use = "pairwise.complete.obs"),
                          cor(T12, TBetaCross, use = "pairwise.complete.obs"),

                          mseCNORM,
                          mseBeta,
                          mseConv1,
                          mseConv3,
                          mseConv6,
                          mseConv12,

                          biasCNORM,
                          biasBeta,
                          biasT1,
                          biasT3,
                          biasT6,
                          biasT12,

                          MAD_CNORM,
                          MAD_Beta,
                          MAD_T1,
                          MAD_T3,
                          MAD_T6,
                          MAD_T12,

                          mean(T, na.rm=TRUE),
                          mean(tCNORM.cross, na.rm=TRUE),
                          mean(TBetaCross, na.rm=TRUE),
                          mean(T1, na.rm=TRUE),
                          mean(T3, na.rm=TRUE),
                          mean(T6, na.rm=TRUE),
                          mean(T12, na.rm=TRUE),


                          sd(T, na.rm=TRUE),
                          sd(tCNORM.cross, na.rm=TRUE),
                          sd(TBetaCross, na.rm=TRUE),
                          sd(T1, na.rm=TRUE),
                          sd(T3, na.rm=TRUE),
                          sd(T6, na.rm=TRUE),
                          sd(T12, na.rm=TRUE)), "\t");


          cat("\n", file=file, append=TRUE)
          cat(text, file=file, append=TRUE)

          print(paste0("Saving cycle ", k, " ", i, " ", m , " ", j))
        }
      }
    }
  }

  end <- Sys.time()
  print(paste0(c("Duration: ", end-start)))
}

# regression function for population mean in dependence of age
simMean <- function(age) {
  return(1.5 * age - 0.05 * age^2 + 0.0001 * age^4)
}

# regression function for population sd in dependence of age
simSD <- function(age) {
  return((1.5 * age - 0.05 * age^2 + 0.0001 * age^4) * 0.2 + 1)
}


# Set up representative month specific cross validation sample with n cases each
generateCrossValidationSample <- function(n) {

  # representative age specific z distribution based on equi distant ranks
  # that is: 7 years with 84 month, each 1000 cases
  z <- qnorm(seq(from = 0.5/n, to = (n - .5)/n, length.out = n))
  z <- rep(z, times = 84)

  # build year variable
  year <- c(rep.int(0, 6*n),
          rep(seq(from = 1, to = 6), each = 12*n),
          rep.int(7, 6*n))

  # build month variable
  month <- c(rep(seq(from = 6.5, to = 11.5, length.out = 6), each = n),
           rep(rep(seq(from = 0.5, to = 11.5, length.out = 12), each = n), times = 6),
           rep(seq(from = 0.5, to = 5.5, length.out = 6), each = n))

  # decimal age variable
  age <- year + (month / 12)

  # grouping variables
  group12 <- rep(seq(from = 1, to = 7, length.out = 7), each = 12*n)
  group6 <- rep(seq(from = 0.75, to = 7.25, length.out = 14), each = 6*n)
  group3 <- rep(seq(from = 0.625, to = 7.375, length.out = 28), each = 3*n)
  group <- age

  # age specific mean and sd
  mean <- simMean(age)
  sd <- simSD(age)

  # age specific latent variable
  latent <- z * sd + mean

  # overall latent variable, compute standardized value over complete sample
  meanL <- mean(latent)
  sdL <- sd(latent)

  # compute standardized value over complete sample
  zOverall <- (latent - meanL) / sdL

  data <- data.frame(age, group, mean, sd, z, latent, zOverall)
}

# generate sample data for 28 age bracket with 3 month, ranging from 0.5 to 7.5
generateData <- function(minAge, maxAge, totalN) {
  sample <- totalN/28
  group <- rep(seq(from = 0.625, to = 7.375, length.out = 28), each = sample)
  latent <- vector(mode = "numeric", length = 0)
  age <- vector(mode = "numeric", length = 0)
  i <- 0

  while (i < 28) {
    age <- c(age, runif(sample, min = (i*0.25) + 0.5, max = (i*0.25) + 0.75))

    i <- i + 1
  }

  data <- data.frame(age, group, mean = simMean(age), sd = simSD(age))
  data$z <- rnorm(nrow(data))
  data$latent <- data$z * data$sd + data$mean

  meanL <- mean(data$latent)
  sdL <- sd(data$latent)

  # compute standardized value over complete sample
  data$zOverall <- (data$latent - meanL) / sdL

  return(data)
}

# takes an age variableand forms age groups by rounding them to the month interval midst
recodeGroup <- function(age, month){
  correction <- month / 24
  factor = 12 / month

  group <- round((age + correction) * factor)/factor - correction

  return(group)
}
