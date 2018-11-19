cNORM.cv <- function(d, repetitions) {
  raw <- attr(d, "raw")
  k <- attr(d, "k")
  n.models <- 2 * k + k * k

  # set up regression formulas (from bestModel function)
  if (k == 1) {
    lmX <- formula(paste(raw, "L1 + A1 + L1A1", sep = " ~ "))
  } else if (k == 2) {
    lmX <-
      formula(paste(raw, "L1 + L2 + A1 + A2 + L1A1 + L1A2 + L2A1 + L2A2", sep = " ~ "))
  } else if (k == 3) {
    lmX <- formula(paste(raw, "L1 + L2 + L3 + A1 + A2 + A3 + L1A1 + L1A2 + L1A3 + L2A1 + L2A2 + L2A3 + L3A1 + L3A2 + L3A3", sep = " ~ "))
  } else if (k == 4) {
    lmX <- formula(paste(raw, "L1 + L2 + L3 + L4 + A1 + A2 + A3 + A4 + L1A1 + L1A2 + L1A3 + L1A4 + L2A1 + L2A2 + L2A3 + L2A4 + L3A1 + L3A2 + L3A3 + L3A4 + L4A1 + L4A2 + L4A3 + L4A4", sep = " ~ "))
  } else if (k == 5) {
    lmX <- formula(paste(raw, "L1 + L2 + L3 + L4 + L5 + A1 + A2 + A3 + A4 + A5 + L1A1 + L1A2 + L1A3 + L1A4 + L1A5 + L2A1 + L2A2 + L2A3 + L2A4 + L2A5 + L3A1 + L3A2 + L3A3 + L3A4 + L3A5 + L4A1 + L4A2 + L4A3 + L4A4 + L4A5 + L5A1 + L5A2 + L5A3 + L5A4 + L5A5", sep = " ~ "))
  } else if (k == 6) {
    lmX <-
      formula(paste(raw, "L1 + L2 + L3 + L4 + L5 + L6 + A1 + A2 + A3 + A4 + A5 + A6 + L1A1 + L1A2 + L1A3 + L1A4 + L1A5 + L1A6 + L2A1 + L2A2 + L2A3 + L2A4 + L2A5 + L2A6 + L3A1 + L3A2 + L3A3 + L3A4 + L3A5 + L3A6 + L4A1 + L4A2 + L4A3 + L4A4 + L4A5 + L4A6 + L5A1 + L5A2 + L5A3 + L5A4 + L5A5 + L5A6 + L6A1 + L6A2 + L6A3 + L6A4 + L6A5 + L6A6", sep = " ~ "))
  }


  # set up vectors to store RMSE for training, test and complete dataset models
  val.errors = rep(0, n.models)
  train.errors = rep(0, n.models)
  complete.errors <- rep(0, n.models)


  # draw test and training data several times ('repetitions' parameter), odel data and store MSE
  for(a in 1:repetitions){

  # check for imbalances in data and repeat of stratification was unsatisfactory - usually never occurs
  p.value <- .01
  while (p.value < .1) {
    # shuffle data and split into groups (for stratification)
    d <- d[sample(nrow(d)), ]
    d <- d[order(d$group), ]
    sp <- split(d, list(d$group))
    sp <- lapply(sp, function(x) x[sample(nrow(x)), ])

    # draw 9 tenth of data from each group for training
    train <- lapply(sp, function(x) x[c(FALSE, rep(TRUE, 9)), ])
    train <- do.call(rbind, train)

    # draw one tenth from each group for testing
    test <- lapply(sp, function(x) x[c(TRUE, rep(FALSE, 9)), ])
    test <- do.call(rbind, test)

    # test for overall significant differences between groups, restart stratification if necessary
    p.value <- t.test(train$raw, test$raw)$p.value
  }

  # compute leaps model
  subsets <- leaps::regsubsets(lmX, data = train, nbest = 1, nvmax = n.models, really.big = n.models > 25)

  # retrieve models coefficients for each number of terms
  for (i in 1:n.models) {
    variables <- names(coef(subsets, id = i))
    variables <- variables[2:length(variables)] # remove '(Intercept)' variable
    reg <- paste0(raw, " ~ ", paste(variables, collapse = " + ")) # build regression formula

    # run linear regression for specific model
    model <- lm(reg, train)

    # predict values in test data
    test.fitted <- predict.lm(model, test)

    # store MSE for test and train data
    train.errors[i] <- train.errors[i] + mean((model$fitted.values - train[, raw])^2)
    val.errors[i] <- val.errors[i] + mean((test.fitted - test[, raw])^2)
  }
  }

  # now for the complete data the same logic
  complete <- leaps::regsubsets(lmX, data = d, nbest = 1, nvmax = n.models, really.big = n.models > 25)
  for (i in 1:n.models) {
    variables <- names(coef(complete, id = i))
    variables <- variables[2:length(variables)]
    reg <- paste0(raw, " ~ ", paste(variables, collapse = " + "))
    model <- lm(reg, d)

    # mse for the complete data based on number of terms
    complete.errors[i] <- sqrt(mean((model$fitted.values - d[, raw])^2))

    # build the average over repetitions and the root
    train.errors[i] <- sqrt(train.errors[i] / repetitions)
    val.errors[i] <-  sqrt(val.errors[i] / repetitions)
  }

  # plot results
  plot(val.errors, pch = 19, type = "b", col = "blue", ylab = "Root MSE", xlab = "Number of terms")
  points(complete.errors, pch = 19, type = "b", col = "black")
  points(train.errors, pch = 19, type = "b", col = "red")
  legend("topright", legend = c("Validation", "Complete Dataset", "Training"), col = c("blue", "black", "red"), pch = 19)
}


nfold <- function(data, type = 0){
  #results <- data.frame(nr = )
  g <- attr(d, "group")
  groups <- unique(data$g)
  results <- data.frame(rsqTest=double(), rsqTrain=double(), CROSSFIT=double(), SE = double(), MAD=double(), terms = integer(), inconsistency = logical())

  if(type == 0){
    for(i in 2:(length(groups) - 1)){

      train <- data[data$group != groups[[i]], ]

      test <- data[data$group == groups[[i]], ]

      model <- bestModel(train)
      train$T <- predictNorm(train$raw, train$group, model, model$minL1, model$maxL1)
      test$T <- predictNorm(test$raw, test$group, model, model$minL1, model$maxL1)

      r2Train <- cor(train$normValue, train$T, use = "pairwise.complete.obs")
      r2Train <- r2Train * r2Train


      r2Test <- cor(test$normValue, test$T, use = "pairwise.complete.obs")
      r2Test <- r2Test * r2Test
      se <- sqrt(sum((test$normValue - test$T)^2)/(nrow(test) - 2))
      mad <- mean(abs((test$normValue - test$T)))

      r2Train <- model$subsets$rsq[[model$ideal.model]]
      CROSSFIT <- r2Train/r2Test
      cons <- checkConsistency(model)
      df <- data.frame(rsqTest=r2Test, rsqTrain=r2Train, CROSSFIT=CROSSFIT, SE = se, MAD=mad, terms = model$ideal.model, inconsistency = cons)
      results <- rbind(results, df)

      print(paste0("Group ", groups[[i]]))
      print(paste0("R2Test: ", r2Test))
      print(paste0("R2Train: ", r2Train))
      print(paste0("CROSSFIT: ", CROSSFIT))
      print(paste0("Inconsistencies: ", cons))
    }
  }

  return(results)
}
