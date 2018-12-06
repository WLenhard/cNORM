.onLoad <- function(libname, pkgname) {
  op <- options()
  op.cNORM <-
    list(
      cNORM.install.args = "",
      cNORM.name = "Wolfgang Lenhard, Alexandra Lenhard & Sebastian Gary",
      cNORM.desc.author = "Wolfgang Lenhard <wolfgang.lenhard@uni-wuerzburg.de> [aut, cre];
      Alexandra Lenhard <lenhard@psychometrica.de> [aut];  Sebastian Gary [aut]",
      cNORM.desc.license = "AGPL-3",
      cNORM.desc = list()
    )
  toset <- !(names(op.cNORM) %in% names(op))
  if (any(toset)) {
    options(op.cNORM[toset])
  }

  invisible()
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Good morning star-shine, cNORM says 'Hello!'")
}


#' Set up example dataset and compute model
#'
#' This is a convenience method to either load the inbuilt sample dataset, or
#' to provide a data frame with the variables "raw" (for the raw scores) and "group"
#' The function ranks the data within groups, computes norm values, powers of the norm
#' scores and interactions. Afterwards the best fitting model is determined, based on
#' all default parameters.
#' @param data data.frame with a grouping variable named 'group' and a raw score variable
#' named 'raw'. In case no object is provided, cNORM uses the inbuilt sample data to
#' demonstrate the procedure.
#' @param group grouping variable in the data, e. g. age groups, grades ...
#' @param raw the raw scores
#' @param age the continuous explanatory variable; by default set to "group"
#' @param width if a width is provided, the function switches to rankBySlidingWindow to determine the
#' observed raw scores, otherwise, ranking is done by group (default)
#' @return data frame including the norm scores, powers and interactions of the norm score and
#' grouping variable
#' @examples
#' # conducts ranking and computation of powers and interactions with the 'elfe' dataset
#' data.elfe <- prepareData()
#'
#' # variable names can be specified as well, here with the BMI data included in the package
#' data.bmi <- prepareData(CDC, group="group", raw="bmi", age="age")
#' @export
prepareData <- function(data = NULL, group = "group", raw = "raw", age = "group", width = NA) {
  if (is.null(data)) {
    normData <- cNORM::elfe
  } else {
    normData <- data
  }

  # checks
  if (!(group %in% colnames(normData))) {
    stop(paste(c("ERROR: Grouping variable '", group, "' does not exist in data object."), collapse = ""))
  } else if (!(raw %in% colnames(normData))) {
    stop(paste(c("ERROR: Raw score variable '", raw, "' does not exist in data object."), collapse = ""))
  } else if (!(age %in% colnames(normData))) {
    stop(paste(c("ERROR: Age variable '", age, "' does not exist in data object."), collapse = ""))
  }

  if (!is.numeric(normData[, group])) {
    warning(paste(c("Grouping variable '", group, "' has to be numeric."), collapse = ""))
  }

  if (!is.numeric(normData[, raw])) {
    warning(paste(c("Raw variable '", raw, "' has to be numeric."), collapse = ""))
  }

  if (!is.numeric(normData[, age])) {
    warning(paste(c("Age variable '", age, "' has to be numeric."), collapse = ""))
  }

  # exclude missings
  normData <- as.data.frame(normData)
  normData <- normData[!is.na(normData[, group]), ]
  normData <- normData[!is.na(normData[, raw]), ]
  normData <- normData[!is.na(normData[, age]), ]

  # ranking and powers
  if(is.na(width)){
    normData <- rankByGroup(normData, group = group, raw = raw)
  }else{
    normData <- rankBySlidingWindow(normData, group = group, raw = raw, width = width)
  }
  normData <- computePowers(normData, k = 4, norm = "normValue", age = age)
  return(normData)
}

#' Determine the norm scores of the participants in each subsample
#'
#' This is the initial step, usually done in all kinds of test norming projects,
#' after the scale is constructed and the norm sample is established. First,
#' the data is grouped according to a grouping variable and afterwards, the percentile
#' for each raw value is retrieved. The percentile can be used for the modeling
#' procedure, but in case, the samples to not deviate too much from normality,
#' T, IQ or z scores can be computed via a normal rank procedure based on the
#' inverse cumulative normal distribution. In case of bindings, we use the medium rank
#' and there are different methods for estimating the percentiles (default RankIt).
#'
#' @param data data.frame with norm sample data
#' @param group name of the grouping variable (default 'group'), e. g. grade, setting
#' group to FALSE cancels grouping (data is treated as one group)
#' @param raw name of the raw value variable (default 'raw')
#' @param method Ranking method in case of bindings, please provide an index,
#' choosing from the following methods: 1 = Blom (1958), 2 = Tukey (1949),
#' 3 = Van der Warden (1952), 4 = Rankit (default), 5 = Levenbach (1953),
#' 6 = Filliben (1975), 7 = Yu & Huang (2001)
#' @param scale type of norm scale, either T (default), IQ, z or percentile (= no
#' transformation); a double vector with the mean and standard deviation can as well,
#' be provided f. e. c(10, 3) for Wechsler scale index points
#' @param descend ranking order (default descent = FALSE): inverses the
#' ranking order with higher raw scores getting lower norm scores; relevant
#' for example when norming error scores, where lower scores mean higher
#' performance
#' @param descriptives If set to TRUE (default), information in n, mean, median and
#' standard deviation per group is added to each observation
#' @return the dataset with the percentiles and norm scales per group
#'
#' @examples
#' #Transformation with default parameters: RandIt and converting to T scores
#' data.elfe <- rankByGroup(elfe, group = "group")
#'
#' #Transformation into Wechsler scores with Yu & Huang (2001) ranking procedure
#' data.elfe <- rankByGroup(elfe, group = "group", method = 7, scale=c(10, 3))
#'
#' @seealso rankBySlidingWindow, computePowers
#' @export
rankByGroup <-
  function(data,
             group = "group",
             raw = "raw",
             method = 4,
             scale = "T",
             descend = FALSE,
             descriptives = TRUE) {
    d <- as.data.frame(data)

    if(anyNA(d[, group]) || anyNA(d[, raw])){
      cat("Missing values found in grouping or raw score variable... excluding from dataset")
      d <- d[!is.na(d[, group]), ]
      d <- d[!is.na(d[, raw]), ]
    }

    # check if columns exist
    if ((typeof(group) != "logical") && !(group %in% colnames(d))) {
      stop(paste(c("ERROR: Grouping variable '", group, "' does not exist in data object."), collapse = ""))
    }

    if (!(raw %in% colnames(d))) {
      stop(paste(c("ERROR: Raw value variable '", raw, "' does not exist in data object."), collapse = ""))
    }

    if (!is.numeric(d[, group])) {
      warning(paste(c("Grouping variable '", group, "' has to be numeric."), collapse = ""))
    }

    if (!is.numeric(d[, raw])) {
      warning(paste(c("Raw variable '", raw, "' has to be numeric."), collapse = ""))
    }

    # define Q-Q-plot alorithm, use rankit as standard
    # 1 = Blom (1958), 2 = Tukey (1949), 3 = Van der Warden (1952), 4 = Rankit, 5 = Levenbach (1953),
    # 6 = Filliben (1975), 7 = Yu & Huang (2001)
    numerator <- c(-3.75, -1 / 3, 0, -0.5, -1 / 3, -0.3175, -0.326)
    denominator <- c(0.25, 1 / 3, 1, 0, 0.4, 0.365, 0.348)

    if (method < 1 || method > length(numerator)) {
      message("Method parameter out of range, setting to RankIt")
    }

    if (typeof(group) == "logical" && !group) {
      if (descend) {
        d$percentile <- (rank(-1 * (d[, raw])) + numerator[method]) / (length(d[, raw]) + denominator[method])
      } else {
        d$percentile <- (rank(d[, raw]) + numerator[method]) / (length(d[, raw]) + denominator[method])
      }

      if (descriptives) {
        d$n <- length(d[, raw])
        d$m <- mean(d[, raw])
        d$md <- median(d[, raw])
        d$sd <- sd(d[, raw])
      }
    } else {
      if (descend) {
        d$percentile <- ave(d[, raw], d[, group], FUN = function(x) {
          (rank(-x) + numerator[method]) / (length(x) + denominator[method])
        })
      } else {
        d$percentile <- ave(d[, raw], d[, group], FUN = function(x) {
          (rank(x) + numerator[method]) / (length(x) + denominator[method])
        })
      }
      if (descriptives) {
        d$n <- ave(d[, raw], d[, group], FUN = function(x) {
          length(x)
        })
        d$m <- ave(d[, raw], d[, group], FUN = function(x) {
          mean(x)
        })
        d$md <- ave(d[, raw], d[, group], FUN = function(x) {
          median(x)
        })
        d$sd <- ave(d[, raw], d[, group], FUN = function(x) {
          sd(x)
        })
      }
    }

    scaleM <- NA
    scaleSD <- NA


    if ((typeof(scale) == "double" && length(scale) == 2)) {
      d$normValue <- qnorm(d$percentile, scale[1], scale[2])
      scaleM <- scale[1]
      scaleSD <- scale[2]
    } else if (scale == "IQ") {
      d$normValue <- qnorm(d$percentile, 100, 15)
      scaleM <- 100
      scaleSD <- 15
    } else if (scale == "z") {
      d$normValue <- qnorm(d$percentile, 0, 1)
      scaleM <- 0
      scaleSD <- 1
    } else if (scale == "T") {
      scaleM <- 50
      scaleSD <- 10
      d$normValue <- qnorm(d$percentile, 50, 10)
    } else if (scale == "percentile") {
      d$normValue <- d$percentile
    }

    # add attributes to d
    attr(d, "group") <- group
    attr(d, "age") <- group
    attr(d, "raw") <- raw
    attr(d, "scaleMean") <- scaleM
    attr(d, "scaleSD") <- scaleSD
    attr(d, "descend") <- descend
    attr(d, "normValue") <- "normValue"

    return(d)
  }


#' Determine the norm scores of the participants by sliding window (experimental)
#'
#' The function retrieves all individuals in the predefined age range (x +/- width/2)
#' around each case and ranks that individual based on this individually drawn sample.
#' This function can be directly used with a continuous age variable in order to avoid
#' grouping. When collecting data on the basis of a continuous age variable, cases
#' located far from the mean age of the group receive distorted percentiles when building
#' discrete groups and generating percentiles with the traditional approach. The distortion
#' increases with distance from the group mean and this effect can be avoided by the
#' sliding window. Nonetheless, please ensure, that the optional grouping variable in fact
#' represents the correct mean age of the respective age groups, as this variable is
#' later on used for displaying the manifest data in the percentile plots.
#'
#' In case of bindings, the function uses the medium rank and applies the algorithms
#' already described in the \code{\link{rankByGroup}} function. At the upper and lower end of the
#' data sample, the sliding stops and the sample is drawn from the interval min + width and
#' max - width, respectively.
#' @param data data.frame with norm sample data
#' @param age the continuous age variable
#' group to FALSE cancels grouping (data is treated as one group)
#' @param raw name of the raw value variable (default 'raw')
#' @param width the width of the sliding window
#' @param method Ranking method in case of bindings, please provide an index,
#' choosing from the following methods: 1 = Blom (1958), 2 = Tukey (1949),
#' 3 = Van der Warden (1952), 4 = Rankit (default), 5 = Levenbach (1953),
#' 6 = Filliben (1975), 7 = Yu & Huang (2001)
#' @param scale type of norm scale, either T (default), IQ, z or percentile (= no
#' transformation); a double vector with the mean and standard deviation can as well,
#' be provided f. e. c(10, 3) for Wechsler scale index points
#' @param descend ranking order (default descent = FALSE): inverses the
#' ranking order with higher raw scores getting lower norm scores; relevant
#' for example when norming error scores, where lower scores mean higher
#' performance
#' @param descriptives If set to TRUE (default), information in n, mean, median and
#' standard deviation per group is added to each observation
#' @param nGroup If set to a positive value, a grouping variable is created with the desired number of
#' equi distant groups, named by the group mean age of each group. It creates the
#' column 'group' in the data.frame and in case, there is already one with that name,
#' overwrites it.
#' @param group Optional parameter for providing the name of the grouping variable (if present; overwritten
#' if ngroups is used)
#' @return the dataset with the individual percentiles and norm scores
#'
#' @examples
#' \dontrun{
#' # Transformation using a sliding window
#' data.elfe2 <- rankBySlidingWindow(elfe, raw="raw", age="group", width=0.5)
#'
#' # Comparing this to the traditional approach should give us exactly the same
#' # values, since the sample dataset only has a grouping variable for age
#' data.elfe <- rankByGroup(elfe, group = "group")
#' mean(data.elfe$normValue - data.elfe2$normValue)
#' }
#' @seealso rankByGroup, computePowers
#' @export
rankBySlidingWindow <- function(data,
                                age = "age",
                                raw = "raw",
                                width,
                                method = 4,
                                scale = "T",
                                descend = FALSE,
                                descriptives = TRUE,
                                nGroup = 0,
                                group = NA) {

  # copy data frame
  d <- as.data.frame(data)

  if(anyNA(d[, raw]) || anyNA(d[, age])){
    cat("Missing values found in raw score or age variable... excluding from dataset")
    d <- d[!is.na(d[, raw]), ]
    d <- d[!is.na(d[, age]), ]
  }

  # check if columns exist
  if (!(age %in% colnames(d))) {
    stop(paste(c("ERROR: Age variable '", age, "' does not exist in data object."), collapse = ""))
  }

  if (!(raw %in% colnames(d))) {
    stop(paste(c("ERROR: Raw value variable '", raw, "' does not exist in data object."), collapse = ""))
  }

  if (!is.numeric(d[, age])) {
    warning(paste(c("Age variable '", age, "' has to be numeric."), collapse = ""))
  }

  if (!is.numeric(d[, raw])) {
    warning(paste(c("Raw variable '", raw, "' has to be numeric."), collapse = ""))
  }



  # define Q-Q-plot algorithm, use rankit as standard
  # 1 = Blom (1958), 2 = Tukey (1949), 3 = Van der Warden (1952), 4 = Rankit, 5 = Levenbach (1953),
  # 6 = Filliben (1975), 7 = Yu & Huang (2001)
  numerator <- c(-3.75, -1 / 3, 0, -0.5, -1 / 3, -0.3175, -0.326)
  denominator <- c(0.25, 1 / 3, 1, 0, 0.4, 0.365, 0.348)

  # add columns to data.frame
  d$percentile <- NA
  if (descriptives) {
    d$n <- NA
    d$m <- NA
    d$md <- NA
    d$sd <- NA
  }
  # upper and lower bounds
  i <- 1
  n <- nrow(d)
  MIN.AGE <- min(d[, age])
  MAX.AGE <- max(d[, age])


  while (i <= n) {
    a <- d[i, age]
    r <- d[i, raw]
    minAge <- a - (width / 2)
    maxAge <- a + (width / 2)

    # limitation at the upper and lower end of the distribution
    if (minAge < MIN.AGE) {
      minAge <- MIN.AGE
      maxAge <- MIN.AGE + width
    } else if (maxAge > MAX.AGE) {
      minAge <- MAX.AGE - width
      maxAge <- MAX.AGE
    }

    observations <- d[which(d[, age] >= minAge & d[, age] <= maxAge), ]
    nObs <- nrow(observations)

    # print((rank(observations[, raw]) + numerator[method]) / (n + denominator[method]))
    if (descend) {
      observations$percentile <- (rank(-observations[, raw]) + numerator[method]) / (nObs + denominator[method])
    } else {
      observations$percentile <- (rank(observations[, raw]) + numerator[method]) / (nObs + denominator[method])
    }
    # get percentile for raw value in sliding window subsample
    d$percentile[[i]] <- tail(observations$percentile[which(observations[, raw] == r)], n = 1)
    if (descriptives) {
      d$n[[i]] <- nObs
      d$m[[i]] <- mean(observations[, raw])
      d$md[[i]] <- median(observations[, raw])
      d$sd[[i]] <- sd(observations[, raw])
    }
    i <- i + 1
  }

  # norm scale definition
  scaleM <- NA
  scaleSD <- NA

  if ((typeof(scale) == "double" && length(scale) == 2)) {
    d$normValue <- qnorm(d$percentile, scale[1], scale[2])
    scaleM <- scale[1]
    scaleSD <- scale[2]
  } else if (scale == "IQ") {
    d$normValue <- qnorm(d$percentile, 100, 15)
    scaleM <- 100
    scaleSD <- 15
  } else if (scale == "z") {
    d$normValue <- qnorm(d$percentile, 0, 1)
    scaleM <- 0
    scaleSD <- 1
  } else if (scale == "T") {
    scaleM <- 50
    scaleSD <- 10
    d$normValue <- qnorm(d$percentile, 50, 10)
  } else if (scale == "percentile") {
    d$normValue <- d$percentile
  }

  # build grouping variable - unnecessary for norming,
  # but necessary for plotting the percentiles
  if (nGroup > 0) {
    group <- as.factor(as.numeric(cut(d[, age], nGroup)))
    d$group <- ave(d[, age], group, FUN = function(x) {
      mean(x)
    })
  }

  # add attributes to d
  attr(d, "age") <- age
  attr(d, "raw") <- raw
  attr(d, "scaleMean") <- scaleM
  attr(d, "scaleSD") <- scaleSD
  attr(d, "descend") <- descend
  attr(d, "normValue") <- "normValue"
  attr(d, "group") <- "group"

  return(d)
}


#' Compute powers of the explanatory variable a as well as of the person
#' location l (data preparation)
#'
#' The function computes powers of the norm variable e. g. T scores (location, L),
#' an explanatory variable, e. g. age or grade of a data frame (age, A) and the
#' interactions of both (L X A). The k variable indicates the degree up to which
#' powers and interactions are build. These predictors can be used later on in the
#' \code{\link{bestModel}} function to model the norm sample. Higher values of k
#' allow for modeling the norm sample closer, but might lead to over-fit. In general
#' k = 3 or k = 4 (default) is sufficient to model human performance data. For example,
#' k = 2 results in the variables L1, L2, A1, A2, and their interactions L1A1, L2A1, L1A2
#' and L2A2 (but k = 2 is usually not sufficient for the modeling). Please note, that
#' you do not need to use a normal rank transformed scale like T r IQ, but you can
#' as well use the percentiles for the 'normValue' as well.
#'
#' @param data data.frame with the norm data
#' @param k degree
#' @param norm the variable containing the norm data in the data.frame; might be
#' T scores, IQ scores, percentiles ...
#' @param age Explanatory variable like age or grade, which was as well used for the grouping.
#' Can be either the grouping variable itself or a finer grained variable like the exact age. Other
#' explanatory variables can be used here instead an age variable as well, as long as the variable is
#' at least ordered metric, e. g. language or development levels ... The label 'age' is used, as this is the
#' most common field of application.
#' @return data.frame with the powers and interactions of location and explanatory variable / age
#' @seealso bestModel
#' @examples
#' # Dataset with grade levels as grouping
#' data.elfe <- rankByGroup(elfe)
#' data.elfe <- computePowers(data.elfe)
#'
#' # Dataset with continuous age variable and k = 5
#' data.ppvt <- rankByGroup(ppvt)
#' data.ppvt <- computePowers(data.ppvt, age = "age", k = 5)
#' @export
computePowers <-
  function(data,
             k = 4,
             norm = NULL,
             age = NULL) {
    d <- as.data.frame(data)

    # check variables, if NULL take attributes from d
    if (is.null(norm)) {
      norm <- attr(d, "normValue")
    }

    if (is.null(age)) {
      age <- attr(d, "age")
    }

    # check if columns exist
    if (!(norm %in% colnames(d))) {
      stop(paste(c("ERROR: Norm variable '", norm, "' does not exist in data object."), collapse = ""))
    }

    if (!(age %in% colnames(d))) {
      stop(paste(c("ERROR: Explanatory variable '", age, "' does not exist in data object."), collapse = ""))
    }

    if (!is.numeric(d[, norm])) {
      warning(paste(c("Norm score variable '", norm, "' has to be numeric."), collapse = ""))
    }

    if (!is.numeric(d[, age])) {
      warning(paste(c("Age variable '", age, "' has to be numeric."), collapse = ""))
    }

    if ((k < 1) | (k > 6)) {
      message("Parameter k out of range, setting to 4")
      k <- 6
    }


    L1 <- as.numeric(d[[norm]])
    A1 <- as.numeric(d[[age]])
    L1A1 <- L1 * A1

    d$L1 <- L1
    d$A1 <- A1
    d$L1A1 <- L1A1

    if (k > 1) {
      d$L2 <- d$L1 * d$L1
      d$A2 <- d$A1 * d$A1
      d$L1A2 <- d$L1 * d$A2
      d$L2A1 <- d$L2 * d$A1
      d$L2A2 <- d$L2 * d$A2
    }

    if (k > 2) {
      d$L3 <- d$L2 * d$L1
      d$A3 <- d$A2 * d$A1

      d$L1A3 <- d$L1 * d$A3
      d$L2A3 <- d$L2 * d$A3

      d$L3A1 <- d$L3 * d$A1
      d$L3A2 <- d$L3 * d$A2
      d$L3A3 <- d$L3 * d$A3
    }

    if (k > 3) {
      d$L4 <- d$L3 * d$L1
      d$A4 <- d$A3 * d$A1
      d$L1A4 <- d$L1 * d$A4
      d$L2A4 <- d$L2 * d$A4
      d$L3A4 <- d$L3 * d$A4
      d$L4A1 <- d$L4 * d$A1
      d$L4A2 <- d$L4 * d$A2
      d$L4A3 <- d$L4 * d$A3
      d$L4A4 <- d$L4 * d$A4
    }

    if (k > 4) {
      d$L5 <- d$L4 * d$L1
      d$A5 <- d$A4 * d$A1
      d$L1A5 <- d$L1 * d$A5
      d$L2A5 <- d$L2 * d$A5
      d$L3A5 <- d$L3 * d$A5
      d$L4A5 <- d$L4 * d$A5
      d$L5A1 <- d$L5 * d$A1
      d$L5A2 <- d$L5 * d$A2
      d$L5A3 <- d$L5 * d$A3
      d$L5A4 <- d$L5 * d$A4
      d$L5A5 <- d$L5 * d$A5
    }

    if (k > 5) {
      d$L6 <- d$L5 * d$L1
      d$A6 <- d$A5 * d$A1
      d$L1A6 <- d$L1 * d$A6
      d$L2A6 <- d$L2 * d$A6
      d$L3A6 <- d$L3 * d$A6
      d$L4A6 <- d$L4 * d$A6
      d$L5A6 <- d$L5 * d$A6
      d$L6A1 <- d$L6 * d$A1
      d$L6A2 <- d$L6 * d$A2
      d$L6A3 <- d$L6 * d$A3
      d$L6A4 <- d$L6 * d$A4
      d$L6A5 <- d$L6 * d$A5
      d$L6A6 <- d$L6 * d$A6
    }

    # attributes
    attr(d, "age") <- age
    attr(d, "normValue") <- norm
    attr(d, "k") <- k

    return(d)
  }
