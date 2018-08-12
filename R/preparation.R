#' Sentence comprehension data from grade 2.0 to 5.0
#'
#' A dataset containing the raw data of 1400 students from grade 2 to 5
#' in the sentence comprehension test from ELFE 1-6 (Lenhard &
#' Schneider, 2006). In this test, students are presented lists of
#' sentences with one gap. The student has to fill in the correct solution
#' by selecting from a list of 5 alternatives per sentence. The text is speeded,
#' with a time cutoff of 180 seconds. The variables are as follows:
#'
#' \itemize{
#'   \item personID. Identification number of each single student
#'   \item group. Grade level, beginning with 2.0 up to 5.0. Value of .0
#'   indicate the middle of the school year, 2.5 the end of the school year
#'   \item raw. The raw score of the test, ranging from 0 to 28
#' }
#'
#' @docType data
#' @keywords datasets
#' @name elfe
#' @usage data(elfe)
#' @format A data frame with 1400 rows and 3 columns
NULL


.onLoad <- function(libname, pkgname) {
  op <- options()
  op.cNORM <-
    list(
      cNORM.install.args = "",
      cNORM.name = "Wolfgang & Alexandra Lenhard",
      cNORM.desc.author = "Wolfgang Lenhard <wolfgang.lenhard@uni-wuerzburg.de> [aut, cre];
      Alexandra Lenhard <lenhard@psychometrica.de> [aut]",
      cNORM.desc.license = "BSD 3-clause License",
      cNORM.desc = list()
    )
  toset <- !(names(op.cNORM) %in% names(op))
  if (any(toset)) {
    options(op.cNORM[toset])
  }

  invisible()
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Good morning starshine, cNORM says 'Hello!'")
}


#' Set up example dataset and compute model
#'
#' This is a convenience method to either load the inbuilt sample dataset, or
#' to provide a data frame with the variables "raw" (for the raw values) and "group"
#' The function ranks the data within groups, computes norm values, powers of the norm
#' values and interactions. Afterwards the best fitting model is determined, based on
#' all default parameters.
#' @param data data.frame with a grouping variable named 'group' and a raw value variable
#' named 'raw'. In case no object is provided, cNORM uses the inbuild sample data to demonstrate
#' the procedure
#' @param group manually specify the grouping variable in the data
#' @return data frame including the norm values, powers and interactions of the norm value and
#' grouping variable
#' @examples
#' normData <- prepareData()
#' @export
prepareData <- function(data = NULL, group = "group") {
  if (is.null(data)) {
    normData <- cNORM::elfe
  } else {
    normData <- data
  }

  normData <- rankByGroup(normData, group = "group")
  normData <- computePowers(normData, k = 4, normVariable = "normValue", explanatoryVariable = "group")
  return(normData)
}

#' Determine the norm values of the participants in each subsample
#'
#' This is the initial step, usually done in all kinds of test norming projects,
#' after the scale is constructed and the norm sample is established. First,
#' the data is grouped according to a grouping variable and afterwards, the percentile
#' for each raw value is retrieved. The percentile can be used for the modeling
#' procedure, but in case, the samples to not deviate too much from normality,
#' T, IQ or z values can be computed via a normal rank procedure based on the
#' inverse cumulative normal distribution. In case of bindings, we use the medium rank
#' and there are different methods for estimating the percentiles (default RankIt).
#'
#' @param data data.frame with norm sample data
#' @param group name of the grouping variable (default 'group'), e. g. grade, setting
#' group to FALSE cancels grouping (data is treated as one goup)
#' @param raw name of the raw value variable (default 'raw')
#' @param method Ranking method in case of bindings, please provide an index,
#' choosing from the following methods: 1 = Blom (1958), 2 = Tukey (1949),
#' 3 = Van der Warden (1952), 4 = Rankit (default), 5 = Levenbach (1953),
#' 6 = Filliben (1975), 7 = Yu & Huang (2001)
#' @param scale type of norm scale, either T (default), IQ, z or percentile (= no
#' transformation); a double vector with the mean and standard deviation can as well,
#' be provided f. e. c(10, 3) for Wechsler scale index points
#' @param descend ranking order (default descent = FALSE): inverses the
#' ranking order with higher raw values getting lower norm values; relevant
#' for example when norming error values, where lower values mean higher
#' performance
#' @param descriptives If set to TRUE (default), information in n, mean, median and
#' standard deviation per group is added to each observation
#' @return the dataset with the percentiles and norm scales per group
#'
#' @examples
#' #Transformation with default parameters: RandIt and converting to T values
#' normData <- rankByGroup(elfe, group = "group")
#'
#' #Transformation into Wechsler points with Yu & Huang (2001) ranking procedure
#' normData <- rankByGroup(elfe, group = "group", method = 7, scale=c(10, 3))
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

    # check if columns exist
    if((typeof(group) != "logical") && !(group %in% colnames(data))){
      message(paste(c("ERROR: Grouping variable ", group, " does not exist in data object."), collapse = ""));
      stop();
    }

    if(!(raw %in% colnames(data))){
      message(paste(c("ERROR: Raw value variable ", data, " does not exist in data object."), collapse = ""));
      stop();
    }

    # define Q-Q-plot alorithm, use rankit as standard
    # 1 = Blom (1958), 2 = Tukey (1949), 3 = Van der Warden (1952), 4 = Rankit, 5 = Levenbach (1953),
    # 6 = Filliben (1975), 7 = Yu & Huang (2001)
    numerator <- c(-3.75, -1 / 3, 0, -0.5, -1 / 3, -0.3175, -0.326)
    denominator <- c(0.25, 1 / 3, 1, 0, 0.4, 0.365, 0.348)

    if (method < 1 || method > length(numerator)) {
      message("Method parameter out of range, setting to RankIt")
    }

    d <- as.data.frame(data)
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



    if ((typeof(scale) == "double" && length(scale) == 2)) {
      d$normValue <- stats::qnorm(d$percentile, scale[1], scale[2])
    } else if (scale == "IQ") {
      d$normValue <- stats::qnorm(d$percentile, 100, 15)
    } else if (scale == "z") {
      d$normValue <- stats::qnorm(d$percentile, 0, 1)
    } else if (scale == "T") {
      d$normValue <- stats::qnorm(d$percentile, 50, 10)
    } else if (scale == "percentile") {
      d$normValue <- d$percentile
    }

    return(d)
  }


#' Determine the norm values of the participants by sliding window (experimental)
#'
#' The function retrieves all individuals in the predefined age range (x +/- width/2)
#' around each case and ranks that individual based on this individually drawn sample.
#' This function can be directly used with a continuous age variable in order to avoid grouping.
#' When collecting data on the basis of a continuous age variable, cases located far
#' from the mean age of the group receive distorted percentiles when building discrete
#' groups and generating percentiles with the traditional approach. The distortion increases with
#' distance from the group mean and this effect can be avoided by the sliding window.
#' In case of bindings, the function uses the medium rank and applies the algorithms
#' already described in the 'rankByGroup' function. At the upper and lower end of the
#' data sample, the sliding stops and the sample is drawn from the interval min + width and
#' max - width, repsectively
#' @param data data.frame with norm sample data
#' @param age the continuous age variable
#' group to FALSE cancels grouping (data is treated as one goup)
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
#' ranking order with higher raw values getting lower norm values; relevant
#' for example when norming error values, where lower values mean higher
#' performance
#' @param descriptives If set to TRUE (default), information in n, mean, median and
#' standard deviation per group is added to each observation
#' @return the dataset with the individual percentiles and norm values
#'
#' @examples
#' # Transformation using a sliding window
#' normData <- rankBySlidingWindow(elfe, raw="raw", age="group", width=0.5)
#'
#' # Comparing this to the traditional approach should give us exactly the same
#' # values, since the sample dataset only has a grouping variable for age
#' normData2 <- rankByGroup(elfe, group = "group")
#' mean(normData$normValue - normData2$normValue)
#' @seealso rankByGroup, computePowers
#' @export
rankBySlidingWindow <- function(data,
                                age = "age",
                                raw = "raw",
                                width,
                                method = 4,
                                scale = "T",
                                descend = FALSE,
                                descriptives = TRUE) {

  # check if columns exist
  if(!(age %in% colnames(data))){
    message(paste(c("ERROR: Age variable ", age, " does not exist in data object."), collapse = ""));
    stop();
  }

  if(!(raw %in% colnames(data))){
    message(paste(c("ERROR: Raw value variable ", data, " does not exist in data object."), collapse = ""));
    stop();
  }

  # copy data frame
  d <- as.data.frame(data)

  # define Q-Q-plot alorithm, use rankit as standard
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
  if ((typeof(scale) == "double" && length(scale) == 2)) {
    d$normValue <- stats::qnorm(d$percentile, scale[1], scale[2])
  } else if (scale == "IQ") {
    d$normValue <- stats::qnorm(d$percentile, 100, 15)
  } else if (scale == "z") {
    d$normValue <- stats::qnorm(d$percentile, 0, 1)
  } else if (scale == "T") {
    d$normValue <- stats::qnorm(d$percentile, 50, 10)
  } else if (scale == "percentile") {
    d$normValue <- d$percentile
  }

  # build grouping variable - unnecessary for norming, but necessary for plotting the percentiles
  # TODO not working currently
  # TODO add parameters to function to switch on/off
  # numberOfGroups <- ((MAX.AGE - MIN.AGE) / width) - 1
  # groups1 <- seq(MIN.AGE + width, MAX.AGE - width, length.out = numberOfGroups)
  # groups2 <- c(-Inf, groups1)
  # groups1 <- c(groups1, MAX.AGE)
  #
  # grouping <- groups1[findInterval(d[, age], groups2)]


  return(d)
}


#' Compute powers of the explanatory variable #' a as well as of the person
#' location l (data preparation)
#'
#' The function computes powers of the norm variable e. g. T values (location, L),
#' an explanatory variable, e. g. age or grade of a data frame (age, A) and the interactions of both (L X A). The k
#' variable indicates the degree up to which powers and interactions are build.
#' These predictors can be used later on in the 'bestModel' function to model the
#' norm sample. Higher values of k allow for modeling the norm sample closer, but
#' might lead to over-fit. In general k = 3 or k = 4 (default) is sufficient to model
#' human performance data. For example, k = 2 results in the variables L1, L2, A1, A2,
#' and their interactions L1A1, L2A1, L1A2 and L2A2.
#' Please note, that you do not need to use a normal rank transformed scale like T r IQ, but you can
#' as well use the percentiles for the normVariable as well.
#'
#' @param data data.frame with the norm data
#' @param k degree
#' @param normVariable the variable containing the norm data in the data.frame; might be
#' T values, IQ values, percentiles ...
#' @param explanatoryVariable Variable like age or grade, which was as well used for the grouping.
#' Can be either the grouping variable itself or a finer grained variable like the exact age
#' @return data.frame with the powers and interactions of location and explanatory variable / age
#'
#' @examples
#' normData <- elfe
#' normData <- rankByGroup(normData, group="group")
#' normData <- computePowers(normData, k = 4, normVariable = "normValue", explanatoryVariable="group")
#' @export
computePowers <-
  function(data,
             k = 4,
             normVariable = "normValue",
             explanatoryVariable = "group") {

    # check if columns exist
    if(!(normVariable %in% colnames(data))){
      message(paste(c("ERROR: Norm variable ", normVariable, " does not exist in data object."), collapse = ""));
      stop();
    }

    if(!(explanatoryVariable %in% colnames(data))){
      message(paste(c("ERROR: Explanatory variable ", explanatoryVariable, " does not exist in data object."), collapse = ""));
      stop();
    }

    if ((k < 1) | (k > 6)) {
      message("Parameter k out of range, setting to 4")
      k <- 6
    }

    d <- data
    L1 <- as.numeric(d[[normVariable]])
    A1 <- as.numeric(d[[explanatoryVariable]])
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

    return(d)
  }
