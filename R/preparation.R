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
  if (any(toset))
    options(op.cNORM[toset])

  invisible()
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to Continuous Norming in R (cNORM). Hello world! cNORM greets you.")
}


#' Set up example dataset and compute model
#'
#' This is a convenience method to load the inbuilt sample dataset, rank the
#' data and compute norm values, compute powers and interactions and
#' determine the best fitting model based on all default parameters.
#'
#' @examples
#' normData <- prepareData()
#' @export
prepareData <- function() {
  normData <- rankByGroup(cNORM::elfe, group = "group")
  normData <- computePowers(normData, k = 4, normVariable = "normValue", explanatoryVariable = "group")
  return(normData)
}

#' Determine the norm values of the participants in each subsample
#'
#' This is the initial step, usually done in all kinds of test norming projects,
#' after the scale is constructed and the norm sample is established. First,
#' the data is grouped according to a grouping variable and afterwards, the percentiles
#' for each raw value is retrieved. The percentile can be used for the modeling
#' procedure, but in case, the samples to not deviate too much from normality,
#' T, IQ or z values can be computed via a normal rank procedure based on the
#' inverse cumulative normal distribution. In case of bindings, we use the medium rank
#' and there are different methods for estimating the percentiles (default RankIt).
#'
#' @param data data.frame with norm sample data
#' @param group the grouping variable, e. g. grade
#' @param method Ranking method in case of bindings, either 'blom', 'tukey', 'rankit' (default),
#' and 'vanderwarden'
#' @param scale type of norm scale, either T (default), IQ or z
#' @return the dataset with the percentiles and norm scales per group
#'
#' @examples
#' normData <- rankByGroup(elfe, group = "group")
#' @export
rankByGroup <-
  function(data,
           group = "group",
           method = "rankit",
           scale = "T") {
    d <- data

    if (method == "blom") {
      d <- data %>% dplyr::arrange(group, data$raw) %>%
        dplyr::group_by(group) %>%
        dplyr::mutate(percentile = (base::rank(raw) - 0.375) / (base::length(raw) + 0.25))
    } else if (method == "tukey") {
      d <- data %>% dplyr::arrange(group, data$raw) %>%
        dplyr::group_by(group) %>%
        dplyr::mutate(percentile = (base::rank(raw) - 0.3333333334) / (base::length(raw) +
                                                                         0.3333333334))
    } else if (method == "vanderwarden") {
      d <- data %>% dplyr::arrange(group, data$raw) %>%
        dplyr::group_by(group) %>%
        dplyr::mutate(percentile = (base::rank(raw)) / (base::length(raw) +
                                                          1))
    } else {
       d <- data %>% dplyr::arrange(group, data$raw) %>%
        dplyr::group_by(group) %>%
        dplyr::mutate(percentile = (base::rank(raw) - 0.5) / base::length(raw))
    }

    if (scale == "IQ") {
      d$normValue <- stats::qnorm(d$percentile, 100, 15)
    } else if (scale == "z") {
      d$normValue <- stats::qnorm(d$percentile, 0, 1)
    } else {
      d$normValue <- stats::qnorm(d$percentile, 50, 10)
    }

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
#' and their interactions L1A1, L2A1, L1A2 and L2A2
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
    if ((k < 1) | (k > 6)) {
      base::message("Parameter k out of range, setting to 4")
      k <- 6
    }


    d <- data
    L1 <- base::as.numeric(d[[normVariable]])
    A1 <- base::as.numeric(d[[explanatoryVariable]])
    L1A1 <- L1*A1

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
