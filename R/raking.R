#' Weighting of cases through iterative proportional fitting (Raking)
#'
#' Computes and standardizes weights via raking to compensate for non-stratified samples.
#' It is based on the implementation in the survey R package. It reduces data collection
#' biases in the norm data by the means of post stratification, thus reducing the effect
#' of unbalanced data in percentile estimation and norm data modeling.
#'
#' This function computes standardized raking weights based on the implementation in
#' the survey R package to overcome biases in norm samples. It generates weights, by drawing
#' on the information of population shares (e. g. for sex, ethnic group, region ...) and
#' subsequently reduces the influence of over-represented groups or increases underrepresented
#' cases. The returned weights are standardized and scaled to be larger than 0.
#'
#' Raking in general has a number of advantages over post stratification and it additionally
#' allows cNORM to draw on larger datasets, since less cases have to be removed during
#' stratification. To use this function, additionally to the data, a data frame with stratification
#' variables has to be specified. The data frame should include a row with (a) the variable name,
#' (b) the level of the variable and (c) the according population proportion.
#'
#' @param data data.frame with norm sample data.
#' The function is used to compute the actual raking weights using the survey package.
#' @param population.margins A data.frame including three columns, specifying the variable name in
#' the original dataset used for data stratification, the factor level of the variable and the
#' according population share. Please ensure, the original data does not include factor levels,
#' not present in the population.margins. Additionally, summing up the shares of the
#' different levels of a variable should result in a value near 1.0.
#' @return a vector with the standardized weights
#' @examples
#' # cNORM features a dataset on vocabulary development (ppvt)
#' # that includes variables like sex or migration. In order
#' # to weight the data, we have to specify the population shares.
#' # According to census, the population includes 52% boys
#' # (factor level 1 in the ppvt dataset) and 70% / 30% of persons
#' # without / with a a history of migration (= 0 / 1 in the dataset).
#' # First we set up the popolation margins with all shares of the
#' # different levels:
#'
#' margins <- data.frame(variables = c("sex", "sex",
#'                                     "migration", "migration"),
#'                       levels = c(1, 2, 0, 1),
#'                       share = c(.52, .48, .7, .3))
#' head(margins)
#'
#' # Now we use the population margins to generate weights
#' # through raking
#'
#' weights <- computeWeights(ppvt, margins)
#'
#'
#' # There are as many different weights as combinations of
#' # factor levels, thus only four in this specific case
#'
#' unique(weights)
#'
#'
#' # To include the weights in the cNORM modelling, we have
#' # to pass them as weights. They are then used to set up
#' # weighted quantiles and as weights in the regession.
#'
#' model <- cnorm(raw = ppvt$raw,
#'                group=ppvt$group,
#'                weights = weights)
#' @export
computeWeights<- function(data, population.margins){

  # Require survey package; returns error with hint to install survey if
  # not already installed

  packageList <- c("survey")

  if (!requireNamespace(packageList, quietly = TRUE)) {
    cat("The survey package is needed. Would you like to try to install it now?")
    installChoice <- menu(c("yes", "no"))
    if(installChoice == 1){
      utils::install.packages(packageList)
    } else {
      stop("Packages are missing. Unable to conduct the weighting")
    }
  }

  # Create non-weighted survey object
  unweighted_survey_object <- survey::svydesign(ids = ~1,
                                                data = data,
                                                weights = NULL)


  # Split data frame containing marginals into a list of data frames
  # containing proportions for every stratification variable separately
  list_of_marginals_new <- split(x = population.margins, f = population.margins[1])
  list_of_marginals_new <- lapply(list_of_marginals_new, FUN =function(.) {

    names(.)[names(.) == names(.)[[2]]] <-.[[1]][1]
    names(.)[names(.) == names(.)[[3]]] <- "Freq"
    .[[1]] <- NULL
    return(.)
  })
  list_of_marginals_new <- lapply(list_of_marginals_new, as.data.frame)

  # Build list of variables in formula format
  variables_formalized <- list()
  for(i in seq(1, length(list_of_marginals_new))){
    sv <- names(list_of_marginals_new[[i]])[[1]]

    # Check if variable is contained in data set
    if(!(sv %in% names(data))){
      stop(paste("Variable", toString(sv), "not in data set.\n  Please check make sure data set contains all variables."))
    }

    # Check every level of the current sv in the data set is contained in the
    # corresponding marginals
    levels_of_sv_data <- levels(as.factor(data[[sv]]))
    for(lev in levels_of_sv_data){
      if(!(lev %in% list_of_marginals_new[[i]][[1]])){
        stop("Level ", paste(toString(lev), "of variable", toString(names(list_of_marginals_new[[i]])[[1]]),"not in marginals set.\n  Please make sure, the marginals contain a value for every possible level in the data set."))
      }
    }

    # Check, vice versa,  if every level of every sv in the marginals is contained
    # in the data set at least once.
    levels_of_sv_marginals <- levels(as.factor(list_of_marginals_new[[i]][[sv]]))
    for(lev in levels_of_sv_marginals){
      if(!(lev %in% levels_of_sv_data)){
          stop("Level ", paste(toString(lev), "of variable", toString(sv), "not present in data set.\n  Please make sure, every level of every variable is contained in the data set at least once."))
      }
    }

    variables_formalized[[length(variables_formalized)+1]] <- as.formula(paste0("~", toString(sv)))
  }

  # Check, if sum of proportions lies between 0.95 and 1.05 for
  # every single stratification variable
  for(marg in list_of_marginals_new){
    marg_sum <- sum(marg[[2]])
    if(!abs(marg_sum-1.0)<=0.05)
    {
      warning(paste("Sum of proportions of variable", toString(names(marg)[[1]]), "is not within [0.95;1.05].\nPlease make sure, the proportions for every single sratification variable sum up to almost 1.00"))
    }
  }

  # Standardize population margins to data size
  # This step is necessary to control that the absolute population marginals
  # are given for the same population size for all stratification variables
  # since otherwise the raking weights might be influenced by the different
  # population sizes used for specifying the marginals
  population.margins.standardized <- standardizeMargins(data, population.margins = list_of_marginals_new)


  # Generate raked survey object
  # By generating this objects, the raking weights are computed
  rake_original_input <- survey::rake(design = unweighted_survey_object,
                                      sample.margins = variables_formalized,
                                      population.margins = population.margins.standardized)

  # Get weights from the survey object
  weights <- weights(rake_original_input)
  if(min(weights<=0)){
    warning("Negative values or zeros occured during raking. Using the raking weights is not recommended.")
  }else if(sum(is.na(weights)>0)){
    warning("Undefined value occured during raking. Using the raking weights is not recommended.")
  }

  # Standardizing raking weights by dividing every weight through the smallest
  # weight
  weights <- standardizeRakingWeights(weights = weights)

  return(weights)
}

#' Function for standardizing raking weights
#' Raking weights get divided by the smallest weight. Thereby, all weights
#' become larger or equal to 1 without changing the ratio of the weights
#' to each other.
#' @param weights Raking weights computed by computeRakingWeightsStandardized()
#' @export
#' @param weights Raking weights computed by computeWeights()
standardizeRakingWeights <- function(weights){
  weights_min <- min(weights)
  if(min(weights) <=0){
    warning("Smallest raking weight is zero or below. Weights can not be standardized.\nTherefore, non-standardized weights are used.")
  }
  weights <- weights/weights_min
  return(weights)
}

#' Function for standardizing population margins with respect to the sample size.
#' This step is necessary, since the in advance of the computation it must be
#' guaranteed that all marginals are passed to the rake function of survey
#' with respect to the same sample size, i.e., that all marginals are passed
#' to the rake function in realtion to the norm sample size.
#' to the rake function in reaction to the norm sample size.
#' @param data data.frame with norm sample data.
#' @param population.margins List of data frames containing the marginals of
#' the specified stratification variables. The first column of the data frame
#' must contain the different levels of the variable and the second rows the
#' frequency.
standardizeMargins <- function(data, population.margins) {

  n_sample <- length(data[[1]])

  for (i in seq(1,length(population.margins))) {
    n_populationmargins <- sum(population.margins[[i]][['Freq']])
    ratio <- n_sample/n_populationmargins
    population.margins[[i]][['Freq']] <- population.margins[[i]][['Freq']] * ratio
  }
  return(population.margins)
}
