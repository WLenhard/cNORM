#' Integration of raking into cNORM
#'
#' Computes and standardize raking weights based on the implementation in
#' the survey R package.
#' Generation of weights to compensate for non-stratified samples
#'
#' This function computes standardized raking weights based on the implementation in
#' the survey R package to overcome biases in norm samples. It generates weights, by drawing
#' on the information of population shares (e. g. for sex, ethnic group, region ...) and
#' subsequently reduces the influence of over-represented groups or increases underrepresented
#' cases. The returned weights or scaled to be larger than 0.
#' @param data data.frame with norm sample data.
#' The function is used to compute the actual raking weights using the survey package.
#' @param population.margins List of data frames containing the marginals of
#' the specified stratification variables. The first column of each data frame
#' must contain the different levels of one variable and the second rows the
#' frequency. Moreover, the name of the first column must match the corresponding
#' variable name, while the name of the second column must be "Freq". That means that
#' the marginals must be passed to function as a list of single data frames, one data frame
#' per stratification variable.
#' @export
computeWeights<- function(data, population.margins){

  # Require survey package; returns error with hint to install survey if
  # not already installed

  packageList <- c("survey")

  if (!requireNamespace(packageList, quietly = TRUE)) {
    cat("The survey package is needed to start the user interface. Would you like to try to install it now?")
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
      stop(paste("Variable", toString(sv), "not in data set.\nPlease check make sure data set contains all variables."))
    }

    # Check every level of the current sv in the data set is contained in the
    # corresponding marginals
    levels_of_sv_data <- levels(as.factor(data[[sv]]))
    for(lev in levels_of_sv_data){
      if(!(lev %in% list_of_marginals_new[[i]][[1]])){
        stop("Level", paste(toString(lev), "of variable", toString(names(list_of_marginals_new[[i]])[[1]]),"not in marginals set.\nPlease make sure, the marginals contain a value for every possible level in the data set."))
      }
    }

    # Check, vice versa,  if every level of every sv in the marginals is contained
    # in the data set at least once.
    levels_of_sv_marginals <- levels(as.factor(list_of_marginals_new[[i]][[sv]]))
    for(lev in levels_of_sv_marginals){
      if(!(lev %in% levels_of_sv_data)){
        warning(paste(toString(lev), "of variable", toString(sv), "not in data set.\nPlease make sure, every level of every variable is contained in the data set at least once."))
      }
    }

    variables_formalized[[length(variables_formalized)+1]] <- as.formula(paste0("~", toString(sv)))
  }

  # Check, if sum of proportions lies between 0.95 and 1.05 for
  # every single stratification variable
  for(marg in list_of_marginals_new){
    marg_sum <- sum(marg[[2]])
    if(!abs(marg_sum-1.0)<=0.5)
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
  if(weights_min ==0){
    warning("Smallest raking weight is zero and weights can not be standardized.\nTherefore, non-standardized weights are used.")
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
