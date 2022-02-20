#' Generation of weights to compensate for unstratified samples
#'
#' This function computes standardized raking weights based on the implementation in
#' the survey R package to overcome biases in norm samples. It generates weights, by drawing
#' on the information of population shares (e. g. for sex, ethnic group, region ...) and
#' subsequently reduces the influence of overrepresented groups or increases underrepresented
#' cases. The returned weights or scaled to be larger than 0.
#' @param data data.frame with norm sample data.
#' The function is used to compute the actual raking weights using the survey package.
#' @param variables List of strings containing the names of the stratification variables
#' Please make sure that the used names are the same as in the norm samples as well as
#' the same as used in the list of population marginals population.margins
#' @param population.margins List of data frames containing the marginals of
#' the specified stratification variables. The first column of each data frame
#' must contain the different levels of one variable and the second rows the
#' frequency. Moreover, the name of the first column must match the corresponding
#' variable name, while the name of the second column must be "Freq". That means that
#' the marginals must be passed to function as a list of single data frames, one data frame
#' per stratification variable.
#' @export
computeWeights<- function(data, variables, population.margins){

  # Require survey package; returns error with hint to install survey if
  # not already installed
  require(survey)
  # Create non-weighted survey object
  unweighted_survey_object <- survey::svydesign(ids = ~1,
                                                data = data,
                                                weights = NULL)

  variables_formalized <- list()
  for(sv in variables){
    if(!(sv %in% names(data))){
      stop(paste(toString, "not in data."))
    }
    variables_formalized[[length(variables_formalized)+1]] <- as.formula(paste0("~", toString(sv)))
  }

  # Standardize population margins to data size
  # This step is necessary to control that the absolute population marginals
  # are given for the same population size for all stratification variables
  # since otherwise the raking weights might be influenced by the different
  # population sizes used for specifying the marginals
  population.margins.standardized <- standardizeMargins(sample_data = data, population.margins = population.margins)

  # Generate raked survey object
  # By generating this objects, the raking weights are computed
  rake_original_input <- survey::rake(design = unweighted_survey_object,
                                      sample.margins = variables_formalized,
                                      population.margins = population.margins)

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
#' to the rake function in reaction to the norm sample size.
#' @param data data.frame with norm sample data.
#' @param population.margins List of data frames containing the marginals of
#' the specified stratification variables. The first column of the data frame
#' must contain the different levels of the variable and the second rows the
#' frequency.
standardizeMargins <- function(sample_data, population.margins) {

  n_sample <- length(sample_data[[1]])

  for (i in seq(1,length(population.margins))) {
    n_populationmargins <- sum(population.margins[[i]][2])
    ratio <- n_sample/n_populationmargins
    population.margins[[i]][2] <- population.margins[[i]][2] * ratio
  }
  return(population.margins)
}
