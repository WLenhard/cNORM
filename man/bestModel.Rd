% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modelling.R
\name{bestModel}
\alias{bestModel}
\title{Best-fitting Regression Model Based on Powers and Interactions}
\usage{
bestModel(
  data,
  raw = NULL,
  R2 = NULL,
  k = NULL,
  t = NULL,
  predictors = NULL,
  terms = 0,
  weights = NULL,
  force.in = NULL,
  plot = TRUE
)
}
\arguments{
\item{data}{Preprocessed dataset with 'raw' scores, powers, interactions, and usually an explanatory variable (like age).}

\item{raw}{Name of the raw score variable (default: 'raw').}

\item{R2}{Adjusted R^2 stopping criterion for model building (default: 0.99).}

\item{k}{Power constant influencing model complexity (default: 4, max: 6).}

\item{t}{Age power parameter. If unset, defaults to `k`.}

\item{predictors}{List of predictors or regression formula for model selection. Overrides 'k' and can include additional variables.}

\item{terms}{Desired number of terms in the model.}

\item{weights}{Optional case weights. If set to FALSE, default weights (if any) are ignored.}

\item{force.in}{Variables forcibly included in the regression.}

\item{plot}{If TRUE (default), displays a percentile plot of the model.}
}
\value{
The model meeting the R^2 criteria. Further exploration can be done using \code{plotSubset(model)} and \code{plotPercentiles(data, model)}.
}
\description{
Computes and selects the best-fitting regression model by evaluating a series of models with increasing predictors.
It aims to find a parsimonious model that effectively captures the variance in the data. This can be useful in
psychometric test construction to smooth out data and reduce noise while retaining key diagnostic information.
Model selection can be based on the number of terms or the explained variance (R^2). Setting high values for the
number of terms, R^2 cutoff, or `k` may lead to overfitting. Typical recommended starting points are `terms = 5`,
`R^2 = .99`, and `k = 4`.
}
\details{
The functions \code{rankBySlidingWindow}, \code{rankByGroup}, \code{bestModel},
\code{computePowers} and \code{prepareData} are usually not called directly, but accessed
through other functions like \code{cnorm}.

Additional functions like \code{plotSubset(model)} and \code{cnorm.cv} can aid in model evaluation.
}
\examples{

# Example with sample data
\dontrun{
normData <- prepareData(elfe)
model <- bestModel(normData)
plotSubset(model)
plotPercentiles(buildCnormObject(normData, model))

# Specifying variables explicitly
preselectedModel <- bestModel(normData, predictors = c("L1", "L3", "L1A3", "A2", "A3"))
print(regressionFunction(preselectedModel))
}
}
\seealso{
plotSubset, plotPercentiles, plotPercentileSeries, checkConsistency

Other model: 
\code{\link{checkConsistency}()},
\code{\link{cnorm.cv}()},
\code{\link{derive}()},
\code{\link{modelSummary}()},
\code{\link{print.cnorm}()},
\code{\link{printSubset}()},
\code{\link{rangeCheck}()},
\code{\link{regressionFunction}()},
\code{\link{summary.cnorm}()}
}
\concept{model}
