% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/s3methods.R
\name{print.cnorm}
\alias{print.cnorm}
\title{S3 method for printing model selection information}
\usage{
\method{print}{cnorm}(x, ...)
}
\arguments{
\item{x}{The model from the 'bestModel' function or a cnorm object}

\item{...}{additional parameters}
}
\value{
A table with information criteria
}
\description{
After conducting the model fitting procedure on the data set, the best fitting
model has to be chosen. The print function shows the R2 and other information
on the different best fitting models with increasing number of predictors.
}
\seealso{
Other model: 
\code{\link{bestModel}()},
\code{\link{checkConsistency}()},
\code{\link{cnorm.cv}()},
\code{\link{derive}()},
\code{\link{modelSummary}()},
\code{\link{printSubset}()},
\code{\link{rangeCheck}()},
\code{\link{regressionFunction}()},
\code{\link{summary.cnorm}()}
}
\concept{model}
