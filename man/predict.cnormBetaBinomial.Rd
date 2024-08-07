% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/betaBinomial.R
\name{predict.cnormBetaBinomial}
\alias{predict.cnormBetaBinomial}
\title{Predict Norm Scores from Raw Scores}
\usage{
\method{predict}{cnormBetaBinomial}(object, ...)
}
\arguments{
\item{object}{A fitted model object of class 'cnormBetaBinomial' or 'cnormBetaBinomial2'.}

\item{...}{Additional arguments passed to the prediction method:
\itemize{
   \item age A numeric vector of ages, same length as raw.
   \item score A numeric vector of raw scores.
   \item range The range of the norm scores in standard deviations. Default is 3. Thus, scores in the range of +/- 3 standard deviations are considered.
 }}
}
\value{
A numeric vector of norm scores.
}
\description{
This function calculates norm scores based on raw scores, age, and a fitted cnormBetaBinomial model.
}
\details{
The function first predicts the alpha and beta parameters of the beta-binomial distribution
for each age using the provided model. It then calculates the cumulative probability for
each raw score given these parameters. Finally, it converts these probabilities to the
norm scale specified in the model.
}
\examples{
\dontrun{
# Assuming you have a fitted model named 'bb_model':
model <- cnorm.betabinomial(ppvt$age, ppvt$raw)
raw <- c(100, 121, 97, 180)
ages <- c(7, 8, 9, 10)
norm_scores <- predict(model, ages, raw)
}

}
\seealso{
Other predict: 
\code{\link{derivationTable}()},
\code{\link{getNormCurve}()},
\code{\link{normTable}()},
\code{\link{predict.cnormBetaBinomial2}()},
\code{\link{predictNorm}()},
\code{\link{predictRaw}()},
\code{\link{rawTable}()}
}
\concept{predict}
