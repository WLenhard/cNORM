---
output:
  md_document:
    variant: markdown_github
---

```{r, echo = FALSE}
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "README-"
)
```

# cNORM

The package provides methods for generating non-parametric regression based continuous norms, as f. e. for psychometric test development, biological and physiological growth curves, and
screenings in the medical domain.

## Approach

Conventional methods for producing test norms are often plagued with "jumps" or "gaps"
(i.e., discontinuities) in norm tables and low confidence for assessing extreme scores.
cNORM addresses these problems and also has the added advantage of not requiring
assumptions about the distribution of the raw data: The norm values are established from
raw data by modeling the latter ones as a function  of both percentile scores and an
explanatory variable (e.g., age) through Taylor polynomials. The method minimizes
bias arising from sampling and measurement error, while handling marked deviations from
normality – such as are commonplace in clinical samples. Contrary to parametric approaches, it does not rely on distribution assumptions of the initial norm data and is thus a very robust approach in generating norm tables.

## Example

Conducting the analysis consists of four steps:
1.  Data preparation
1.  Establishing the regression model and selecting the parameters
1.  Validating the model
1.  Generating norm tables and plotting the results

cNORM offers function for all of these steps, helps in selecting the best
fitting models and generating the norm tables.

```{r example}
## basic example code for modeling the sample dataset
library(cNORM)
normData <- prepareData()
model <- bestModel(normData)

# Plot model
plotSubset(model)
plotPercentiles(normData, model)

# print norm table (for grade 3)
normTable(3, model)

# start vignette for a complete walk through
vignette(cNORM-Demo)
```

## Installation
Once it is on CRAN, cNORM can be installed via
```{r example}
install.packages("cNORM", dependencies = TRUE)
```

Until then, you can access the github development version via
```{r example}
install.packages("devtools")
devtools::install_github("WLenhard/cNORM")
```
