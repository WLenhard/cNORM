## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----message = FALSE----------------------------------------------------------
library(cNORM)
# Assign data to object norm.data
norm.data <- ppvt
head(norm.data)

## -----------------------------------------------------------------------------
# Generate population marginals
marginals <- data.frame(var = c("sex", "sex", "migration", "migration"),
                             level = c(1,2,0,1),
                             prop = c(0.51, 0.49, 0.65, 0.35))
head(marginals)

