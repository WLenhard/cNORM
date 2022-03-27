## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(cNORM)
# Displays the first lines of the data of the example dataset 'elfe
head(elfe)

## -----------------------------------------------------------------------------
# Display some descriptive results by group
by(elfe$raw, elfe$group, summary)

## ----fig0, fig.height = 4, fig.width = 7--------------------------------------
# If you only need the model, than use
# model <- bestModel(normData)
#
# Or just the convenience method that does everything at once
model <- cnorm(raw=elfe$raw, group=elfe$group)

## -----------------------------------------------------------------------------
printSubset(model)

## ----fig1, fig.height = 4, fig.width = 7--------------------------------------
plotSubset(model, type = 0) 

## ----fig2, fig.height = 4, fig.width = 7--------------------------------------
plotSubset(model, type = 1) 

## ----fig3, fig.height = 4, fig.width = 7--------------------------------------
# Plots the fitted and the manifest percentiles
# modelling already  displays the plot; you can call it
# directly with plot(results) as well
plotPercentiles(model)


## ----fig4, fig.height = 4, fig.width = 7--------------------------------------
plotRaw(model)

## ----fig5, fig.height = 4, fig.width = 7--------------------------------------
plotDerivative(model, minAge=1, maxAge=6, minNorm=20, maxNorm=80)
# if parameters on age an norm are not specified, cnorm plots within
# the ranges of the current dataset

## ----fig6, fig.height = 4, fig.width = 7--------------------------------------
plotNormCurves(model, normList = c(30, 40, 50, 60, 70), minAge = 2, maxAge = 5, step = 0.1, minRaw = 0, maxRaw = 28)

## -----------------------------------------------------------------------------
predictNorm(15, 4.7, model, minNorm = 25, maxNorm = 75)

## -----------------------------------------------------------------------------
predictRaw(55, 4.5, model)

## -----------------------------------------------------------------------------
predictRaw(c(45, 50, 55), c(2.5, 3, 3.5, 4, 4.5), model)

