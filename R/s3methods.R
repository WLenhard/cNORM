# Mapping of S3 methods to cNORM methods

plot.cnorm <- function(object, ...) {  UseMethod("plot.cnorm") }
plot.raw.cnorm <- function(object, ...) { UseMethod("plot.raw.cnorm") }
plot.norm.cnorm <- function(object, ...) { UseMethod("plot.norm.cnorm") }
plot.subset.cnorm <- function(object, ...) { UseMethod("plot.subset.cnorm") }
plot.curves.cnorm <- function(object, ...) { UseMethod("plot.curves.cnorm") }
plot.series.cnorm <- function(object, ...) { UseMethod("plot.series.cnorm") }
plot.density.cnorm <- function(object, ...) { UseMethod("plot.density.cnorm") }
plot.derivative.cnorm <- function(object, ...) { UseMethod("plot.derivative.cnorm") }

cv.cnorm <- function(object, ...) {  UseMethod("cnorm.cv") }
summary.cnorm <- function(object, ...) {  UseMethod("summary.cnorm") }
print.cnorm <- function(object, ...) {  UseMethod("printSubset") }
check.cnorm <- function(object, ...) {  UseMethod("checkConsistency") }

predict.raw.cnorm <- function(raw, age, object, ...) {  UseMethod("predictRaw") }
predict.norm.cnorm <- function(raw, age, object, ...) {  UseMethod("predictNorm") }

table.raw.cnorm <- function(object, ...) {  UseMethod("rawTable") }
table.cnorm <- function(object, ...) {  UseMethod("normTable") }

plot <- plotPercentiles
plot.series <- plotPercentileSeries
plot.raw <- plotRaw
plot.norm <- plotNorm
plot.curves <- plotNormCurves
plot.density <- plotDensity
plot.subset <- plotSubset
plot.derivative <- plotDerivative

cv <- cnorm.cv
summary <- summary.cnorm
print <- printSubset
check <- checkConsistency
predict.raw <- predictRaw
predict.norm <- predictNorm
table <- normTable
table.raw <- rawTable
