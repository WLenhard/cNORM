summary.cnorm <- function(object, ...) {  UseMethod("summary") }
print.cnorm <- function(x, ...) {  UseMethod("print") }


summary.cnorm <- modelSummary
print.cnorm <- printSubset

