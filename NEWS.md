# cNORM:news and changelog
This file documents the development of the package as well as open issues or points for further improvements.

### TODO
*   in general check parameter names for coherence (e. g. for the age / group / explanatory variable)
*   Check rawTableQuick for usage with descending values



### Version in 0.9.3

Date: 2018.08.20

Changes:

*   plotNormCurves enhanced (Thanks to Sebastian Gary)
*   new function to plot semi parametric analyses via box cox power transformation: plotBoxCox
*   variable "explanatoryVariable" and "normVariable" in computePowers function renamed for easier API



### Version in 0.9.2

Date: 2018.08.18

Changes:

*   Addeditional dataset: vocabulary development (PPVT4)



### Version in 0.9.1

Date: 2018.08.16

Changes:

*   Added predictRawBC and predictNormBC for computing norm and raw values based on the parametric box cox power function parameters
*   New contributor: Sebastian Gary, welcome to the team!
*   Missing raw variable definition in plotValues corrected



### Version in 0.9.0

Date: 2018.08.14

Changes:

*   Box Cox power transformation for regression model at specific age: optional parametric modelling for non-paramteric regression model



### Version in 0.8.9

Date: 2018.08.13

Changes:

*   Convenience method for selection best model added: 'printSubset'
*   predictNormValue now accepts lists of values as well



### Version in 0.8.8
Date: 2018.08.12

Changes:

*   parameter checks added
*   new parameter 'descriptives' addad to rankByGroup and rankBySlidingWindow added to retrieve descriptive statistics for each observation
*   improvements in the documentation
*   errors in bestModel and plotPercentiles corrected, when variable names are not as in example sample



### Version in 0.8.6

Date: 2018.08.11

Changes:

*   new function: 'rankBySlidingWindow' which can be used for data sets with continuous age variables
*   error corrected for data being loaded from SPSS files
*   improvements in the documentation



### Version in 0.8.5

Date: 2018.08.06

Changes:

*   new function for simulating data



### Version in 0.8.3

Date: 2018.08.03

Changes:

*   Code cleaning and formatting



### Version in 0.8.2

Date: 2018.08.02

Changes:

*   new internal function: rawTableQuick for speeding up generating norm tables
  Still has to be checked for working with descending values.
  Works only, if model assumptions are valid



### Version in 0.8.0

Date: 2018.08.01

Changes:

*   new function: rawTable allows creating norm tables with assignment of raw -> norm values solves inverse function of regression model with brute force



### Version in 0.7.11

Date: 2018.07.31

Changes:

*   improved 'prepareData' function



### Version in 0.7.10

Date: 2018.07.28

Changes:

*   Description for computePowers improved
*   option in plotPercentile to use percentile scale or self defined c(mean, sd)



### Version in 0.7.9

Date: 2018.07.28

Changes:

*   'descend' parameter added to consistencyCheck and normTable
*   dependency rColorBrewer removed; plotPercentiles changed accordingly
*   latticeExtra moved to 'suggests'



### Version in 0.7.8

Date: 2018.07.27

Changes:

*   'descend' parameter added to consistencyCheck and normTable



### Version in 0.7.7

Date: 2018.07.27

Changes:

*   Small changes to error messages in bestModel
*   printing of min value in plotDerivate removed



### Version in 0.7.6

Date: 2018.07.27

Changes:

*   parameter 'predictors' added to allow self defined regression functions, e. g. for the inclusion of other ranking parameters like sex
*   'type' parameter added to 'plotPercentile' to allow selection of quantile algorithm. Please consult help(quantile) for further information on 'type'



### Version in 0.7.5

Date: 2018.07.26

Changes:

*   dependency dplyr removed: rankByOrder and plotPercentiles rewritten
*   API-change: derivationPlot renamed to plotDerivate
*   small changes to vignette and readme
*   parameter "raw" added to rankByGroup to specify raw value variable
*   rawVar and groupVar in plotPercentiles renamed to raw and group to make API more coherent



### Version in 0.7.4

Date: 2018.07.25

Changes:

*   rankByOrder: ranking in descending order added



### Version in 0.7.3

Date: 2018.07.25

Changes:

*   Additional ranking algorithms: Filliben, Levenbach, Yu & Huang; API changed to index
*   scale can be specified as double vector with c(mean, sd)
*   vignette updated accordingly



### Version in 0.7.2

Date: 2018.07.24

Changes:

*   None. This is the first release