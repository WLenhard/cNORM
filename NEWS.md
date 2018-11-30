# cNORM:news and change-log
This file documents the development of the package as well as open issues or points for further improvements.




### Version in 1.1.1 (current)
Date: 2019.11.29

Changes:
*    Parameters added to cv.norm: Significance level for stratification process
*    Additional plot in cv.norm: delta R2 in norm score validation
*    Example in readme improved
*    CDC data: group variable set to center of interval
*    descend parameter removed from plotPercentileSeries, plotPercentiles, checkConsistency,
     rawTable & normTable; instead take default from model; vigniette updated accordingly



### Version in 1.1.0
Date: 2019.11.23

Changes:

*    Cross validation added: new function: cnorm.cv() for assessing RMSE for raw data and R2 and CROSSFIT for norm data
*    Data table output fpr cnorm.cv
*    rankBySlidingWindow now accessibe via prepareData()
*    group, raw, age and width can now be provided in cnorm.cv
*    parameter for full cross validation (seperate ranking for train and validation)
*    Additional NA checks and warning messages
*    plotPercentiles now with R2adjr in title



### Version in 1.0.3
Date: 2019.11.16

Changes:

*    Additional instruction on series section of visualization tab in Shiny GUI
*    Code cleanup in bestModel function
*    SE added to plotNorm based on Oosterhuis, van der Ark & Sijtsma (2016)
*    RMSE added to model object (m$subsets), to plotRaw and to plotSubset
*    additional plotting options added to GUI:
     +    plotting of differences in raw and norm plot
     +    RMSE in model selection information function




### Version in 1.0.2
Date: 2018.11.16

Changes:

*    Improvements in precision of plotPercentiles
*    error corrected in ppvt dataset: groups did not represent group means
*    function description in 'ranBySlidingWindow' updated
*    checking for missing packages in shiny GUI improved
*    user menu asking to install missing packages added
*    derive-function: more general approach with "order" parameter
*    plotDerivative function can now plot derivatives of higher order
*    exclude cases with missing values in rankByX functions
*    percentile columns added to rawTable and normTable
*    additional data cleansing for data objects imported from Excel file format



### Version in 1.0.1
Date: 2018.11.03

Changes:

*   Improvements in the GUI: Waiting circle shown to indicate ongoing computation
*   Additional help texts on best model in GUI
*   Additional plotting options in cNORM.GUI(): Raw Score and Norm Score plots
*   User input asking for missing suggested packages to install



### Version in 1.0.0
Date: 2018.10.26

Changes:

*   Final polishing finished; realising first major version



### Version in 0.9.20
Date: 2018.10.24

Changes:

*   GUI with Shiny finished
*   ... now working on finally releasing the package



### Version in 0.9.19
Date: 2018.10.20

Changes:

*   API changed: predictNormValue renamed to predictNorm
*   Shiny GUI enhanced
*   Additional plotting options in plotNorm and plotRaw
*   less strict warning messages in predictNormValue function and checkConsistency



### Version in 0.9.18
Date: 2018.10.08

Changes:

*   First shiny prototype (many thanks to Sebastian Gary); please use cNORM.GUI() to start user interface


### Version in 0.9.17
Date: 2018.10.01

Changes:

*   predictNormValue fixed and optimized (many thanks to Sebastian Gary)
*   API change with respect to predictNormValue, rawTable and plotNorm
*   plotNorm: norm score boundaries guessed by min and max score from modelling 



### Version in 0.9.16
Date: 2018.09.21

Changes:

*   bug in predictNormValue partly fixed (further optimization necessary)
*   API change: plotValues renamed to plotRaw
*   new function: plotNorm



### Version in 0.9.15
Date: 2018.09.18

Changes:

*   plotDensity function added
*   attributes added to data.frame to increase usability



### Version in 0.9.13
Date: 2018.09.16

Changes:

*   predictNormValue with higher precision and effectivity
*   rawTableQuick removed from source code



### Version in 0.9.12
Date: 2018.09.11

Changes:

*   'simulateRasch' to simulate test data was added
*   old sim functions removed
*   documentation improved
*   new parameters to bestModel in order to force covariates into regression
*   additional checks in box cox functions



### Version in 0.9.9
Date: 2018.09.06

Changes:

*   Enhancements to the 'prepareData' function



### Version in 0.9.8
Date: 2018.09.05

Changes:

*   Life expectancy dataset of the World Bank added
*   Mortality of infants per 1000 life birth from 1960 to 2017 added
*   Minor chanes in functions to check data integrity and exceptions
*   Vignette updated



### Version in 0.9.7
Date: 2018.08.31

Changes:

*   License changed to AGPL
*   Capitalizations in labels of plots
*   min and max renamed to minRaw and maxRaw (where appropriate)
*   terminology: standard or normal score instead of norm; score instead of value
*   new function for model validation: plotPercentileSeries
*   many functions now draw the default values from the model (plotting and predicting)



### Version in 0.9.6
Date: 2018.08.28

Changes:

*   Minor improvements in function descriptions
*   API of plotSubset changed due to new plotting options



### Version in 0.9.5
Date: 2018.08.25

Changes:

*   New, large dataset for BMI centile estimation from CDC included, type ?CDC for explanation
*   Extensive documentation available via https://www.psychometrica.de/cNorm_en.html (in progress)



### Version in 0.9.4
Date: 2018.08.23

Changes:

*   Generating group variable in rankBySlidingWindow
*   prarameters in plotPercentile to restrict age range
*   ppvt dataset restricted



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
