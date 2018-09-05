#' Sentence completion test from ELFE 1-6
#'
#' A dataset containing the raw data of 1400 students from grade 2 to 5 in the sentence
#' comprehension test from ELFE 1-6 (Lenhard & Schneider, 2006). In this test, students
#' are presented lists of sentences with one gap. The student has to fill in the correct
#' solution by selecting from a list of 5 alternatives per sentence. The alternatives
#' include verbs, adjectives, nouns, pronouns and conjunctives. Each item stems from
#' the same word type. The text is speeded, with a time cutoff of 180 seconds. The
#' variables are as follows:
#'
#' @format A data frame with 1400 rows and 3 variables:
#' \describe{
#'   \item{personID}{ID of the student}
#'   \item{group}{grade level, with x.5 indicating the end of the school year and x.0 indicating the middle of the school year}
#'   \item{raw}{the raw score of the student, spanning values from 0 to 28}
#' }
#' @source \url{https://www.psychometrica.de/elfe2.html}
#' @references Lenhard, W. & Schneider, W.(2006). Ein Leseverstaendnistest fuer Erst- bis Sechstklaesser. Goettingen/Germany: Hogrefe.
#' @docType data
#' @keywords datasets
#' @name elfe
#' @examples
#' # prepare data, retrieve model and plot percentiles
#' data.elfe <- prepareData(elfe)
#' model.elfe <- bestModel(data.elfe)
#' plotPercentiles(data.elfe, model.elfe)
#' @format A data frame with 1400 rows and 3 columns
"elfe"

#' Vocabulary development from 4 to 16
#'
#' A dataset based on an unstratified sample of PPVT4 data (German adaption). The PPVT4 consists of blocks of items with
#' 12 items each. Each item consists of 4 pictures. The test taker is given a word orally and he or she has to point out
#' the picture matching the oral word. Bottom and ceiling blocks of items are determined according to age and performance. For
#' instance, when a student knows less than 4 word from a block of 12 items, the testing stops. The sample is not identical
#' with the norm sample and includes doublettes of cases in order to align the sample size per age group. It is
#' primarily intended for running the cNORM analyses. The cleaned and stratified data is available on request.
#'
#' @format A data frame with 5600 rows and 4 variables:
#' \describe{
#'   \item{age}{the chronological age of the child}
#'   \item{group}{the according age group, e.g. age group 4 consists of children age 3.5 to 4.5}
#'   \item{sex}{the sex of the test taker, 1=male, 2=female}
#'   \item{raw}{the raw score of the student, spanning values from 0 to 228}
#' }
#' @source \url{https://www.psychometrica.de/ppvt4.html}
#' @references Lenhard, A., Lenhard, W., Segerer, R. & Suggate, S. (2015). Peabody Picture Vocabulary Test - Revision IV (Deutsche Adaption). Frankfurt a. M./Germany: Pearson Assessment.
#' @docType data
#' @keywords datasets
#' @name ppvt
#' @examples
#' \dontrun{
#' # Example with continuous age variable
#' data.ppvt <- rankBySlidingWindow(ppvt, age="age", width=1)
#' data.ppvt <- computePowers(data.ppvt, age="age")
#' model.ppvt <- bestModel(data.ppvt)
#'
#' # plot information function
#' plotSubset(model.ppvt, type=2)
#'
#' # check model consistency
#' checkConsistency(model.ppvt)
#'
#' # plot percentiles
#' plotPercentiles(data.ppvt, model.ppvt)
#' }
#' @format A data frame with 5600 rows and 9 columns
"ppvt"

#' BMI growth curves from age 2 to 25
#'
#' By the courtesy of the Center of Disease Control (CDC), cNORM includes human growth data for children and adolescents
#' age 2 to 25 that can be used to model trajectories of the body mass index and to estimate centiles for clinical
#' definitions of under- and overweight. The data stems from the NHANES surveys in the US and was published in 2012
#' as public domain. The data was cleaned by removing missing values and it includes the following variables from or
#' based on the original dataset.
#'
#' @format A data frame with 45053 rows and 7 variables:
#' \describe{
#'   \item{age}{continuous age in years, based on the month variable}
#'   \item{group}{age group; chronological age in years at the time of examination}
#'   \item{month}{chronological age in month at the time of examination}
#'   \item{sex}{sex of the participant, 1 = male, 2 = female}
#'   \item{height}{height of the participan in cm}
#'   \item{weight}{weight of the participan in kg}
#'   \item{bmi}{the body mass index, computed by (weight in kg)/(height in m)^2}
#' }
#' @docType data
#' @keywords datasets
#' @source \url{https://wwwn.cdc.gov/nchs/nhanes/OtherNhanesData.aspx}
#' @references CDC (2012). National Health and Nutrition Examination Survey: Questionaires, Datasets and Related
#' Documentation. available \url{https://wwwn.cdc.gov/nchs/nhanes/OtherNhanesData.aspx} (date of retrieval: 25/08/2018)
#' @keywords datasets, growth curves, bmi
#' @name CDC
#' @format A data frame with 45035 rows and 7 columns
"CDC"

#' Life expectancy at birth from 1960 to 2017
#'
#' The data is available by the courtesy of the World Bank under Creative Commons Attribution 4.0 (CC-BY 4.0).
#' It includes the life expectancy at birth on nation level from 1960 to 2017. The data has been converted to
#' long data format, aggregates for groups of nations and missings have been deleted and a grouping variable
#' with a broader scope spanning 4 years each has been added. It shows, that it can be better to reduce
#' predictors. The model does not converge anymore after using 8 predictors and the optimal solution is
#' achieved with four predictors, equaling R2=.9825.
#'
#' @format A data frame with 11182 rows and 4 variables:
#' \describe{
#'   \item{Country}{The name of the country}
#'   \item{year}{reference year of data collection}
#'   \item{life}{the life expectancy at birth}
#'   \item{group}{a grouping variable based on 'year' but with a lower resolution; spans intervals of 4 years each}
#' }
#' @docType data
#' @keywords datasets
#' @source \url{https://data.worldbank.org/indicator/sp.dyn.le00.in}
#' @references The World Bank (2018). Life expectancy at birth, total (years). Data Source	World Development Indicators
#' available \url{https://data.worldbank.org/indicator/sp.dyn.le00.in} (date of retrieval: 01/09/2018)
#' @keywords datasets, life expectany, time series
#' @name life
#' @examples
#' # data preparatiom
#' data.life <- rankByGroup(life, raw="life")
#' data.life <- computePowers(data.life, age="year")
#'
#' #determining best suiting model by plotting series
#' model.life <- bestModel(data.life, raw="life")
#' plotPercentileSeries(data.life, model.life, end=10)
#'
#' # model with four predictors seems to work best
#' model2.life <- bestModel(data.life, raw="life", terms=4)
#' @format A data frame with 11182 rows and 4 columns
"life"

#' Mortality of infants per 1000 life birth from 1960 to 2017
#'
#' The data is available by the courtesy of the World Bank under Creative Commons Attribution 4.0 (CC-BY 4.0).
#' It includes the mortality rate of life birth per country from 1960 to 2017. The data has been converted to
#' long data format, aggregates for groups of nations and missings have been deleted and a grouping variable
#' with a broader scope spanning 4 years each has been added. It can be used for demonstrating intersecting
#' percentile curves at bottom effects.
#'
#' @format A data frame with 9547 rows and 4 variables:
#' \describe{
#'   \item{Country}{The name of the country}
#'   \item{year}{reference year of data collection}
#'   \item{mortality}{the mortality per 1000 life born children}
#'   \item{group}{grouping variable based on 'year' with a lower resolution; spans intervals of 4 years each}
#' }
#' @docType data
#' @keywords datasets
#' @source \url{https://data.worldbank.org/indicator/SP.DYN.IMRT.IN}
#' @references The World Bank (2018). Mortality rate, infant (per 1,000 live births). Data Source	available
#' \url{https://data.worldbank.org/indicator/SP.DYN.IMRT.IN} (date of retrieval: 02/09/2018)
#' @keywords datasets, child mortality, time series
#' @name mortality
#' @examples
#' # data preparation
#' data.mortality <- rankByGroup(mortality, raw="mortality")
#' data.mortality <- computePowers(data.mortality, age="year")
#'
#' # modelling
#' model.mortality <- bestModel(data.mortality, raw="mortality")
#' plotSubset(model.mortality, type = 0)
#' plotPercentileSeries(data.mortality, model.mortality, end=9, percentiles = c(.1, .25, .5, .75, .9))
#'
#' @format A data frame with 9547 rows and 4 columns
"mortality"
