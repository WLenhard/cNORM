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
#'   \item{group}{grade level, with x.5 indicating the end of the school year and x.0 indicatin the midth}
#'   \item{raw}{the raw score of the student, spanning values from 0 to 28}
#' }
#' @source \url{https://www.psychometrica.de/elfe2.html}
#' @references Lenhard, W. & Schneider, W.(2006). Ein Leseverstaendnistest fuer Erst- bis Sechstklaesser. Goettingen/Germany: Hogrefe.
#' @docType data
#' @keywords datasets
#' @name elfe
#' @format A data frame with 1400 rows and 3 columns
"elfe"

#' Vocabulary development from 4 to 16
#'
#' A dataset based on an unstratified sample of PPVT4 data (German adaption), including demografic data like
#' age, sex, school form, migration background and language spoken at home. The PPVT4 consists of blocks of items with
#' 12 items each. Each item consists of 4 pictures. The test taker is given a word orally and he or she has to point out
#' the picture matching the oral word. Bottom and ceiling blocks of items are determined according to age and performance. For
#' instance, when a student knows less than 4 word from a block of 12 items, the testing stops.
#'
#' @format A data frame with 5600 rows and 9 variables:
#' \describe{
#'   \item{age}{the chronological age of the child}
#'   \item{group}{the according age group, e.g. age group 4 consists of children age 3.5 to 4.5}
#'   \item{sex}{the sex of the test taker, 1=male, 2=female}
#'   \item{schoolform}{the type of school in the German school system, the child is in. 0=missing, 1=preschool,
#'   2=elementary school, 3=Gesamtschule, 4=secondary modern school (Hauptschule), 5 = senior high school
#'   (Realschule), 6 = grammar school (Gymnasium), 7 = Training on the job (Berufsschule), 8 = university,
#'   9 = special educational services}
#'   \item{grade}{the grade, in case the child is in the school}
#'   \item{migration}{the migration backgroun with 0 = no migration, 1 = either one of the parents or the child immigrated}
#'   \item{language}{language spoken at home; 0 = missing, 1 = German, 2 = Turkish, 3 = Russian, 4 = Polish,
#'   5 = Spanish, 6 = Italian, 7 = other}
#'   \item{raw}{the raw score of the student, spanning values from 0 to 228}
#'   \item{duration}{the time needed for completing the test in ms}
#' }
#' @source \url{https://www.psychometrica.de/ppvt4.html}
#' @references Lenhard, A., Lenhard, W., Segerer, R. & Suggate, S. (2015). Peabody Picture Vocabulary Test - Revision IV (Deutsche Adaption). Frankfurt a. M./Germany: Pearson Assessment.
#' @docType data
#' @keywords datasets
#' @name ppvt
#' @format A data frame with 5600 rows and 9 columns
"ppvt"