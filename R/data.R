#' dataset with response variable y and predictors and models scores
#'
#' The data combined with modeling data and sores from a predictive models(glm, rf, gbm, automl and xgboost).
#' It contains one response variable y and 37 predictive variables and 11,701 records. In addition, it contains scores
#' from five models (glm, rf, gbm, automl and xgboost)
#'
#' @format A data frame with 11,701 rows and 43 variables:
#' \itemize{
#'   \item y: equals 1 if the person response the marketing offer, and 0 if the person didn't response.
#'   \item Age: age of the person.
#'   \item glm: score from glm model.
#'   \item rf: score from rf model.
#'   \item gbm: score from gbm model.
#'   \item automl: score from h2o automl model.
#'   \item xgboost: score from xgboost model.
#'   \item Population Density: demographic variable
#'   \item Health and Beauty: demographic variable
#'   \item Home Length of Residence - Actual (RP)1: demographic variable
#'   \item Other variables are from demographic data
#' }
"testing_frame"

#' Variable importance dataset from a model
#'
#' The vaiable importance data is from a h2o xgboost model.
#' It contains 3 variables and 36 records.
#'
#' @format A data frame with 36 rows and 3 variables:
#' \itemize{
#'   \item Variable: the names of predictors in a model
#'   \item Importance: the percentage of importance
#'   \item Sources: the sources of the predictors
#' }
"ImportantVariables"

#' Predictors response profiles dataset from WOE analysis
#'
#' The data is from Information package WOE analysis
#' It contains 7 variables and 335 records.
#'
#' @format A data frame with 335 rows and 7 variables:
#' \itemize{
#'   \item Sources: the sources of the predictors
#'   \item Variable: the names of predictors in a model
#'   \item Variable_Bin: the bin of numerical variables or the value of catogorical variables
#'   \item Population: the total records in the bin or values
#'   \item IV: the information value from WOE analysis
#'   \item Responders: the number of responders in that bin or values
#'   \item Conversion_Rate: the conversion rate in that bin or values
#' }
"IV_WOE_Table"
