#' Find redundant variables based on the correlation data.frame
#'
#' @param data correlation data.frame from get_correlation function
#' @param V1 the column name of first variable in the input correlation data.frame
#' @param V2 the column name of second variable in the input correlation data.frame
#' @param correlation_var the column name of variable that measures the correlations between two variables.
#' @param cutoff the value of correlation that considered to be highly correlated between two variables.
#' @importFrom dplyr filter %>%
#' @return a character vectors
#' @export get_redundant_vars
#'

get_redundant_vars <- function(data, V1, V2, correlation_var, cutoff=0.7){
  names(data)[which(names(data)==V1)] <- "V1"
  names(data)[which(names(data)==V2)] <- "V2"
  names(data)[which(names(data)==correlation_var)] <- "Correlation"
  Correlation <- NULL

  data$V1 <- as.character(data$V1)
  data$V2 <- as.character(data$V2)
  vars <- unique(data$V1)
  redundant_vars <- vector()
  var1 <- vars[1]
  while (!is.na(var1)) {
    correlation_high <- data %>%
      filter(V1==var1) %>%
      filter(Correlation >= cutoff)
    redundant_vars_1 <- unique(correlation_high$V2)
    redundant_vars <- c(redundant_vars,redundant_vars_1)

    data <- data %>%
      filter(!(V1 %in% c(var1,redundant_vars)))
    vars <- unique(data$V1)
    var1 <- vars[1]
  }
  vec_redundant <- unique(redundant_vars)
  return(vec_redundant)
}
