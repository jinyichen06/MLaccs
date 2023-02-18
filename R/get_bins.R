#' Get Bins for a Numerical Vector
#'
#' @param x a numerical vector
#' @param bins a single number (greater than or equal to 2) giving the number of intervals into which x is to be cut.
#'
#' @importFrom stats quantile
#'
#' @return a vector of bins
#' @export get_bins
#'

get_bins <- function(x, bins){
  q <- stats::quantile(x, probs=c(0:(bins)/bins), na.rm=TRUE, type=3)
  cuts <- unique(q)

  if (is.character(x)==FALSE & is.factor(x)==FALSE){
    if (length(cuts)==1){
      x_bin <- x
    } else{
      x_bin <- cut(x, cuts,include.lowest = TRUE)
      x_bin <- as.character(x_bin)
      x_bin[is.na(x_bin)]<-""
    }
  } else{
    x_bin <- x
  }
  return(x_bin)
}



#' Title
#'
#' @param data a data.frame
#' @param x a numerical variable's names that needs to break its values in to bins
#' @param bins a single number (greater than or equal to 2) giving the number of intervals into which x is to be cut.
#'
#' @importFrom stats quantile

#' @return a vector of bins
#' @export get_bins_df
#'

get_bins_df <- function(data, x, bins){
  q <- stats::quantile(data[[x]], probs=c(0:(bins)/bins), na.rm=TRUE, type=3)
  cuts <- unique(q)

  if (is.character(data[[x]])==FALSE & is.factor(data[[x]])==FALSE){
    if (length(cuts)==1){
      x_bin <- data[[x]]
    } else{
      x_bin <- cut(data[[x]], cuts,include.lowest = TRUE)
      x_bin <- as.character(x_bin)
      x_bin[is.na(x_bin)]<-""
    }
  } else{
    x_bin <- data[[x]]
  }
  return(x_bin)
}
