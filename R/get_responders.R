#' calculate number of responders in each bin from the results for WOE table from Information package
#'
#' @param IV_row IV of rows from WOE function in Information package
#' @param WOE Weight of Evidence value of each row from WOE function in Information package
#' @param N_total total records
#' @param N_i records in bin i
#'
#' @return a number of responders
#' @export get_responders
#'

get_responders <- function(IV_row, WOE, N_total, N_i){
  alpha <- (IV_row*exp(WOE))/(WOE*(exp(WOE)-1))
  beta <- IV_row/(WOE*(exp(WOE)-1))
  responders <- alpha*(beta*N_total-N_i)/(beta-alpha)
  return(responders)
}
