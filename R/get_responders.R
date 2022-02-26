get_responders <- function(IV_row, WOE, N_total, N_i){
  alpha <- (IV_row*exp(WOE))/(WOE*(exp(WOE)-1))
  beta <- IV_row/(WOE*(exp(WOE)-1))
  responders <- alpha*(beta*N_total-N_i)/(beta-alpha)
  return(responders)
}
