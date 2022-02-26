get_bins <- function(x, bins){
  q <- quantile(x, probs=c(0:(bins)/bins), na.rm=TRUE, type=3)
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



get_bins_df <- function(data, x, bins){
  q <- quantile(data[[x]], probs=c(0:(bins)/bins), na.rm=TRUE, type=3)
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
