#' Calculate Correlation
#'
#' @param data input data.frame for calculating correlations between variables
#' @param ncore number of cores used. Default is to use available cores - 1.
#' @param parallel set to TRUE for parallel processing. Number of cores is determined by the ncore parameter.
#'
#' @import foreach
#' @importFrom parallel detectCores
#' @importFrom dplyr mutate across %>%
#' @importFrom tidyselect where
#' @importFrom doParallel registerDoParallel
#' @importFrom stats quantile
#' @importFrom methods is
#' @importFrom stats cor chisq.test
#' @return a data.frame with variables and their correlations
#' @export get_correlation
#'

get_correlation <- function(data=NULL, ncore=NULL, parallel=TRUE){

  ### Helper function
  combine <- function(x, ...) {
    lapply(seq_along(x),
           function(i) c(x[[i]], lapply(list(...), function(y) y[[i]])))
  }

  ### Dataset provided?
  if (is.null(data)){
    stop("Error: No dataset provided")
  }

  ### Variable_Type provided?
  # if (is.null(Variable_Type)){
  #   stop("Error: No Variable_Type provided")
  # }

  data <- data %>%
    dplyr::mutate(dplyr::across(where(is.factor),as.character))
  Len<-dim(data)[2]
  ColNames <- names(data)
  isCharacter <- sapply(data, function(x){class(x)=="character"})

  if(parallel==TRUE){
    if (is.null(ncore)){
      ncore <- parallel::detectCores()-1
    }
    if (ncore<1) ncore <- 1
    doParallel::registerDoParallel(ncore)

    ### Loop through variables
    Correlation_df <-
      foreach(i=1:(Len-1), .combine='rbind') %:%
      foreach(j=(i+1):Len, .combine='rbind') %dopar% {
        #print(c(i,j))
        V1<-ColNames[i]
        V2<-ColNames[j]
        isCharacter1<-isCharacter[i]
        isCharacter2<-isCharacter[j]
        if(isCharacter1 & isCharacter2){
          tbl<-table(data[[V1]],data[[V2]])
          #chi2 <- chisq.test(tbl, correct=F)
          chi2 <- tryCatch(stats::chisq.test(tbl, correct=F),error=function(e) e, warning=function(w) w)
          chi2_statistic <- ifelse(is(chi2,"error") | is(chi2,"warning"), NA, chi2$statistic)
          r<-dim(tbl)[1]
          k<-dim(tbl)[2]
          c<-min(r-1,k-1)
          CrammerValue<-sqrt(chi2_statistic / (sum(tbl)*c))
          #CrammerValue<-sqrt(chi2_statistic / sum(tbl))
          Correlation_df<-data.frame(V1=V1,V2=V2,isCharacter1=isCharacter1,isCharacter2=isCharacter2,Correlation=CrammerValue)
        }else if((!isCharacter1 & isCharacter2)){
          if(length(unique(data[[V1]]))<14){
            V1_character <- as.character(data[[V1]])
            tbl<-table(V1_character,data[[V2]])
            #chi2 <- chisq.test(tbl, correct=F)
            chi2 <- tryCatch(stats::chisq.test(tbl, correct=F),error=function(e) e, warning=function(w) w)
            chi2_statistic <- ifelse(is(chi2,"error") | is(chi2,"warning"), NA, chi2$statistic)
            r<-dim(tbl)[1]
            k<-dim(tbl)[2]
            c<-min(r-1,k-1)
            CrammerValue<-sqrt(chi2_statistic / (sum(tbl)*c))
            Correlation_df<-data.frame(V1=V1,V2=V2,isCharacter1=isCharacter1,isCharacter2=isCharacter2,Correlation=CrammerValue)
          }else{

            V1_bin <- get_bins(data[[V1]],bins = 10)
            tbl<-table(V1_bin,data[[V2]])
            #chi2 <- chisq.test(tbl, correct=F)
            chi2 <- tryCatch(stats::chisq.test(tbl, correct=F),error=function(e) e, warning=function(w) w)
            chi2_statistic <- ifelse(is(chi2,"error") | is(chi2,"warning"), NA, chi2$statistic)
            r<-dim(tbl)[1]
            k<-dim(tbl)[2]
            c<-min(r-1,k-1)
            CrammerValue<-sqrt(chi2_statistic / (sum(tbl)*c))
            Correlation_df<-data.frame(V1=V1,V2=V2,isCharacter1=isCharacter1,isCharacter2=isCharacter2,Correlation=CrammerValue)
          }
        }else if((isCharacter1 & !isCharacter2)){
          if(length(unique(data[[V2]]))<14){
            V2_character <- as.character(data[[V2]])
            tbl<-table(data[[V1]],V2_character)
            #chi2 <- chisq.test(tbl, correct=F)
            chi2 <- tryCatch(stats::chisq.test(tbl, correct=F),error=function(e) e, warning=function(w) w)
            chi2_statistic <- ifelse(is(chi2,"error") | is(chi2,"warning"), NA, chi2$statistic)
            r<-dim(tbl)[1]
            k<-dim(tbl)[2]
            c<-min(r-1,k-1)
            CrammerValue<-sqrt(chi2_statistic / (sum(tbl)*c))
            Correlation_df<-data.frame(V1=V1,V2=V2,isCharacter1=isCharacter1,isCharacter2=isCharacter2,Correlation=CrammerValue)
          }else{
            V2_bin <- get_bins(data[[V2]],bins = 10)
            tbl<-table(data[[V1]],V2_bin)
            #chi2 <- chisq.test(tbl, correct=F)
            chi2 <- tryCatch(stats::chisq.test(tbl, correct=F),error=function(e) e, warning=function(w) w)
            chi2_statistic <- ifelse(is(chi2,"error") | is(chi2,"warning"), NA, chi2$statistic)
            r<-dim(tbl)[1]
            k<-dim(tbl)[2]
            c<-min(r-1,k-1)
            CrammerValue<-sqrt(chi2_statistic / (sum(tbl)*c))
            Correlation_df<-data.frame(V1=V1,V2=V2,isCharacter1=isCharacter1,isCharacter2=isCharacter2,Correlation=CrammerValue)
          }
        }else {
          Correlation<-stats::cor(data[[V1]],data[[V2]],method = "pearson", use = "na.or.complete")
          Correlation_df<-data.frame(V1=V1,V2=V2,isCharacter1=isCharacter1,isCharacter2=isCharacter2,Correlation=abs(Correlation))
        }
      }

  }else{
    Correlation_df<-data.frame()
    for(i in 1:(Len-1)){
      for(j in (i+1):Len){
        print(c(i,j))
        V1<-ColNames[i]
        V2<-ColNames[j]
        isCharacter1<-isCharacter[i]
        isCharacter2<-isCharacter[j]
        if(isCharacter1 & isCharacter2){
          tbl<-table(data[[V1]],data[[V2]])
          #chi2 <- chisq.test(tbl, correct=F)
          chi2 <- tryCatch(stats::chisq.test(tbl, correct=F),error=function(e) e, warning=function(w) w)
          chi2_statistic <- ifelse(is(chi2,"error") | is(chi2,"warning"), NA, chi2$statistic)
          r<-dim(tbl)[1]
          k<-dim(tbl)[2]
          c<-min(r-1,k-1)
          CrammerValue<-sqrt(chi2_statistic / (sum(tbl)*c))
          #CrammerValue<-sqrt(chi2_statistic / sum(tbl))
          df<-data.frame(V1=V1,V2=V2,isCharacter1=isCharacter1,isCharacter2=isCharacter2,Correlation=CrammerValue)
        }else if((!isCharacter1 & isCharacter2)){
          if(length(unique(data[[V1]]))<14){
            V1_character <- as.character(data[[V1]])
            tbl<-table(V1_character,data[[V2]])
            #chi2 <- chisq.test(tbl, correct=F)
            chi2 <- tryCatch(stats::chisq.test(tbl, correct=F),error=function(e) e, warning=function(w) w)
            chi2_statistic <- ifelse(is(chi2,"error") | is(chi2,"warning"), NA, chi2$statistic)
            r<-dim(tbl)[1]
            k<-dim(tbl)[2]
            c<-min(r-1,k-1)
            CrammerValue<-sqrt(chi2_statistic / (sum(tbl)*c))
            df<-data.frame(V1=V1,V2=V2,isCharacter1=isCharacter1,isCharacter2=isCharacter2,Correlation=CrammerValue)
          }else{

            V1_bin <- get_bins(data[[V1]],bins = 10)
            tbl<-table(V1_bin,data[[V2]])
            #chi2 <- chisq.test(tbl, correct=F)
            chi2 <- tryCatch(stats::chisq.test(tbl, correct=F),error=function(e) e, warning=function(w) w)
            chi2_statistic <- ifelse(is(chi2,"error") | is(chi2,"warning"), NA, chi2$statistic)
            r<-dim(tbl)[1]
            k<-dim(tbl)[2]
            c<-min(r-1,k-1)
            CrammerValue<-sqrt(chi2_statistic / (sum(tbl)*c))
            df<-data.frame(V1=V1,V2=V2,isCharacter1=isCharacter1,isCharacter2=isCharacter2,Correlation=CrammerValue)
          }
        }else if((isCharacter1 & !isCharacter2)){
          if(length(unique(data[[V2]]))<14){
            V2_character <- as.character(data[[V2]])
            tbl<-table(data[[V1]],V2_character)
            #chi2 <- chisq.test(tbl, correct=F)
            chi2 <- tryCatch(stats::chisq.test(tbl, correct=F),error=function(e) e, warning=function(w) w)
            chi2_statistic <- ifelse(is(chi2,"error") | is(chi2,"warning"), NA, chi2$statistic)
            r<-dim(tbl)[1]
            k<-dim(tbl)[2]
            c<-min(r-1,k-1)
            CrammerValue<-sqrt(chi2_statistic / (sum(tbl)*c))
            df<-data.frame(V1=V1,V2=V2,isCharacter1=isCharacter1,isCharacter2=isCharacter2,Correlation=CrammerValue)
          }else{
            V2_bin <- get_bins(data[[V2]],bins = 10)
            tbl<-table(data[[V1]],V2_bin)
            #chi2 <- chisq.test(tbl, correct=F)
            chi2 <- tryCatch(stats::chisq.test(tbl, correct=F),error=function(e) e, warning=function(w) w)
            chi2_statistic <- ifelse(is(chi2,"error") | is(chi2,"warning"), NA, chi2$statistic)
            r<-dim(tbl)[1]
            k<-dim(tbl)[2]
            c<-min(r-1,k-1)
            CrammerValue<-sqrt(chi2_statistic / (sum(tbl)*c))
            df<-data.frame(V1=V1,V2=V2,isCharacter1=isCharacter1,isCharacter2=isCharacter2,Correlation=CrammerValue)
          }
        }else {
          Correlation<-stats::cor(data[[V1]],data[[V2]],method = "pearson", use = "na.or.complete")
          df<-data.frame(V1=V1,V2=V2,isCharacter1=isCharacter1,isCharacter2=isCharacter2,Correlation=abs(Correlation))
        }
        Correlation_df<-rbind(Correlation_df,df)
      }
    }
  }
  return(Correlation_df)
}
