get_correlation <- function(data=NULL,Variable_Type=NULL,ncore=NULL, parallel=TRUE){

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
  if (is.null(Variable_Type)){
    stop("Error: No Variable_Type provided")
  }

  data <- data %>%
    mutate(across(where(~is.factor(.)),as.character))
  Len<-dim(data)[2]
  ColNames <- names(data)
  isCharacter <- sapply(data, function(x){class(x)=="character"})
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
        chi2 <- chisq.test(tbl, correct=F)
        r<-dim(tbl)[1]
        k<-dim(tbl)[2]
        c<-min(r-1,k-1)
        CrammerValue<-sqrt(chi2$statistic / (sum(tbl)*c))
        #CrammerValue<-sqrt(chi2$statistic / sum(tbl))
        df<-data.frame(V1=V1,V2=V2,isCharacter1=isCharacter1,isCharacter2=isCharacter2,Correlation=CrammerValue)
      }else if((!isCharacter1 & isCharacter2)){
        if(length(unique(data[[V1]]))<14){
          V1_character <- as.character(data[[V1]])
          tbl<-table(V1_character,data[[V2]])
          chi2 <- chisq.test(tbl, correct=F)
          r<-dim(tbl)[1]
          k<-dim(tbl)[2]
          c<-min(r-1,k-1)
          CrammerValue<-sqrt(chi2$statistic / (sum(tbl)*c))
          df<-data.frame(V1=V1,V2=V2,isCharacter1=isCharacter1,isCharacter2=isCharacter2,Correlation=CrammerValue)
        }else{

          V1_bin <- getBins(data[[V1]],bins = 10)
          tbl<-table(V1_bin,data[[V2]])
          chi2 <- chisq.test(tbl, correct=F)
          r<-dim(tbl)[1]
          k<-dim(tbl)[2]
          c<-min(r-1,k-1)
          CrammerValue<-sqrt(chi2$statistic / (sum(tbl)*c))
          df<-data.frame(V1=V1,V2=V2,isCharacter1=isCharacter1,isCharacter2=isCharacter2,Correlation=CrammerValue)
        }
      }else if((isCharacter1 & !isCharacter2)){
        if(length(unique(data[[V2]]))<14){
          V2_character <- as.character(data[[V2]])
          tbl<-table(data[[V1]],V2_character)
          chi2 <- chisq.test(tbl, correct=F)
          r<-dim(tbl)[1]
          k<-dim(tbl)[2]
          c<-min(r-1,k-1)
          CrammerValue<-sqrt(chi2$statistic / (sum(tbl)*c))
          df<-data.frame(V1=V1,V2=V2,isCharacter1=isCharacter1,isCharacter2=isCharacter2,Correlation=CrammerValue)
        }else{
          V2_bin <- getBins(data[[V2]],bins = 10)
          tbl<-table(data[[V1]],V2_bin)
          chi2 <- chisq.test(tbl, correct=F)
          r<-dim(tbl)[1]
          k<-dim(tbl)[2]
          c<-min(r-1,k-1)
          CrammerValue<-sqrt(chi2$statistic / (sum(tbl)*c))
          df<-data.frame(V1=V1,V2=V2,isCharacter1=isCharacter1,isCharacter2=isCharacter2,Correlation=CrammerValue)
        }
      }else {
        Correlation<-cor(data[[V1]],data[[V2]],method = "spearman", use = "complete.obs")
        df<-data.frame(V1=V1,V2=V2,isCharacter1=isCharacter1,isCharacter2=isCharacter2,Correlation=abs(Correlation))
      }
      Correlation_df<-rbind(Correlation_df,df)
    }
  }
  return(Correlation_df)
}
