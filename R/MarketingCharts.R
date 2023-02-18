#' Marketing Gains Charts
#'
#' @param pred a vector of the probability of the positive class (response).
#' @param test a vector of the real observation.
#' @param type a string of CumulativeGain","Gain","Lift"
#' @param NBin a number of bins
#'
#' @return a data.frame of gains charts
#' @export
#'

MarketingCharts=function(pred,test,type,NBin=10){
  type0=c("CumulativeGain","Gain","Lift")
  ## Define some messages if they are NULL
  messages <- c('ERROR: The length of pred and test are different',
                'ERROR: the type is not included in this function')
  # The input arguments are not of the same length, return pred and quit
  if(length(pred) != length(test)) {warning(messages[1]); return(pred)}
  zerovec  <- rep(length(pred), 0.0)
  # the type is not included in this function, just return a zero vector and quit
  if(!(type %in% type0)) {warning(messages[2]); return(zerovec)}

  score= vector(length = length(pred))          # create a vector which have length of pred.
  score[order(pred)]=1:length(pred)
  TestScore=cbind(test,score)
  TestScoreSort=TestScore[rev(order(score)),]    # sort TestScore by column score from largest to smallest.
  bin=round(length(pred)/NBin)
  CumulativeResponseRate=NULL
  CResponsePercent=NULL
  CustomerContacted=NULL
  INumberResponse=NULL
  INumberofMailed=NULL
  CResponseNumber=NULL
  IRR=NULL
  CRR=NULL
  lift=NULL
  Tile=NULL
  N_rows<-length(pred)
  customerN=round(stats::quantile(0:N_rows,probs = seq(0,1,1/NBin)))
  total1=summary(test)[2]
  for(i in 1:length(customerN) ){
    if (customerN[i]==0){
      INumberofMailed[i]=0
      INumberResponse[i]=0
      CResponsePercent[i]=0
      CResponseNumber[i]=0
      CumulativeResponseRate[i]=0
      CustomerContacted[i]=0
      IRR[i]=0
      CRR[i]=0
      lift[i]=0
      Tile[i]=0
    }
    else{
      CResponseNumber[i]=sum(TestScoreSort[1:customerN[i],1]==2)
      INumberofMailed[i]=customerN[i]-customerN[i-1]
      INumberResponse[i]<-CResponseNumber[i]-CResponseNumber[i-1]
      IRR[i]<-INumberResponse[i]/INumberofMailed[i]
      CResponsePercent[i]=sum(TestScoreSort[1:customerN[i],1]==2)/customerN[i]
      CumulativeResponseRate[i]=sum(TestScoreSort[1:customerN[i],1]==2)/total1
      CRR[i]=sum(TestScoreSort[1:customerN[i],1]==2)/customerN[i]
      CustomerContacted[i]=customerN[i]/length(test)
      lift[i]=CumulativeResponseRate[i]/CustomerContacted[i]       ####### This is Cumulative Lift
      #lift[i]=ResponsePercent[i]
      Tile[i]=i-1
    }

  }
  if(type=="CumulativeGain"){
    CumulativeGainData=data.frame(x=CustomerContacted,y=CumulativeResponseRate)
    return(CumulativeGainData)
  }
  if(type=="Gain"){
    GainData=data.frame(x=CustomerContacted[-1],y=CRR[-1])
    return(GainData)
  }
  if(type=="Lift"){
    LiftData=data.frame(INumberofMailed=INumberofMailed[-1],INumberofResponse=INumberResponse[-1],CPercentofMailed=CustomerContacted[-1],IRR=IRR[-1],CRR=CRR[-1],LiftoverRandom=lift[-1],Tile=Tile[-1])

    return(LiftData)
  }
  remove(score)
  remove(TestScore)
  remove(TestScoreSort)
  remove(bin)
  remove(CumulativeResponseRate)
  remove(CustomerContacted)
  remove(lift)
  remove(customerN)
  remove(total1)
}# end of function MarketingCharts
