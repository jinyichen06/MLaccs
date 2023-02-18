#' Title
#'
#' @param preds.MarketingCharts_model  input data.frame with  IRR and CRR
#' @param adjust a number use to adjust line position
#' @param Interval a number for interval between breaks in y axis
#'
#' @importFrom dplyr if_else
#' @import ggplot2
#' @importFrom grid grid.newpage grid.draw
#' @importFrom scales percent
#' @return IRR and CRR plot
#' @export IRRCRR_plot
#'

IRRCRR_plot<-function(preds.MarketingCharts_model,adjust=1,Interval=0.01){
  # require(dplyr)
  # require(ggplot2)
  # require(gridExtra)
  # require(grid)
  # require(reshape2)

  CPercentofMailed <- Measurement <- Type <- NULL
  ### Reshape data to long format
  preds.MarketingCharts_model_long<-reshape2::melt(preds.MarketingCharts_model,id.vars = c("CPercentofMailed"),measure.vars = c("IRR","CRR"),variable.name = "Type",value.name = "Measurement")
  preds.MarketingCharts_model_long$Measurement<-preds.MarketingCharts_model_long$Measurement*adjust
  xBreaks<-seq(0,1,0.1)
  displacement<-xBreaks[2]

  myYBreaks <- function(x){
    #Interval<-0.02
    YMax<-ceiling(max(x)/Interval)*Interval
    breaks<-seq(0,YMax,Interval)
    #breaks <- labeling::extended(0, x_Range[2], m = 8)
    names(breaks) <- attr(breaks,"labels")
    breaks
  }

  PrimaryBreaks<-myYBreaks(preds.MarketingCharts_model_long$Measurement)

  Total_max<-max(preds.MarketingCharts_model_long$Measurement)
  PrimaryBin<-PrimaryBreaks[2]-PrimaryBreaks[1]
  PrimaryExtraBreak<-max(PrimaryBreaks)+PrimaryBin

  UpperLimit<-dplyr::if_else(Total_max<max(PrimaryBreaks) ,max(PrimaryBreaks),PrimaryExtraBreak)
  #names(preds.MarketingCharts.df)[1]<-"product"
  # preds.MarketingCharts.df<-preds.MarketingCharts.df %>%
  #   mutate(pos=CPercentofMailed+displacement/2.2)      # for text position

  p<- ggplot2::ggplot()+
    ggplot2::theme_classic()+
    ggplot2::geom_line(data = preds.MarketingCharts_model_long, ggplot2::aes(x=CPercentofMailed,y=Measurement,colour=Type),size=2 )+
    ggplot2::geom_point(data = preds.MarketingCharts_model_long, ggplot2::aes(x=CPercentofMailed,y=Measurement,colour=Type),size=5,shape=18)+
    #geom_text(data = preds.MarketingCharts_model_long, aes(x=pos,y=LiftoverRandom,label=CPercentofMailed))+
    ggplot2::scale_color_manual(values=c("blue","red")) +
    ggplot2::scale_x_continuous(breaks=xBreaks,expand = c(0.02, 0),labels = scales::percent)+
    ggplot2::scale_y_continuous(breaks=PrimaryBreaks,expand = c(0, 0),limits=c(0,UpperLimit),labels = scales::percent)+
    ggplot2::theme_classic()+
    ggplot2::labs(title="IRR and CRR Plot", x="Decile",y=NULL,col=NULL)+
    ggplot2::theme(panel.grid=ggplot2::element_line(color = "#808080"), panel.grid.major.y=ggplot2::element_line(color = "#808080"),
          plot.title=ggplot2::element_text(size=16, hjust=0.5,face="bold",colour="black", margin = ggplot2::margin(t = 15,r = 0,b=25,l=0)),
          plot.margin = ggplot2::margin(t = 5.5, r = 5.5, b = 15.5, l = 10.5, unit = "pt"),
          axis.title=ggplot2::element_blank(),
          axis.text.y=ggplot2::element_text(size=12,margin = ggplot2::margin(r=10)),axis.text.x=ggplot2::element_text(size=12,margin = ggplot2::margin(t=10)))

  ### make geom_text plot within the canvas's bounds
  gt <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(p))
  gt$layout$clip[gt$layout$name == "panel"] <- "off"
  grid::grid.newpage()
  grid::grid.draw(gt)
}# end of function IRRCRR.plot
