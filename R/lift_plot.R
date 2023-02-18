#' Lift over Random Plot
#'
#' @param preds.MarketingCharts.df  nput data.frame with  Lift
#' @param Interval  a number for interval between breaks in y axis
#' @param color color for line
#' @param type a string if it is "single" then create lift plot for one model otherwise create lift plot for multiple models
#' @param color_legend legend for color
#' @param color_V vector of colors
#'
#' @importFrom dplyr mutate if_else
#' @import ggplot2
#' @importFrom grid grid.newpage grid.draw
#' @importFrom scales percent
#' @return lift plot
#' @export lift_plot
#'

lift_plot <- function(preds.MarketingCharts.df,Interval=0.25,color="#0066CC",type="single",color_legend=NULL,color_V=NULL){
  # require(dplyr)
  # require(ggplot2)
  # require(gridExtra)
  # require(grid)
  CPercentofMailed <- LiftoverRandom <- pos <- .id <- NULL

  xBreaks<-seq(0,1,0.1)
  displacement<-xBreaks[2]

  myYBreaks <- function(x){
    #Interval<-0.25
    YMax<-ceiling(max(x)/Interval)*Interval
    breaks<-seq(0,YMax,Interval)
    #breaks <- labeling::extended(0, x_Range[2], m = 8)
    names(breaks) <- attr(breaks,"labels")
    return(breaks)
  }

  PrimaryBreaks<-myYBreaks(preds.MarketingCharts.df$LiftoverRandom)

  Total_max<-max(preds.MarketingCharts.df$LiftoverRandom)
  Total_min<-min(preds.MarketingCharts.df$LiftoverRandom)
  PrimaryBin<-PrimaryBreaks[2]-PrimaryBreaks[1]
  PrimaryExtraBreak<-max(PrimaryBreaks)+PrimaryBin
  N_Ind<-max(which(PrimaryBreaks<=Total_min))
  LowerLimit<-dplyr::if_else(Total_min<1 ,PrimaryBreaks[N_Ind],1)
  UpperLimit<-dplyr::if_else(Total_max<max(PrimaryBreaks) ,max(PrimaryBreaks),PrimaryExtraBreak)
  #names(preds.MarketingCharts.df)[1]<-"product"
  preds.MarketingCharts.df<-preds.MarketingCharts.df %>%
    dplyr::mutate(pos=CPercentofMailed+displacement/2.2)      # for text position
  if(type=="single"){
    p<-ggplot2::ggplot()+
      ggplot2::theme_classic()+
      ggplot2::geom_line(data = preds.MarketingCharts.df, ggplot2::aes(x=CPercentofMailed,y=LiftoverRandom),size=2,color=color )+
      ggplot2::geom_point(data = preds.MarketingCharts.df, ggplot2::aes(x=CPercentofMailed,y=LiftoverRandom),size=5,color=color,shape=18)+
      ggplot2::geom_text(data = preds.MarketingCharts.df, ggplot2::aes(x=pos,y=LiftoverRandom,label=sprintf("%0.2f", round(LiftoverRandom, digits = 2))),size=5)+
      ggplot2::scale_x_continuous(breaks=xBreaks,expand = c(0.02, 0),labels = scales::percent)+  #
      ggplot2::scale_y_continuous(breaks=PrimaryBreaks,expand = c(0, 0),limits=c(LowerLimit,UpperLimit))+   #
      ggplot2::theme_classic()+
      ggplot2::labs(title="Lift over Random", x="Decile",y=NULL)+
      ggplot2::theme(panel.grid.major.y=ggplot2::element_line(color = "#808080",size=1),
            plot.title=ggplot2::element_text(size=16, hjust=0.5,face="bold",colour="black", margin = ggplot2::margin(t = 15,r = 0,b=25,l=0)),
            plot.margin = ggplot2::margin(t = 5.5, r = 30, b = 15.5, l = 10.5, unit = "pt"),
            axis.title=ggplot2::element_blank(),
            axis.text.y=ggplot2::element_text(size=14,margin = ggplot2::margin(r=10)),axis.text.x=ggplot2::element_text(size=14,margin = ggplot2::margin(t=10)))
    #return(p)
  }else{
    p<-ggplot2::ggplot()+
      ggplot2::theme_classic()+

      ggplot2::geom_line(data = preds.MarketingCharts.df, ggplot2::aes(x=CPercentofMailed,y=LiftoverRandom,colour=.id),size=2 )+
      ggplot2::geom_point(data = preds.MarketingCharts.df, ggplot2::aes(x=CPercentofMailed,y=LiftoverRandom,colour=.id),size=5,shape=18)+
      #ggplot2::geom_text(data = preds.MarketingCharts.df, ggplot2::aes(x=pos,y=LiftoverRandom,label=CPercentofMailed))+
      ggplot2::scale_color_manual(values=color_V) +
      ggplot2::scale_x_continuous(breaks=xBreaks,expand = c(0.02, 0),labels = scales::percent)+  #
      ggplot2::scale_y_continuous(breaks=PrimaryBreaks,expand = c(0, 0),limits=c(LowerLimit,UpperLimit))+   #breaks=yBreaks,limits=c(0,1),
      ggplot2::theme_classic()+
      ggplot2::labs(title="Lift over Random", x="Decile",y=NULL,col=color_legend)+
      ggplot2::theme(panel.grid=ggplot2::element_line(color = "#808080"),panel.grid.major.y=ggplot2::element_line(color = "#808080"),
            plot.title=ggplot2::element_text(size=16, hjust=0.5,face="bold",colour="black", margin = ggplot2::margin(t = 15,r = 0,b=25,l=0)),
            plot.margin = ggplot2::margin(t = 5.5, r = 5.5, b = 15.5, l = 10.5, unit = "pt"),
            axis.title=ggplot2::element_blank(),
            axis.text.y=ggplot2::element_text(size=12,margin = ggplot2::margin(r=10)),axis.text.x=ggplot2::element_text(size=12,margin = ggplot2::margin(t=10)))
  }

  ### make ggplot2::geom_text plot within the canvas's bounds
  gt <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(p))
  gt$layout$clip[gt$layout$name == "panel"] <- "off"
  grid::grid.newpage()
  grid::grid.draw(gt)
}# end of function lift_plot



#' Group Level Lift over Random Plot
#'
#' @param Data_LiftPlot input data.frame with  lift and group variable
#' @param Group_Variable variable name for  grouping
#' @param Interval  a number for interval between breaks in y axis
#' @param color color for line
#' @param color_legend legend for color
#' @param color_V vector of colors
#'
#' @importFrom dplyr mutate if_else
#' @import ggplot2
#' @importFrom grid grid.newpage grid.draw
#'
#' @return lift plot
#' @export lift_plot_x
#'

lift_plot_x <- function(Data_LiftPlot,Group_Variable,Interval=0.25,color="#0066CC",color_legend=NULL,color_V=NULL){
  # require(dplyr)
  # require(ggplot2)
  # require(gridExtra)
  # require(grid)
  CPercentofMailed <- LiftoverRandom <- NULL

  n_v<-which(names(Data_LiftPlot)==Group_Variable)
  names(Data_LiftPlot)[n_v]="Group_Variable"
  color_legend<-Group_Variable

  xBreaks<-seq(0,1,0.1)
  displacement<-xBreaks[2]

  myYBreaks <- function(x){
    #Interval<-0.25
    YMax<-ceiling(max(x)/Interval)*Interval
    breaks<-seq(0,YMax,Interval)
    #breaks <- labeling::extended(0, x_Range[2], m = 8)
    names(breaks) <- attr(breaks,"labels")
    return(breaks)
  }

  PrimaryBreaks<-myYBreaks(Data_LiftPlot$LiftoverRandom)

  Total_max<-max(Data_LiftPlot$LiftoverRandom)
  Total_min<-min(Data_LiftPlot$LiftoverRandom)
  PrimaryBin<-PrimaryBreaks[2]-PrimaryBreaks[1]
  PrimaryExtraBreak<-max(PrimaryBreaks)+PrimaryBin
  N_Ind<-max(which(PrimaryBreaks<=Total_min))
  LowerLimit<-dplyr::if_else(Total_min<1 ,PrimaryBreaks[N_Ind],1)
  UpperLimit<-dplyr::if_else(Total_max<max(PrimaryBreaks) ,max(PrimaryBreaks),PrimaryExtraBreak)
  #names(Data_LiftPlot)[1]<-"product"
  Data_LiftPlot<-Data_LiftPlot %>%
    dplyr::mutate(pos=CPercentofMailed+displacement/2.2)      # for text position

  p<-ggplot2::ggplot()+
    ggplot2::theme_classic()+
    ggplot2::geom_line(data = Data_LiftPlot, ggplot2::aes(x=CPercentofMailed,y=LiftoverRandom,colour=Group_Variable),size=2 )+
    ggplot2::geom_point(data = Data_LiftPlot, ggplot2::aes(x=CPercentofMailed,y=LiftoverRandom,colour=Group_Variable),size=5,shape=18)+
    #ggplot2::geom_text(data = Data_LiftPlot, ggplot2::aes(x=pos,y=LiftoverRandom,label=CPercentofMailed))+
    #ggplot2::scale_color_manual(values=color_V) +
    ggplot2::scale_x_continuous(breaks=xBreaks,expand = c(0.02, 0),labels = scales::percent)+  #
    ggplot2::scale_y_continuous(breaks=PrimaryBreaks,expand = c(0, 0),limits=c(LowerLimit,UpperLimit))+   #breaks=yBreaks,limits=c(0,1),
    ggplot2::theme_classic()+
    ggplot2::labs(title="Lift over Random", x="Decile",y=NULL,col=color_legend)+
    ggplot2::theme(panel.grid=ggplot2::element_line(color = "#808080"),panel.grid.major.y=ggplot2::element_line(color = "#808080"),
          plot.title=ggplot2::element_text(size=16, hjust=0.5,face="bold",colour="black", margin = ggplot2::margin(t = 15,r = 0,b=25,l=0)),
          plot.margin = ggplot2::margin(t = 5.5, r = 5.5, b = 15.5, l = 10.5, unit = "pt"),
          axis.title=ggplot2::element_blank(),
          axis.text.y=ggplot2::element_text(size=12,margin = ggplot2::margin(r=10)),axis.text.x=ggplot2::element_text(size=12,margin = ggplot2::margin(t=10)))

  ### make ggplot2::geom_text plot within the canvas's bounds
  gt <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(p))
  gt$layout$clip[gt$layout$name == "panel"] <- "off"
  grid::grid.newpage()
  grid::grid.draw(gt)
}# end of function lift_plot_x


