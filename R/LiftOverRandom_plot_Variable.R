LiftOverRandom_plot_Variable<-function(Data_LiftPlot,Group_Variable,Interval=0.25,color="#0066CC",color_legend=NULL,color_V=NULL){
  require(dplyr)
  require(ggplot2)
  require(gridExtra)
  require(grid)

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
  LowerLimit<-if_else(Total_min<1 ,PrimaryBreaks[N_Ind],1)
  UpperLimit<-if_else(Total_max<max(PrimaryBreaks) ,max(PrimaryBreaks),PrimaryExtraBreak)
  #names(Data_LiftPlot)[1]<-"product"
  Data_LiftPlot<-Data_LiftPlot %>%
    mutate(pos=CPercentofMailed+displacement/2.2)      # for text position

  p<-ggplot()+
    theme_classic()+
    geom_line(data = Data_LiftPlot, aes(x=CPercentofMailed,y=LiftoverRandom,colour=Group_Variable),size=2 )+
    geom_point(data = Data_LiftPlot, aes(x=CPercentofMailed,y=LiftoverRandom,colour=Group_Variable),size=5,shape=18)+
    #geom_text(data = Data_LiftPlot, aes(x=pos,y=LiftoverRandom,label=CPercentofMailed))+
    #scale_color_manual(values=color_V) +
    scale_x_continuous(breaks=xBreaks,expand = c(0.02, 0),labels = scales::percent)+  #
    scale_y_continuous(breaks=PrimaryBreaks,expand = c(0, 0),limits=c(LowerLimit,UpperLimit))+   #breaks=yBreaks,limits=c(0,1),
    theme_classic()+
    labs(title="Lift over Random", x="Decile",y=NULL,col=color_legend)+
    theme(panel.grid=element_line(color = "#808080"),panel.grid.major.y=element_line(color = "#808080"),
          plot.title=element_text(size=16, hjust=0.5,face="bold",colour="black", margin = ggplot2::margin(t = 15,r = 0,b=25,l=0)),
          plot.margin = ggplot2::margin(t = 5.5, r = 5.5, b = 15.5, l = 10.5, unit = "pt"),
          axis.title=element_blank(),
          axis.text.y=element_text(size=12,margin = ggplot2::margin(r=10)),axis.text.x=element_text(size=12,margin = ggplot2::margin(t=10)))

  ### make geom_text plot within the canvas's bounds
  gt <- ggplot_gtable(ggplot_build(p))
  gt$layout$clip[gt$layout$name == "panel"] <- "off"
  grid.newpage()
  grid::grid.draw(gt)
}# end of function LiftOverRandom.plot_Variable


