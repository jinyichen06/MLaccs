IRRCRR_plot<-function(preds.MarketingCharts_model,adjust=1,Interval=0.01){
  require(dplyr)
  require(ggplot2)
  require(gridExtra)
  require(grid)
  require(reshape2)
  ### Reshape data to long format
  preds.MarketingCharts_model_long<-melt(preds.MarketingCharts_model,id.vars = c("CPercentofMailed"),measure.vars = c("IRR","CRR"),variable.name = "Type",value.name = "Measuremen")
  preds.MarketingCharts_model_long$Measuremen<-preds.MarketingCharts_model_long$Measuremen*adjust
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

  PrimaryBreaks<-myYBreaks(preds.MarketingCharts_model_long$Measuremen)

  Total_max<-max(preds.MarketingCharts_model_long$Measuremen)
  PrimaryBin<-PrimaryBreaks[2]-PrimaryBreaks[1]
  PrimaryExtraBreak<-max(PrimaryBreaks)+PrimaryBin

  UpperLimit<-if_else(Total_max<max(PrimaryBreaks) ,max(PrimaryBreaks),PrimaryExtraBreak)
  #names(preds.MarketingCharts.df)[1]<-"product"
  # preds.MarketingCharts.df<-preds.MarketingCharts.df %>%
  #   mutate(pos=CPercentofMailed+displacement/2.2)      # for text position

  p<-ggplot()+
    theme_classic()+
    geom_line(data = preds.MarketingCharts_model_long, aes(x=CPercentofMailed,y=Measuremen,colour=Type),size=2 )+
    geom_point(data = preds.MarketingCharts_model_long, aes(x=CPercentofMailed,y=Measuremen,colour=Type),size=5,shape=18)+
    #geom_text(data = preds.MarketingCharts_model_long, aes(x=pos,y=LiftoverRandom,label=CPercentofMailed))+
    scale_color_manual(values=c("blue","red")) +
    scale_x_continuous(breaks=xBreaks,expand = c(0.02, 0),labels = scales::percent)+
    scale_y_continuous(breaks=PrimaryBreaks,expand = c(0, 0),limits=c(0,UpperLimit),labels = scales::percent)+
    theme_classic()+
    labs(title="IRR and CRR Plot", x="Decile",y=NULL,col=NULL)+
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
}# end of function IRRCRR.plot
