LiftOverRandom_plot<-function(preds.MarketingCharts.df,Interval=0.25,color="#0066CC",type="single",color_legend=NULL,color_V=NULL){
  require(dplyr)
  require(ggplot2)
  require(gridExtra)
  require(grid)
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
  LowerLimit<-if_else(Total_min<1 ,PrimaryBreaks[N_Ind],1)
  UpperLimit<-if_else(Total_max<max(PrimaryBreaks) ,max(PrimaryBreaks),PrimaryExtraBreak)
  #names(preds.MarketingCharts.df)[1]<-"product"
  preds.MarketingCharts.df<-preds.MarketingCharts.df %>%
    mutate(pos=CPercentofMailed+displacement/2.2)      # for text position
  if(type=="single"){
    p<-ggplot()+
      theme_classic()+
      geom_line(data = preds.MarketingCharts.df, aes(x=CPercentofMailed,y=LiftoverRandom),size=2,color=color )+
      geom_point(data = preds.MarketingCharts.df, aes(x=CPercentofMailed,y=LiftoverRandom),size=5,color=color,shape=18)+
      geom_text(data = preds.MarketingCharts.df, aes(x=pos,y=LiftoverRandom,label=sprintf("%0.2f", round(LiftoverRandom, digits = 2))),size=5)+
      scale_x_continuous(breaks=xBreaks,expand = c(0.02, 0),labels = scales::percent)+  #
      scale_y_continuous(breaks=PrimaryBreaks,expand = c(0, 0),limits=c(LowerLimit,UpperLimit))+   #
      theme_classic()+
      labs(title="Lift over Random", x="Decile",y=NULL)+
      theme(panel.grid.major.y=element_line(color = "#808080",size=1),
            plot.title=element_text(size=16, hjust=0.5,face="bold",colour="black", margin = ggplot2::margin(t = 15,r = 0,b=25,l=0)),
            plot.margin = ggplot2::margin(t = 5.5, r = 30, b = 15.5, l = 10.5, unit = "pt"),
            axis.title=element_blank(),
            axis.text.y=element_text(size=14,margin = ggplot2::margin(r=10)),axis.text.x=element_text(size=14,margin = ggplot2::margin(t=10)))
    #return(p)
  }else{
    p<-ggplot()+
      theme_classic()+

      geom_line(data = preds.MarketingCharts.df, aes(x=CPercentofMailed,y=LiftoverRandom,colour=.id),size=2 )+
      geom_point(data = preds.MarketingCharts.df, aes(x=CPercentofMailed,y=LiftoverRandom,colour=.id),size=5,shape=18)+
      #geom_text(data = preds.MarketingCharts.df, aes(x=pos,y=LiftoverRandom,label=CPercentofMailed))+
      scale_color_manual(values=color_V) +
      scale_x_continuous(breaks=xBreaks,expand = c(0.02, 0),labels = scales::percent)+  #
      scale_y_continuous(breaks=PrimaryBreaks,expand = c(0, 0),limits=c(LowerLimit,UpperLimit))+   #breaks=yBreaks,limits=c(0,1),
      theme_classic()+
      labs(title="Lift over Random", x="Decile",y=NULL,col=color_legend)+
      theme(panel.grid=element_line(color = "#808080"),panel.grid.major.y=element_line(color = "#808080"),
            plot.title=element_text(size=16, hjust=0.5,face="bold",colour="black", margin = ggplot2::margin(t = 15,r = 0,b=25,l=0)),
            plot.margin = ggplot2::margin(t = 5.5, r = 5.5, b = 15.5, l = 10.5, unit = "pt"),
            axis.title=element_blank(),
            axis.text.y=element_text(size=12,margin = ggplot2::margin(r=10)),axis.text.x=element_text(size=12,margin = ggplot2::margin(t=10)))
  }

  ### make geom_text plot within the canvas's bounds
  gt <- ggplot_gtable(ggplot_build(p))
  gt$layout$clip[gt$layout$name == "panel"] <- "off"
  grid.newpage()
  grid::grid.draw(gt)
}# end of function LiftOverRandom.plot
