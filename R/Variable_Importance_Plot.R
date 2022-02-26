Variable_Importance_Plot<-function(Data_Plot,variable_name,importance_name,bar_fill="#4f81bd",subtitle=NULL,caption=NULL,
                                   source_name=NULL,source_colors=NULL,num_of_features){
  require(dplyr)
  require(ggplot2)
  require(gridExtra)
  require(grid)

  myBreaks <- function(x){
    x_Range<-range(x)
    breaks <- labeling::extended(0, x_Range[2], m = 8)
    names(breaks) <- attr(breaks,"labels")
    breaks
  }
  names(Data_Plot)[which(names(Data_Plot)==variable_name)] <- "Variable"
  names(Data_Plot)[which(names(Data_Plot)==importance_name)] <- "Importance"

  Data_Plot <- Data_Plot %>%
    arrange(desc(Importance))

  Data_Plot <-  Data_Plot[1:num_of_features,]

  PrimaryBreaks<-myBreaks(Data_Plot$Importance)
  Total_max<-max(Data_Plot$Importance)
  PrimaryBin<-PrimaryBreaks[2]-PrimaryBreaks[1]
  PrimaryExtraBreak<-max(PrimaryBreaks)+PrimaryBin

  UpperLimit<-PrimaryExtraBreak #if_else(Total_max<max(PrimaryBreaks) ,max(PrimaryBreaks),PrimaryExtraBreak)
  displacement<-PrimaryBin
  Data_Plot<-Data_Plot %>%
    mutate(pos=Importance+displacement/4)

  if(is.null(source_name)){
    ImportantPlot<-ggplot() +
      geom_bar(data=Data_Plot,mapping = aes(x=reorder(Variable,Importance), y=Importance),fill= bar_fill, stat = "identity",width=0.5) +
      scale_x_discrete(name = NULL) +
      scale_y_continuous(name = NULL, expand=c(0,0),labels = scales::percent_format(accuracy = 1),limits = c(0,UpperLimit),breaks=myBreaks)+
      #geom_text(data = Data_Plot, aes(x=reorder(Variable,Importance),y=pos,label=paste(round(Importance*100, digits = 1),"%"),size=5),show.legend=FALSE)+
      coord_flip()+
      theme_bw()+
      labs(title="Variable Importance", subtitle=subtitle, caption = caption,x=NULL,y=NULL)+
      theme(axis.line=element_line(color = "#7f7f7f"),panel.grid.major.y=element_blank(),panel.grid.minor.y=element_blank(),axis.ticks=element_blank(),panel.border=element_rect(color = "#7f7f7f",fill=NA,inherit.blank = TRUE),
            plot.title=element_text(family = "Calibri",size=18, hjust=0,colour="black", margin = ggplot2::margin(t = 15,r = 0,b=15,l=0)),
            plot.margin = ggplot2::margin(t = 5.5, r = 10, b = 15.5, l = 10.5, unit = "pt"),
            plot.background = element_rect(colour = "#d3d3d3",fill=NULL,size = 1),
            axis.title=element_blank(),
            axis.text.y=element_text(size=12,colour="black"),axis.text.x=element_text(size=12,colour="black", angle=0,margin = ggplot2::margin(t=10)),
            legend.title=element_blank(),legend.text=element_text(family = "Calibri",size=12)
      )
  }else{
    names(Data_Plot)[which(names(Data_Plot)==source_name)] <- "Source"

    ImportantPlot<-ggplot() +
      geom_bar(data=Data_Plot,mapping = aes(x=reorder(Variable,Importance), y=Importance, fill=Source), stat = "identity",width=0.5) + #fill= "#4f81bd",
      scale_fill_manual(values=source_colors)+
      scale_x_discrete(name = NULL) +
      scale_y_continuous(name = NULL, expand=c(0,0),labels = scales::percent_format(accuracy = 1),limits = c(0,UpperLimit),breaks=myBreaks)+
      #geom_text(data = Data_Plot, aes(x=reorder(Variable,Importance),y=pos,label=paste(round(Importance*100, digits = 1),"%"),size=5),show.legend=FALSE)+
      coord_flip()+
      theme_bw()+
      labs(title="Variable Importance", subtitle=subtitle,caption = caption, x=NULL,y=NULL)+
      theme(axis.line=element_line(color = "#7f7f7f"),panel.grid.major.y=element_blank(),panel.grid.minor.y=element_blank(),axis.ticks=element_blank(),panel.border=element_rect(color = "#7f7f7f",fill=NA,inherit.blank = TRUE),
            plot.title=element_text(family = "Calibri",size=18, hjust=0,colour="black", margin = ggplot2::margin(t = 15,r = 0,b=15,l=0)),
            plot.margin = ggplot2::margin(t = 5.5, r = 10, b = 15.5, l = 10.5, unit = "pt"),
            plot.background = element_rect(colour = "#d3d3d3",fill=NULL,size = 1),
            axis.title=element_blank(),
            axis.text.y=element_text(size=12,colour="black"),axis.text.x=element_text(size=12,colour="black", angle=0,margin = ggplot2::margin(t=10)),
            legend.title=element_blank(),legend.text=element_text(family = "Calibri",size=12)
      )
  }

  ### make geom_text plot within the canvas's bounds
  gt <- ggplot_gtable(ggplot_build(ImportantPlot))
  gt$layout$clip[gt$layout$name == "panel"] <- "off"
  grid.newpage()
  grid::grid.draw(gt)
  return(ImportantPlot)
}# end of function Variable_Importance_Plot

