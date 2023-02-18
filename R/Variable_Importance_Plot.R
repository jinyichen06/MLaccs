
#' Create a variable importance plot
#'
#' @param Data_Plot input data.frame for plot (it includes variable and it’s importance ).
#' @param variable_name the column name in the input data.frame that includes the variables of interest.
#' @param importance_name The column name in the input data.frame that includes measures of variable importance.
#' @param bar_fill the color for bar chart.
#' @param subtitle The subtitle of the plot.
#' @param caption The caption of the plot.
#' @param source_name the column name in the input data.frame that includes source of the variables of analysis.
#' @param source_colors the vector of colors for the different source. (The source_name must specified)
#' @param num_of_features the number of variables display in the plot.
#' @importFrom dplyr mutate arrange desc
#' @import ggplot2
#' @importFrom grid grid.newpage grid.draw
#' @importFrom scales percent_format
#' @importFrom labeling extended
#' @return variable importance plot
#' @export variable_importance_plot
#'




variable_importance_plot<-function(Data_Plot,variable_name,importance_name,bar_fill="#4f81bd",subtitle=NULL,caption=NULL,
                                   source_name=NULL,source_colors=NULL,num_of_features=10){

  ### load extra font in R
  # install.packages("extrafont")
  # library(extrafont)
  # font_import()
  #loadfonts(device = 'win')
  Variable <- Importance <- Source <- NULL

  myBreaks <- function(x){
    x_Range<-range(x)
    breaks <- labeling::extended(0, x_Range[2], m = 8)
    names(breaks) <- attr(breaks,"labels")
    breaks
  }
  names(Data_Plot)[which(names(Data_Plot)==variable_name)] <- "Variable"
  names(Data_Plot)[which(names(Data_Plot)==importance_name)] <- "Importance"

  Data_Plot <- Data_Plot %>%
    dplyr::arrange(dplyr::desc(Importance))

  Data_Plot <-  Data_Plot[1:num_of_features,]

  PrimaryBreaks<-myBreaks(Data_Plot$Importance)
  Total_max<-max(Data_Plot$Importance)
  PrimaryBin<-PrimaryBreaks[2]-PrimaryBreaks[1]
  PrimaryExtraBreak<-max(PrimaryBreaks)+PrimaryBin

  UpperLimit<-PrimaryExtraBreak #if_else(Total_max<max(PrimaryBreaks) ,max(PrimaryBreaks),PrimaryExtraBreak)
  displacement<-PrimaryBin
  Data_Plot<-Data_Plot %>%
    dplyr::mutate(pos=Importance+displacement/4)

  if(is.null(source_name)){
    ImportantPlot<-ggplot2::ggplot() +
      ggplot2::geom_bar(data=Data_Plot,mapping = ggplot2::aes(x=reorder(Variable,Importance), y=Importance),fill= bar_fill, stat = "identity",width=0.5) +
      ggplot2::scale_x_discrete(name = NULL) +
      ggplot2::scale_y_continuous(name = NULL, expand=c(0,0),labels = scales::percent_format(accuracy = 1),limits = c(0,UpperLimit),breaks=myBreaks)+
      #geom_text(data = Data_Plot, ggplot2::aes(x=reorder(Variable,Importance),y=pos,label=paste(round(Importance*100, digits = 1),"%"),size=5),show.legend=FALSE)+
      ggplot2::coord_flip()+
      ggplot2::theme_bw()+
      ggplot2::labs(title="Variable Importance", subtitle=subtitle, caption = caption,x=NULL,y=NULL)+
      ggplot2::theme(axis.line=ggplot2::element_line(color = "#7f7f7f"),panel.grid.major.y=ggplot2::element_blank(),panel.grid.minor.y=ggplot2::element_blank(),axis.ticks=ggplot2::element_blank(),panel.border=ggplot2::element_rect(color = "#7f7f7f",fill=NA,inherit.blank = TRUE),
            plot.title=ggplot2::element_text(family = "Calibri",size=18, hjust=0,colour="black", margin = ggplot2::margin(t = 15,r = 0,b=15,l=0)),
            plot.margin = ggplot2::margin(t = 5.5, r = 10, b = 15.5, l = 10.5, unit = "pt"),
            plot.background = ggplot2::element_rect(colour = "#d3d3d3",fill=NULL,size = 1),
            axis.title=ggplot2::element_blank(),
            axis.text.y=ggplot2::element_text(size=12,colour="black"),axis.text.x=ggplot2::element_text(size=12,colour="black", angle=0,margin = ggplot2::margin(t=10)),
            legend.title=ggplot2::element_blank(),legend.text=ggplot2::element_text(family = "Calibri",size=12)
      )
  }else{
    names(Data_Plot)[which(names(Data_Plot)==source_name)] <- "Source"

    ImportantPlot<-ggplot2::ggplot() +
      ggplot2::geom_bar(data=Data_Plot,mapping = ggplot2::aes(x=reorder(Variable,Importance), y=Importance, fill=Source), stat = "identity",width=0.5) + #fill= "#4f81bd",
      ggplot2::scale_fill_manual(values=source_colors)+
      ggplot2::scale_x_discrete(name = NULL) +
      ggplot2::scale_y_continuous(name = NULL, expand=c(0,0),labels = scales::percent_format(accuracy = 1),limits = c(0,UpperLimit),breaks=myBreaks)+
      #geom_text(data = Data_Plot, ggplot2::aes(x=reorder(Variable,Importance),y=pos,label=paste(round(Importance*100, digits = 1),"%"),size=5),show.legend=FALSE)+
      ggplot2::coord_flip()+
      ggplot2::theme_bw()+
      ggplot2::labs(title="Variable Importance", subtitle=subtitle,caption = caption, x=NULL,y=NULL)+
      ggplot2::theme(axis.line=ggplot2::element_line(color = "#7f7f7f"),panel.grid.major.y=ggplot2::element_blank(),panel.grid.minor.y=ggplot2::element_blank(),axis.ticks=ggplot2::element_blank(),panel.border=ggplot2::element_rect(color = "#7f7f7f",fill=NA,inherit.blank = TRUE),
            plot.title=ggplot2::element_text(family = "Calibri",size=18, hjust=0,colour="black", margin = ggplot2::margin(t = 15,r = 0,b=15,l=0)),
            plot.margin = ggplot2::margin(t = 5.5, r = 10, b = 15.5, l = 10.5, unit = "pt"),
            plot.background = ggplot2::element_rect(colour = "#d3d3d3",fill=NULL,size = 1),
            axis.title=ggplot2::element_blank(),
            axis.text.y=ggplot2::element_text(size=12,colour="black"),axis.text.x=ggplot2::element_text(size=12,colour="black", angle=0,margin = ggplot2::margin(t=10)),
            legend.title=ggplot2::element_blank(),legend.text=ggplot2::element_text(family = "Calibri",size=12)
      )
  }

  ### make geom_text plot within the canvas's bounds
  gt <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(ImportantPlot))
  gt$layout$clip[gt$layout$name == "panel"] <- "off"
  grid::grid.newpage()
  grid::grid.draw(gt)
  return(ImportantPlot)
}# end of function Variable_Importance_Plot

