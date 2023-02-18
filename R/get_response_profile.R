#' Create Response Profile Chart for the Selected Variable (Predictor)
#'
#' @param data input data.frame including decile and responder information
#' @param variable_name variable name for response profile
#' @param y_name primary y axis title
#' @param y2_name secondary y axis title
#' @param title chart title
#'
#' @import ggplot2
#' @importFrom stats reorder
#' @return response profile plot
#' @export get_response_profile
#'

get_response_profile <- function(data,variable_name,y_name, y2_name,title){
  options(scipen = 100)
  variable <- y <- y2 <- NULL
  ### ggplot2 custom theme
  custom_theme <- theme(plot.title = element_text(hjust = 0.5),
                        plot.subtitle = element_text(hjust = 0.5),
                        plot.background = element_rect(colour = "#d3d3d3",fill=NULL,size = 1),
                        #panel.background = element_rect(size=rel(1.8)), #margin=c(t = 1, r = 0, b = 1, l = 0, unit = "pt")
                        #panel.border= element_rect(colour = "#d3d3d3",fill=NULL),
                        #panel.spacing = unit(1, "lines"),
                        legend.title = element_text(),
                        legend.text = element_text(size = (12)),  #,margin=margin(2, 2, 2, 2)
                        axis.title = element_text(size = (12)),
                        axis.text = element_text(size = (10)),
                        strip.text = element_text(size = (12)))


  names(data)[which(names(data)==variable_name)] <- "variable"
  names(data)[which(names(data)==y_name)] <- "y"
  names(data)[which(names(data)==y2_name)] <- "y2"
  #Data_Plot <- data
  # plot
  y2_breaks = pretty(data$y2, n = 10)
  y_breaks = pretty(data$y, n = 10)
  #x_breaks = pretty(model_performance_decile$variable, n = 10)
  coeff <- max(y_breaks)/max(y2_breaks)
  nchar_max <- max(nchar(as.character(data$variable),keepNA = FALSE))
  if(nchar_max <=5){
    bar_line_plot <- ggplot(data = data)+
      geom_bar(mapping = aes(x=variable,y=y),stat="identity",fill = "#8bbc9f",color="#555555",size =0.2)+
      geom_line(mapping = aes(x=variable,y=y2*coeff, group=1),stat="identity",color="orange",size=1)+
      geom_point(mapping = aes(x=variable,y=y2*coeff, group=1),stat="identity",color="darkorange",size=1) +
      geom_text(mapping = aes(x=variable,y=y, label = (formatC(y, format= "d",big.mark = ","))), vjust = 1.5)+
      geom_text(aes(x=variable,y=y2*coeff,label = sprintf("%1.2f%%", y2*100), hjust= -0.1,vjust = -0.1)) +
      scale_y_continuous(name=y_name, breaks=NULL, labels=NULL ,sec.axis = sec_axis(trans=~./coeff,name = y2_name, breaks=NULL))+
      scale_x_discrete(name=variable_name)+
      labs(title = title)+
      #theme(plot.title = element_text(hjust = 0.5))
      custom_theme
  }else{
    bar_line_plot <- ggplot(data = data)+
      geom_bar(mapping = aes(x=variable,y=y),stat="identity",fill = "#8bbc9f",color="#555555",size =0.2)+
      geom_line(mapping = aes(x=variable,y=y2*coeff, group=1),stat="identity",color="orange",size=1)+
      geom_point(mapping = aes(x=variable,y=y2*coeff, group=1),stat="identity",color="darkorange",size=1) +
      geom_text(mapping = aes(x=variable,y=y, label = (formatC(y, format= "d",big.mark = ","))), vjust = 1.5)+
      geom_text(aes(x=variable,y=y2*coeff,label = sprintf("%1.2f%%", y2*100), hjust= -0.1,vjust = -0.1)) +
      scale_y_continuous(name=y_name, breaks=NULL, labels=NULL ,sec.axis = sec_axis(trans=~./coeff,name = y2_name, breaks=NULL))+
      scale_x_discrete(name=variable_name)+
      labs(title = title)+
      custom_theme+
      theme(axis.text.x=element_text(angle=45,margin = ggplot2::margin(t=10)))
  }

  return(bar_line_plot)
}
