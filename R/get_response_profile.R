get_response_profile <- function(data,variable_name,y_name, y2_name,title){
  options(scipen = 100)
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
      geom_text(aes(x=variable,y=y2*coeff,label = sprintf("%1.1f%%", y2*100), hjust= -0.1,vjust = -0.1)) +
      scale_y_continuous(name=y_name, breaks=NULL, label=NULL ,sec.axis = sec_axis(trans=~./coeff,name = y2_name, breaks=NULL))+
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
      geom_text(aes(x=variable,y=y2*coeff,label = sprintf("%1.1f%%", y2*100), hjust= -0.1,vjust = -0.1)) +
      scale_y_continuous(name=y_name, breaks=NULL, label=NULL ,sec.axis = sec_axis(trans=~./coeff,name = y2_name, breaks=NULL))+
      scale_x_discrete(name=variable_name)+
      labs(title = title)+
      custom_theme+
      theme(axis.text.x=element_text(angle=45,margin = ggplot2::margin(t=10)))
  }

  return(bar_line_plot)
}
