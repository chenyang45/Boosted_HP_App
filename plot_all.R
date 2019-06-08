#===========================================================
# plot xts
# R version 3.5.1 (2018-07-02) -- "Feather Spray"
# Copyright (C) 2018 The R Foundation for Statistical Computing
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# By: Chen Yang (chen_yang@link.cuhk.edu.hk)
# Date: 2019-05-23
#===========================================================

Boostedplot <- function(rawdata, trend, p_history, plot_type){
  
  if(plot_type == "Plain"){
    
    message("under developing") 
    
  }
  
  if(plot_type == "Time Series"){
  
  leg <- length(rawdata)
  leg_p <- length(p_history)
  
  bx <- data.frame(trend = trend,raw_data=rawdata)
  bx <- as.xts(bx, Sys.Date() + 1:leg)
  
  plot(bx)

  #as.xts(bx_ADF$adf_p_hist, Sys.Date()+1:leg_p)
  lines(as.xts(p_history, Sys.Date()+1:leg_p), type="h", on=NA,col = "blue")


  # add legend 
  addLegend("topleft", on=1, 
          legend.names = c("trend","raw data", "p value histroy"), 
          lty=c(1, 1), lwd=c(2, 1),
          col=c("black", "red" , "blue"))


  }
  
  if(plot_type == "SVG"){
    
    message("under developing")
  }
  
  
  if(plot_type == "GGPlot"){
    
    message("under developing")
  }
  
  
  if(plot_type == "Dynamic"){
    
    message("under developing")
  }
  
}