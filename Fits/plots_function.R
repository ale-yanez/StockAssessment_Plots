plots <- function(x){

  # Funciones y Directorios ####
  library(ggplot2)
  library(reshape)
  library(ggpubr)
  library(devtools)
  
  devtools::source_url("https://github.com/ale-yanez/RFunctions/blob/master/read.admb.R?raw=TRUE")
  
  #file<-paste(x,'.par', sep='')
  out1 <- read.admb("../data/paste(x,'.par', sep='')")
  
  return(out1$YRS)
}

x <- 'LAM_nor2008'

plots(x)
