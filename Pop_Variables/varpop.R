# Funciones y Directorios ####
library(ggplot2)
library(reshape)
library(ggpubr)
library(devtools)

devtools::source_url("https://github.com/ale-yanez/RFunctions/blob/master/read.admb.R?raw=TRUE")

#out1 <- read.admb("../LAM_nor2008")
out1 <- read.admb("../data/LAM_nor2008")
out2 <- read.admb("../data/LAM_nor2003")

std1 <- read.table('../data/LAM_nor2008.std', header=T, sep="", na="NA", fill=T)
std2 <- read.table('../data/LAM_nor2003.std', header=T, sep="", na="NA", fill=T)

# Para graficar ... ####
 yrs <- out1$YRS
# nyrs <- length(yrs)
# tallas <- seq(10,52,1)
# class(tallas)
# M <- 0.3
# Brms <- out1$BDoLP*0.4
# Frms <- out1$Fpbr[3]
# B0 <- out1$BDoLP
# 

 
# #predichos y estimados 
 Rec_est1      <- subset(std1,name=='Restim')$value
 Rec_est2      <- subset(std2,name=='Restim')$value
 desvRec1      <- subset(std1,name=='log_dev_Ro')$value
 desvRec2      <- subset(std2,name=='dev_log_Ro')$value
# BT_est1       <- subset(std1,name=='BT')$value
# BT_est2       <- subset(std2,name=='BT')$value
# BD_est1       <- subset(std1,name=='BD')$value
# BD_est2       <- subset(std2,name=='BD')$value
# F_est1        <- exp(subset(std1,name=='log_Fh')$value)
# F_est2        <- exp(subset(std2,name=='log_Fh')$value)
# #F_est1        <- out1$Fm_Fh[2,]
# #F_est2        <- out2$Fm_Fh[2,]
 
# # std 
 stdRec1       <- subset(std1,name=='Restim')$std
 stdRec2       <- subset(std2,name=='Restim')$std
 stddesvRec1   <- subset(std1,name=='log_dev_Ro')$std
 stddesvRec2   <- subset(std2,name=='dev_log_Ro')$std
# stdBT1        <- subset(std1,name=='BT')$std
# stdBT2        <- subset(std2,name=='BT')$std
# stdBD1        <- subset(std1,name=='BD')$std
# stdBD2        <- subset(std2,name=='BD')$std
# stdF1         <- subset(std1,name=='log_Fh')$std
# stdF2         <- subset(std2,name=='log_Fh')$std
 
# # Confidence Intervals
 rec1_lwr      <-Rec_est1-1.96*stdRec1
 rec1_upr      <-Rec_est1+1.96*stdRec1
 rec2_lwr      <-Rec_est2-1.96*stdRec2
 rec2_upr      <-Rec_est2+1.96*stdRec2
 desvrec1_lwr  <- desvRec1-1.96*stddesvRec1
 desvrec1_upr  <- desvRec1+1.96*stddesvRec1
 desvrec2_lwr  <- desvRec2-1.96*stddesvRec2
 desvrec2_upr  <- desvRec2+1.96*stddesvRec2
# BT1_lwr       <-BT_est1-1.96*stdBT1
# BT1_upr       <-BT_est1+1.96*stdBT1
# BT2_lwr       <-BT_est2-1.96*stdBT2
# BT2_upr       <-BT_est2+1.96*stdBT2
# BD1_lwr       <-BD_est1-1.96*stdBD1
# BD1_upr       <-BD_est1+1.96*stdBD1
# BD2_lwr       <-BD_est2-1.96*stdBD2
# BD2_upr       <-BD_est2+1.96*stdBD2
# F1_lwr        <-exp(log(F_est1)-1.96*stdF1)
# F1_upr        <-exp(log(F_est1)+1.96*stdF1)
# F2_lwr        <-exp(log(F_est2)-1.96*stdF2)
# F2_upr        <-exp(log(F_est2)+1.96*stdF2)


#Var Pop LAM MODEL ###

# Reclutamiento ####

p8 <- ggplot(data = NULL, aes(x = yrs)) + 
  geom_line(aes(y = Rec_est1, colour = 'actual', linetype = 'actual')) +
  geom_line(aes(y = c(Rec_est2,NA), colour = 'anterior', linetype = 'anterior')) +
  geom_ribbon(data=NULL, aes(ymin=rec1_lwr, ymax=rec1_upr), fill = 'grey37', alpha = 0.4) + 
  geom_ribbon(data=NULL, aes(ymin=c(rec2_lwr,NA), ymax=c(rec2_upr,NA)), fill = 'grey70', alpha = 0.4) + 
  scale_color_manual(name = '',
                     values = c('royalblue3', 'red1'),
                     limits = c('actual', 'anterior'),
                     breaks = c('actual', 'anterior')) +
  scale_linetype_manual(name = '',
                        values = c('solid', 'longdash'),
                        limits = c('actual', 'anterior'),
                        breaks = c('actual', 'anterior'))
p8 <- p8 + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=8)) +
  theme(legend.position = 'bottom') + ylab('Reclutas x 10^6') + xlab('Años') + scale_x_continuous(breaks=round(seq(min(yrs), 2020, by = 5),1))

p8

# Desvíos Reclutamiento ####

p9 <- ggplot(data = NULL, aes(x = yrs)) + 
  geom_line(aes(y = desvRec1, colour = 'actual', linetype = 'actual')) +
  geom_line(aes(y = c(desvRec2,NA), colour = 'anterior', linetype = 'anterior')) +
  geom_ribbon(data=NULL, aes(ymin=desvrec1_lwr, ymax=desvrec1_upr), fill = 'grey37', alpha = 0.4) + 
  geom_ribbon(data=NULL, aes(ymin=c(desvrec2_lwr,NA), ymax=c(desvrec2_upr,NA)),fill = 'grey70', alpha = 0.4) + 
  geom_line(aes(y = c(rep(0,36)), colour = '', linetype = '')) +
  scale_color_manual(name = '',
                     values = c('royalblue3', 'red1','black'),
                     limits = c('actual', 'anterior', ''),
                     breaks = c('actual', 'anterior', '')) +
  scale_linetype_manual(name = '',
                        values = c('solid', 'longdash','dotted'),
                        limits = c('actual', 'anterior', ''),
                        breaks = c('actual', 'anterior', ''))
p9 <- p9 + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=8)) +
  theme(legend.position = 'bottom') + ylab('Desvíos Reclutamientos') + xlab('Años') + 
  scale_x_continuous(breaks=round(seq(min(yrs), 2020, by = 5),1))

p9

plot_rec <- ggarrange(p8, p9, ncol = 1, nrow = 2, align = "v", common.legend = TRUE, legend = "bottom")

ggexport(plot_rec, filename = "VarPop1_Rec.pdf")

