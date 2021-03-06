# Funciones y Directorios ####
library(rstudioapi)
library(ggplot2)
library(reshape)
library(ggpubr)
library(devtools)

# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))

devtools::source_url("https://github.com/ale-yanez/RFunctions/blob/master/read.admb.R?raw=TRUE")

#out1 <- read.admb("../LAM_nor2008")
out1 <- read.admb("../data/LAM_nor2008")
out2 <- read.admb("../data/LAM_nor2003")

std1 <- read.table('../data/LAM_nor2008.std', header=T, sep="", na="NA", fill=T)
std2 <- read.table('../data/LAM_nor2003.std', header=T, sep="", na="NA", fill=T)

# Para graficar ... ####
 yrs <- out1$YRS
 M <- 0.3
 Brms <- out1$BDoLP*0.4
 Frms <- out1$Fpbr[3]

# #predichos y estimados 
 Rec_est1      <- subset(std1,name=='Restim')$value
 Rec_est2      <- subset(std2,name=='Restim')$value
 desvRec1      <- subset(std1,name=='log_dev_Ro')$value
 desvRec2      <- subset(std2,name=='dev_log_Ro')$value
 BT_est1       <- subset(std1,name=='BT')$value
 BT_est2       <- subset(std2,name=='BT')$value
 BD_est1       <- subset(std1,name=='BD')$value
 BD_est2       <- subset(std2,name=='BD')$value
 F_est1        <- exp(subset(std1,name=='log_Fh')$value)
 F_est2        <- exp(subset(std2,name=='log_Fh')$value)
 F_est1_b      <- exp(subset(std1,name=='log_Fh')$value) + exp(subset(std1,name=='log_Fm')$value)
 F_est2_b      <- exp(subset(std2,name=='log_Fh')$value) + exp(subset(std2,name=='log_Fm')$value)
 
# # std 
 stdRec1       <- subset(std1,name=='Restim')$std
 stdRec2       <- subset(std2,name=='Restim')$std
 stddesvRec1   <- subset(std1,name=='log_dev_Ro')$std
 stddesvRec2   <- subset(std2,name=='dev_log_Ro')$std
 stdBT1        <- subset(std1,name=='BT')$std
 stdBT2        <- subset(std2,name=='BT')$std
 stdBD1        <- subset(std1,name=='BD')$std
 stdBD2        <- subset(std2,name=='BD')$std
 stdF1         <- subset(std1,name=='log_Fh')$std
 stdF2         <- subset(std2,name=='log_Fh')$std
 stdF1_b       <- subset(std1,name=='log_Fh')$std + subset(std1,name=='log_Fm')$std
 stdF2_b       <- subset(std2,name=='log_Fh')$std + subset(std2,name=='log_Fm')$std
 
# # Confidence Intervals
 rec1_lwr      <-Rec_est1-1.96*stdRec1
 rec1_upr      <-Rec_est1+1.96*stdRec1
 rec2_lwr      <-Rec_est2-1.96*stdRec2
 rec2_upr      <-Rec_est2+1.96*stdRec2
 desvrec1_lwr  <- desvRec1-1.96*stddesvRec1
 desvrec1_upr  <- desvRec1+1.96*stddesvRec1
 desvrec2_lwr  <- desvRec2-1.96*stddesvRec2
 desvrec2_upr  <- desvRec2+1.96*stddesvRec2
 BT1_lwr       <-BT_est1-1.96*stdBT1
 BT1_upr       <-BT_est1+1.96*stdBT1
 BT2_lwr       <-BT_est2-1.96*stdBT2
 BT2_upr       <-BT_est2+1.96*stdBT2
 BD1_lwr       <-BD_est1-1.96*stdBD1
 BD1_upr       <-BD_est1+1.96*stdBD1
 BD2_lwr       <-BD_est2-1.96*stdBD2
 BD2_upr       <-BD_est2+1.96*stdBD2
 F1_lwr        <-exp(log(F_est1)-1.96*stdF1)
 F1_upr        <-exp(log(F_est1)+1.96*stdF1)
 F2_lwr        <-exp(log(F_est2)-1.96*stdF2)
 F2_upr        <-exp(log(F_est2)+1.96*stdF2)
 F1_lwr_b        <-exp(log(F_est1_b)-1.96*stdF1_b)
 F1_upr_b        <-exp(log(F_est1_b)+1.96*stdF1_b)
 F2_lwr_b        <-exp(log(F_est2_b)-1.96*stdF2_b)
 F2_upr_b        <-exp(log(F_est2_b)+1.96*stdF2_b)


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

ggexport(plot_rec, filename = "VarPop1_Rec.pdf", width=8, height=6.5, dpi=300)


# Biomasa Total ####

p10 <- ggplot(data = NULL, aes(x = yrs)) + 
  geom_line(aes(y = BT_est1, colour = 'actual', linetype = 'actual')) +
  geom_line(aes(y = c(BT_est2,NA), colour = 'anterior', linetype = 'anterior')) +
  geom_ribbon(data=NULL, aes(ymin=BT1_lwr, ymax=BT1_upr), fill = 'grey37', alpha = 0.4) + 
  geom_ribbon(data=NULL, aes(ymin=c(BT2_lwr,NA), ymax=c(BT2_upr,NA)),fill = 'grey70', alpha = 0.4) + 
  geom_line(aes(y = c(rep(1,36)), colour = 'Brms', linetype = '')) +
  scale_color_manual(name = '',
                     values = c('royalblue3', 'red1', 'chartreuse4'),
                     limits = c('actual', 'anterior', 'Brms'),
                     breaks = c('actual', 'anterior', 'Brms')) +
  scale_linetype_manual(name = '',
                        values = c('solid', 'longdash', 'twodash'),
                        limits = c('actual', 'anterior', 'Brms'),
                        breaks = c('actual', 'anterior', 'Brms'))
p10 <- p10 + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=8)) +
  theme(legend.position = 'bottom') + ylab('Biomasa Total (t)') + xlab('Años') + 
  scale_x_continuous(breaks=round(seq(min(yrs), 2020, by = 5),1))

p10


# Biomasa Desovante ####

p11 <- ggplot(data = NULL, aes(x = yrs)) + 
  geom_line(aes(y = BD_est1, colour = 'actual', linetype = 'actual')) +
  geom_line(aes(y = c(BD_est2,NA), colour = 'anterior', linetype = 'anterior')) +
  geom_ribbon(data=NULL, aes(ymin=BD1_lwr, ymax=BD1_upr), fill = 'grey37', alpha = 0.4) + 
  geom_ribbon(data=NULL, aes(ymin=c(BD2_lwr,NA), ymax=c(BD2_upr,NA)),fill = 'grey70', alpha = 0.4) + 
  geom_line(aes(y = c(rep(Brms,36)), colour = 'Brms', linetype = 'Brms')) +
  scale_color_manual(name = '',
                     values = c('royalblue3', 'red1', 'chartreuse4'),
                     limits = c('actual', 'anterior', 'Brms'),
                     breaks = c('actual', 'anterior', 'Brms')) +
  scale_linetype_manual(name = '',
                        values = c('solid', 'longdash', 'twodash'),
                        limits = c('actual', 'anterior', 'Brms'),
                        breaks = c('actual', 'anterior', 'Brms'))
p11 <- p11 + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=8)) +
  theme(legend.position = 'bottom') + ylab('Biomasa Desovante (t)') + xlab('Años') + 
  scale_x_continuous(breaks=round(seq(min(yrs), 2020, by = 5),1))

p11

plot_B <- ggarrange(p10, p11, ncol = 1, nrow = 2, align = "v", common.legend = TRUE, legend = "bottom")
ggexport(plot_B, filename = "VarPop2_Biom.pdf", width=8, height=6.5, dpi=300)


# Mortalidad por Pesca ####

p12 <- ggplot(data = NULL, aes(x = yrs)) + 
  geom_line(aes(y = F_est1, colour = 'actual', linetype = 'actual')) +
  geom_line(aes(y = c(F_est2,NA), colour = 'anterior', linetype = 'anterior')) +
  geom_ribbon(data=NULL, aes(ymin=F1_lwr, ymax=F1_upr), fill = 'grey37', alpha = 0.4) + 
  geom_ribbon(data=NULL, aes(ymin=c(F2_lwr,NA), ymax=c(F2_upr,NA)),fill = 'grey70', alpha = 0.4) + 
  geom_line(aes(y = c(rep(M,36)), colour = 'M', linetype = 'M')) +
  geom_line(aes(y = c(rep(Frms,36)), colour = 'Frms', linetype = 'Frms')) +
  
  scale_color_manual(name = '',
                     values = c('royalblue3', 'red1', 'chartreuse3','black'),
                     limits = c('actual', 'anterior', 'M', 'Frms'),
                     breaks = c('actual', 'anterior', 'M', 'Frms')) +
  scale_linetype_manual(name = '',
                        values = c('solid', 'longdash', 'twodash','dotted'),
                        limits = c('actual', 'anterior', 'M', 'Frms'),
                        breaks = c('actual', 'anterior', 'M', 'Frms'))
p12 <- p12 + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=8)) +
  theme(legend.position = 'bottom') + ylab('Mortalidad por Pesca (1/años)') + xlab('Años') + 
  scale_x_continuous(breaks=round(seq(min(yrs), 2020, by = 5),1))

p12
ggexport(p12, filename = "VarPop3_Fh.pdf", width=8, height=6.5, dpi=300)


# Mortalidad por Pesca b ####

p12_b <- ggplot(data = NULL, aes(x = yrs)) + 
  geom_line(aes(y = F_est1_b, colour = 'actual', linetype = 'actual')) +
  geom_line(aes(y = c(F_est2_b,NA), colour = 'anterior', linetype = 'anterior')) +
  geom_ribbon(data=NULL, aes(ymin=F1_lwr_b, ymax=F1_upr_b), fill = 'grey37', alpha = 0.4) + 
  geom_ribbon(data=NULL, aes(ymin=c(F2_lwr_b,NA), ymax=c(F2_upr_b,NA)),fill = 'grey70', alpha = 0.4) + 
  geom_line(aes(y = c(rep(M,36)), colour = 'M', linetype = 'M')) +
  geom_line(aes(y = c(rep(Frms,36)), colour = 'Frms', linetype = 'Frms')) +
  
  scale_color_manual(name = '',
                     values = c('royalblue3', 'red1', 'chartreuse3','black'),
                     limits = c('actual', 'anterior', 'M', 'Frms'),
                     breaks = c('actual', 'anterior', 'M', 'Frms')) +
  scale_linetype_manual(name = '',
                        values = c('solid', 'longdash', 'twodash','dotted'),
                        limits = c('actual', 'anterior', 'M', 'Frms'),
                        breaks = c('actual', 'anterior', 'M', 'Frms'))
p12_b <- p12_b + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=8)) +
  theme(legend.position = 'bottom') + ylab('Mortalidad por Pesca (1/años)') + xlab('Años') + 
  scale_x_continuous(breaks=round(seq(min(yrs), 2020, by = 5),1))

p12_b
ggexport(p12_b, filename = "VarPop3_Ftot.pdf", width=8, height=6.5, dpi=300)

