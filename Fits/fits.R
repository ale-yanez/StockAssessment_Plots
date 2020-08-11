# Funciones y Directorios ####
rm(list=ls())

library(ggplot2)
library(reshape)
library(ggpubr)
library(RCurl)
library(devtools)

#source('~/Documents/Rwork/Functions/read.admb.R')
devtools::source_url("https://github.com/ale-yanez/RFunctions/blob/master/read.admb.R?raw=TRUE")

dir.1<-'~/GitHub/StockAssessment_Plots/Fits/'
# dir.2<-'~/Documents/ADMwork/IFOP/2019/Lama_model/Cons_2003/norte/Lamnor2003/'
# dir.3<-'~/Documents/ADMwork/IFOP/2020/Lama_model/Estatus_2008/norte/Lamnor2008/'


# Lee Reportes actual y anterior ####

setwd(dir.1)
dir()

out1 <- read.admb("LAM_nor2008")
names(out1)


# Para graficar ... ####
 yrs <- out1$YRS
 nyrs <- length(yrs)
# tallas <- seq(10,52,1)
# class(tallas)
# M <- 0.3
# Brms <- out1$BDoLP*0.4
# Frms <- out1$Fpbr[3]
# B0 <- out1$BDoLP
# 
# #Observado
 obsD <- out1$Desemb[1,]
 obsC <- out1$CPUE[1,] ; obsC[obsC <= 0.01]   <-NA
 obsS <- out1$BCRU[1,] ; obsS[obsS <= 1]  <-NA
# 
# 
# #predichos y estimados 
 predD         <- out1$Desemb[2,]
 predC         <- out1$CPUE[2,]
 predS         <- out1$BCRU[2,]
# 
# Lobs_mf       <- out1$Lm_obs_pred[1,] ; Lobs_mf[Lobs_mf <=1]  <-NA
# Lpred_mf      <- out1$Lm_obs_pred[2,] ; Lpred_mf[Lpred_mf <=1]  <-NA
# Lobs_hf       <- out1$Lh_obs_pred[1,] ; Lobs_hf[Lobs_hf <=1]  <-NA
# Lpred_hf      <- out1$Lh_obs_pred[2,] ; Lpred_hf[Lpred_hf <=1]  <-NA
# Lobs_mc       <- out1$Lmc_obs_est[1,] ; Lobs_mc[Lobs_mc <=1]  <-NA
# Lpred_mc      <- out1$Lmc_obs_est[2,] ; Lpred_mc[Lpred_mc <=1]  <-NA
# Lobs_hc       <- out1$Lhc_obs_est[1,] ; Lobs_hc[Lobs_hc <=1]  <-NA
# Lpred_hc      <- out1$Lhc_obs_est[2,] ; Lpred_hc[Lpred_hc <=1]  <-NA
# 
# 
# 
# 
# # std 
# stdpredD      <- subset(std1,name=="pred_Desemb")$std
# stdpredC      <- subset(std1,name=="pred_CPUE")$std
# stdpredS      <- subset(std1,name=="pred_Bcru")$std
# 
# stdL_mf       <- subset(std1,name=="Lmf_pred")$std
# stdL_hf       <- subset(std1,name=="Lhf_pred")$std
# stdL_mc       <- subset(std1,name=="Lmc_pred")$std
# stdL_hc       <- subset(std1,name=="Lhc_pred")$std
# 
# 
# # Confidence Intervals
 cvdes         <- rep(0.1,nyrs)
 cvcpue        <- rep(0.15,nyrs)
 cvsurv        <- rep(0.30,nyrs)
# 
# 
 obsD95i   <- obsD*exp(-1.96*cvdes); obsD95s <- obsD*exp(1.96*cvdes)
 obsC95i   <- obsC*exp(-1.96*cvcpue); obsC95s <-obsC*exp(1.96*cvcpue)
 obsS95i   <- obsS*exp(-1.96*cvsurv); obsS95s <-obsS*exp(1.96*cvsurv)



#FITS LAMA MODEL ####

#  Desembarques ####

p1 <-  ggplot(NULL, aes(x=yrs)) +
  geom_point(aes(y= obsD, colour="Desemb Obs"), size = 2, shape = 21) +
  geom_line(aes(y= predD, colour="Desemb Est")) +
  geom_errorbar(aes(ymin = obsD95i, ymax = obsD95s), colour='black', width=0.6) +
  scale_colour_manual(name='', values=c('Desemb Est'='royalblue3', 'Desemb Obs'='black'), guide='legend') +
  guides(colour = guide_legend(override.aes = list(linetype=c(1,0), shape=c(NA, 21)))) +
  
  xlab('Años') + scale_x_continuous(breaks=round(seq(min(yrs), 2020, by = 5),1)) +
  ylab('Desembarque (t)') + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=8)) +
  theme(legend.position = 'bottom')  + theme(legend.title=element_blank())

p1

# op<-par(no.readonly=TRUE)
# ps.options(horizontal=F,bg="white",onefile=FALSE,paper="special")
# postscript("FigsEstatus/Fig1.eps",height=5, width=7.5) 
# p1
# dev.off()

# Indice de Abundncia ####

p2 <-  ggplot(NULL, aes(x=yrs)) +
  geom_point(aes(y= obsC, colour="CPUE Obs"), size = 2, shape = 21) +
  geom_line(aes(y= predC, colour="CPUE Est")) +
  geom_errorbar(aes(ymin = obsC95i, ymax = obsC95s), colour='black', width=0.6) +
  scale_colour_manual(name='', values=c('CPUE Est'='royalblue3', 'CPUE Obs'='black'), guide='legend') +
  guides(colour = guide_legend(override.aes = list(linetype=c(1,0), shape=c(NA, 21)))) +
  
  xlab('Años') + scale_x_continuous(breaks=round(seq(min(yrs), 2020, by = 5),1)) +
  ylab('Indice Relativo') + scale_y_continuous(breaks=round(seq(min(predC), 3, by = 0.5),1)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=8)) +
  theme(legend.position = 'bottom')  + theme(legend.title=element_blank())

p2

# op<-par(no.readonly=TRUE)
# ps.options(horizontal=F,bg="white",onefile=FALSE,paper="special")
# postscript("FigsEstatus/Fig2.eps",height=5, width=7.5) 
# p2
# dev.off()

#  Crucero ####

p3 <-  ggplot(NULL, aes(x=yrs)) +
  geom_point(aes(y= obsS, colour="Crucero Obs"), size = 2, shape = 21) +
  geom_line(aes(y= predS, colour="Crucero Est")) +
  geom_errorbar(aes(ymin = obsS95i, ymax = obsS95s), colour='black', width=0.6) +
  scale_colour_manual(name='', values=c('Crucero Est'='royalblue3', 'Crucero Obs'='black'), guide='legend') +
  guides(colour = guide_legend(override.aes = list(linetype=c(1,0), shape=c(NA, 21)))) +
  
  xlab('Años') + scale_x_continuous(breaks=round(seq(min(yrs), 2020, by = 5),1)) +
  ylab('Biomasa (t)') +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=8)) +
  theme(legend.position = 'bottom')  + theme(legend.title=element_blank())

p3

# op<-par(no.readonly=TRUE)
# ps.options(horizontal=F,bg="white",onefile=FALSE,paper="special")
# postscript("FigsEstatus/Fig3.eps",height=5, width=7.5) 
# p3
# dev.off()


p_1 <- ggarrange(p1, p2, p3,
                 ncol = 1, nrow = 3, align = "v")
#plot <- ggarrange(p_1, p_2, ncol = 1, nrow = 2, align = "v", common.legend = TRUE, legend = "bottom")


ggexport(p_1, filename = "Fig1.pdf", width=6.5, height=8, dpi=300)


