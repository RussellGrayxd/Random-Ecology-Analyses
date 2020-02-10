library(lme4)
library(ggplot2)
library(ggiraph)
library(ggiraphExtra)
library(cowplot)


#turn you yes/no column into a binary 1, 0 column
yesno<-ifelse(BOCY057$`Snake moved`=="Yes", 1, 0)


#non-scaled data
RH<-BOCY057$`Ambient RH`
Temp<-BOCY057$`Ambient C`
Rain<-BOCY057$`Rainfall (mm/day)`
Illum<-BOCY057$`Lunar illumination`

#scale your data for plotting glm
SRH<-c(scale(BOCY057$`Ambient RH`))
STemp<-c(scale(BOCY057$`Ambient C`))
SRain<-c(scale(BOCY057$`Rainfall (mm/day)`))
SIllum<-c(scale(BOCY057$`Lunar illumination`))


dat<-data.frame(cbind(RH, SRH, Temp, STemp, Rain, SRain, Illum, SIllum))

#---------------------------------------------------------------------
############### Run General Linear Models (GLM) #######################

#run glm on the non-scaled data
m1 <- glm(yesno ~ RH, binomial, dat)
confint(m1)
#run glm on the scaled data
m2 <- glm(yesno ~ SRH, binomial, dat)
confint(m2)

m3<-glm(yesno ~ Temp, binomial, dat)
confint(m3)
m4<-glm(yesno ~ STemp, binomial, dat)
confint(m4)

m5<-glm(yesno ~ Rain, binomial, dat)
confint(m5)
m6<-glm(yesno ~ SRain, binomial, dat)
confint(m6)

m7<-glm(yesno ~ Illum, binomial, dat)
confint(m7)
m8<-glm(yesno ~ SIllum, binomial, dat)
confint(m8)


#make the plot
p1<-ggPredict(m1,se=TRUE,interactive=F,digits=3, jitter = T)
p1
p2<-ggPredict(m2,se=TRUE,interactive=F,digits=3, jitter = T)
p2

p3<-ggPredict(m3,se=TRUE,interactive=F,digits=3, jitter = T)
p3
p4<-ggPredict(m4,se=TRUE,interactive=F,digits=3, jitter = T)
p4

p5<-ggPredict(m5,se=TRUE,interactive=F,digits=3, jitter = T)
p5
p6<-ggPredict(m6,se=TRUE,interactive=F,digits=3, jitter = T)
p6

p7<-ggPredict(m7,se=TRUE,interactive=F,digits=3, jitter = T)
p7
p8<-ggPredict(m8,se=TRUE,interactive=F,digits=3, jitter = T)
p8


#add labels
plab<-p1 + labs(title = "RH Non-Scaled")+ 
  xlab("RH (%)")+ 
  ylab("Move vs. No Move")+
  theme_minimal(base_size = 10)+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))


plab2<-p2 + labs(title = "RH Scaled")+ 
  xlab("RH (%)")+ 
  ylab("Move vs. No Move")+
  theme_minimal(base_size = 10)+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

#---------------------------------

plab3<-p3 + labs(title = "RH Non-Scaled")+ 
  xlab("Temp (C)")+ 
  ylab("Move vs. No Move")+
  theme_minimal(base_size = 10)+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))


plab4<-p4 + labs(title = "RH Scaled")+ 
  xlab("Temp (C)")+ 
  ylab("Move vs. No Move")+
  theme_minimal(base_size = 10)+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

#----------------------------------------------------------
plab5<-p5 + labs(title = "RH Non-Scaled")+ 
  xlab("Rain (mm)")+ 
  ylab("Move vs. No Move")+
  theme_minimal(base_size = 10)+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))


plab6<-p6 + labs(title = "RH Scaled")+ 
  xlab("Rain (mm)")+ 
  ylab("Move vs. No Move")+
  theme_minimal(base_size = 10)+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

#----------------------------------------------------
plab7<-p7 + labs(title = "RH Non-Scaled")+ 
  xlab("Illumination (%)")+ 
  ylab("Move vs. No Move")+
  theme_minimal(base_size = 10)+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))


plab8<-p8 + labs(title = "RH Scaled")+ 
  xlab("Illumination (%)")+ 
  ylab("Move vs. No Move")+
  theme_minimal(base_size = 10)+
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        plot.subtitle = element_text(hjust = 0.5))



plot_grid(plab, plab2, plab3, plab4, plab5, plab6, plab7, plab8, 
          nrow = 4, ncol = 2)



