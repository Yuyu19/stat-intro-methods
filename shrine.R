woww<- read.table("http://www.uwyo.edu/crawford/datasets/used/shrine.txt", header= TRUE)
woww$children<- as.numeric(as.factor(woww$children))
wow<-woww[woww$shrine<1000, ]
wow<-na.omit(wow)
hist(wow$shrine)
fit<- lm(shrine~ rain+crops+log(children)*deaths+ citysize++ I(wonders^2)*chief+wonders
         +I(wonders^3)*chief, data= wow)
plot(shrine~rain,data=wow, col=wow$chief)
plot(shrine~crops,data=wow, col=wow$chief)
  plot(shrine~children,data=wow, col=wow$chief)
plot(shrine~deaths,data=wow)
plot(shrine~citysize,data=wow, col=wow$chief)
plot(shrine~wonders,data=wow, col=wow$chief)
plot(shrine~chief,data=wow)


plot(fit$residuals~wow$rain, col=wow$chief)
plot(fit$residuals~wow$crops, col=wow$chief)
plot(fit$residuals~wow$children, col=wow$chief)
plot(fit$residuals~wow$deaths, col=wow$chief)
plot(fit$residuals~wow$citysize, col=wow$chief)
plot(fit$residuals~wow$wonders, col=wow$chief)
plot(fit$residuals~wow$chief)

plot(fit$residuals~wow$rain)
plot(fit$residuals~wow$crops)
plot(fit$residuals~wow$children)
plot(fit$residuals~wow$deaths)
plot(fit$residuals~wow$citysize)
plot(fit$residuals~wow$wonders)
plot(fit$residuals~wow$chief)

wow$deaths==0
wow$shrine==0

fit<- lm(shrine~ log(rain)*log(crops)+
          +log(children)*deaths
          +log(citysize)
         + I(wonders^2)*chief+wonders
         +I(wonders^3)*chief
         , data= wow)

summary(fit)
plot(fit)

slopes<-fit$coefficients
cbind(seq(1:length(slopes)),slopes)


plot(shrine~children,data=wow)
rainhat<-15 ### 0-30
cropshat<-70 ###10-130
  childrenhat<-seq(0,101, length=100) ###0-101
  deathshat<- 10 ###0-200
  citysizehat<-1400  ### 500-2500
  wondershat<- 60  ###0-120
  chiefChieftesshat=0
  
shrinehat<-slopes[1]+
  slopes[2]*log(rainhat)+
  slopes[3]*log(cropshat)+
  slopes[4]*log(childrenhat)+
  slopes[5]*deathshat+
  slopes[6]*log(citysizehat)+
  slopes[7]*I(wondershat^2)+
  slopes[8]*chiefChieftesshat+
  slopes[9]*wondershat+
  slopes[10]*I(wondershat^3)+
  slopes[11]*log(rainhat)*log(cropshat)+
  slopes[12]*log(childrenhat)*deathshat+
  slopes[13]*I(wondershat^2)*chiefChieftesshat+
  slopes[14]*chiefChieftesshat*I(wondershat^3)
lines(shrinehat~childrenhat,col="red",lwd=5)

rainhat<-15 ### 0-30
cropshat<-70 ###10-130
childrenhat<-seq(0,101, length=100) ###0-101
deathshat<- 50 ###0-200
citysizehat<-1400  ### 500-2500
wondershat<- 60  ###0-120
chiefChieftesshat=0

shrinehat<-slopes[1]+
  slopes[2]*log(rainhat)+
  slopes[3]*log(cropshat)+
  slopes[4]*log(childrenhat)+
  slopes[5]*deathshat+
  slopes[6]*log(citysizehat)+
  slopes[7]*I(wondershat^2)+
  slopes[8]*chiefChieftesshat+
  slopes[9]*wondershat+
  slopes[10]*I(wondershat^3)+
  slopes[11]*log(rainhat)*log(cropshat)+
  slopes[12]*log(childrenhat)*deathshat+
  slopes[13]*I(wondershat^2)*chiefChieftesshat+
  slopes[14]*chiefChieftesshat*I(wondershat^3)
lines(shrinehat~childrenhat,col="yellow",lwd=5)


rainhat<-15 ### 0-30
cropshat<-70 ###10-130
childrenhat<-seq(0,101, length=100) ###0-101
deathshat<- 70 ###0-200
citysizehat<-1400  ### 500-2500
wondershat<- 60  ###0-120
chiefChieftesshat=0

shrinehat<-slopes[1]+
  slopes[2]*log(rainhat)+
  slopes[3]*log(cropshat)+
  slopes[4]*log(childrenhat)+
  slopes[5]*deathshat+
  slopes[6]*log(citysizehat)+
  slopes[7]*I(wondershat^2)+
  slopes[8]*chiefChieftesshat+
  slopes[9]*wondershat+
  slopes[10]*I(wondershat^3)+
  slopes[11]*log(rainhat)*log(cropshat)+
  slopes[12]*log(childrenhat)*deathshat+
  slopes[13]*I(wondershat^2)*chiefChieftesshat+
  slopes[14]*chiefChieftesshat*I(wondershat^3)
lines(shrinehat~childrenhat,col="green",lwd=5)


rainhat<-15 ### 0-30
cropshat<-70 ###10-130
childrenhat<-seq(0,101, length=100) ###0-101
deathshat<- 120 ###0-200
citysizehat<-1400  ### 500-2500
wondershat<- 60  ###0-120
chiefChieftesshat=0

shrinehat<-slopes[1]+
  slopes[2]*log(rainhat)+
  slopes[3]*log(cropshat)+
  slopes[4]*log(childrenhat)+
  slopes[5]*deathshat+
  slopes[6]*log(citysizehat)+
  slopes[7]*I(wondershat^2)+
  slopes[8]*chiefChieftesshat+
  slopes[9]*wondershat+
  slopes[10]*I(wondershat^3)+
  slopes[11]*log(rainhat)*log(cropshat)+
  slopes[12]*log(childrenhat)*deathshat+
  slopes[13]*I(wondershat^2)*chiefChieftesshat+
  slopes[14]*chiefChieftesshat*I(wondershat^3)
lines(shrinehat~childrenhat,col="purple",lwd=5)


rainhat<-15 ### 0-30
cropshat<-70 ###10-130
childrenhat<-seq(0,101, length=100) ###0-101
deathshat<- 160 ###0-200
citysizehat<-1400  ### 500-2500
wondershat<- 60  ###0-120
chiefChieftesshat=0

shrinehat<-slopes[1]+
  slopes[2]*log(rainhat)+
  slopes[3]*log(cropshat)+
  slopes[4]*log(childrenhat)+
  slopes[5]*deathshat+
  slopes[6]*log(citysizehat)+
  slopes[7]*I(wondershat^2)+
  slopes[8]*chiefChieftesshat+
  slopes[9]*wondershat+
  slopes[10]*I(wondershat^3)+
  slopes[11]*log(rainhat)*log(cropshat)+
  slopes[12]*log(childrenhat)*deathshat+
  slopes[13]*I(wondershat^2)*chiefChieftesshat+
  slopes[14]*chiefChieftesshat*I(wondershat^3)
lines(shrinehat~childrenhat,col="blue",lwd=5)
legend("topleft",col=c("red","yellow","green","purple","blue"),legend=c("deaths=40","deaths=80","deaths=120","deaths=170","deaths=200"),lty=1, lwd=3)


rainhat<-15 ### 0-30
cropshat<-70 ###10-130
childrenhat<-seq(0,101, length=100) ###0-101
deathshat<- 40 ###0-200
citysizehat<-1400  ### 500-2500
wondershat<- 60  ###0-120
chiefChieftesshat=1

shrinehat<-slopes[1]+
  slopes[2]*log(rainhat)+
  slopes[3]*log(cropshat)+
  slopes[4]*log(childrenhat)+
  slopes[5]*deathshat+
  slopes[6]*log(citysizehat)+
  slopes[7]*I(wondershat^2)+
  slopes[8]*chiefChieftesshat+
  slopes[9]*wondershat+
  slopes[10]*I(wondershat^3)+
  slopes[11]*log(rainhat)*log(cropshat)+
  slopes[12]*log(childrenhat)*deathshat+
  slopes[13]*I(wondershat^2)*chiefChieftesshat+
  slopes[14]*chiefChieftesshat*I(wondershat^3)
lines(shrinehat~childrenhat,col="red",lwd=5,lty=3)

rainhat<-15 ### 0-30
cropshat<-70 ###10-130
childrenhat<-seq(0,101, length=100) ###0-101
deathshat<- 80 ###0-200
citysizehat<-1400  ### 500-2500
wondershat<- 60  ###0-120
chiefChieftesshat=1

shrinehat<-slopes[1]+
  slopes[2]*log(rainhat)+
  slopes[3]*log(cropshat)+
  slopes[4]*log(childrenhat)+
  slopes[5]*deathshat+
  slopes[6]*log(citysizehat)+
  slopes[7]*I(wondershat^2)+
  slopes[8]*chiefChieftesshat+
  slopes[9]*wondershat+
  slopes[10]*I(wondershat^3)+
  slopes[11]*log(rainhat)*log(cropshat)+
  slopes[12]*log(childrenhat)*deathshat+
  slopes[13]*I(wondershat^2)*chiefChieftesshat+
  slopes[14]*chiefChieftesshat*I(wondershat^3)
lines(shrinehat~childrenhat,col="yellow",lwd=5,lty=3)


rainhat<-15 ### 0-30
cropshat<-70 ###10-130
childrenhat<-seq(0,101, length=100) ###0-101
deathshat<- 120 ###0-200
citysizehat<-1400  ### 500-2500
wondershat<- 60  ###0-120
chiefChieftesshat=1

shrinehat<-slopes[1]+
  slopes[2]*log(rainhat)+
  slopes[3]*log(cropshat)+
  slopes[4]*log(childrenhat)+
  slopes[5]*deathshat+
  slopes[6]*log(citysizehat)+
  slopes[7]*I(wondershat^2)+
  slopes[8]*chiefChieftesshat+
  slopes[9]*wondershat+
  slopes[10]*I(wondershat^3)+
  slopes[11]*log(rainhat)*log(cropshat)+
  slopes[12]*log(childrenhat)*deathshat+
  slopes[13]*I(wondershat^2)*chiefChieftesshat+
  slopes[14]*chiefChieftesshat*I(wondershat^3)
lines(shrinehat~childrenhat,col="green",lwd=5,lty=3)


rainhat<-15 ### 0-30
cropshat<-70 ###10-130
childrenhat<-seq(0,101, length=100) ###0-101
deathshat<- 170 ###0-200
citysizehat<-1400  ### 500-2500
wondershat<- 60  ###0-120
chiefChieftesshat=1

shrinehat<-slopes[1]+
  slopes[2]*log(rainhat)+
  slopes[3]*log(cropshat)+
  slopes[4]*log(childrenhat)+
  slopes[5]*deathshat+
  slopes[6]*log(citysizehat)+
  slopes[7]*I(wondershat^2)+
  slopes[8]*chiefChieftesshat+
  slopes[9]*wondershat+
  slopes[10]*I(wondershat^3)+
  slopes[11]*log(rainhat)*log(cropshat)+
  slopes[12]*log(childrenhat)*deathshat+
  slopes[13]*I(wondershat^2)*chiefChieftesshat+
  slopes[14]*chiefChieftesshat*I(wondershat^3)
lines(shrinehat~childrenhat,col="purple",lwd=5,lty=3)




####rain
plot(shrine~rain,data=wow)
rainhat<-seq(0,30, length=100) ### 0-30
cropshat<-30 ###10-130
childrenhat<-50 ###0-101
deathshat<- 100 ###0-200
citysizehat<-1400  ### 500-2500
wondershat<- 60  ###0-120
chiefChieftesshat=0

shrinehat<-slopes[1]+
  slopes[2]*log(rainhat)+
  slopes[3]*log(cropshat)+
  slopes[4]*log(childrenhat)+
  slopes[5]*deathshat+
  slopes[6]*log(citysizehat)+
  slopes[7]*I(wondershat^2)+
  slopes[8]*chiefChieftesshat+
  slopes[9]*wondershat+
  slopes[10]*I(wondershat^3)+
  slopes[11]*log(rainhat)*log(cropshat)+
  slopes[12]*log(childrenhat)*deathshat+
  slopes[13]*I(wondershat^2)*chiefChieftesshat+
  slopes[14]*chiefChieftesshat*I(wondershat^3)
lines(shrinehat~rainhat,col="red",lwd=5)


rainhat<-seq(0,30, length=100) ### 0-30
cropshat<-60 ###10-130
childrenhat<-50 ###0-101
deathshat<- 100 ###0-200
citysizehat<-1400  ### 500-2500
wondershat<- 60  ###0-120
chiefChieftesshat=0

shrinehat<-slopes[1]+
  slopes[2]*log(rainhat)+
  slopes[3]*log(cropshat)+
  slopes[4]*log(childrenhat)+
  slopes[5]*deathshat+
  slopes[6]*log(citysizehat)+
  slopes[7]*I(wondershat^2)+
  slopes[8]*chiefChieftesshat+
  slopes[9]*wondershat+
  slopes[10]*I(wondershat^3)+
  slopes[11]*log(rainhat)*log(cropshat)+
  slopes[12]*log(childrenhat)*deathshat+
  slopes[13]*I(wondershat^2)*chiefChieftesshat+
  slopes[14]*chiefChieftesshat*I(wondershat^3)
lines(shrinehat~rainhat,col="yellow",lwd=5)


rainhat<-seq(0,30, length=100) ### 0-30
cropshat<-90 ###10-130
childrenhat<-50 ###0-101
deathshat<- 100 ###0-200
citysizehat<-1400  ### 500-2500
wondershat<- 60  ###0-120
chiefChieftesshat=0

shrinehat<-slopes[1]+
  slopes[2]*log(rainhat)+
  slopes[3]*log(cropshat)+
  slopes[4]*log(childrenhat)+
  slopes[5]*deathshat+
  slopes[6]*log(citysizehat)+
  slopes[7]*I(wondershat^2)+
  slopes[8]*chiefChieftesshat+
  slopes[9]*wondershat+
  slopes[10]*I(wondershat^3)+
  slopes[11]*log(rainhat)*log(cropshat)+
  slopes[12]*log(childrenhat)*deathshat+
  slopes[13]*I(wondershat^2)*chiefChieftesshat+
  slopes[14]*chiefChieftesshat*I(wondershat^3)
lines(shrinehat~rainhat,col="green",lwd=5)

rainhat<-seq(0,30, length=100) ### 0-30
cropshat<-110 ###10-130
childrenhat<-50 ###0-101
deathshat<- 100 ###0-200
citysizehat<-1400  ### 500-2500
wondershat<- 60  ###0-120
chiefChieftesshat=0

shrinehat<-slopes[1]+
  slopes[2]*log(rainhat)+
  slopes[3]*log(cropshat)+
  slopes[4]*log(childrenhat)+
  slopes[5]*deathshat+
  slopes[6]*log(citysizehat)+
  slopes[7]*I(wondershat^2)+
  slopes[8]*chiefChieftesshat+
  slopes[9]*wondershat+
  slopes[10]*I(wondershat^3)+
  slopes[11]*log(rainhat)*log(cropshat)+
  slopes[12]*log(childrenhat)*deathshat+
  slopes[13]*I(wondershat^2)*chiefChieftesshat+
  slopes[14]*chiefChieftesshat*I(wondershat^3)
lines(shrinehat~rainhat,col="purple",lwd=5)



rainhat<-seq(0,30, length=100) ### 0-30
cropshat<-130 ###10-130
childrenhat<-50 ###0-101
deathshat<- 100 ###0-200
citysizehat<-1400  ### 500-2500
wondershat<- 60  ###0-120
chiefChieftesshat=0

shrinehat<-slopes[1]+
  slopes[2]*log(rainhat)+
  slopes[3]*log(cropshat)+
  slopes[4]*log(childrenhat)+
  slopes[5]*deathshat+
  slopes[6]*log(citysizehat)+
  slopes[7]*I(wondershat^2)+
  slopes[8]*chiefChieftesshat+
  slopes[9]*wondershat+
  slopes[10]*I(wondershat^3)+
  slopes[11]*log(rainhat)*log(cropshat)+
  slopes[12]*log(childrenhat)*deathshat+
  slopes[13]*I(wondershat^2)*chiefChieftesshat+
  slopes[14]*chiefChieftesshat*I(wondershat^3)
lines(shrinehat~rainhat,col="blue",lwd=5)
legend("topleft",col=c("red","yellow","green","purple","blue"),legend=c("crops=30","crops=60","crops=90","crops=110","crops=130"),lty=1, lwd=3)




####wonders
plot(shrine~wonders,data=wow)
rainhat<-15 ### 0-30
cropshat<-30 ###10-130
childrenhat<-50 ###0-101
deathshat<- 100 ###0-200
citysizehat<-1400  ### 500-2500
wondershat<- seq(0,120, length=100)  ###0-120
chiefChieftesshat=0

shrinehat<-slopes[1]+
  slopes[2]*log(rainhat)+
  slopes[3]*log(cropshat)+
  slopes[4]*log(childrenhat)+
  slopes[5]*deathshat+
  slopes[6]*log(citysizehat)+
  slopes[7]*I(wondershat^2)+
  slopes[8]*chiefChieftesshat+
  slopes[9]*wondershat+
  slopes[10]*I(wondershat^3)+
  slopes[11]*log(rainhat)*log(cropshat)+
  slopes[12]*log(childrenhat)*deathshat+
  slopes[13]*I(wondershat^2)*chiefChieftesshat+
  slopes[14]*chiefChieftesshat*I(wondershat^3)
lines(shrinehat~wondershat,col="red",lwd=5)

rainhat<-15 ### 0-30
cropshat<-30 ###10-130
childrenhat<-50 ###0-101
deathshat<- 100 ###0-200
citysizehat<-1400  ### 500-2500
wondershat<- seq(0,120, length=100)  ###0-120
chiefChieftesshat=1

shrinehat<-slopes[1]+
  slopes[2]*log(rainhat)+
  slopes[3]*log(cropshat)+
  slopes[4]*log(childrenhat)+
  slopes[5]*deathshat+
  slopes[6]*log(citysizehat)+
  slopes[7]*I(wondershat^2)+
  slopes[8]*chiefChieftesshat+
  slopes[9]*wondershat+
  slopes[10]*I(wondershat^3)+
  slopes[11]*log(rainhat)*log(cropshat)+
  slopes[12]*log(childrenhat)*deathshat+
  slopes[13]*I(wondershat^2)*chiefChieftesshat+
  slopes[14]*chiefChieftesshat*I(wondershat^3)
lines(shrinehat~wondershat,col="yellow",lwd=5)


legend("topleft",col=c("red","yellow"),legend=c("chief","chiefteness"),lty=1, lwd=3)





####deaths
plot(shrine~deaths,data=wow)
rainhat<-15 ### 0-30
cropshat<-70 ###10-130
childrenhat<-10 ###0-101
deathshat<- seq(0,200, length=100) ###0-200
citysizehat<-1400  ### 500-2500
wondershat<- 60  ###0-120
chiefChieftesshat=0

shrinehat<-slopes[1]+
  slopes[2]*log(rainhat)+
  slopes[3]*log(cropshat)+
  slopes[4]*log(childrenhat)+
  slopes[5]*deathshat+
  slopes[6]*log(citysizehat)+
  slopes[7]*I(wondershat^2)+
  slopes[8]*chiefChieftesshat+
  slopes[9]*wondershat+
  slopes[10]*I(wondershat^3)+
  slopes[11]*log(rainhat)*log(cropshat)+
  slopes[12]*log(childrenhat)*deathshat+
  slopes[13]*I(wondershat^2)*chiefChieftesshat+
  slopes[14]*chiefChieftesshat*I(wondershat^3)
lines(shrinehat~deathshat,col="red",lwd=5)


rainhat<-15 ### 0-30
cropshat<-70 ###10-130
childrenhat<-30 ###0-101
deathshat<- seq(0,200, length=100) ###0-200
citysizehat<-1400  ### 500-2500
wondershat<- 60  ###0-120
chiefChieftesshat=0

shrinehat<-slopes[1]+
  slopes[2]*log(rainhat)+
  slopes[3]*log(cropshat)+
  slopes[4]*log(childrenhat)+
  slopes[5]*deathshat+
  slopes[6]*log(citysizehat)+
  slopes[7]*I(wondershat^2)+
  slopes[8]*chiefChieftesshat+
  slopes[9]*wondershat+
  slopes[10]*I(wondershat^3)+
  slopes[11]*log(rainhat)*log(cropshat)+
  slopes[12]*log(childrenhat)*deathshat+
  slopes[13]*I(wondershat^2)*chiefChieftesshat+
  slopes[14]*chiefChieftesshat*I(wondershat^3)
lines(shrinehat~deathshat,col="yellow",lwd=5)

rainhat<-15 ### 0-30
cropshat<-70 ###10-130
childrenhat<-60 ###0-101
deathshat<- seq(0,200, length=100) ###0-200
citysizehat<-1400  ### 500-2500
wondershat<- 60  ###0-120
chiefChieftesshat=0

shrinehat<-slopes[1]+
  slopes[2]*log(rainhat)+
  slopes[3]*log(cropshat)+
  slopes[4]*log(childrenhat)+
  slopes[5]*deathshat+
  slopes[6]*log(citysizehat)+
  slopes[7]*I(wondershat^2)+
  slopes[8]*chiefChieftesshat+
  slopes[9]*wondershat+
  slopes[10]*I(wondershat^3)+
  slopes[11]*log(rainhat)*log(cropshat)+
  slopes[12]*log(childrenhat)*deathshat+
  slopes[13]*I(wondershat^2)*chiefChieftesshat+
  slopes[14]*chiefChieftesshat*I(wondershat^3)
lines(shrinehat~deathshat,col="green",lwd=5)

rainhat<-15 ### 0-30
cropshat<-70 ###10-130
childrenhat<-90 ###0-101
deathshat<- seq(0,200, length=100) ###0-200
citysizehat<-1400  ### 500-2500
wondershat<- 60  ###0-120
chiefChieftesshat=0

shrinehat<-slopes[1]+
  slopes[2]*log(rainhat)+
  slopes[3]*log(cropshat)+
  slopes[4]*log(childrenhat)+
  slopes[5]*deathshat+
  slopes[6]*log(citysizehat)+
  slopes[7]*I(wondershat^2)+
  slopes[8]*chiefChieftesshat+
  slopes[9]*wondershat+
  slopes[10]*I(wondershat^3)+
  slopes[11]*log(rainhat)*log(cropshat)+
  slopes[12]*log(childrenhat)*deathshat+
  slopes[13]*I(wondershat^2)*chiefChieftesshat+
  slopes[14]*chiefChieftesshat*I(wondershat^3)
lines(shrinehat~deathshat,col="purple",lwd=5)



rainhat<-15 ### 0-30
cropshat<-70 ###10-130
childrenhat<-100 ###0-101
deathshat<- seq(0,200, length=100) ###0-200
citysizehat<-1400  ### 500-2500
wondershat<- 60  ###0-120
chiefChieftesshat=0

shrinehat<-slopes[1]+
  slopes[2]*log(rainhat)+
  slopes[3]*log(cropshat)+
  slopes[4]*log(childrenhat)+
  slopes[5]*deathshat+
  slopes[6]*log(citysizehat)+
  slopes[7]*I(wondershat^2)+
  slopes[8]*chiefChieftesshat+
  slopes[9]*wondershat+
  slopes[10]*I(wondershat^3)+
  slopes[11]*log(rainhat)*log(cropshat)+
  slopes[12]*log(childrenhat)*deathshat+
  slopes[13]*I(wondershat^2)*chiefChieftesshat+
  slopes[14]*chiefChieftesshat*I(wondershat^3)
lines(shrinehat~deathshat,col="blue",lwd=5)

legend("topleft",col=c("red","yellow","green","purple","blue"),legend=c("children=10","children=30","children=60","children=90","children=110"),lty=1, lwd=3)




####crops
plot(shrine~crops,data=wow)
rainhat<-5 ### 0-30
cropshat<-seq(0,200, length=100) ###10-130
childrenhat<-50 ###0-101
deathshat<- 100 ###0-200
citysizehat<-1400  ### 500-2500
wondershat<- 60  ###0-120
chiefChieftesshat=0

shrinehat<-slopes[1]+
  slopes[2]*log(rainhat)+
  slopes[3]*log(cropshat)+
  slopes[4]*log(childrenhat)+
  slopes[5]*deathshat+
  slopes[6]*log(citysizehat)+
  slopes[7]*I(wondershat^2)+
  slopes[8]*chiefChieftesshat+
  slopes[9]*wondershat+
  slopes[10]*I(wondershat^3)+
  slopes[11]*log(rainhat)*log(cropshat)+
  slopes[12]*log(childrenhat)*deathshat+
  slopes[13]*I(wondershat^2)*chiefChieftesshat+
  slopes[14]*chiefChieftesshat*I(wondershat^3)
lines(shrinehat~cropshat,col="red",lwd=5)


rainhat<-10 ### 0-30
cropshat<-seq(0,200, length=100) ###10-130
childrenhat<-50 ###0-101
deathshat<- 100 ###0-200
citysizehat<-1400  ### 500-2500
wondershat<- 60  ###0-120
chiefChieftesshat=0

shrinehat<-slopes[1]+
  slopes[2]*log(rainhat)+
  slopes[3]*log(cropshat)+
  slopes[4]*log(childrenhat)+
  slopes[5]*deathshat+
  slopes[6]*log(citysizehat)+
  slopes[7]*I(wondershat^2)+
  slopes[8]*chiefChieftesshat+
  slopes[9]*wondershat+
  slopes[10]*I(wondershat^3)+
  slopes[11]*log(rainhat)*log(cropshat)+
  slopes[12]*log(childrenhat)*deathshat+
  slopes[13]*I(wondershat^2)*chiefChieftesshat+
  slopes[14]*chiefChieftesshat*I(wondershat^3)
lines(shrinehat~cropshat,col="yellow",lwd=5)

rainhat<-15 ### 0-30
cropshat<-seq(0,200, length=100) ###10-130
childrenhat<-50 ###0-101
deathshat<- 100 ###0-200
citysizehat<-1400  ### 500-2500
wondershat<- 60  ###0-120
chiefChieftesshat=0

shrinehat<-slopes[1]+
  slopes[2]*log(rainhat)+
  slopes[3]*log(cropshat)+
  slopes[4]*log(childrenhat)+
  slopes[5]*deathshat+
  slopes[6]*log(citysizehat)+
  slopes[7]*I(wondershat^2)+
  slopes[8]*chiefChieftesshat+
  slopes[9]*wondershat+
  slopes[10]*I(wondershat^3)+
  slopes[11]*log(rainhat)*log(cropshat)+
  slopes[12]*log(childrenhat)*deathshat+
  slopes[13]*I(wondershat^2)*chiefChieftesshat+
  slopes[14]*chiefChieftesshat*I(wondershat^3)
lines(shrinehat~cropshat,col="green",lwd=5)

rainhat<-20 ### 0-30
cropshat<-seq(0,200, length=100) ###10-130
childrenhat<-50 ###0-101
deathshat<- 100 ###0-200
citysizehat<-1400  ### 500-2500
wondershat<- 60  ###0-120
chiefChieftesshat=0

shrinehat<-slopes[1]+
  slopes[2]*log(rainhat)+
  slopes[3]*log(cropshat)+
  slopes[4]*log(childrenhat)+
  slopes[5]*deathshat+
  slopes[6]*log(citysizehat)+
  slopes[7]*I(wondershat^2)+
  slopes[8]*chiefChieftesshat+
  slopes[9]*wondershat+
  slopes[10]*I(wondershat^3)+
  slopes[11]*log(rainhat)*log(cropshat)+
  slopes[12]*log(childrenhat)*deathshat+
  slopes[13]*I(wondershat^2)*chiefChieftesshat+
  slopes[14]*chiefChieftesshat*I(wondershat^3)
lines(shrinehat~cropshat,col="purple",lwd=5)



rainhat<-30 ### 0-30
cropshat<-seq(0,200, length=100) ###10-130
childrenhat<-50 ###0-101
deathshat<- 100 ###0-200
citysizehat<-1400  ### 500-2500
wondershat<- 60  ###0-120
chiefChieftesshat=0

shrinehat<-slopes[1]+
  slopes[2]*log(rainhat)+
  slopes[3]*log(cropshat)+
  slopes[4]*log(childrenhat)+
  slopes[5]*deathshat+
  slopes[6]*log(citysizehat)+
  slopes[7]*I(wondershat^2)+
  slopes[8]*chiefChieftesshat+
  slopes[9]*wondershat+
  slopes[10]*I(wondershat^3)+
  slopes[11]*log(rainhat)*log(cropshat)+
  slopes[12]*log(childrenhat)*deathshat+
  slopes[13]*I(wondershat^2)*chiefChieftesshat+
  slopes[14]*chiefChieftesshat*I(wondershat^3)
lines(shrinehat~cropshat,col="blue",lwd=5)


legend("topleft",col=c("red","yellow","green","purple","blue"),legend=c("rains=5","rains=10","rains=15","rains=20","rains=30"),lty=1, lwd=3)





####citysize
plot(shrine~citysize,data=wow)
rainhat<-15 ### 0-30
cropshat<-30 ###10-130
childrenhat<-50 ###0-101
deathshat<- 100 ###0-200
citysizehat<-seq(500,2500, length=100)  ### 500-2500
wondershat<- 60  ###0-120
chiefChieftesshat=0

shrinehat<-slopes[1]+
  slopes[2]*log(rainhat)+
  slopes[3]*log(cropshat)+
  slopes[4]*log(childrenhat)+
  slopes[5]*deathshat+
  slopes[6]*log(citysizehat)+
  slopes[7]*I(wondershat^2)+
  slopes[8]*chiefChieftesshat+
  slopes[9]*wondershat+
  slopes[10]*I(wondershat^3)+
  slopes[11]*log(rainhat)*log(cropshat)+
  slopes[12]*log(childrenhat)*deathshat+
  slopes[13]*I(wondershat^2)*chiefChieftesshat+
  slopes[14]*chiefChieftesshat*I(wondershat^3)
lines(shrinehat~citysizehat,col="red",lwd=5)

plot(shrine~citysize,data=wow)
rainhat<-15 ### 0-30
cropshat<-30 ###10-130
childrenhat<-50 ###0-101
deathshat<- 100 ###0-200
citysizehat<-seq(500,2500, length=100)  ### 500-2500
wondershat<- 60  ###0-120
chiefChieftesshat=1

shrinehat<-slopes[1]+
  slopes[2]*log(rainhat)+
  slopes[3]*log(cropshat)+
  slopes[4]*log(childrenhat)+
  slopes[5]*deathshat+
  slopes[6]*log(citysizehat)+
  slopes[7]*I(wondershat^2)+
  slopes[8]*chiefChieftesshat+
  slopes[9]*wondershat+
  slopes[10]*I(wondershat^3)+
  slopes[11]*log(rainhat)*log(cropshat)+
  slopes[12]*log(childrenhat)*deathshat+
  slopes[13]*I(wondershat^2)*chiefChieftesshat+
  slopes[14]*chiefChieftesshat*I(wondershat^3)
lines(shrinehat~citysizehat,col="yellow",lwd=5)



legend("topleft",col=c("red","yellow"),legend=c("chief","chiefteness"),lty=1, lwd=3)
