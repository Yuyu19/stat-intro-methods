uu<-read.table("http://www.uwyo.edu/crawford/methods/final%20project/yuyufan.txt", header= TRUE)
plot(uu$Enemy~uu$Stock)
hist(yuyu$FirstAid)
uu<-na.omit(uu)
yuyu<-uu[uu$Enemy>0, ]
yuyu<-yuyu[yuyu$FirstAid<8e+07, ]
levels(yuyu$Terrorism)<-c("High","High","Low","Medium")
levels(yuyu$IG88)<-c("None","Installed","None")
plot(Enemy~Stock, data= yuyu,col=Terrorism)
plot(Enemy~Media, data= yuyu,col=Terrorism)

plot(Enemy~Firepower, data= yuyu,col=Terrorism)
plot(Enemy~Bombs, data= yuyu,col=Terrorism)

plot(Enemy~Payload, data= yuyu,col=Terrorism)
plot(Enemy~Weapons, data= yuyu,col=Terrorism)
plot(Enemy~FirstAid, data= yuyu,col=Terrorism)
plot(Enemy~Temperature, data= yuyu,col=Terrorism)

plot(Enemy~Missiles, data= yuyu,col=Terrorism)
plot(Enemy~Napalm, data= yuyu,col=Terrorism)

plot(Enemy~Spies, data= yuyu,col=Terrorism)
plot(Enemy~Personnel, data= yuyu,col=Terrorism)


plot(Enemy~IG88, data= yuyu,col=Terrorism)
plot(Enemy~Terrorism, data= yuyu,col=Terrorism)
hist(yuyu$Enemy)

#### bad model
fit<- lm(Enemy~
           log(Firepower)*log(Bombs)+
           Weapons+I(Weapons^2)+ 
           Missiles * Napalm +
           Media+
           Personnel+I(Personnel^2)+I(Personnel^3)+
           Temperature+
           IG88, data=yuyu )
Enemyhat<-slopes[1]+
  slopes[2]*log(Firepowerhat)+
  slopes[3]*log(Bombshat)+
  slopes[4]*Weaponshat+
  slopes[5]*I(Weaponshat^2)+
  slopes[6]*Missileshat+
  slopes[7]*Napalmhat+
  slopes[8]*Mediahat+
  slopes[9]*Personnelhat+
  slopes[10]*I(Personnelhat^2)+
  slopes[11]*I(Personnelhat^3)+
  slopes[12]*Temperaturehat+
  slopes[13]*IG88Installedhat+
  slopes[14]*log(Firepowerhat)*log(Bombshat)+
  slopes[15]*Missileshat*Napalmhat      ### enemyhat <- 3980.65      3923  49.77 

### good one

fit<-lm(Enemy ~ 
          Weapons + I(Weapons^2) + Missiles * Napalm
        + Media + Personnel + I(Personnel^2) + I(Personnel^3) + Temperature +
     IG88, data = yuyu)
summary(fit)
plot(fit)
confint(fit)
summary(fit)
plot(fit)
plot(fit$residuals~Stock, data= yuyu)
plot(fit$residuals~Terrorism, data= yuyu)
plot(fit$residuals~Firepower, data= yuyu)  ### looks like bomb
plot(fit$residuals~Payload, data= yuyu)
plot(fit$residuals~Weapons, data= yuyu)
plot(fit$residuals~Bombs, data= yuyu)
plot(fit$residuals~Missiles, data= yuyu) ###bowtie
plot(fit$residuals~FirstAid, data= yuyu)
plot(fit$residuals~Spies, data= yuyu)### looks like personeel
plot(fit$residuals~Media, data= yuyu) ### bowtie
plot(fit$residuals~Personnel, data= yuyu) ### cubic
plot(fit$residuals~Temperature, data= yuyu)
plot(fit$residuals~Napalm, data= yuyu) ### bowtie
plot(fit$residuals~IG88, data= yuyu)




slopes<-fit$coefficients
cbind(seq(1:length(slopes)),slopes)

Mediahat<-0.9905
Weaponshat<-255
 Spieshat<-9
 Stockhat<-903.5
Payloadhat<-2762.2575
 TerrorismLowhat<-0
TerrorismMediumhat<-0
Firepowerhat<-1.13
Bombshat<-1308
Personnelhat<-29
Missileshat<-63
Napalmhat<-28.42

Enemyhat<-slopes[1]+
  slopes[2]*Weaponshat+
  slopes[3]*I(Weaponshat^2)+
  slopes[4]*Missileshat+
  slopes[5]*Napalmhat+
  slopes[6]*Mediahat+
  slopes[7]*Personnelhat+
  slopes[8]*I(Personnelhat^2)+
  slopes[9]*I(Personnelhat^3)+
  slopes[10]*Temperaturehat+
  slopes[11]*IG88Installedhat+
  slopes[12]*Missileshat*Napalmhat  



Mediahat<-0.5 ###0-1
Weaponshat<-seq(100,300, length=1000) ### 100-300
Spieshat<- 11 ###0-22
Stockhat<-1300 ### 900-1500
Payloadhat<-2762.2575  ### 0-4500
TerrorismLowhat<-0 
TerrorismMediumhat<-0
Firepowerhat<-1.7  ### 0-2.5
Bombshat<-5000 ### 0-10000
Personnelhat<-29  ##26-34
Missileshat<-50   ###20-80
Napalmhat<-25   ###10-40

plot(Enemy~Weapons, data=yuyu)
Enemyhat<-slopes[1]+
  slopes[2]*Weaponshat+
  slopes[3]*I(Weaponshat^2)+
  slopes[4]*Missileshat+
  slopes[5]*Napalmhat+
  slopes[6]*Mediahat+
  slopes[7]*Personnelhat+
  slopes[8]*I(Personnelhat^2)+
  slopes[9]*I(Personnelhat^3)+
  slopes[10]*Temperaturehat+
  slopes[11]*IG88Installedhat+
  slopes[12]*Missileshat*Napalmhat 
  plot(Enemyhat~Weaponshat,col="red",lwd=3)

  
  
  ###Missileshat
  plot(Enemy~Missiles, data=yuyu)
  Mediahat<-0.5 ###0-1
  Weaponshat<-200 ### 100-300
  Spieshat<- 11 ###0-22
  Stockhat<-1300 ### 900-1500
  Payloadhat<-2762.2575  ### 0-4500
  TerrorismLowhat<-0 
  TerrorismMediumhat<-0
  Firepowerhat<-1.7  ### 0-2.5
  Bombshat<-5000 ### 0-10000
  Personnelhat<-29  ##26-34
  Missileshat<-seq(20,80, length=1000)   ###20-80
  Napalmhat<-10   ###10-40
  
  Enemyhat<-slopes[1]+
    slopes[2]*Weaponshat+
    slopes[3]*I(Weaponshat^2)+
    slopes[4]*Missileshat+
    slopes[5]*Napalmhat+
    slopes[6]*Mediahat+
    slopes[7]*Personnelhat+
    slopes[8]*I(Personnelhat^2)+
    slopes[9]*I(Personnelhat^3)+
    slopes[10]*Temperaturehat+
    slopes[11]*IG88Installedhat+
    slopes[12]*Missileshat*Napalmhat 
  lines(Enemyhat~Missileshat,col="red",lwd=3)
  

  Napalmhat<-20   ###10-40
  
  Enemyhat<-slopes[1]+
    slopes[2]*Weaponshat+
    slopes[3]*I(Weaponshat^2)+
    slopes[4]*Missileshat+
    slopes[5]*Napalmhat+
    slopes[6]*Mediahat+
    slopes[7]*Personnelhat+
    slopes[8]*I(Personnelhat^2)+
    slopes[9]*I(Personnelhat^3)+
    slopes[10]*Temperaturehat+
    slopes[11]*IG88Installedhat+
    slopes[12]*Missileshat*Napalmhat 
  lines(Enemyhat~Missileshat,col="orange",lwd=3)
  
  Napalmhat<-30   ###10-40
  
  Enemyhat<-slopes[1]+
    slopes[2]*Weaponshat+
    slopes[3]*I(Weaponshat^2)+
    slopes[4]*Missileshat+
    slopes[5]*Napalmhat+
    slopes[6]*Mediahat+
    slopes[7]*Personnelhat+
    slopes[8]*I(Personnelhat^2)+
    slopes[9]*I(Personnelhat^3)+
    slopes[10]*Temperaturehat+
    slopes[11]*IG88Installedhat+
    slopes[12]*Missileshat*Napalmhat 
  lines(Enemyhat~Missileshat,col="yellow",lwd=3)
  
  
  Napalmhat<-40   ###10-40
  
  Enemyhat<-slopes[1]+
    slopes[2]*Weaponshat+
    slopes[3]*I(Weaponshat^2)+
    slopes[4]*Missileshat+
    slopes[5]*Napalmhat+
    slopes[6]*Mediahat+
    slopes[7]*Personnelhat+
    slopes[8]*I(Personnelhat^2)+
    slopes[9]*I(Personnelhat^3)+
    slopes[10]*Temperaturehat+
    slopes[11]*IG88Installedhat+
    slopes[12]*Missileshat*Napalmhat 
  lines(Enemyhat~Missileshat,col="green",lwd=3)
  
  legend("bottomleft",col=c("red","orange","yellow","green"),legend=c("Napalm=10","Napalm=20","Napalm=30","Napalm=40"),lty=1)


  
  ###Napalmhat
  plot(Enemy~Napalm, data=yuyu)
  Mediahat<-0.5 ###0-1
  Weaponshat<-200 ### 100-300
  Spieshat<- 11 ###0-22
  Stockhat<-1300 ### 900-1500
  Payloadhat<-2762.2575  ### 0-4500
  TerrorismLowhat<-0 
  TerrorismMediumhat<-0
  Firepowerhat<-1.7  ### 0-2.5
  Bombshat<-5000 ### 0-10000
  Personnelhat<-29  ##26-34
  Missileshat<-20   ###20-80
   Napalmhat<-seq(10,40, length=1000)   ###10-40
   Enemyhat<-slopes[1]+
         slopes[2]*Weaponshat+
       slopes[3]*I(Weaponshat^2)+
         slopes[4]*Missileshat+
         slopes[5]*Napalmhat+
         slopes[6]*Mediahat+
         slopes[7]*Personnelhat+
         slopes[8]*I(Personnelhat^2)+
         slopes[9]*I(Personnelhat^3)+
         slopes[10]*Temperaturehat+
         slopes[11]*IG88Installedhat+
         slopes[12]*Missileshat*Napalmhat
   lines(Enemyhat~Napalmhat,col="red",lwd=3)
   
   Missileshat<-40   ###20-80
   Napalmhat<-seq(10,40, length=1000)   ###10-40
   Enemyhat<-slopes[1]+
     slopes[2]*Weaponshat+
     slopes[3]*I(Weaponshat^2)+
     slopes[4]*Missileshat+
     slopes[5]*Napalmhat+
     slopes[6]*Mediahat+
     slopes[7]*Personnelhat+
     slopes[8]*I(Personnelhat^2)+
     slopes[9]*I(Personnelhat^3)+
     slopes[10]*Temperaturehat+
     slopes[11]*IG88Installedhat+
     slopes[12]*Missileshat*Napalmhat
   lines(Enemyhat~Napalmhat,col="orange",lwd=3)
   
   Missileshat<-60   ###20-80
   Napalmhat<-seq(10,40, length=1000)   ###10-40
   Enemyhat<-slopes[1]+
     slopes[2]*Weaponshat+
     slopes[3]*I(Weaponshat^2)+
     slopes[4]*Missileshat+
     slopes[5]*Napalmhat+
     slopes[6]*Mediahat+
     slopes[7]*Personnelhat+
     slopes[8]*I(Personnelhat^2)+
     slopes[9]*I(Personnelhat^3)+
     slopes[10]*Temperaturehat+
     slopes[11]*IG88Installedhat+
     slopes[12]*Missileshat*Napalmhat
   lines(Enemyhat~Napalmhat,col="yellow",lwd=3)
   
   Missileshat<-80   ###20-80
   Napalmhat<-seq(10,40, length=1000)   ###10-40
   Enemyhat<-slopes[1]+
     slopes[2]*Weaponshat+
     slopes[3]*I(Weaponshat^2)+
     slopes[4]*Missileshat+
     slopes[5]*Napalmhat+
     slopes[6]*Mediahat+
     slopes[7]*Personnelhat+
     slopes[8]*I(Personnelhat^2)+
     slopes[9]*I(Personnelhat^3)+
     slopes[10]*Temperaturehat+
     slopes[11]*IG88Installedhat+
     slopes[12]*Missileshat*Napalmhat
   lines(Enemyhat~Napalmhat,col="green",lwd=3)
  
   
   legend("bottomleft",col=c("red","orange","yellow","green"),legend=c("Missiles=20","Missiles=40","Missiles=60","Missiles=80"),lty=1)
  
  
   ###Media
   plot(Enemy~Media, data=yuyu)
 
   Enemyhat<-slopes[1]+
     slopes[2]*Weaponshat+
     slopes[3]*I(Weaponshat^2)+
     slopes[4]*Missileshat+
     slopes[5]*Napalmhat+
     slopes[6]*Mediahat+
     slopes[7]*Personnelhat+
     slopes[8]*I(Personnelhat^2)+
     slopes[9]*I(Personnelhat^3)+
     slopes[10]*Temperaturehat+
     slopes[11]*IG88Installedhat+
     slopes[12]*Missileshat*Napalmhat
   plot(Enemyhat~Mediahat,col="red",lwd=3)

   
   ###Personnel
   plot(Enemy~Personnel, data=yuyu)
   Mediahat<-0.5 ###0-1
   Weaponshat<-200 ### 100-300
   Spieshat<- 11 ###0-22
   Stockhat<-1300 ### 900-1500
   Payloadhat<-2762.2575  ### 0-4500
   TerrorismLowhat<-0 
   TerrorismMediumhat<-0
   Firepowerhat<-1.7  ### 0-2.5
   Bombshat<-5000 ### 0-10000
   Personnelhat<-seq(26,34, length=1000)  ##26-34
   Missileshat<-50   ###20-80
   Napalmhat<-25   ###10-40
   Enemyhat<-slopes[1]+
     slopes[2]*Weaponshat+
     slopes[3]*I(Weaponshat^2)+
     slopes[4]*Missileshat+
     slopes[5]*Napalmhat+
     slopes[6]*Mediahat+
     slopes[7]*Personnelhat+
     slopes[8]*I(Personnelhat^2)+
     slopes[9]*I(Personnelhat^3)+
     slopes[10]*Temperaturehat+
     slopes[11]*IG88Installedhat+
     slopes[12]*Missileshat*Napalmhat
   plot(Enemyhat~Personnelhat,col="red",lwd=3)
   
   
   ###Personnel
   plot(Enemy~Temperature, data=yuyu)
   Mediahat<-0.5 ###0-1
   Weaponshat<-200 ### 100-300
   Spieshat<- 11 ###0-22
   Stockhat<-1300 ### 900-1500
   Payloadhat<-2762.2575  ### 0-4500
   TerrorismLowhat<-0 
   TerrorismMediumhat<-0
   Firepowerhat<-1.7  ### 0-2.5
   Bombshat<-5000 ### 0-10000
   Personnelhat<-30  ##26-34
   Missileshat<-50   ###20-80
   Napalmhat<-25   ###10-40
   Temperaturehat<- seq(-150,200, length=1000) ## -150-200   
     Enemyhat<-slopes[1]+
     slopes[2]*Weaponshat+
     slopes[3]*I(Weaponshat^2)+
     slopes[4]*Missileshat+
     slopes[5]*Napalmhat+
     slopes[6]*Mediahat+
     slopes[7]*Personnelhat+
     slopes[8]*I(Personnelhat^2)+
     slopes[9]*I(Personnelhat^3)+
     slopes[10]*Temperaturehat+
     slopes[11]*IG88Installedhat+
     slopes[12]*Missileshat*Napalmhat
   lines(Enemyhat~Temperaturehat,col="red",lwd=3)



####civilian
   hist(yuyu$Civilian)
  yuyu<-yuyu[yuyu$Civilian<1500, ]
fit<-lm( log(Civilian)~Stock+Terrorism+sin(Firepower)+Payload+Weapons*Spies+sin(Bombs)+Spies*sin(Bombs)+ Spies*Personnel+Missiles 
         +FirstAid +  Media+ Personnel+ Temperature+ Napalm++I(Napalm^2)+I(Napalm^3)+IG88,data=yuyu)
summary(fit)
fit<-lm(log(Civilian)~
          
          Weapons*Spies
        
        + Stock+Spies*Payload +Terrorism + sin(Firepower)+Spies*sin(Bombs)+ Spies*Personnel+Spies*Missiles+Napalm
        +I(Napalm^2)+I(Napalm^3)
        ,data=yuyu)
plot(fit$residuals~Stock, data= yuyu)

plot(fit$residuals~Terrorism, data= yuyu)
plot(fit$residuals~Firepower, data= yuyu)  ### 

plot(fit$residuals~Weapons, data= yuyu) ##bowtie  fold
plot(fit$residuals~sin(Bombs), data= yuyu)
plot(fit$residuals~Missiles, data= yuyu) ###

plot(fit$residuals~Payload, data= yuyu)
plot(fit$residuals~FirstAid, data= yuyu)
plot(fit$residuals~Spies, data= yuyu)### bowtie
plot(fit$residuals~Media, data= yuyu) ###  bowtie
plot(fit$residuals~Personnel, data= yuyu) ### 
plot(fit$residuals~Temperature, data= yuyu)
plot(fit$residuals~Napalm, data= yuyu) ### 
plot(fit$residuals~IG88, data= yuyu)


plot(Civilian~Stock, data= yuyu,col=Terrorism)
plot(Civilian~Media, data= yuyu,col=Terrorism)

plot(Civilian~Firepower, data= yuyu,col=Terrorism)
plot(Civilian~Bombs, data= yuyu,col=Terrorism)

plot(Civilian~Payload, data= yuyu,col=Terrorism)
plot(Civilian~Weapons, data= yuyu,col=Terrorism)
plot(Civilian~FirstAid, data= yuyu,col=Terrorism)
plot(Civilian~Temperature, data= yuyu,col=Terrorism)

plot(Civilian~Missiles, data= yuyu,col=Terrorism)
plot(Civilian~Napalm, data= yuyu,col=Terrorism)

plot(Civilian~Spies, data= yuyu,col=Terrorism)
plot(Civilian~Personnel, data= yuyu,col=Terrorism)


plot(Civilian~IG88, data= yuyu,col=Terrorism)
plot(Civilian~Terrorism, data= yuyu,col=Terrorism)

plot(log(Civilian)~Stock, data= yuyu)
plot(log(Civilian)~Terrorism, data= yuyu)
plot(log(Civilian)~Firepower, data= yuyu)
plot(log(Civilian)~Payload, data= yuyu)
plot(log(Civilian)~Weapons, data= yuyu)
plot(log(Civilian)~Bombs, data= yuyu)
plot(log(Civilian)~Missiles, data= yuyu)
plot(log(Civilian)~FirstAid, data= yuyu)
plot(log(Civilian)~Spies, data= yuyu)
plot(log(Civilian)~Media, data= yuyu)
plot(log(Civilian)~Personnel, data= yuyu)
plot(log(Civilian)~Temperature, data= yuyu)
plot(log(Civilian)~Napalm, data= yuyu)
plot(log(Civilian)~IG88, data= yuyu)


fit<-lm(Civilian~
          Payload+    
          Weapons*Spies+I(Weapons^2)+
          
          sin(Spies)+I(Spies^2)+I(Spies^3)+I(Spies^4)+
          +I(Weapons^2)*Spies+I(Weapons^2)*I(Spies^2)+I(Weapons^2)*I(Spies^3)+I(Weapons^2)*I(Spies^4)+
          Weapons*I(Spies^2)+Weapons*I(Spies^3)+Weapons*I(Spies^4)+
          Spies+I(Spies^2)+I(Spies^3)+I(Spies^4)+
          Personnel*Weapons
        +I(Spies^4)*Personnel+I(Spies^3)*Personnel+I(Spies^2)*Personnel,data=yuyu)



fit2<-lm(log(Civilian)~
        
          Weapons*Spies
         
          + Stock+Spies*Payload +Terrorism + sin(Firepower)+Spies*sin(Bombs)+ Spies*Personnel+Spies*Missiles+Napalm
          +I(Napalm^2)+I(Napalm^3)
          ,data=yuyu)
confint(fit2)
summary(fit2)

plot(Missiles~Personnel, data=yuyu)
plot(fit2)
plot(fit2$residuals~Stock, data= yuyu)

plot(fit2$residuals~Terrorism, data= yuyu)
plot(fit2$residuals~sin(Firepower), data= yuyu)  ### 

plot(fit2$residuals~Weapons, data= yuyu) ##bowtie  fold
plot(fit2$residuals~sin(Bombs), data= yuyu)
plot(fit2$residuals~Missiles, data= yuyu) ###

plot(fit2$residuals~Payload, data= yuyu)
plot(fit2$residuals~FirstAid, data= yuyu)
plot(fit2$residuals~Spies, data= yuyu)### bowtie
plot(fit2$residuals~Media, data= yuyu) ###  bowtie
plot(fit2$residuals~Personnel, data= yuyu) ### 
plot(fit2$residuals~Temperature, data= yuyu)
plot(fit2$residuals~Napalm, data= yuyu) ### 
plot(fit2$residuals~IG88, data= yuyu)


fit2<-lm(log(Civilian)~
          
          Weapons*Spies
        
        + Stock+Spies*Payload +Terrorism + sin(Firepower)+Spies*sin(Bombs)+Spies*Bombs+ Spies*Personnel+Spies*Missiles+Napalm
        +I(Napalm^2)+I(Napalm^3)
        ,data=yuyu)

slopes<-fit2$coefficients
cbind(seq(1:length(slopes)),slopes)

Weaponshat<-255
Spieshat<-9
Stockhat<-903.5
Payloadhat<-2762.2575
TerrorismLowhat<-0
TerrorismMediumhat<-0
Firepowerhat<-1.13
Bombshat<-1308
Personnelhat<-29
Missileshat<-63
Napalmhat<-28.42


plot(Civilia)

Civilianhat<-exp(slopes[1]+
  slopes[2]*Weaponshat+
  slopes[3]* Spieshat+
  slopes[4]*Stockhat+
  slopes[5]*Payloadhat + 
  slopes[6]*TerrorismLowhat+
  slopes[7]*TerrorismMediumhat+
  slopes[8]*sin(Firepowerhat)+
  slopes[9]*sin(Bombshat)+
    slopes[10]*Bombshat+  
  slopes[11]*Personnelhat+
  slopes[12]*Missileshat+
  slopes[13]*Napalmhat+
  slopes[14]*I(Napalmhat^2)+
  slopes[15]*I(Napalmhat^3)+
  slopes[16]*Weaponshat*Spieshat+
  slopes[17]*Spieshat*Payloadhat+
  slopes[18]*Spieshat*sin(Bombshat)+
    slopes[19]*Spieshat*Bombshat+
  slopes[20]*Spieshat*Personnelhat+
  slopes[21]*Spieshat*Missileshat)
####  -64.58954 
log(66.55158)-log(67 )        0.006715333




###weapons
plot(Civilian~Weapons,data=yuyu)
Mediahat<-0.5 ###0-1
Weaponshat<-seq(100,300, length=1000) ### 100-300
Spieshat<- 5 ###0-22
Stockhat<-1300 ### 900-1500
Payloadhat<-2762.2575  ### 0-4500
TerrorismLowhat<-0 
TerrorismMediumhat<-0
Firepowerhat<-1.7  ### 0-2.5
Bombshat<-5000 ### 0-10000
Personnelhat<-29  ##26-34
Missileshat<-20   ###20-80
Napalmhat<-25   ###10-40
Civilianhat<-exp(7.553007e+00+
                   1.150814e-03*Weaponshat+
                   slopes[3]* Spieshat+
                   slopes[4]*Stockhat+
                   slopes[5]*Payloadhat + 
                   slopes[6]*TerrorismLowhat+
                   slopes[7]*TerrorismMediumhat+
                   slopes[8]*sin(Firepowerhat)+
                   slopes[9]*sin(Bombshat)+
                   slopes[10]*Bombshat+  
                   slopes[11]*Personnelhat+
                   slopes[12]*Missileshat+
                   slopes[13]*Napalmhat+
                   slopes[14]*I(Napalmhat^2)+
                   slopes[15]*I(Napalmhat^3)+
                   slopes[16]*Weaponshat*Spieshat+
                   slopes[17]*Spieshat*Payloadhat+
                   slopes[18]*Spieshat*sin(Bombshat)+
                   slopes[19]*Spieshat*Bombshat+
                   slopes[20]*Spieshat*Personnelhat+
                   slopes[21]*Spieshat*Missileshat)
lines(Civilianhat~Weaponshat,data=yuyu,col="red",lwd=3)

Spieshat<- 10 ###0-22
Civilianhat<-exp(7.553007e+00+
                   1.150814e-03*Weaponshat+
                   slopes[3]* Spieshat+
                   slopes[4]*Stockhat+
                   slopes[5]*Payloadhat + 
                   slopes[6]*TerrorismLowhat+
                   slopes[7]*TerrorismMediumhat+
                   slopes[8]*sin(Firepowerhat)+
                   slopes[9]*sin(Bombshat)+
                   slopes[10]*Bombshat+  
                   slopes[11]*Personnelhat+
                   slopes[12]*Missileshat+
                   slopes[13]*Napalmhat+
                   slopes[14]*I(Napalmhat^2)+
                   slopes[15]*I(Napalmhat^3)+
                   slopes[16]*Weaponshat*Spieshat+
                   slopes[17]*Spieshat*Payloadhat+
                   slopes[18]*Spieshat*sin(Bombshat)+
                   slopes[19]*Spieshat*Bombshat+
                   slopes[20]*Spieshat*Personnelhat+
                   slopes[21]*Spieshat*Missileshat)
lines(Civilianhat~Weaponshat,data=yuyu,col="orange",lwd=3)


Spieshat<- 15 ###0-22
Civilianhat<-exp(7.553007e+00+
                   1.150814e-03*Weaponshat+
                   slopes[3]* Spieshat+
                   slopes[4]*Stockhat+
                   slopes[5]*Payloadhat + 
                   slopes[6]*TerrorismLowhat+
                   slopes[7]*TerrorismMediumhat+
                   slopes[8]*sin(Firepowerhat)+
                   slopes[9]*sin(Bombshat)+
                   slopes[10]*Bombshat+  
                   slopes[11]*Personnelhat+
                   slopes[12]*Missileshat+
                   slopes[13]*Napalmhat+
                   slopes[14]*I(Napalmhat^2)+
                   slopes[15]*I(Napalmhat^3)+
                   slopes[16]*Weaponshat*Spieshat+
                   slopes[17]*Spieshat*Payloadhat+
                   slopes[18]*Spieshat*sin(Bombshat)+
                   slopes[19]*Spieshat*Bombshat+
                   slopes[20]*Spieshat*Personnelhat+
                   slopes[21]*Spieshat*Missileshat)
lines(Civilianhat~Weaponshat,data=yuyu,col="yellow",lwd=3)

Spieshat<- 20 ###0-22
Civilianhat<-exp(7.553007e+00+
                   1.150814e-03*Weaponshat+
                   slopes[3]* Spieshat+
                   slopes[4]*Stockhat+
                   slopes[5]*Payloadhat + 
                   slopes[6]*TerrorismLowhat+
                   slopes[7]*TerrorismMediumhat+
                   slopes[8]*sin(Firepowerhat)+
                   slopes[9]*sin(Bombshat)+
                   slopes[10]*Bombshat+  
                   slopes[11]*Personnelhat+
                   slopes[12]*Missileshat+
                   slopes[13]*Napalmhat+
                   slopes[14]*I(Napalmhat^2)+
                   slopes[15]*I(Napalmhat^3)+
                   slopes[16]*Weaponshat*Spieshat+
                   slopes[17]*Spieshat*Payloadhat+
                   slopes[18]*Spieshat*sin(Bombshat)+
                   slopes[19]*Spieshat*Bombshat+
                   slopes[20]*Spieshat*Personnelhat+
                   slopes[21]*Spieshat*Missileshat)
lines(Civilianhat~Weaponshat,data=yuyu,col="green",lwd=3)
legend("topright",col=c("red","orange","yellow","green"),legend=c("Spies=5","Spies=10","Spies=15","Spies=20"),lty=1,lwd=2)




###spies
plot(Civilian~Spies,data=yuyu)
Mediahat<-0.5 ###0-1
Weaponshat<-100 ### 100-300
Spieshat<- seq(0,22, length=1000) ###0-22
Stockhat<-1300 ### 900-1500
Payloadhat<-2762.2575  ### 0-4500
TerrorismLowhat<-0 
TerrorismMediumhat<-0
Firepowerhat<-1.7  ### 0-2.5
Bombshat<-5000 ### 0-10000
Personnelhat<-29  ##26-34
Missileshat<-20   ###20-80
Napalmhat<-25   ###10-40
Civilianhat<-exp(7.553007e+00+
                   1.150814e-03*Weaponshat+
                   slopes[3]* Spieshat+
                   slopes[4]*Stockhat+
                   slopes[5]*Payloadhat + 
                   slopes[6]*TerrorismLowhat+
                   slopes[7]*TerrorismMediumhat+
                   slopes[8]*sin(Firepowerhat)+
                   slopes[9]*sin(Bombshat)+
                   slopes[10]*Bombshat+  
                   slopes[11]*Personnelhat+
                   slopes[12]*Missileshat+
                   slopes[13]*Napalmhat+
                   slopes[14]*I(Napalmhat^2)+
                   slopes[15]*I(Napalmhat^3)+
                   slopes[16]*Weaponshat*Spieshat+
                   slopes[17]*Spieshat*Payloadhat+
                   slopes[18]*Spieshat*sin(Bombshat)+
                   slopes[19]*Spieshat*Bombshat+
                   slopes[20]*Spieshat*Personnelhat+
                   slopes[21]*Spieshat*Missileshat)
lines(Civilianhat~Spieshat,data=yuyu,col="red",lwd=3)

Weaponshat<-300 ### 100-300
 Civilianhat<-exp(7.553007e+00+
                                        1.150814e-03*Weaponshat+
                                        slopes[3]* Spieshat+
                                         slopes[4]*Stockhat+
                                         slopes[5]*Payloadhat + 
                                         slopes[6]*TerrorismLowhat+
                                         slopes[7]*TerrorismMediumhat+
                                         slopes[8]*sin(Firepowerhat)+
                                         slopes[9]*sin(Bombshat)+
                                         slopes[10]*Bombshat+  
                                         slopes[11]*Personnelhat+
                                         slopes[12]*Missileshat+
                                         slopes[13]*Napalmhat+
                                         slopes[14]*I(Napalmhat^2)+
                                         slopes[15]*I(Napalmhat^3)+
                                         slopes[16]*Weaponshat*Spieshat+
                                         slopes[17]*Spieshat*Payloadhat+
                                         slopes[18]*Spieshat*sin(Bombshat)+
                                         slopes[19]*Spieshat*Bombshat+
                                         slopes[20]*Spieshat*Personnelhat+
                                         slopes[21]*Spieshat*Missileshat)
lines(Civilianhat~Spieshat,data=yuyu,col="yellow",lwd=3)

legend("topright",col=c("red","orange","yellow"),legend=c("Weapons=100","Weapons=200","Weapons=300"),lty=1,lwd=2)




### vspies~personnel
plot(Civilian~Spies,data=yuyu)
Mediahat<-0.5 ###0-1
Weaponshat<-250 ### 100-300
Spieshat<- seq(0,22, length=1000) ###0-22
Stockhat<-1300 ### 900-1500
Payloadhat<-2762.2575  ### 0-4500
TerrorismLowhat<-0 
TerrorismMediumhat<-0
Firepowerhat<-1.7  ### 0-2.5
Bombshat<-5000 ### 0-10000
Personnelhat<-26  ##26-34
Missileshat<-20   ###20-80
Napalmhat<-26   ###10-40
Civilianhat<-exp(7.553007e+00+
                   1.150814e-03*Weaponshat+
                   slopes[3]* Spieshat+
                   slopes[4]*Stockhat+
                   slopes[5]*Payloadhat + 
                   slopes[6]*TerrorismLowhat+
                   slopes[7]*TerrorismMediumhat+
                   slopes[8]*sin(Firepowerhat)+
                   slopes[9]*sin(Bombshat)+
                   slopes[10]*Bombshat+  
                   slopes[11]*Personnelhat+
                   slopes[12]*Missileshat+
                   slopes[13]*Napalmhat+
                   slopes[14]*I(Napalmhat^2)+
                   slopes[15]*I(Napalmhat^3)+
                   slopes[16]*Weaponshat*Spieshat+
                   slopes[17]*Spieshat*Payloadhat+
                   slopes[18]*Spieshat*sin(Bombshat)+
                   slopes[19]*Spieshat*Bombshat+
                   slopes[20]*Spieshat*Personnelhat+
                   slopes[21]*Spieshat*Missileshat)
lines(Civilianhat~Spieshat,data=yuyu,col="red",lwd=3)


Personnelhat<-28  ##26-34
Civilianhat<-exp(7.553007e+00+
                   1.150814e-03*Weaponshat+
                   slopes[3]* Spieshat+
                   slopes[4]*Stockhat+
                   slopes[5]*Payloadhat + 
                   slopes[6]*TerrorismLowhat+
                   slopes[7]*TerrorismMediumhat+
                   slopes[8]*sin(Firepowerhat)+
                   slopes[9]*sin(Bombshat)+
                   slopes[10]*Bombshat+  
                   slopes[11]*Personnelhat+
                   slopes[12]*Missileshat+
                   slopes[13]*Napalmhat+
                   slopes[14]*I(Napalmhat^2)+
                   slopes[15]*I(Napalmhat^3)+
                   slopes[16]*Weaponshat*Spieshat+
                   slopes[17]*Spieshat*Payloadhat+
                   slopes[18]*Spieshat*sin(Bombshat)+
                   slopes[19]*Spieshat*Bombshat+
                   slopes[20]*Spieshat*Personnelhat+
                   slopes[21]*Spieshat*Missileshat)
lines(Civilianhat~Spieshat,data=yuyu,col="orange",lwd=1)

Personnelhat<-30  ##26-34
Civilianhat<-exp(7.553007e+00+
                   1.150814e-03*Weaponshat+
                   slopes[3]* Spieshat+
                   slopes[4]*Stockhat+
                   slopes[5]*Payloadhat + 
                   slopes[6]*TerrorismLowhat+
                   slopes[7]*TerrorismMediumhat+
                   slopes[8]*sin(Firepowerhat)+
                   slopes[9]*sin(Bombshat)+
                   slopes[10]*Bombshat+  
                   slopes[11]*Personnelhat+
                   slopes[12]*Missileshat+
                   slopes[13]*Napalmhat+
                   slopes[14]*I(Napalmhat^2)+
                   slopes[15]*I(Napalmhat^3)+
                   slopes[16]*Weaponshat*Spieshat+
                   slopes[17]*Spieshat*Payloadhat+
                   slopes[18]*Spieshat*sin(Bombshat)+
                   slopes[19]*Spieshat*Bombshat+
                   slopes[20]*Spieshat*Personnelhat+
                   slopes[21]*Spieshat*Missileshat)
lines(Civilianhat~Spieshat,data=yuyu,col="green",lwd=1)

Personnelhat<-32  ##26-34
Civilianhat<-exp(7.553007e+00+
                   1.150814e-03*Weaponshat+
                   slopes[3]* Spieshat+
                   slopes[4]*Stockhat+
                   slopes[5]*Payloadhat + 
                   slopes[6]*TerrorismLowhat+
                   slopes[7]*TerrorismMediumhat+
                   slopes[8]*sin(Firepowerhat)+
                   slopes[9]*sin(Bombshat)+
                   slopes[10]*Bombshat+  
                   slopes[11]*Personnelhat+
                   slopes[12]*Missileshat+
                   slopes[13]*Napalmhat+
                   slopes[14]*I(Napalmhat^2)+
                   slopes[15]*I(Napalmhat^3)+
                   slopes[16]*Weaponshat*Spieshat+
                   slopes[17]*Spieshat*Payloadhat+
                   slopes[18]*Spieshat*sin(Bombshat)+
                   slopes[19]*Spieshat*Bombshat+
                   slopes[20]*Spieshat*Personnelhat+
                   slopes[21]*Spieshat*Missileshat)
lines(Civilianhat~Spieshat,data=yuyu,col="purple",lwd=3)

Personnelhat<-34  ##26-34
Civilianhat<-exp(7.553007e+00+
                   1.150814e-03*Weaponshat+
                   slopes[3]* Spieshat+
                   slopes[4]*Stockhat+
                   slopes[5]*Payloadhat + 
                   slopes[6]*TerrorismLowhat+
                   slopes[7]*TerrorismMediumhat+
                   slopes[8]*sin(Firepowerhat)+
                   slopes[9]*sin(Bombshat)+
                   slopes[10]*Bombshat+  
                   slopes[11]*Personnelhat+
                   slopes[12]*Missileshat+
                   slopes[13]*Napalmhat+
                   slopes[14]*I(Napalmhat^2)+
                   slopes[15]*I(Napalmhat^3)+
                   slopes[16]*Weaponshat*Spieshat+
                   slopes[17]*Spieshat*Payloadhat+
                   slopes[18]*Spieshat*sin(Bombshat)+
                   slopes[19]*Spieshat*Bombshat+
                   slopes[20]*Spieshat*Personnelhat+
                   slopes[21]*Spieshat*Missileshat)
lines(Civilianhat~Spieshat,data=yuyu,col="black",lwd=1)

legend("topright",col=c("red","orange","green","purple","black"),legend=c("personnel=26","personnel=28","personnel=30","personnel=32","personnel=34"),lty=1,lwd=2)



### vspies~missiles
plot(Civilian~Spies,data=yuyu)
Mediahat<-0.5 ###0-1
Weaponshat<-250 ### 100-300
Spieshat<- seq(0,22, length=1000) ###0-22
Stockhat<-1300 ### 900-1500
Payloadhat<-2762.2575  ### 0-4500
TerrorismLowhat<-0 
TerrorismMediumhat<-0
Firepowerhat<-1.7  ### 0-2.5
Bombshat<-5000 ### 0-10000
Personnelhat<-30  ##26-34
Missileshat<-20   ###20-80
Napalmhat<-26   ###10-40
Civilianhat<-exp(7.553007e+00+
                   1.150814e-03*Weaponshat+
                   slopes[3]* Spieshat+
                   slopes[4]*Stockhat+
                   slopes[5]*Payloadhat + 
                   slopes[6]*TerrorismLowhat+
                   slopes[7]*TerrorismMediumhat+
                   slopes[8]*sin(Firepowerhat)+
                   slopes[9]*sin(Bombshat)+
                   slopes[10]*Bombshat+  
                   slopes[11]*Personnelhat+
                   slopes[12]*Missileshat+
                   slopes[13]*Napalmhat+
                   slopes[14]*I(Napalmhat^2)+
                   slopes[15]*I(Napalmhat^3)+
                   slopes[16]*Weaponshat*Spieshat+
                   slopes[17]*Spieshat*Payloadhat+
                   slopes[18]*Spieshat*sin(Bombshat)+
                   slopes[19]*Spieshat*Bombshat+
                   slopes[20]*Spieshat*Personnelhat+
                   slopes[21]*Spieshat*Missileshat)
plot(Civilianhat~Spieshat,data=yuyu,col="red",lwd=1)


Missileshat<-40   ###20-80
Civilianhat<-exp(7.553007e+00+
                   1.150814e-03*Weaponshat+
                   slopes[3]* Spieshat+
                   slopes[4]*Stockhat+
                   slopes[5]*Payloadhat + 
                   slopes[6]*TerrorismLowhat+
                   slopes[7]*TerrorismMediumhat+
                   slopes[8]*sin(Firepowerhat)+
                   slopes[9]*sin(Bombshat)+
                   slopes[10]*Bombshat+  
                   slopes[11]*Personnelhat+
                   slopes[12]*Missileshat+
                   slopes[13]*Napalmhat+
                   slopes[14]*I(Napalmhat^2)+
                   slopes[15]*I(Napalmhat^3)+
                   slopes[16]*Weaponshat*Spieshat+
                   slopes[17]*Spieshat*Payloadhat+
                   slopes[18]*Spieshat*sin(Bombshat)+
                   slopes[19]*Spieshat*Bombshat+
                   slopes[20]*Spieshat*Personnelhat+
                   slopes[21]*Spieshat*Missileshat)
plot(Civilianhat~Spieshat,data=yuyu,col="orange",lwd=1)

Missileshat<-60   ###20-80
Civilianhat<-exp(7.553007e+00+
                   1.150814e-03*Weaponshat+
                   slopes[3]* Spieshat+
                   slopes[4]*Stockhat+
                   slopes[5]*Payloadhat + 
                   slopes[6]*TerrorismLowhat+
                   slopes[7]*TerrorismMediumhat+
                   slopes[8]*sin(Firepowerhat)+
                   slopes[9]*sin(Bombshat)+
                   slopes[10]*Bombshat+  
                   slopes[11]*Personnelhat+
                   slopes[12]*Missileshat+
                   slopes[13]*Napalmhat+
                   slopes[14]*I(Napalmhat^2)+
                   slopes[15]*I(Napalmhat^3)+
                   slopes[16]*Weaponshat*Spieshat+
                   slopes[17]*Spieshat*Payloadhat+
                   slopes[18]*Spieshat*sin(Bombshat)+
                   slopes[19]*Spieshat*Bombshat+
                   slopes[20]*Spieshat*Personnelhat+
                   slopes[21]*Spieshat*Missileshat)
plot(Civilianhat~Spieshat,data=yuyu,col="green",lwd=1)

Missileshat<-80   ###20-80
Civilianhat<-exp(7.553007e+00+
                   1.150814e-03*Weaponshat+
                   slopes[3]* Spieshat+
                   slopes[4]*Stockhat+
                   slopes[5]*Payloadhat + 
                   slopes[6]*TerrorismLowhat+
                   slopes[7]*TerrorismMediumhat+
                   slopes[8]*sin(Firepowerhat)+
                   slopes[9]*sin(Bombshat)+
                   slopes[10]*Bombshat+  
                   slopes[11]*Personnelhat+
                   slopes[12]*Missileshat+
                   slopes[13]*Napalmhat+
                   slopes[14]*I(Napalmhat^2)+
                   slopes[15]*I(Napalmhat^3)+
                   slopes[16]*Weaponshat*Spieshat+
                   slopes[17]*Spieshat*Payloadhat+
                   slopes[18]*Spieshat*sin(Bombshat)+
                   slopes[19]*Spieshat*Bombshat+
                   slopes[20]*Spieshat*Personnelhat+
                   slopes[21]*Spieshat*Missileshat)
plot(Civilianhat~Spieshat,data=yuyu,col="purple",lwd=1)



legend("topright",col=c("red","orange","green","purple","black"),legend=c("personnel=26","personnel=28","personnel=30","personnel=32","personnel=34"),lty=1,lwd=2)



### vspies~missiles
plot(Civilian~Stock,data=yuyu)
Mediahat<-0.5 ###0-1
Weaponshat<-250 ### 100-300
Spieshat<- 11 ###0-22
Stockhat<-seq(900,1500, length=1000) ### 900-1500
Payloadhat<-2762.2575  ### 0-4500
TerrorismLowhat<-0 
TerrorismMediumhat<-0
Firepowerhat<-1.7  ### 0-2.5
Bombshat<-5000 ### 0-10000
Personnelhat<-30  ##26-34
Missileshat<-50  ###20-80
Napalmhat<-26   ###10-40
Civilianhat<-exp(7.553007e+00+
                   1.150814e-03*Weaponshat+
                   slopes[3]* Spieshat+
                   slopes[4]*Stockhat+
                   slopes[5]*Payloadhat + 
                   slopes[6]*TerrorismLowhat+
                   slopes[7]*TerrorismMediumhat+
                   slopes[8]*sin(Firepowerhat)+
                   slopes[9]*sin(Bombshat)+
                   slopes[10]*Bombshat+  
                   slopes[11]*Personnelhat+
                   slopes[12]*Missileshat+
                   slopes[13]*Napalmhat+
                   slopes[14]*I(Napalmhat^2)+
                   slopes[15]*I(Napalmhat^3)+
                   slopes[16]*Weaponshat*Spieshat+
                   slopes[17]*Spieshat*Payloadhat+
                   slopes[18]*Spieshat*sin(Bombshat)+
                   slopes[19]*Spieshat*Bombshat+
                   slopes[20]*Spieshat*Personnelhat+
                   slopes[21]*Spieshat*Missileshat)
plot(Civilianhat~Stockhat,data=yuyu,col="red",lwd=3)


### vspies~Payloadhat
plot(Civilian~Payload,data=yuyu)
Mediahat<-0.5 ###0-1
Weaponshat<-250 ### 100-300
Spieshat<- 10 ###0-22
Stockhat<-1300 ### 900-1500
Payloadhat<-seq(0,4500, length=1000)  ### 0-4500
TerrorismLowhat<-0 
TerrorismMediumhat<-0
Firepowerhat<-1.7  ### 0-2.5
Bombshat<-5000 ### 0-10000
Personnelhat<-30  ##26-34
Missileshat<-50  ###20-80
Napalmhat<-26   ###10-40
Spieshat<- 5 ###0-22Spieshat<- 5 ###0-22
Civilianhat<-exp(7.553007e+00+
                   1.150814e-03*Weaponshat+
                   slopes[3]* Spieshat+
                   slopes[4]*Stockhat+
                   slopes[5]*Payloadhat + 
                   slopes[6]*TerrorismLowhat+
                   slopes[7]*TerrorismMediumhat+
                   slopes[8]*sin(Firepowerhat)+
                   slopes[9]*sin(Bombshat)+
                   slopes[10]*Bombshat+  
                   slopes[11]*Personnelhat+
                   slopes[12]*Missileshat+
                   slopes[13]*Napalmhat+
                   slopes[14]*I(Napalmhat^2)+
                   slopes[15]*I(Napalmhat^3)+
                   slopes[16]*Weaponshat*Spieshat+
                   slopes[17]*Spieshat*Payloadhat+
                   slopes[18]*Spieshat*sin(Bombshat)+
                   slopes[19]*Spieshat*Bombshat+
                   slopes[20]*Spieshat*Personnelhat+
                   slopes[21]*Spieshat*Missileshat)
plot(Civilianhat~Payloadhat,data=yuyu,col="purple",lwd=3)

Spieshat<- 10 ###0-22
Civilianhat<-exp(7.553007e+00+
                   1.150814e-03*Weaponshat+
                   slopes[3]* Spieshat+
                   slopes[4]*Stockhat+
                   slopes[5]*Payloadhat + 
                   slopes[6]*TerrorismLowhat+
                   slopes[7]*TerrorismMediumhat+
                   slopes[8]*sin(Firepowerhat)+
                   slopes[9]*sin(Bombshat)+
                   slopes[10]*Bombshat+  
                   slopes[11]*Personnelhat+
                   slopes[12]*Missileshat+
                   slopes[13]*Napalmhat+
                   slopes[14]*I(Napalmhat^2)+
                   slopes[15]*I(Napalmhat^3)+
                   slopes[16]*Weaponshat*Spieshat+
                   slopes[17]*Spieshat*Payloadhat+
                   slopes[18]*Spieshat*sin(Bombshat)+
                   slopes[19]*Spieshat*Bombshat+
                   slopes[20]*Spieshat*Personnelhat+
                   slopes[21]*Spieshat*Missileshat)
plot(Civilianhat~Payloadhat,data=yuyu,col="red",lwd=3)


Spieshat<- 15 ###0-22
Civilianhat<-exp(7.553007e+00+
                   1.150814e-03*Weaponshat+
                   slopes[3]* Spieshat+
                   slopes[4]*Stockhat+
                   slopes[5]*Payloadhat + 
                   slopes[6]*TerrorismLowhat+
                   slopes[7]*TerrorismMediumhat+
                   slopes[8]*sin(Firepowerhat)+
                   slopes[9]*sin(Bombshat)+
                   slopes[10]*Bombshat+  
                   slopes[11]*Personnelhat+
                   slopes[12]*Missileshat+
                   slopes[13]*Napalmhat+
                   slopes[14]*I(Napalmhat^2)+
                   slopes[15]*I(Napalmhat^3)+
                   slopes[16]*Weaponshat*Spieshat+
                   slopes[17]*Spieshat*Payloadhat+
                   slopes[18]*Spieshat*sin(Bombshat)+
                   slopes[19]*Spieshat*Bombshat+
                   slopes[20]*Spieshat*Personnelhat+
                   slopes[21]*Spieshat*Missileshat)
plot(Civilianhat~Payloadhat,data=yuyu,col="green",lwd=3)


Spieshat<- 20 ###0-22
Civilianhat<-exp(7.553007e+00+
                   1.150814e-03*Weaponshat+
                   slopes[3]* Spieshat+
                   slopes[4]*Stockhat+
                   slopes[5]*Payloadhat + 
                   slopes[6]*TerrorismLowhat+
                   slopes[7]*TerrorismMediumhat+
                   slopes[8]*sin(Firepowerhat)+
                   slopes[9]*sin(Bombshat)+
                   slopes[10]*Bombshat+  
                   slopes[11]*Personnelhat+
                   slopes[12]*Missileshat+
                   slopes[13]*Napalmhat+
                   slopes[14]*I(Napalmhat^2)+
                   slopes[15]*I(Napalmhat^3)+
                   slopes[16]*Weaponshat*Spieshat+
                   slopes[17]*Spieshat*Payloadhat+
                   slopes[18]*Spieshat*sin(Bombshat)+
                   slopes[19]*Spieshat*Bombshat+
                   slopes[20]*Spieshat*Personnelhat+
                   slopes[21]*Spieshat*Missileshat)
plot(Civilianhat~Payloadhat,data=yuyu,col="yellow",lwd=3)

legend("topright",col=c("purple","red","green","yellow"),legend=c("Spies=5","Spies=10","Spies=15","Spies=20"),lty=1,lwd=2)


### terrosiom
plot(Civilian~Spies,data=yuyu)
Mediahat<-0.5 ###0-1
Weaponshat<-250 ### 100-300
Spieshat<- seq(0,22, length=1000) ###0-22
Stockhat<-1300 ### 900-1500
Payloadhat<-2762.2575  ### 0-4500
TerrorismLowhat<-0 
TerrorismMediumhat<-0
Firepowerhat<-1.7  ### 0-2.5
Bombshat<-5000 ### 0-10000
Personnelhat<-30  ##26-34
Missileshat<-50   ###20-80
Napalmhat<-26   ###10-40
Civilianhat<-exp(7.553007e+00+
                   1.150814e-03*Weaponshat+
                   slopes[3]* Spieshat+
                   slopes[4]*Stockhat+
                   slopes[5]*Payloadhat + 
                   slopes[6]*TerrorismLowhat+
                   slopes[7]*TerrorismMediumhat+
                   slopes[8]*sin(Firepowerhat)+
                   slopes[9]*sin(Bombshat)+
                   slopes[10]*Bombshat+  
                   slopes[11]*Personnelhat+
                   slopes[12]*Missileshat+
                   slopes[13]*Napalmhat+
                   slopes[14]*I(Napalmhat^2)+
                   slopes[15]*I(Napalmhat^3)+
                   slopes[16]*Weaponshat*Spieshat+
                   slopes[17]*Spieshat*Payloadhat+
                   slopes[18]*Spieshat*sin(Bombshat)+
                   slopes[19]*Spieshat*Bombshat+
                   slopes[20]*Spieshat*Personnelhat+
                   slopes[21]*Spieshat*Missileshat)
lines(Civilianhat~Spieshat,data=yuyu,col="red",lwd=3)



TerrorismLowhat<-1
TerrorismMediumhat<-0
Civilianhat<-exp(7.553007e+00+
                   1.150814e-03*Weaponshat+
                   slopes[3]* Spieshat+
                   slopes[4]*Stockhat+
                   slopes[5]*Payloadhat + 
                   slopes[6]*TerrorismLowhat+
                   slopes[7]*TerrorismMediumhat+
                   slopes[8]*sin(Firepowerhat)+
                   slopes[9]*sin(Bombshat)+
                   slopes[10]*Bombshat+  
                   slopes[11]*Personnelhat+
                   slopes[12]*Missileshat+
                   slopes[13]*Napalmhat+
                   slopes[14]*I(Napalmhat^2)+
                   slopes[15]*I(Napalmhat^3)+
                   slopes[16]*Weaponshat*Spieshat+
                   slopes[17]*Spieshat*Payloadhat+
                   slopes[18]*Spieshat*sin(Bombshat)+
                   slopes[19]*Spieshat*Bombshat+
                   slopes[20]*Spieshat*Personnelhat+
                   slopes[21]*Spieshat*Missileshat)
lines(Civilianhat~Spieshat,data=yuyu,col="green",lwd=3)


TerrorismLowhat<-0
TerrorismMediumhat<-1
Civilianhat<-exp(7.553007e+00+
                   1.150814e-03*Weaponshat+
                   slopes[3]* Spieshat+
                   slopes[4]*Stockhat+
                   slopes[5]*Payloadhat + 
                   slopes[6]*TerrorismLowhat+
                   slopes[7]*TerrorismMediumhat+
                   slopes[8]*sin(Firepowerhat)+
                   slopes[9]*sin(Bombshat)+
                   slopes[10]*Bombshat+  
                   slopes[11]*Personnelhat+
                   slopes[12]*Missileshat+
                   slopes[13]*Napalmhat+
                   slopes[14]*I(Napalmhat^2)+
                   slopes[15]*I(Napalmhat^3)+
                   slopes[16]*Weaponshat*Spieshat+
                   slopes[17]*Spieshat*Payloadhat+
                   slopes[18]*Spieshat*sin(Bombshat)+
                   slopes[19]*Spieshat*Bombshat+
                   slopes[20]*Spieshat*Personnelhat+
                   slopes[21]*Spieshat*Missileshat)
lines(Civilianhat~Spieshat,data=yuyu,col="purple",lwd=3)



### firepower
plot(Civilian~Firepower,data=yuyu)
Mediahat<-0.5 ###0-1
Weaponshat<-250 ### 100-300
Spieshat<- 11 ###0-22
Stockhat<-1300 ### 900-1500
Payloadhat<-2762.2575  ### 0-4500
TerrorismLowhat<-0 
TerrorismMediumhat<-0
Firepowerhat<-seq(0,2.5, length=1000) ### 0-2.5
Bombshat<-5000 ### 0-10000
Personnelhat<-30  ##26-34
Missileshat<-50   ###20-80
Napalmhat<-26   ###10-40
Civilianhat<-exp(7.553007e+00+
                   1.150814e-03*Weaponshat+
                   slopes[3]* Spieshat+
                   slopes[4]*Stockhat+
                   slopes[5]*Payloadhat + 
                   slopes[6]*TerrorismLowhat+
                   slopes[7]*TerrorismMediumhat+
                   slopes[8]*sin(Firepowerhat)+
                   slopes[9]*sin(Bombshat)+
                   slopes[10]*Bombshat+  
                   slopes[11]*Personnelhat+
                   slopes[12]*Missileshat+
                   slopes[13]*Napalmhat+
                   slopes[14]*I(Napalmhat^2)+
                   slopes[15]*I(Napalmhat^3)+
                   slopes[16]*Weaponshat*Spieshat+
                   slopes[17]*Spieshat*Payloadhat+
                   slopes[18]*Spieshat*sin(Bombshat)+
                   slopes[19]*Spieshat*Bombshat+
                   slopes[20]*Spieshat*Personnelhat+
                   slopes[21]*Spieshat*Missileshat)
plot(Civilianhat~Firepowerhat,data=yuyu,col="red",lwd=3)


### bomb
plot(Civilian~Bombs,data=yuyu)
Mediahat<-0.5 ###0-1
Weaponshat<-250 ### 100-300
Spieshat<- 1 ###0-22
Stockhat<-1300 ### 900-1500
Payloadhat<-2762.2575  ### 0-4500
TerrorismLowhat<-0 
TerrorismMediumhat<-0
Firepowerhat<-1.3 ### 0-2.5
Bombshat<-seq(0,10000, length=1000) ### 0-10000
Personnelhat<-30  ##26-34
Missileshat<-50   ###20-80
Napalmhat<-26   ###10-40
Civilianhat<-exp(7.553007e+00+
                   1.150814e-03*Weaponshat+
                   slopes[3]* Spieshat+
                   slopes[4]*Stockhat+
                   slopes[5]*Payloadhat + 
                   slopes[6]*TerrorismLowhat+
                   slopes[7]*TerrorismMediumhat+
                   slopes[8]*sin(Firepowerhat)+
                   slopes[9]*sin(Bombshat)+
                   slopes[10]*Bombshat+  
                   slopes[11]*Personnelhat+
                   slopes[12]*Missileshat+
                   slopes[13]*Napalmhat+
                   slopes[14]*I(Napalmhat^2)+
                   slopes[15]*I(Napalmhat^3)+
                   slopes[16]*Weaponshat*Spieshat+
                   slopes[17]*Spieshat*Payloadhat+
                   slopes[18]*Spieshat*sin(Bombshat)+
                   slopes[19]*Spieshat*Bombshat+
                   slopes[20]*Spieshat*Personnelhat+
                   slopes[21]*Spieshat*Missileshat)
lines(Civilianhat~Bombshat,data=yuyu,col="red",lwd=3)


Spieshat<- 8 ###0-22
Civilianhat<-exp(7.553007e+00+
                   1.150814e-03*Weaponshat+
                   slopes[3]* Spieshat+
                   slopes[4]*Stockhat+
                   slopes[5]*Payloadhat + 
                   slopes[6]*TerrorismLowhat+
                   slopes[7]*TerrorismMediumhat+
                   slopes[8]*sin(Firepowerhat)+
                   slopes[9]*sin(Bombshat)+
                   slopes[10]*Bombshat+  
                   slopes[11]*Personnelhat+
                   slopes[12]*Missileshat+
                   slopes[13]*Napalmhat+
                   slopes[14]*I(Napalmhat^2)+
                   slopes[15]*I(Napalmhat^3)+
                   slopes[16]*Weaponshat*Spieshat+
                   slopes[17]*Spieshat*Payloadhat+
                   slopes[18]*Spieshat*sin(Bombshat)+
                   slopes[19]*Spieshat*Bombshat+
                   slopes[20]*Spieshat*Personnelhat+
                   slopes[21]*Spieshat*Missileshat)
lines(Civilianhat~Bombshat,data=yuyu,col="green",lwd=3)


Spieshat<- 15 ###0-22
Civilianhat<-exp(7.553007e+00+
                   1.150814e-03*Weaponshat+
                   slopes[3]* Spieshat+
                   slopes[4]*Stockhat+
                   slopes[5]*Payloadhat + 
                   slopes[6]*TerrorismLowhat+
                   slopes[7]*TerrorismMediumhat+
                   slopes[8]*sin(Firepowerhat)+
                   slopes[9]*sin(Bombshat)+
                   slopes[10]*Bombshat+  
                   slopes[11]*Personnelhat+
                   slopes[12]*Missileshat+
                   slopes[13]*Napalmhat+
                   slopes[14]*I(Napalmhat^2)+
                   slopes[15]*I(Napalmhat^3)+
                   slopes[16]*Weaponshat*Spieshat+
                   slopes[17]*Spieshat*Payloadhat+
                   slopes[18]*Spieshat*sin(Bombshat)+
                   slopes[19]*Spieshat*Bombshat+
                   slopes[20]*Spieshat*Personnelhat+
                   slopes[21]*Spieshat*Missileshat)
lines(Civilianhat~Bombshat,data=yuyu,col="orange",lwd=3)

Spieshat<- 22 ###0-22
Civilianhat<-exp(7.553007e+00+
                   1.150814e-03*Weaponshat+
                   slopes[3]* Spieshat+
                   slopes[4]*Stockhat+
                   slopes[5]*Payloadhat + 
                   slopes[6]*TerrorismLowhat+
                   slopes[7]*TerrorismMediumhat+
                   slopes[8]*sin(Firepowerhat)+
                   slopes[9]*sin(Bombshat)+
                   slopes[10]*Bombshat+  
                   slopes[11]*Personnelhat+
                   slopes[12]*Missileshat+
                   slopes[13]*Napalmhat+
                   slopes[14]*I(Napalmhat^2)+
                   slopes[15]*I(Napalmhat^3)+
                   slopes[16]*Weaponshat*Spieshat+
                   slopes[17]*Spieshat*Payloadhat+
                   slopes[18]*Spieshat*sin(Bombshat)+
                   slopes[19]*Spieshat*Bombshat+
                   slopes[20]*Spieshat*Personnelhat+
                   slopes[21]*Spieshat*Missileshat)
lines(Civilianhat~Bombshat,data=yuyu,col="purple",lwd=3)
legend("topright",col=c("red","green","orange","purple"),legend=c("Spies=1","Spies=8","Spies=15","Spies=22"),lty=1,lwd=3)


### personnel
plot(Civilian~Personnel,data=yuyu)
Mediahat<-0.5 ###0-1
Weaponshat<-250 ### 100-300
Stockhat<-1300 ### 900-1500
Payloadhat<-2762.2575  ### 0-4500
TerrorismLowhat<-0
TerrorismMediumhat<-0
Firepowerhat<-1.3 ### 0-2.5
Bombshat<-5000 ### 0-10000
Personnelhat<-30  ##26-34
Missileshat<-50   ###20-80
Napalmhat<-26   ###10-40
Personnelhat<-seq(26,34, length=1000)  ##26-34
Spieshat<- 3 ###0-22
Civilianhat<-exp(7.553007e+00+
                   1.150814e-03*Weaponshat+
                   slopes[3]* Spieshat+
                   slopes[4]*Stockhat+
                   slopes[5]*Payloadhat + 
                   slopes[6]*TerrorismLowhat+
                   slopes[7]*TerrorismMediumhat+
                   slopes[8]*sin(Firepowerhat)+
                   slopes[9]*sin(Bombshat)+
                   slopes[10]*Bombshat+  
                   slopes[11]*Personnelhat+
                   slopes[12]*Missileshat+
                   slopes[13]*Napalmhat+
                   slopes[14]*I(Napalmhat^2)+
                   slopes[15]*I(Napalmhat^3)+
                   slopes[16]*Weaponshat*Spieshat+
                   slopes[17]*Spieshat*Payloadhat+
                   slopes[18]*Spieshat*sin(Bombshat)+
                   slopes[19]*Spieshat*Bombshat+
                   slopes[20]*Spieshat*Personnelhat+
                   slopes[21]*Spieshat*Missileshat)
plot(Civilianhat~Personnelhat,data=yuyu,col="red",lwd=3)



Spieshat<- 8 ###0-22
Civilianhat<-exp(7.553007e+00+
                   1.150814e-03*Weaponshat+
                   slopes[3]* Spieshat+
                   slopes[4]*Stockhat+
                   slopes[5]*Payloadhat + 
                   slopes[6]*TerrorismLowhat+
                   slopes[7]*TerrorismMediumhat+
                   slopes[8]*sin(Firepowerhat)+
                   slopes[9]*sin(Bombshat)+
                   slopes[10]*Bombshat+  
                   slopes[11]*Personnelhat+
                   slopes[12]*Missileshat+
                   slopes[13]*Napalmhat+
                   slopes[14]*I(Napalmhat^2)+
                   slopes[15]*I(Napalmhat^3)+
                   slopes[16]*Weaponshat*Spieshat+
                   slopes[17]*Spieshat*Payloadhat+
                   slopes[18]*Spieshat*sin(Bombshat)+
                   slopes[19]*Spieshat*Bombshat+
                   slopes[20]*Spieshat*Personnelhat+
                   slopes[21]*Spieshat*Missileshat)
lines(Civilianhat~Personnelhat,data=yuyu,col="green",lwd=3)

Spieshat<- 15 ###0-22
Civilianhat<-exp(7.553007e+00+
                   1.150814e-03*Weaponshat+
                   slopes[3]* Spieshat+
                   slopes[4]*Stockhat+
                   slopes[5]*Payloadhat + 
                   slopes[6]*TerrorismLowhat+
                   slopes[7]*TerrorismMediumhat+
                   slopes[8]*sin(Firepowerhat)+
                   slopes[9]*sin(Bombshat)+
                   slopes[10]*Bombshat+  
                   slopes[11]*Personnelhat+
                   slopes[12]*Missileshat+
                   slopes[13]*Napalmhat+
                   slopes[14]*I(Napalmhat^2)+
                   slopes[15]*I(Napalmhat^3)+
                   slopes[16]*Weaponshat*Spieshat+
                   slopes[17]*Spieshat*Payloadhat+
                   slopes[18]*Spieshat*sin(Bombshat)+
                   slopes[19]*Spieshat*Bombshat+
                   slopes[20]*Spieshat*Personnelhat+
                   slopes[21]*Spieshat*Missileshat)
lines(Civilianhat~Personnelhat,data=yuyu,col="orange",lwd=1)


Spieshat<- 22 ###0-22
Civilianhat<-exp(7.553007e+00+
                   1.150814e-03*Weaponshat+
                   slopes[3]* Spieshat+
                   slopes[4]*Stockhat+
                   slopes[5]*Payloadhat + 
                   slopes[6]*TerrorismLowhat+
                   slopes[7]*TerrorismMediumhat+
                   slopes[8]*sin(Firepowerhat)+
                   slopes[9]*sin(Bombshat)+
                   slopes[10]*Bombshat+  
                   slopes[11]*Personnelhat+
                   slopes[12]*Missileshat+
                   slopes[13]*Napalmhat+
                   slopes[14]*I(Napalmhat^2)+
                   slopes[15]*I(Napalmhat^3)+
                   slopes[16]*Weaponshat*Spieshat+
                   slopes[17]*Spieshat*Payloadhat+
                   slopes[18]*Spieshat*sin(Bombshat)+
                   slopes[19]*Spieshat*Bombshat+
                   slopes[20]*Spieshat*Personnelhat+
                   slopes[21]*Spieshat*Missileshat)
lines(Civilianhat~Personnelhat,data=yuyu,col="purple",lwd=1)

Spieshat<- 15 ###0-22
Civilianhat<-exp(7.553007e+00+
                   1.150814e-03*Weaponshat+
                   slopes[3]* Spieshat+
                   slopes[4]*Stockhat+
                   slopes[5]*Payloadhat + 
                   slopes[6]*TerrorismLowhat+
                   slopes[7]*TerrorismMediumhat+
                   slopes[8]*sin(Firepowerhat)+
                   slopes[9]*sin(Bombshat)+
                   slopes[10]*Bombshat+  
                   slopes[11]*Personnelhat+
                   slopes[12]*Missileshat+
                   slopes[13]*Napalmhat+
                   slopes[14]*I(Napalmhat^2)+
                   slopes[15]*I(Napalmhat^3)+
                   slopes[16]*Weaponshat*Spieshat+
                   slopes[17]*Spieshat*Payloadhat+
                   slopes[18]*Spieshat*sin(Bombshat)+
                   slopes[19]*Spieshat*Bombshat+
                   slopes[20]*Spieshat*Personnelhat+
                   slopes[21]*Spieshat*Missileshat)
lines(Civilianhat~Missileshat,data=yuyu,col="orange",lwd=3)Spieshat<- 15 ###0-22
Civilianhat<-exp(7.553007e+00+
                   1.150814e-03*Weaponshat+
                   slopes[3]* Spieshat+
                   slopes[4]*Stockhat+
                   slopes[5]*Payloadhat + 
                   slopes[6]*TerrorismLowhat+
                   slopes[7]*TerrorismMediumhat+
                   slopes[8]*sin(Firepowerhat)+
                   slopes[9]*sin(Bombshat)+
                   slopes[10]*Bombshat+  
                   slopes[11]*Personnelhat+
                   slopes[12]*Missileshat+
                   slopes[13]*Napalmhat+
                   slopes[14]*I(Napalmhat^2)+
                   slopes[15]*I(Napalmhat^3)+
                   slopes[16]*Weaponshat*Spieshat+
                   slopes[17]*Spieshat*Payloadhat+
                   slopes[18]*Spieshat*sin(Bombshat)+
                   slopes[19]*Spieshat*Bombshat+
                   slopes[20]*Spieshat*Personnelhat+
                   slopes[21]*Spieshat*Missileshat)
lines(Civilianhat~Missileshat,data=yuyu,col="orange",lwd=3)Spieshat<- 15 ###0-22
legend("topright",col=c("red","green","orange","purple"),legend=c("Spies=3","Spies=8","Spies=15","Spies=22"),lty=1,lwd=3)


### Missiles
plot(Civilian~Missiles,data=yuyu)
Mediahat<-0.5 ###0-1
Weaponshat<-250 ### 100-300
Stockhat<-1300 ### 900-1500
Payloadhat<-2762.2575  ### 0-4500
TerrorismLowhat<-0
TerrorismMediumhat<-0
Firepowerhat<-1.3 ### 0-2.5
Bombshat<-5000 ### 0-10000
Personnelhat<-30  ##26-34
Missileshat<-seq(20,80, length=1000)  ###20-80
Napalmhat<-26   ###10-40
Personnelhat<-30  ##26-34
Spieshat<- 3 ###0-22
Civilianhat<-exp(7.553007e+00+
                   1.150814e-03*Weaponshat+
                   slopes[3]* Spieshat+
                   slopes[4]*Stockhat+
                   slopes[5]*Payloadhat + 
                   slopes[6]*TerrorismLowhat+
                   slopes[7]*TerrorismMediumhat+
                   slopes[8]*sin(Firepowerhat)+
                   slopes[9]*sin(Bombshat)+
                   slopes[10]*Bombshat+  
                   slopes[11]*Personnelhat+
                   slopes[12]*Missileshat+
                   slopes[13]*Napalmhat+
                   slopes[14]*I(Napalmhat^2)+
                   slopes[15]*I(Napalmhat^3)+
                   slopes[16]*Weaponshat*Spieshat+
                   slopes[17]*Spieshat*Payloadhat+
                   slopes[18]*Spieshat*sin(Bombshat)+
                   slopes[19]*Spieshat*Bombshat+
                   slopes[20]*Spieshat*Personnelhat+
                   slopes[21]*Spieshat*Missileshat)
plot(Civilianhat~Missileshat,data=yuyu,col="red",lwd=3)

Spieshat<- 8 ###0-22
Civilianhat<-exp(7.553007e+00+
                   1.150814e-03*Weaponshat+
                   slopes[3]* Spieshat+
                   slopes[4]*Stockhat+
                   slopes[5]*Payloadhat + 
                   slopes[6]*TerrorismLowhat+
                   slopes[7]*TerrorismMediumhat+
                   slopes[8]*sin(Firepowerhat)+
                   slopes[9]*sin(Bombshat)+
                   slopes[10]*Bombshat+  
                   slopes[11]*Personnelhat+
                   slopes[12]*Missileshat+
                   slopes[13]*Napalmhat+
                   slopes[14]*I(Napalmhat^2)+
                   slopes[15]*I(Napalmhat^3)+
                   slopes[16]*Weaponshat*Spieshat+
                   slopes[17]*Spieshat*Payloadhat+
                   slopes[18]*Spieshat*sin(Bombshat)+
                   slopes[19]*Spieshat*Bombshat+
                   slopes[20]*Spieshat*Personnelhat+
                   slopes[21]*Spieshat*Missileshat)
plot(Civilianhat~Missileshat,data=yuyu,col="green",lwd=3)


Spieshat<- 15 ###0-22
Civilianhat<-exp(7.553007e+00+
                   1.150814e-03*Weaponshat+
                   slopes[3]* Spieshat+
                   slopes[4]*Stockhat+
                   slopes[5]*Payloadhat + 
                   slopes[6]*TerrorismLowhat+
                   slopes[7]*TerrorismMediumhat+
                   slopes[8]*sin(Firepowerhat)+
                   slopes[9]*sin(Bombshat)+
                   slopes[10]*Bombshat+  
                   slopes[11]*Personnelhat+
                   slopes[12]*Missileshat+
                   slopes[13]*Napalmhat+
                   slopes[14]*I(Napalmhat^2)+
                   slopes[15]*I(Napalmhat^3)+
                   slopes[16]*Weaponshat*Spieshat+
                   slopes[17]*Spieshat*Payloadhat+
                   slopes[18]*Spieshat*sin(Bombshat)+
                   slopes[19]*Spieshat*Bombshat+
                   slopes[20]*Spieshat*Personnelhat+
                   slopes[21]*Spieshat*Missileshat)
plot(Civilianhat~Missileshat,data=yuyu,col="orange",lwd=1)

  Spieshat<- 22 ###0-22
Civilianhat<-exp(7.553007e+00+
                   1.150814e-03*Weaponshat+
                   slopes[3]* Spieshat+
                   slopes[4]*Stockhat+
                   slopes[5]*Payloadhat + 
                   slopes[6]*TerrorismLowhat+
                   slopes[7]*TerrorismMediumhat+
                   slopes[8]*sin(Firepowerhat)+
                   slopes[9]*sin(Bombshat)+
                   slopes[10]*Bombshat+  
                   slopes[11]*Personnelhat+
                   slopes[12]*Missileshat+
                   slopes[13]*Napalmhat+
                   slopes[14]*I(Napalmhat^2)+
                   slopes[15]*I(Napalmhat^3)+
                   slopes[16]*Weaponshat*Spieshat+
                   slopes[17]*Spieshat*Payloadhat+
                   slopes[18]*Spieshat*sin(Bombshat)+
                   slopes[19]*Spieshat*Bombshat+
                   slopes[20]*Spieshat*Personnelhat+
                   slopes[21]*Spieshat*Missileshat)
plot(Civilianhat~Missileshat,data=yuyu,col="purple",lwd=3)
legend("topright",col=c("red","green","orange","purple"),legend=c("Spies=3","Spies=8","Spies=15","Spies=22"),lty=1,lwd=3)




### Napalm
plot(Civilian~Napalm,data=yuyu)
Mediahat<-0.5 ###0-1
Weaponshat<-250 ### 100-300
Stockhat<-1301 ### 900-1500
Payloadhat<-2762.2575  ### 0-4500
TerrorismLowhat<-0
TerrorismMediumhat<-0
Firepowerhat<-1.3 ### 0-2.5
Bombshat<-5000 ### 0-10000
Personnelhat<-30  ##26-34
Missileshat<-50  ###20-80
Napalmhat<-25   ###10-40
Personnelhat<-30  ##26-34
Spieshat<- 3 ###0-22
Civilianhat<-exp(7.553007e+00+
                   1.150814e-03*Weaponshat+
                   slopes[3]* Spieshat+
                   slopes[4]*Stockhat+
                   slopes[5]*Payloadhat + 
                   slopes[6]*TerrorismLowhat+
                   slopes[7]*TerrorismMediumhat+
                   slopes[8]*sin(Firepowerhat)+
                   slopes[9]*sin(Bombshat)+
                   slopes[10]*Bombshat+  
                   slopes[11]*Personnelhat+
                   slopes[12]*Missileshat+
                   slopes[13]*Napalmhat+
                   slopes[14]*I(Napalmhat^2)+
                   slopes[15]*I(Napalmhat^3)+
                   slopes[16]*Weaponshat*Spieshat+
                   slopes[17]*Spieshat*Payloadhat+
                   slopes[18]*Spieshat*sin(Bombshat)+
                   slopes[19]*Spieshat*Bombshat+
                   slopes[20]*Spieshat*Personnelhat+
                   slopes[21]*Spieshat*Missileshat)
plot(Civilianhat~Napalmhat,data=yuyu,col="red",lwd=3)


Spieshat<- 8 ###0-22
Civilianhat<-exp(7.553007e+00+
                   1.150814e-03*Weaponshat+
                   slopes[3]* Spieshat+
                   slopes[4]*Stockhat+
                   slopes[5]*Payloadhat + 
                   slopes[6]*TerrorismLowhat+
                   slopes[7]*TerrorismMediumhat+
                   slopes[8]*sin(Firepowerhat)+
                   slopes[9]*sin(Bombshat)+
                   slopes[10]*Bombshat+  
                   slopes[11]*Personnelhat+
                   slopes[12]*Missileshat+
                   slopes[13]*Napalmhat+
                   slopes[14]*I(Napalmhat^2)+
                   slopes[15]*I(Napalmhat^3)+
                   slopes[16]*Weaponshat*Spieshat+
                   slopes[17]*Spieshat*Payloadhat+
                   slopes[18]*Spieshat*sin(Bombshat)+
                   slopes[19]*Spieshat*Bombshat+
                   slopes[20]*Spieshat*Personnelhat+
                   slopes[21]*Spieshat*Missileshat)
plot(Civilianhat~Napalmhat,data=yuyu,col="green",lwd=3)

Spieshat<- 15 ###0-22
Civilianhat<-exp(7.553007e+00+
                   1.150814e-03*Weaponshat+
                   slopes[3]* Spieshat+
                   slopes[4]*Stockhat+
                   slopes[5]*Payloadhat + 
                   slopes[6]*TerrorismLowhat+
                   slopes[7]*TerrorismMediumhat+
                   slopes[8]*sin(Firepowerhat)+
                   slopes[9]*sin(Bombshat)+
                   slopes[10]*Bombshat+  
                   slopes[11]*Personnelhat+
                   slopes[12]*Missileshat+
                   slopes[13]*Napalmhat+
                   slopes[14]*I(Napalmhat^2)+
                   slopes[15]*I(Napalmhat^3)+
                   slopes[16]*Weaponshat*Spieshat+
                   slopes[17]*Spieshat*Payloadhat+
                   slopes[18]*Spieshat*sin(Bombshat)+
                   slopes[19]*Spieshat*Bombshat+
                   slopes[20]*Spieshat*Personnelhat+
                   slopes[21]*Spieshat*Missileshat)
plot(Civilianhat~Napalmhat,data=yuyu,col="orange",lwd=3)

Spieshat<- 22 ###0-22
Civilianhat<-exp(7.553007e+00+
                   1.150814e-03*Weaponshat+
                   slopes[3]* Spieshat+
                   slopes[4]*Stockhat+
                   slopes[5]*Payloadhat + 
                   slopes[6]*TerrorismLowhat+
                   slopes[7]*TerrorismMediumhat+
                   slopes[8]*sin(Firepowerhat)+
                   slopes[9]*sin(Bombshat)+
                   slopes[10]*Bombshat+  
                   slopes[11]*Personnelhat+
                   slopes[12]*Missileshat+
                   slopes[13]*Napalmhat+
                   slopes[14]*I(Napalmhat^2)+
                   slopes[15]*I(Napalmhat^3)+
                   slopes[16]*Weaponshat*Spieshat+
                   slopes[17]*Spieshat*Payloadhat+
                   slopes[18]*Spieshat*sin(Bombshat)+
                   slopes[19]*Spieshat*Bombshat+
                   slopes[20]*Spieshat*Personnelhat+
                   slopes[21]*Spieshat*Missileshat)
plot(Civilianhat~Napalmhat,data=yuyu,col="purple",lwd=3)
legend("topright",col=c("red","green","orange","purple"),legend=c("Spies=3","Spies=8","Spies=15","Spies=22"),lty=1,lwd=3)

1-e^9.308792e-06

0.0091/984.5849

