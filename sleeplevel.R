yuck<- read.table("http://www.uwyo.edu/crawford/datasets/used/sleeplevels.txt", header=TRUE)
yuck
plot(sleep~humidity, data=yuck)
plot(sleep~temperature, data=yuck)
plot(sleep~age, data=yuck)
plot(sleep~noise, data=yuck)

### 1)
fit<-lm(sleep~humidity+temperature+age+noise, data= yuck)
plot(fit)
plot(fit$residuals~yuck$humidity)
plot(fit$residuals~yuck$temperature)
plot(fit$residuals~yuck$age)
plot(fit$residuals~yuck$noise)


### 2)
fit<-lm(sleep~humidity*temperature+age+noise, data= yuck)
plot(fit)
plot(fit$residuals~yuck$humidity)
plot(fit$residuals~yuck$temperature)
plot(fit$residuals~yuck$age)
plot(fit$residuals~yuck$noise)


### 3)
fit<-lm(sleep~humidity*temperature+age*noise, data= yuck)
plot(fit)
plot(fit$residuals~yuck$humidity)
plot(fit$residuals~yuck$temperature)
plot(fit$residuals~yuck$age)
plot(fit$residuals~yuck$noise)

### 4)
fit<-lm(sleep~humidity*temperature+I(age^2)*I(noise^2)+age*noise+age*I(noise^2)+I(age^2)*noise, data= yuck)
summary(fit)
plot(fit)
plot(fit$residuals~yuck$humidity)
plot(fit$residuals~yuck$temperature)
plot(fit$residuals~yuck$age)
plot(fit$residuals~yuck$noise)

### 5)
fit<-lm(sleep~humidity*temperature+I(age^2)*I(noise^2)+age*noise+age*I(noise^2)+I(age^2)*noise+I(noise^3), data= yuck)
summary(fit)
plot(fit)
plot(fit$residuals~yuck$humidity)
plot(fit$residuals~yuck$temperature)
plot(fit$residuals~yuck$age)
plot(fit$residuals~yuck$noise)



### 6)
fit<-lm(sleep~humidity*temperature+I(age^2)+I(noise^2)+age*noise+age*I(noise^2)+I(age^2)*noise+I(noise^3), data= yuck)
summary(fit)
plot(fit)
plot(fit$residuals~yuck$humidity)
plot(fit$residuals~yuck$temperature)
plot(fit$residuals~yuck$age)
plot(fit$residuals~yuck$noise)


humidityhat<-40 ###30-70
temperaturehat<-seq(40,80,length=1000) ###40-80
###sleep~humidity
plot(sleep~humidity, data=yuck)

humidityhat<-seq(30,70,length=1000)
temperaturehat<-50 ###40-80
agehat<-10 ###2-30
noisehat<-20  ###0-80
sleephat<-2.066e-03*humidityhat+2.593e-03*temperaturehat-1.847e-03*humidityhat*temperaturehat-7.927e-04*I(agehat^2)-2.113e-04*I(noisehat^2)+1.779e-03*agehat*noisehat+1.980e-05*agehat*I(noisehat^2)+-7.911e-05*I(agehat^2)*noisehat-1.230e-06 *I(noisehat^3)+1.587e-02*agehat-3.184e-02*noisehat+1.231e+01  
lines(sleephat~humidityhat, col="blue",lwd=2)

humidityhat<-seq(30,70,length=1000)
temperaturehat<-40###40-80
agehat<-10 ###2-30
noisehat<-20  ###0-80
sleephat<-2.066e-03*humidityhat+2.593e-03*temperaturehat-1.847e-03*humidityhat*temperaturehat-7.927e-04*I(agehat^2)-2.113e-04*I(noisehat^2)+1.779e-03*agehat*noisehat+1.980e-05*agehat*I(noisehat^2)+-7.911e-05*I(agehat^2)*noisehat-1.230e-06 *I(noisehat^3)+1.587e-02*agehat-3.184e-02*noisehat+1.231e+01 
lines(sleephat~humidityhat, col="purple",lwd=2)


humidityhat<-seq(30,70,length=1000)
temperaturehat<-60  ###40-80
agehat<-10 ###2-30
noisehat<-20  ###0-80
sleephat<-2.066e-03*humidityhat+2.593e-03*temperaturehat-1.847e-03*humidityhat*temperaturehat-7.927e-04*I(agehat^2)-2.113e-04*I(noisehat^2)+1.779e-03*agehat*noisehat+1.980e-05*agehat*I(noisehat^2)+-7.911e-05*I(agehat^2)*noisehat-1.230e-06 *I(noisehat^3)+1.587e-02*agehat-3.184e-02*noisehat+1.231e+01  
lines(sleephat~humidityhat, col="red",lwd=2)

humidityhat<-seq(30,70,length=1000)
temperaturehat<-70  ###40-80
agehat<-10 ###2-30
noisehat<-20  ###0-80
sleephat<-2.066e-03*humidityhat+2.593e-03*temperaturehat-1.847e-03*humidityhat*temperaturehat-7.927e-04*I(agehat^2)-2.113e-04*I(noisehat^2)+1.779e-03*agehat*noisehat+1.980e-05*agehat*I(noisehat^2)+-7.911e-05*I(agehat^2)*noisehat-1.230e-06 *I(noisehat^3)+1.587e-02*agehat-3.184e-02*noisehat+1.231e+01 
lines(sleephat~humidityhat, col="yellow",lwd=2)

humidityhat<-seq(30,70,length=1000)
temperaturehat<-80  ###40-80
agehat<-10 ###2-30
noisehat<-20  ###0-80
sleephat<-2.066e-03*humidityhat+2.593e-03*temperaturehat-1.847e-03*humidityhat*temperaturehat-7.927e-04*I(agehat^2)-2.113e-04*I(noisehat^2)+1.779e-03*agehat*noisehat+1.980e-05*agehat*I(noisehat^2)+-7.911e-05*I(agehat^2)*noisehat-1.230e-06 *I(noisehat^3)+1.587e-02*agehat-3.184e-02*noisehat+1.231e+01 
lines(sleephat~humidityhat, col="green",lwd=2)
legend("bottomleft",col=c("purple","blue","red","yellow","green"),legend=c("temperature=40","temperature=50","temperature=60","temperature=70","temperature=80"),lty=1)


###sleep~temperature
plot(sleep~temperature, data=yuck)

humidityhat<-30 ###30-70
temperaturehat<-seq(40,80,length=1000) ###40-80
agehat<-10 ###2-30
noisehat<-20  ###0-80
sleephat<-2.066e-03*humidityhat+2.593e-03*temperaturehat-1.847e-03*humidityhat*temperaturehat-7.927e-04*I(agehat^2)-2.113e-04*I(noisehat^2)+1.779e-03*agehat*noisehat+1.980e-05*agehat*I(noisehat^2)+-7.911e-05*I(agehat^2)*noisehat-1.230e-06 *I(noisehat^3)+1.587e-02*agehat-3.184e-02*noisehat+1.231e+01  
lines(sleephat~temperaturehat, col="purple",lwd=2)

humidityhat<-40 ###30-70
temperaturehat<-seq(40,80,length=1000) ###40-80
agehat<-10 ###2-30
noisehat<-20  ###0-80
sleephat<-2.066e-03*humidityhat+2.593e-03*temperaturehat-1.847e-03*humidityhat*temperaturehat-7.927e-04*I(agehat^2)-2.113e-04*I(noisehat^2)+1.779e-03*agehat*noisehat+1.980e-05*agehat*I(noisehat^2)+-7.911e-05*I(agehat^2)*noisehat-1.230e-06 *I(noisehat^3)+1.587e-02*agehat-3.184e-02*noisehat+1.231e+01  
lines(sleephat~temperaturehat, col="blue",lwd=2)

humidityhat<-50 ###30-70
temperaturehat<-seq(40,80,length=1000) ###40-80
agehat<-10 ###2-30
noisehat<-20  ###0-80
sleephat<-2.066e-03*humidityhat+2.593e-03*temperaturehat-1.847e-03*humidityhat*temperaturehat-7.927e-04*I(agehat^2)-2.113e-04*I(noisehat^2)+1.779e-03*agehat*noisehat+1.980e-05*agehat*I(noisehat^2)+-7.911e-05*I(agehat^2)*noisehat-1.230e-06 *I(noisehat^3)+1.587e-02*agehat-3.184e-02*noisehat+1.231e+01  

lines(sleephat~temperaturehat, col="red",lwd=2)

humidityhat<-60 ###30-70
temperaturehat<-seq(40,80,length=1000) ###40-80
agehat<-10 ###2-30
noisehat<-20  ###0-80
sleephat<-2.066e-03*humidityhat+2.593e-03*temperaturehat-1.847e-03*humidityhat*temperaturehat-7.927e-04*I(agehat^2)-2.113e-04*I(noisehat^2)+1.779e-03*agehat*noisehat+1.980e-05*agehat*I(noisehat^2)+-7.911e-05*I(agehat^2)*noisehat-1.230e-06 *I(noisehat^3)+1.587e-02*agehat-3.184e-02*noisehat+1.231e+01  
lines(sleephat~temperaturehat, col="yellow",lwd=2)

humidityhat<-70 ###30-70
temperaturehat<-seq(40,80,length=1000) ###40-80
agehat<-10 ###2-30
noisehat<-20  ###0-80
sleephat<-2.066e-03*humidityhat+2.593e-03*temperaturehat-1.847e-03*humidityhat*temperaturehat-7.927e-04*I(agehat^2)-2.113e-04*I(noisehat^2)+1.779e-03*agehat*noisehat+1.980e-05*agehat*I(noisehat^2)+-7.911e-05*I(agehat^2)*noisehat-1.230e-06 *I(noisehat^3)+1.587e-02*agehat-3.184e-02*noisehat+1.231e+01  
lines(sleephat~temperaturehat, col="green",lwd=2)

legend("bottomleft",col=c("purple","blue","red","yellow","green"),legend=c("humidity=30","humidity=40","humidity=50","humidity=60","humidity=70"),lty=1)


###sleep~noise
plot(sleep~noise, data=yuck)

humidityhat<-45 ###30-70
temperaturehat<-60 ##40-80
agehat<-5 ###2-30
noisehat<-seq(0,80,length=1000)  ###0-80
sleephat<-2.066e-03*humidityhat+2.593e-03*temperaturehat-1.847e-03*humidityhat*temperaturehat-7.927e-04*I(agehat^2)-2.113e-04*I(noisehat^2)+1.779e-03*agehat*noisehat+1.980e-05*agehat*I(noisehat^2)+-7.911e-05*I(agehat^2)*noisehat-1.230e-06 *I(noisehat^3)+1.587e-02*agehat-3.184e-02*noisehat+1.231e+01  
lines(sleephat~noisehat, col="purple",lwd=2)

humidityhat<-45 ###30-70
temperaturehat<-60 ##40-80
agehat<-10 ###2-30
noisehat<-seq(0,80,length=1000)  ###0-80
sleephat<-2.066e-03*humidityhat+2.593e-03*temperaturehat-1.847e-03*humidityhat*temperaturehat-7.927e-04*I(agehat^2)-2.113e-04*I(noisehat^2)+1.779e-03*agehat*noisehat+1.980e-05*agehat*I(noisehat^2)+-7.911e-05*I(agehat^2)*noisehat-1.230e-06 *I(noisehat^3)+1.587e-02*agehat-3.184e-02*noisehat+1.231e+01  
lines(sleephat~noisehat, col="blue",lwd=2)

humidityhat<-45 ###30-70
temperaturehat<-60 ##40-80
agehat<-15 ###2-30
noisehat<-seq(0,80,length=1000)  ###0-80
sleephat<-2.066e-03*humidityhat+2.593e-03*temperaturehat-1.847e-03*humidityhat*temperaturehat-7.927e-04*I(agehat^2)-2.113e-04*I(noisehat^2)+1.779e-03*agehat*noisehat+1.980e-05*agehat*I(noisehat^2)+-7.911e-05*I(agehat^2)*noisehat-1.230e-06 *I(noisehat^3)+1.587e-02*agehat-3.184e-02*noisehat+1.231e+01  
lines(sleephat~noisehat, col="red",lwd=2)

humidityhat<-45 ###30-70
temperaturehat<-60 ##40-80
agehat<-20 ###2-30
noisehat<-seq(0,80,length=1000)  ###0-80
sleephat<-2.066e-03*humidityhat+2.593e-03*temperaturehat-1.847e-03*humidityhat*temperaturehat-7.927e-04*I(agehat^2)-2.113e-04*I(noisehat^2)+1.779e-03*agehat*noisehat+1.980e-05*agehat*I(noisehat^2)+-7.911e-05*I(agehat^2)*noisehat-1.230e-06 *I(noisehat^3)+1.587e-02*agehat-3.184e-02*noisehat+1.231e+01  
lines(sleephat~noisehat, col="yellow",lwd=2)

humidityhat<-45 ###30-70
temperaturehat<-60 ##40-80
agehat<-25 ###2-30
noisehat<-seq(0,80,length=1000)  ###0-80
sleephat<-2.066e-03*humidityhat+2.593e-03*temperaturehat-1.847e-03*humidityhat*temperaturehat-7.927e-04*I(agehat^2)-2.113e-04*I(noisehat^2)+1.779e-03*agehat*noisehat+1.980e-05*agehat*I(noisehat^2)+-7.911e-05*I(agehat^2)*noisehat-1.230e-06 *I(noisehat^3)+1.587e-02*agehat-3.184e-02*noisehat+1.231e+01  
lines(sleephat~noisehat, col="green",lwd=2)

humidityhat<-45 ###30-70
temperaturehat<-60 ##40-80
agehat<-30 ###2-30
noisehat<-seq(0,80,length=1000)  ###0-80
sleephat<-2.066e-03*humidityhat+2.593e-03*temperaturehat-1.847e-03*humidityhat*temperaturehat-7.927e-04*I(agehat^2)-2.113e-04*I(noisehat^2)+1.779e-03*agehat*noisehat+1.980e-05*agehat*I(noisehat^2)+-7.911e-05*I(agehat^2)*noisehat-1.230e-06 *I(noisehat^3)+1.587e-02*agehat-3.184e-02*noisehat+1.231e+01  
lines(sleephat~noisehat, col="black",lwd=2)

legend("bottomleft",col=c("purple","blue","red","yellow","green","black"),legend=c("age=5","age=10","age=15","age=20","age=25","age=30"),lty=1)

###sleep~age
plot(sleep~age, data=yuck)

humidityhat<-45 ###30-70
temperaturehat<-60 ##40-80
agehat<-seq(2,30, length=1000) ###2-30
noisehat<-3  ###0-80
sleephat<-2.066e-03*humidityhat+2.593e-03*temperaturehat-1.847e-03*humidityhat*temperaturehat-7.927e-04*I(agehat^2)-2.113e-04*I(noisehat^2)+1.779e-03*agehat*noisehat+1.980e-05*agehat*I(noisehat^2)+-7.911e-05*I(agehat^2)*noisehat-1.230e-06 *I(noisehat^3)+1.587e-02*agehat-3.184e-02*noisehat+1.231e+01  
lines(sleephat~agehat, col="purple",lwd=2)

humidityhat<-45 ###30-70
temperaturehat<-60 ##40-80
agehat<-seq(2,30, length=1000) ###2-30
noisehat<-13  ###0-80
sleephat<-2.066e-03*humidityhat+2.593e-03*temperaturehat-1.847e-03*humidityhat*temperaturehat-7.927e-04*I(agehat^2)-2.113e-04*I(noisehat^2)+1.779e-03*agehat*noisehat+1.980e-05*agehat*I(noisehat^2)+-7.911e-05*I(agehat^2)*noisehat-1.230e-06 *I(noisehat^3)+1.587e-02*agehat-3.184e-02*noisehat+1.231e+01  
lines(sleephat~agehat, col="blue",lwd=2)

humidityhat<-45 ###30-70
temperaturehat<-60 ##40-80
agehat<-seq(2,30, length=1000) ###2-30
noisehat<-23  ###0-80
sleephat<-2.066e-03*humidityhat+2.593e-03*temperaturehat-1.847e-03*humidityhat*temperaturehat-7.927e-04*I(agehat^2)-2.113e-04*I(noisehat^2)+1.779e-03*agehat*noisehat+1.980e-05*agehat*I(noisehat^2)+-7.911e-05*I(agehat^2)*noisehat-1.230e-06 *I(noisehat^3)+1.587e-02*agehat-3.184e-02*noisehat+1.231e+01  
lines(sleephat~agehat, col="red",lwd=2)

humidityhat<-45 ###30-70
temperaturehat<-60 ##40-80
agehat<-seq(2,30, length=1000) ###2-30
noisehat<-33  ###0-80
sleephat<-2.066e-03*humidityhat+2.593e-03*temperaturehat-1.847e-03*humidityhat*temperaturehat-7.927e-04*I(agehat^2)-2.113e-04*I(noisehat^2)+1.779e-03*agehat*noisehat+1.980e-05*agehat*I(noisehat^2)+-7.911e-05*I(agehat^2)*noisehat-1.230e-06 *I(noisehat^3)+1.587e-02*agehat-3.184e-02*noisehat+1.231e+01  
lines(sleephat~agehat, col="yellow",lwd=2)

humidityhat<-45 ###30-70
temperaturehat<-60 ##40-80
agehat<-seq(2,30, length=1000) ###2-30
noisehat<-33  ###0-80
sleephat<-2.066e-03*humidityhat+2.593e-03*temperaturehat-1.847e-03*humidityhat*temperaturehat-7.927e-04*I(agehat^2)-2.113e-04*I(noisehat^2)+1.779e-03*agehat*noisehat+1.980e-05*agehat*I(noisehat^2)+-7.911e-05*I(agehat^2)*noisehat-1.230e-06 *I(noisehat^3)+1.587e-02*agehat-3.184e-02*noisehat+1.231e+01  
lines(sleephat~agehat, col="green",lwd=2)

humidityhat<-45 ###30-70
temperaturehat<-60 ##40-80
agehat<-seq(2,30, length=1000) ###2-30
noisehat<-43  ###0-80
sleephat<-2.066e-03*humidityhat+2.593e-03*temperaturehat-1.847e-03*humidityhat*temperaturehat-7.927e-04*I(agehat^2)-2.113e-04*I(noisehat^2)+1.779e-03*agehat*noisehat+1.980e-05*agehat*I(noisehat^2)+-7.911e-05*I(agehat^2)*noisehat-1.230e-06 *I(noisehat^3)+1.587e-02*agehat-3.184e-02*noisehat+1.231e+01  
lines(sleephat~agehat, col="black",lwd=2)

humidityhat<-45 ###30-70
temperaturehat<-60 ##40-80
agehat<-seq(2,30, length=1000) ###2-30
noisehat<-53  ###0-80
sleephat<-2.066e-03*humidityhat+2.593e-03*temperaturehat-1.847e-03*humidityhat*temperaturehat-7.927e-04*I(agehat^2)-2.113e-04*I(noisehat^2)+1.779e-03*agehat*noisehat+1.980e-05*agehat*I(noisehat^2)+-7.911e-05*I(agehat^2)*noisehat-1.230e-06 *I(noisehat^3)+1.587e-02*agehat-3.184e-02*noisehat+1.231e+01  
lines(sleephat~agehat, col="pink",lwd=2)

humidityhat<-45 ###30-70
temperaturehat<-60 ##40-80
agehat<-seq(2,30, length=1000) ###2-30
noisehat<-63  ###0-80
sleephat<-2.066e-03*humidityhat+2.593e-03*temperaturehat-1.847e-03*humidityhat*temperaturehat-7.927e-04*I(agehat^2)-2.113e-04*I(noisehat^2)+1.779e-03*agehat*noisehat+1.980e-05*agehat*I(noisehat^2)+-7.911e-05*I(agehat^2)*noisehat-1.230e-06 *I(noisehat^3)+1.587e-02*agehat-3.184e-02*noisehat+1.231e+01  
lines(sleephat~agehat, col="orange",lwd=2)

humidityhat<-45 ###30-70
temperaturehat<-60 ##40-80
agehat<-seq(2,30, length=1000) ###2-30
noisehat<-73  ###0-80
sleephat<-2.066e-03*humidityhat+2.593e-03*temperaturehat-1.847e-03*humidityhat*temperaturehat-7.927e-04*I(agehat^2)-2.113e-04*I(noisehat^2)+1.779e-03*agehat*noisehat+1.980e-05*agehat*I(noisehat^2)+-7.911e-05*I(agehat^2)*noisehat-1.230e-06 *I(noisehat^3)+1.587e-02*agehat-3.184e-02*noisehat+1.231e+01  
lines(sleephat~agehat, col="brown",lwd=2)

legend("bottomright",col=c("purple","blue","red","yellow","green","black","pink","orange","brown"),legend=c("noise=3","noise=13","noise=23","noise=33","noise=43","noise=53","noise=63","noise=73"),lty=1)
