netuse<-read.table("http://www.uwyo.edu/crawford/datasets/used/netuse.txt", header= TRUE)
plot(server~time, data=netuse)

### linear 1
fit1<- lm(server~time, data=netuse)

summary(fit1)
plot(server~time, col= site, data=netuse)
timehat<- seq(0,12, length=1000)
serverhat <- -16.662*timehat + 493.924
lines(serverhat~ timehat)


### linear 4
plot(server~time, col= site, data=netuse)
fit2<-lm(server~time*site, data= netuse)
summary(fit2)
timehat<- seq(0,12, length=1000)

###buy
buyhat=1
counthat=0
fishhat=0
logichat=0
serverhat<- -22.045*timehat+599.3068
lines(serverhat~timehat,col="black")

###count
buyhat=0
counthat=1
fishhat=0
logichat=0
serverhat<- 2.6329*timehat+427.4676
lines(serverhat~timehat,col="Red")

###fish
buyhat=0
counthat=0
fishhat=1
logichat=0
serverhat<- -4.6986*timehat+696.3947
lines(serverhat~timehat,col="Green")


###logic
buyhat=0
counthat=0
fishhat=0
logichat=1
serverhat<- -41.2091*timehat+402.3511
lines(serverhat~timehat,col="blue")

###quadratic 1 model 
plot(server~time, col= site, data=netuse)
fit2<- lm(server~I(time^2)+time, data=netuse)

summary(fit2)

timehat<- seq(0,12, length=1000)
serverhat <- 1.903*timehat^2 +-39.945*timehat+541.858
lines(serverhat~ timehat)

### QUADRATIC 4
plot(server~time, col= site, data=netuse)
fit2<-lm(server~(I(time^2)+time)*site, data= netuse)
summary(fit2)
timehat<- seq(0,12, length=1000)

###buy
buyhat=1
counthat=0
fishhat=0
logichat=0
serverhat<- -4.8433*timehat^2+18.9040*timehat+467.1221
lines(serverhat~timehat,col="black")

###count
buyhat=0
counthat=1
fishhat=0
logichat=0
serverhat<- 11.9411*timehat^2-144.9384*timehat+739.546
lines(serverhat~timehat,col="Red")

###fish
buyhat=0
counthat=0
fishhat=1
logichat=0
serverhat<- -9.4386*timehat^2+115.7337*timehat+434.6863
lines(serverhat~timehat,col="Green")


###logic
buyhat=0
counthat=0
fishhat=0
logichat=1
serverhat<- 4.019*timehat^2-87.046*timehat+485.2575
lines(serverhat~timehat,col="blue")

467.1221+ 18.1354
-4.8433+8.8623
18.9040 -105.9500
