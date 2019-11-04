### deal with data 
relationship<-read.table("http://www.uwyo.edu/crawford/datasets/used/server.txt", header=TRUE)
plot(usage~time, data=relationship)
relationship$time[relationship$time==0]
relationship$time[relationship$time<0] <- relationship$time[relationship$time<0]*-1
server <- relationship[relationship$time>0,] 
plot(time~usage, data=server)

### run regression 
fit<- lm(time~I(usage^2)+usage,data=server)
summary(fit)
plot(fit)

### plot regression line 
usagehat<- seq(0,1100, length=1100)
timehat<- 2.696e-05*usagehat^2-7.526e-04*usagehat+3.685e+00
plot(timehat~usagehat,data=server)
