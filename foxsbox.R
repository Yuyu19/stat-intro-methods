foxsbox<-read.table("http://www.uwyo.edu/crawford/datasets/used/foxbooks.txt", header=TRUE)

###fix the plot
foxsbox<-foxsbox[foxsbox$customers< 1000,]
fit<-lm(customers~temperature+humidity+adnum+marketing+childrens+cashier+treats+discounts,data=foxsbox)
plot(fit)
summary(fit)
plot(fit$residuals~foxsbox$temperature)
plot(fit$residuals~foxsbox$humidity)
plot(fit$residuals~foxsbox$adnum)
plot(fit$residuals~foxsbox$marketing)
plot(fit$residuals~foxsbox$childrens)
plot(fit$residuals~foxsbox$cashier)
plot(fit$residuals~foxsbox$treats)
plot(fit$residuals~foxsbox$discounts)

## 1)
fit<-lm(customers~temperature+I(humidity^4)+childrens+cashier+treats*discounts,data=foxsbox)  
###marketing+adnum


##3)
fit<-lm(customers~exp(marketing),data=foxsbox)
fit<-lm(log(customers)~marketing,data=foxsbox)

fit<-lm(customers~temperature*childrens+I(humidity^6)+treats*discounts+exp(marketing)+cashier,data=foxsbox)
hist(foxsbox$customers)
plot(foxsbox$customers~foxsbox$marketing  )
plot(fit,col=foxsbox$childrens )
plot(fit,col=foxsbox$cashier )
plot(fit$residuals~foxsbox$temperature,col=foxsbox$cashier)
plot(fit$residuals~foxsbox$humidity,col=foxsbox$cashier)
plot(fit$residuals~foxsbox$adnum,col=foxsbox$cashier)
plot(fit$residuals~foxsbox$marketing,col=foxsbox$cashier)
plot(fit$residuals~foxsbox$childrens,col=foxsbox$cashier)
plot(fit$residuals~foxsbox$cashier,col=foxsbox$cashier)
plot(fit$residuals~foxsbox$treats,col=foxsbox$cashier)
plot(fit$residuals~foxsbox$discounts,col=foxsbox$cashier)


##5
 plot(treats~discounts,data=foxsbox )
 
###7
 customerhat<-5.789e-02*temperaturehatt+1.255e+00*childrenhat+-1.384e+00*(humidityhat^6)+ 3.279e-03*treathat+5.445e-03*discounthat+2.980e-13*exp(marketinghat)+-3.460e+00*cashierMeanSlow-8.541e-02*temperaturehatt*childrenhat+3.832e-02*treathat*discounthat
 temperaturehatt<-seq(10,90, length=1000) ### 10-90
 childrenhat<-0  ### 0/1
 
 treathat<-  100    ###0-150
 discounthat<- 50  ### 0-100
 humidityhat<- 0.9   ### 0-1.3
 marketinghat<-  10    ### -5-30
   cashierMeanSfast<-
 cashierMeanSlow <-1
 cashierPatientFast <- 0
 cashierPatientSlow <-0
 customerhat<-5.789e-02*temperaturehatt+1.255e+00*childrenhat+-1.384e+00*(humidityhat^6)+ 3.279e-03*treathat+5.445e-03*discounthat+2.980e-13*exp(marketinghat)+-3.460e+00*cashierMeanSlow-8.541e-02*temperaturehatt*childrenhat+3.832e-02*treathat*discounthat
 plot(foxsbox$customers~foxsbox$temperature)
 lines(customerhat~temperaturehatt,col="red")
 
 
 temperaturehatt<-seq(10,90, length=1000) ### 10-90
 childrenhat<-1  ### 0/1
 
 treathat<-  100    ###0-150
 discounthat<- 50  ### 0-100
 humidityhat<- 0.9   ### 0-1.3
 marketinghat<-  10    ### -5-30
 cashierMeanSfast<-
   cashierMeanSlow <-1
 cashierPatientFast <- 0
 cashierPatientSlow <-0
 customerhat<-5.789e-02*temperaturehatt+1.255e+00*childrenhat+-1.384e+00*(humidityhat^6)+ 3.279e-03*treathat+5.445e-03*discounthat+2.980e-13*exp(marketinghat)+-3.460e+00*cashierMeanSlow+-8.541e-02*temperaturehatt*childrenhat+3.832e-02*treathat*discounthat
 

 lines(customerhat~temperaturehatt,col="blue")
 
 temperaturehatt<-seq(10,90, length=1000) ### 10-90
 childrenhat<-0  ### 0/1
 
 legend("topleft",col=c("red","blue"),legend=c("children=no","children=yes"),lty=1)
 
 
 ###treat
 temperaturehatt<-50 ### 10-90
 childrenhat<-1  ### 0/1
 
 treathat<- seq(0,150, length=1000)    ###0-150
 discounthat<- 50  ### 0-100
 humidityhat<- 0.9   ### 0-1.3
 marketinghat<-  10    ### -5-30
 cashierMeanSfast<-
   cashierMeanSlow <-1
 cashierPatientFast <- 0
 cashierPatientSlow <-0
 customerhat<-5.789e-02*temperaturehatt+1.255e+00*childrenhat+-1.384e+00*(humidityhat^6)+ 3.279e-03*treathat+5.445e-03*discounthat+2.980e-13*exp(marketinghat)+-3.460e+00*cashierMeanSlow-8.541e-02*temperaturehatt*childrenhat+3.832e-02*treathat*discounthat
 plot(foxsbox$customers~foxsbox$treats)
 lines(customerhat~treathat,col="red")
 
 temperaturehatt<-50 ### 10-90
 childrenhat<-1  ### 0/1
 
 treathat<- seq(0,150, length=1000)    ###0-150
 discounthat<- 10  ### 0-100
 humidityhat<- 0.9   ### 0-1.3
 marketinghat<-  10    ### -5-30

   cashierMeanSlow <-1
 cashierPatientFast <- 0
 cashierPatientSlow <-0
 customerhat<-5.789e-02*temperaturehatt+1.255e+00*childrenhat+-1.384e+00*(humidityhat^6)+ 3.279e-03*treathat+5.445e-03*discounthat+2.980e-13*exp(marketinghat)+-3.460e+00*cashierMeanSlow-8.541e-02*temperaturehatt*childrenhat+3.832e-02*treathat*discounthat
 lines(customerhat~treathat,col="blue")
 
 treathat<- seq(0,150, length=1000)    ###0-150
 discounthat<- 100  ### 0-100
 humidityhat<- 0.9   ### 0-1.3
 marketinghat<-  10    ### -5-30
 
 cashierMeanSlow <-1
 cashierPatientFast <- 0
 cashierPatientSlow <-0
 customerhat<-5.789e-02*temperaturehatt+1.255e+00*childrenhat+-1.384e+00*(humidityhat^6)+ 3.279e-03*treathat+5.445e-03*discounthat+2.980e-13*exp(marketinghat)+-3.460e+00*cashierMeanSlow-8.541e-02*temperaturehatt*childrenhat+3.832e-02*treathat*discounthat
 lines(customerhat~treathat,col="purple")
 
 legend("topleft",col=c("blue","red","purple"),legend=c("discount=10","discount=50","discount=100"),lty=1)
 
 
 ###discount
 temperaturehatt<-50 ### 10-90
 childrenhat<-1  ### 0/1
 
 treathat<- 30    ###0-150
 discounthat<- seq(0,100, length=1000) ### 0-100
 humidityhat<- 0.9   ### 0-1.3
 marketinghat<-  10    ### -5-30
 cashierMeanSfast<-
   cashierMeanSlow <-1
 cashierPatientFast <- 0
 cashierPatientSlow <-0
 customerhat<-5.789e-02*temperaturehatt+1.255e+00*childrenhat+-1.384e+00*(humidityhat^6)+ 3.279e-03*treathat+5.445e-03*discounthat+2.980e-13*exp(marketinghat)+-3.460e+00*cashierMeanSlow-8.541e-02*temperaturehatt*childrenhat+3.832e-02*treathat*discounthat
 plot(foxsbox$customers~foxsbox$discounts)
 lines(customerhat~discounthat,col="blue")
 
 
 treathat<- 70    ###0-150
 discounthat<- seq(0,100, length=1000) ### 0-100
 humidityhat<- 0.9   ### 0-1.3
 marketinghat<-  10    ### -5-30
 cashierMeanSfast<-
   cashierMeanSlow <-1
 cashierPatientFast <- 0
 cashierPatientSlow <-0
 customerhat<-5.789e-02*temperaturehatt+1.255e+00*childrenhat+-1.384e+00*(humidityhat^6)+ 3.279e-03*treathat+5.445e-03*discounthat+2.980e-13*exp(marketinghat)+-3.460e+00*cashierMeanSlow-8.541e-02*temperaturehatt*childrenhat+3.832e-02*treathat*discounthat
 lines(customerhat~discounthat,col="red")
 
 
 treathat<- 120   ###0-150
 discounthat<- seq(0,100, length=1000) ### 0-100
 humidityhat<- 0.9   ### 0-1.3
 marketinghat<-  10    ### -5-30
 cashierMeanSfast<-
   cashierMeanSlow <-1
 cashierPatientFast <- 0
 cashierPatientSlow <-0
 customerhat<-5.789e-02*temperaturehatt+1.255e+00*childrenhat+-1.384e+00*(humidityhat^6)+ 3.279e-03*treathat+5.445e-03*discounthat+2.980e-13*exp(marketinghat)+-3.460e+00*cashierMeanSlow-8.541e-02*temperaturehatt*childrenhat+3.832e-02*treathat*discounthat
 lines(customerhat~discounthat,col="purple")
 
 legend("topleft",col=c("blue","red","purple"),legend=c("treat=30","treat=70","treat=120"),lty=1)
 
 
 
 
 ###8
 ###.	For the day June 4th 
 .	Temperature will be 47°
 .	Humidity will be 75%, 
 .	There will be one ad per hour
 .	The marketing budget will be 20
 .	There will be children's hour at the bookstore
 .	A patient fast cashier will be at the front.  
 .	There will be 100 treats available
 .	There will be 40 books offered at a discount
 
 temperaturehatt<-47 ### 10-90
 childrenhat<-1  ### 0/1
 
 treathat<- 100   ###0-150
 discounthat<- 40 ### 0-100
 humidityhat<- 75%   ### 0-1.3
 marketinghat<-  20    ### -5-30
 cashierMeanSfast<-
   cashierMeanSlow <-0
 cashierPatientFast <- 1
 cashierPatientSlow <-0
 
 152.9693+0.4932 
 152.9693-0.4932 
 customerhat<-5.789e-02*temperaturehatt+1.255e+00*childrenhat+-1.384e+00*(humidityhat^6)+ 3.279e-03*treathat+5.445e-03*discounthat+2.980e-13*exp(marketinghat)+-3.460e+00*cashierMeanSlow-8.541e-02*temperaturehatt*childrenhat+3.832e-02*treathat*discounthat