food
food$deliciousness[food$deliciousness==110]

fit<- lm(deliciousness~oventemp+goods, data=food)
summary(fit)




fit<-lm(deliciousness~oventemp*goods+oventemp, data= food)
summary(fit)

breadhat<- -0.003397*overtemphat+112.434610
cookiehat<- -0.003397*overtemphat+110.289
pretZelhat<- -0.003397*overtemphat+111.1909
sd(fit)


plot(deliciousness~oventemp, col=goods,data=food)
oventemphat<-seq(200,500,length=1000)
breadhat<- -0.006277*overtemphat+113.461270

cookiehat<- -0.039543*oventemphat+122.7118
Pretzelshat<- 0.050444*overtemphat+100.866
plot(cookiehat~oventemphat,col="green")
plot(pretzelshat~oventemphat,col="black")

bread<- 113.461270
cookiehat<- -0.033266*oventemphat+113.461270
pretzelshat<- 0.031499*oventemphat+100.866