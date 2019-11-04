apple<-read.table("http://www.uwyo.edu/crawford/datasets/used/spiderthreads.txt", header=TRUE)
plot(Threadweight~Temperature,col=Diet,data= apple)
plot(Threadrock~Temperature,col=Diet,data= apple)
t.test(apple$Threadweight, apple$Threadrock)
fit<-lm(Threadweight~Temperature*Diet, data= apple)
summary(fit)

temphat<- seq(0, 100, length=1000)
butterflyhat<- -0.08179*temphat+25.57955
fireantshat<--0.029*temphat+15.49485
vitaminhat<- 0.10444*temphat+20.45968
wormshat<-0.0587*temphat+22.01551
plot(butterflyhat~temphat,col="green")

  0.14049-0.08179
25.57955-3.56404
-0.0587*78+22.01551
20.45968+4.222
20.45968-4.222