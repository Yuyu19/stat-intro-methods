plot(pollen~time,data = beeflower, col= colors)
fit <- lm(pollen~time, data = beeflower)
summary(fit)
hist(beeflower$pollen)

timehat <- seq(0,16, length=1000)
bluehat <- 9.6300-0.8037*timehat
lines(bluehat~timehat, col="blue",lwd=2)

greenhat <- 12.39642921-0.5116*timehat
lines(greenhat~timehat, col="green",lwd=2)

purplehat <- 2.3298+0.5668*timehat
lines(purplehat~timehat, col="purple",lwd=2)

redhat <- 10.1045-0.9735*timehat
lines(redhat~timehat, col="red",lwd=2)


legend("bottomright",col=c("blue","green", "purple","red"),legend=c("blue flower","green flower", "purple flower","red flower"),lty=1)
topic?
  