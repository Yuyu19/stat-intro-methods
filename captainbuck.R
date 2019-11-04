shipgold <- read.csv("//studentfiles/storage$/Desktop/2018 spring/stas/captainbucktooth.csv", header= TRUE)
shipgold
fit <-lm(Gold ~ Ship, data= shipgold)
summary(fit)
plot(fit)
plot(Gold~ Ship, data = shipgold)
shipnumber <- seq(1,300, length=300)
goldheight<- 62.030  + 19.487*shipnumber

lines(goldheight~shipnumber)
cor(shipgold$Gold, shipgold$Ship)
141.9267 - 32.37
