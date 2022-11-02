library(readxl)
library(MASS)
library(dplyr)
library(bestglm)
DC2022 <- read_excel("C:/Users/Owner/Downloads/2022 Fall Data Challenge Dataset.xlsx")
View(DC2022)
DC2022[DC2022 == -1] <- NA

y <- matrix(DC2022$SEGRADES)
X <- matrix(c(DC2022$FSVOL, DC2022$SCCHOICE, DC2022$SPUBCHOIX, DC2022$SCONSIDR, DC2022$FSSPORTX, DC2022$FSATCNFN,
              DC2022$FSCOMMTE, DC2022$FSFREQ, DC2022$FHHELP, DC2022$SEFUTUREX), ncol = 10)
Xy <-as.data.frame(cbind(X,y))
names(Xy) <-  c("FSVOL", "SCCHOICE", "SPUBCHOIX", "SCONSIDR", "FSSPORTX", "FSATCNFN",
                "FSCOMMTE", "FSFREQ", "FHHELP", "SEFUTUREX", "SEGRADES")
FSV.om <- as.matrix(which(is.na(Xy), arr.ind=TRUE))
Xy.n <- Xy[-c(FSV.om),]
g <- bestglm(Xy.n)

lines(Xy.n$FSVOL, predict(Xy.n$SEGRADES, list(FSVOL = Xy.n$FSVOL), type = "response"))
predict(g, list(FSVOL = Xy.n$FSVOL), type = "response")
pig <- predict(g$BestModel)
par(mfrow = c(1,1))

plot(Xy.n$FSVOL, Xy.n$SEGRADES)
lines(Xy.n$FSVOL, pig)