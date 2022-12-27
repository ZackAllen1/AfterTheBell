library(readxl)
library(MASS)
library(dplyr)
library(bestglm)
install.packages("Hmisc")
library("Hmisc")
DC2022 <- read_excel("C:/Users/Owner/Downloads/2022 Fall Data Challenge Dataset.xlsx")
View(DC2022)
DC2022[DC2022 == -1] <- NA

##################################################################

# Creating a full model

y <- matrix(DC2022$SEGRADES)
X <- matrix(c(DC2022$FSVOL, DC2022$SCCHOICE, DC2022$SPUBCHOIX, DC2022$SCONSIDR, DC2022$FSSPORTX, DC2022$FSATCNFN,
                     DC2022$FSCOMMTE, DC2022$FSFREQ, DC2022$FHHELP, DC2022$SEFUTUREX), ncol = 10)
Xy <-as.data.frame(cbind(X,y))
names(Xy) <-  c("FSVOL", "SCCHOICE", "SPUBCHOIX", "SCONSIDR", "FSSPORTX", "FSATCNFN",
          "FSCOMMTE", "FSFREQ", "FHHELP", "SEFUTUREX", "SEGRADES")
FSV.om <- as.matrix(which(is.na(Xy), arr.ind=TRUE))
Xy.n <- Xy[-c(FSV.om),]

# Correlation matrix
res2 <- rcorr(as.matrix(Xy.n))
res2$P > 0.05 # Remove SCONSIDR, FSVOL, and FHHELP as they are not significant and/or highly correlated


######################################################################

# Creating a simpler model with removed predictors

y2 <- matrix(DC2022$SEGRADES)
X2 <- matrix(c(DC2022$SCCHOICE, DC2022$SPUBCHOIX, DC2022$FSSPORTX, DC2022$FSATCNFN,
              DC2022$FSCOMMTE, DC2022$FSFREQ, DC2022$SEFUTUREX), ncol = 7)
Xy2 <-as.data.frame(cbind(X2,y2))
names(Xy2) <-  c("SCCHOICE", "SPUBCHOIX", "FSSPORTX", "FSATCNFN",
                "FSCOMMTE", "FSFREQ", "SEFUTUREX", "SEGRADES")
FSV.om2 <- as.matrix(which(is.na(Xy2), arr.ind=TRUE))
Xy.n2 <- Xy2[-c(FSV.om2),]

# checking correlation again
res3 <- rcorr(as.matrix(Xy.n2))
res3$P > 0.05 # FSATCNFN and SPUBCHOIX may be correlated

########################################################################

# Fitting the model in anova to determine the significance of interaction
PE.aovI <- aov(SEGRADES ~ SCCHOICE+  FSSPORTX + FSCOMMTE +
      FSFREQ + SEFUTUREX + SPUBCHOIX*FSATCNFN , data = DC2022)
anova(PE.aovI) # the interaction between SPUBCHOIX and FSATCNFN is not significant

# testing a fully interacted model just in case
PE.aovI2 <- aov(SEGRADES ~ SCCHOICE * FSSPORTX * FSCOMMTE *
                 FSFREQ*SEFUTUREX*SPUBCHOIX*FSATCNFN , data = DC2022)
anova(PE.aovI2)
########################################################################

# Creating a plot of factor level means without the interaction terms
PE.aov <- aov(SEGRADES ~ SCCHOICE+  FSSPORTX + FSCOMMTE +
      FSFREQ + SEFUTUREX + SPUBCHOIX + FSATCNFN , data = DC2022)
anova(PE.aov) # the terms are all showing significance

plot(PE.aov, which = 1:2)

SCC  <- as.factor(DC2022$SCCHOICE) # 1 - Yes - School choice
FSS <- as.factor(DC2022$FSSPORTX) # 1 - Yes - School Events attended
FSC <- as.factor(DC2022$FSCOMMTE) # 1 - Yes - Served on school committee
FSF <- as.factor(DC2022$FSFREQ) # meetings attended - continuous
SEF <- as.factor(DC2022$SEFUTUREX) # 1 not very far - how far they expect their child to succeed in school
SPC <- as.factor(DC2022$SPUBCHOIX) # 1 - Yes - District lets you choose public school
FSA <- as.factor(DC2022$FSATCNFN) # 1 - Yes - Gone to parent teacher conference

plot.design(SEGRADES ~ SCC +  FSS + FSC + FSF + SEF + SPC + FSA , data = DC2022)

########################################################################

# Things I tried that didn't work are below


DC2022$FSVOL[DC2022$FSVOL == -1] <- NA # Set -1 as NA to omit empty values from data
FSV.om <- as.matrix(which(is.na(DC2022$FSVOL), arr.ind=TRUE)) # find all the rows with NA
SEGrades <- DC2022$SEGRADES[-c(FSV.om)] # omit rows that had NA in FSVOL so that the length in SEGrades response variable now matches
FSV <- DC2022$FSVOL[-c(FSV.om)] # omit rows that contained NA in predictor FSVOL and set as factor


PInv <- glm(SEGrades ~ FSV)
summary(PInv)

plot(jitter(SEGrades, amount = 0.1) ~ FSV, pch = 1)

curve(predict(SEGrades, type = "response"), col = "red", add= TRUE )


DC2022[DC2022 == -1] <- NA
DC2022
ALLGRADEX
DC2022.lda <- lda(SEGrades ~ FSVOL + SCCHOICE + SPUBCHOIX +  SCONSIDR + FSSPORTX + FSMTNG, data = DC2022)
windows() 
plot(DC2022.lda, dimen=2, col = DC2022$ALLGRADEX)
dev.off()
dev.on()


par(mar = c(1, 1, 1, 1))
par(mfrow = c(4,4))
PI <- glm(SEGRADES ~ FSVOL + SCCHOICE + SPUBCHOIX +  SCONSIDR + FSSPORTX + FSATCNFN + FSCOMMTE +
            FSFREQ + FHHELP + SEFUTUREX, data = DC2022)
summary(PI)
plot(PI)
plot(jitter(SEGRADES, 0.1) ~ FSVOL + SCCHOICE + SPUBCHOIX +  SCONSIDR + FSSPORTX + FSATCNFN + FSCOMMTE +
       FSFREQ + FHHELP + SEFUTUREX, data = DC2022)
curve(predict(PI, type = "response"), data = DC2022, col = "blue", lwd = 2, add = TRUE)

glda <- lda(SEGRADES ~ FSVOL + SCCHOICE + SPUBCHOIX +  SCONSIDR + FSSPORTX + FSATCNFN + FSCOMMTE +
              FSFREQ + FHHELP + SEFUTUREX, data = DC2022)
plot(glda, dimen=2, col = DC2022$SEGRADES)

lines(Xy.n$FSVOL, predict(Xy.n$SEGRADES, list(FSVOL = Xy.n$FSVOL), type = "response"))
predict(g, list(FSVOL = Xy.n$FSVOL), type = "response")
pig <- predict(g$BestModel)
par(mfrow = c(1,1))

plot(Xy.n$FSVOL, Xy.n$SEGRADES)
lines(Xy.n$FSVOL, pig)
