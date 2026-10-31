install.packages('faraway')
install.packages('lmtest')
library(faraway)
library(lmtest)
help(sat)
data(sat)

# Full model
fitF = lm(formula = total ~ takers + expend + poly(takers,2) + poly(expend,2) + I(takers * expend), data = sat)

# Dropping quad
fitR1 = lm(formula = total ~ takers + expend  + takers * expend, data = sat)

summary(fitF)
summary(fitR1)
anova(fitR1, fitF)

# Dropping int
fitR2 = lm(formula = total ~ takers + expend + poly(takers,2) + poly(expend,2), data = sat)
summary(fitR2)
anova(fitR2, fitF)

# Dropping expend terms
fitR3 = lm(formula = total ~ takers +  poly(takers,2), data = sat)
summary(fitR3)
anova(fitR3, fitF)

fitFinal = lm(formula = total ~ takers +  poly(takers,2), data = sat)

# Breusch-Pagan Test for the constancy of the error variance

bptest(fitFinal, studentize=FALSE)

# Normality of error terms.

shapiro.test(fitFinal$residuals)

# outlier deduction

rstandard(fitFinal)         #R function to get standardized residuals

plot(fitFinal$fitted.values,rstandard(fitFinal),main="Final Model",xlab="Fitted values",ylab="Residual", pch=23,bg="red",cex=2,lwd=2)
abline(h=0,col="red")
abline(h=2,col="blue")
abline(h=-2,col="blue")


# identify the point > crit
(1:length(rstandard(fitFinal)))[rstandard(fitFinal)>2]
(1:length(rstandard(fitFinal)))[rstandard(fitFinal)< -2]
