install.packages("tidyverse")
install.packages("leaps")
install.packages("asbio")
install.packages("qpcR")
library(tidyverse)
library(leaps)
library(asbio)
library(qpcR)
mortdata <-read_table("S:\\raw 430\\Mortgage Data.txt", col_names = FALSE, show_col_types = FALSE, skip = 1)

colnames(mortdata) <- c("cityid" , "y", "x1", "x2", "x3" , "x4", "x5" , "x6")

firstorder <- regsubsets( y ~ x1 + x2 + x3 + x4 + x5 + x6, data = mortdata)
firstsum <- summary(firstorder)
data.frame (
Adj.R2 = which.max(firstsum$adjr2),
CP = which.min(firstsum$cp),
BIC = which.min(firstsum$bic)
)
summary(firstorder)


highmortdata <- as.matrix(mortdata [,3:8])
highmortdata2 <- poly(highmortdata, degree = 2, raw = TRUE)

finalhighmort <- as.data.frame(cbind(highmortdata2, y = mortdata$y ))
secorder  <- regsubsets( y ~ ., data = finalhighmort)

secsum <- summary(secorder)
data.frame (
  Adj.R2 = which.max(secsum$adjr2),
  CP = which.min(secsum$cp),
  BIC = which.min(secsum$bic)
)


