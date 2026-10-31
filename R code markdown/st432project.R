library(readxl)
library(tidyverse)
set.seed(51)

islandData <- read_excel("S:\\raw 432\\st432mcdata.xlsx")

#Some sampling
sample(27, 10)
sample(417, 8)
sample(436, 10)
sample(1671, 10)
sample(451, 10)
sample(562, 10)
sample(599, 10)
sample(640, 10)
sample(464, 10)
sample(845, 10)
sample(432, 10)

#Calc means and vars

summary <- as.data.frame(islandData %>% group_by(city) %>% summarize(mean = mean(bloodchol), var = var(bloodchol)))
Mi <- c(1671,432,599,436,451,417,562,845,464,640)
ur <- sum(summary$mean * Mi) / sum(Mi)
Mbar <- 40007 /27
Sr <- (1/9) * sum((Mi * summary$mean - ur * Mi)^2)
var <- (1/Mbar^2) * (Sr / 9)
se <- sqrt(var)

#iron
iron <- c(189, 203.2)
ironMi <- c(417,640)
urI <- sum(iron * ironMi) / sum(ironMi)
SrI <- (1/1) * sum((ironMi * iron - urI * ironMi)^2)
varI <- (1/Mbar^2) * (SrI / 1)
seI <- sqrt(varI)

#prov
prov <- c(195.4, 212.4, 185.4, 185.4)
provMi <- c(1671,451, 845, 464)
urP <- sum(prov * provMi) / sum(provMi)
SrP <- (1/3) * sum((provMi * prov - urP * provMi)^2)
varP <- (1/Mbar^2) * (SrP / 3)
seP <- sqrt(varP)

#bonne
bonne <- c(195, 199.4, 211.4, 199)
bonneMi <- c(432,599, 436, 562)
urB <- sum(bonne * bonneMi) / sum(bonneMi)
SrB <- (1/3) * sum((bonneMi * bonne - urB * bonneMi)^2)
varB <- (1/Mbar^2) * (SrB / 3)
seB <- sqrt(varB)

#pairwise
#iron and prov
IP <- urI - urP 
IPse <- sqrt(varI + varP) 

#iron and bonne
IB <- urI - urB 
IBse <- sqrt(varI + varB) 

#bonne and prov
PB <- urP - urB 
PBse <- sqrt(varP + varB) 
