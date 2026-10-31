students=c(31,29,25,35,15,31,22,27,25,19,30,18,21,40,38,28,17,22,41,32,35,19,29,18,31)
Tscore=c(1590,1510,1490,1610,800,1720,1310,1427,1290,860,1620,710,1140,
         1980,1990,1420,900,1080,2010,1740,1750,890,1470,910,1740)



meanscore <- mean(Tscore)
stscore <- var(Tscore - meanscore)
print(108^2 *(1 - 25/108) *stscore/25)


numitems <- c(15,27,9,31,16)
numnotid <- c(2,3,1,1,2)
phat <-sum(numnotid) /sum(numitems)
spitem <-var(numnotid - phat * numitems)
print(spitem)

#Mu 2 stage
numsuper <- c(45,36,20,18,28)
numsupersam <- c(9,7,4,4,6)
meansuper <- c(102,90,76,94,120)
varsuper <- c(20,16,22,26,12)
mubarsuper <- sum(meansuper * numitems) / sum (numitems)
srsuper <- var(meansuper * numitems - numitems * mubarsuper)
mbar <- sum(numsuper)/20
varmubarsuper <- (1/mbar^2) * srsuper/5

#total 2 stage
numsec <- c(21,23,20,20)
numsecsam <- c(10,10,10,10)
meansec <- c(15.5, 15.8, 17, 14.9)

totalsec <- sum(meansec * numsec) * 12 / 4
sbsec <- var(meansec * numsec - totalsec / 12)
varmubarsuper <- 12^2 * sbsec/4
print(totalsec) 
print(varmubarsuper)

#
voters=c(1290,1170,840,1620,1381,1893,1942,971,1143,2041,843,1066,1171,1213,1741)
favorA=c(680,631,475,935,472,1143,1187,542,973,1541,321,487,596,782,980)
prop <- favorA/voters
  
phat <- sum(favorA)/sum(voters)
spv <- var(voters * prop - phat * voters)
Mbarvote <- sum(voters) /15
varvote <- (1-15/497) * (spv / (15 * Mbarvote *Mbarvote))
print(phat)
print(varvote)

n = (497 * spv) / ((497 * 0.05^2 * Mbarvote^2/4) + spv)

d <-diff(high) #succ diff
sum(d^2)
highstrat1 <- high[c(1:6)]
highstrat2 <- high[c(7:12)]
highstrat3 <- high[c(13:18)]
mean(highstrat1)
var(highstrat1)
mean(highstrat2)
var(highstrat2)
mean(highstrat3)
var(highstrat3)

as.data.frame(rain)
d2 <-diff(rain)
sum(d2^2)
rainstrat1 <- rain[c(1:6)]
rainstrat2 <- rain[c(7:12)]
rainstrat3 <- rain[c(13:18)]
var(rainstrat1)
var(rainstrat2)
var(rainstrat3)