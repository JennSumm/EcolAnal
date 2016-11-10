#practice repeated measures

demo1 <- read.csv("http://www.ats.ucla.edu/stat/data/demo1.csv")
## Convert variables to factor
demo1 <- within(demo1, {
  group <- factor(group)
  time <- factor(time)
  id <- factor(id)
})

par(cex = .6)

with(demo1, interaction.plot(time, group, pulse,
                             ylim = c(5, 20), lty= c(1, 12), lwd = 3,
                             ylab = "mean of pulse", xlab = "time", trace.label = "group"))

demo1.aov <- aov(pulse ~ group * time + Error(id), data = demo1)
summary(demo1.aov)


#nifty. Ok so let's try it with my data - first I'm going to use 
#MSAT lengths for four different groups which are the different soil layers
#aka the different time horizons

#your explanatory variables are both the loci(factor) and soil horizon/time period 
#plus the interaction

#I altered the names of the loci and the different soil horizons so they would be simple
#sequential integers. Therefore, SCAM_4, 5, 7, 8, 11, 13, 14, 16, 17, 18, and 19 are 
#now (respectively) 1:11. 
#the soil layers are going to be #s 1-5 with 5 being the deepest/oldest
scam<-read.csv("rptmsr_msat_num.csv")
scam
#convert variables to factor
scam <- within(scam, {
  SOIL_DEPTH <- factor(SOIL_DEPTH)
  LOCI <- factor(LOCI)
})

par(cex = .6)

with(scam, interaction.plot(SOIL_DEPTH, LOCI, MSAT_LNGTH,
                             ylim = c(5, 300),
                             ylab = "MSAT Mean Length", xlab = "Time", trace.label = "LOCI", col=1:11))
?interaction.plot()
, lty= c(1, 50)

scam.aov <- aov(MSAT_LNGTH ~ LOCI * SOIL_DEPTH + LOCI*SOIL_DEPTH, data = scam)
summary(scam.aov)


# in class we used lm() or gls() so check this out
 blah blha blahalsdjfaiweroihawdf
