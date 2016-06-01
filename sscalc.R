library(foreign)
library(plyr)
library(ggplot2)
library(scales)
setwd("~/data/misc")
ss1 <- read.csv("sscalc.csv")
attach(ss1)
#########################################################
############INPUTS NEEDED TO RUN CALCULATOR##############
############startsal=base salary#########################
############salgrowth=salary growth rate#################
############birthyr=year born############################
############length=years working plus ages up to 70######
############age_length=calculates age####################
############age_range=ages in data set###################
#########################################################

startsal <- 15992
salgrowth <- 1.05
birthyr <- 1950
length <- 41  #years working plus ages up to 70, adjust if changing age_range (now at age 30-70)
age_length <- ss1$year-birthyr
age_range <- age_length>=30 & age_length<=70 #year start working, plus ages up to 70 to calculate benefit


#index factor function
age2 <-function(z) {
  age <- ss1$year - z
  
  factor<-ifelse(age<=60 & age>=22, wgindex[age==60]/wgindex, 1)
  return(factor)
}

#age function
age3 <-function(z) {
  age <- ss1$year - z
  
  return(age)
}

#merging age, index factor functions into social security dataset--make function input objects at top 
age <- age3(birthyr)
ifactor <- age2(birthyr)

Z <- data.frame(ss1, age, ifactor)
######subsetting data set to only include ages 30-64--adjust this if needed#####
Z2 <- subset(Z, age_range)

#salary function
salary <- function(x,y) {
  
  len <- length
  sal <- numeric(len)
  sal[1] <- x
  for(i in 2:len) {
    sal[i] <- sal[i-1]*y
  }
  return(sal)
}
sal <- salary(startsal,salgrowth)

#merging all function into single social security data set
ss2 <- data.frame(Z2, sal)

#indexing wage history
ss2$indsal <- pmin(ss2$sal*ss2$ifactor, ss2$ifactor*ss2$maxsal)

#average indexed monthly earnings
high35 <-tail(sort(ss2$indsal[ss2$age>=22 & ss2$age<=64]), 35)
aime <- floor(sum(high35)/420)

#primary insurance amount pieces
piece1a <- ifelse(aime>=ss2$pia1[ss2$age==62], ss2$pia1[ss2$age==62], aime)
piece2a <- ifelse(aime>=ss2$pia1, 
                  pmin((aime - ss2$pia1[ss2$age==62]), (ss2$pia2[ss2$age==62] - ss2$pia1[ss2$age==62])), 0)
piece3a <- ifelse(aime>=ss2$pia2[ss2$age==62], aime - ss2$pia2[ss2$age==62], 0)

piece1b <- piece1a*.9
piece2b <- ifelse(aime>piece1a, piece2a*.32, 0)
piece3b <- piece3a*.15

totpia=round(piece1b + piece2b + piece3b, digits=1)    #need to learn how to round down

#adjusting cola to allow for multiplication by base benefit
ss2$cola2 <- ss2$cola+1

#benefit at age 65
pia65 <- totpia*ss2$cola2[ss2$age==62]*ss2$cola2[ss2$age==63]*ss2$cola2[ss2$age==64]
#benefit65=floor(pia65*(1-(5/9)*24*.01))
benefit65=floor(ifelse(birthyr<1960, 
                 pia65*(1-(5/9)*12*.01),
                 pia65*(1-(5/9)*24*.01)))

#benefit at age 62
benefit62=floor(totpia*(1-(5/9)*36*.01)*(1-(5/12)*24*.01))

#full benefit at age 67
fullbenefit=floor(totpia*ss2$cola2[ss2$age==62]*ss2$cola2[ss2$age==63]*ss2$cola2[ss2$age==64]*
  ss2$cola2[ss2$age==65]*ss2$cola2[ss2$age==66])

#increased benefit at age 70
benefit70=floor(fullbenefit*(1+(16/24)*36*.01))

#benefits in 2015 dollars with 2.7% inflation
realb62=round(benefit62/(1.027^(ss2$year[ss2$age==62]-2015)), digits=0)
realb65=round(benefit65/(1.027^(ss2$year[ss2$age==65]-2015)), digits=0)
realb67=round(fullbenefit/(1.027^(ss2$year[ss2$age==67]-2015)), digits=0)
realb70=round(benefit70/(1.027^(ss2$year[ss2$age==70]-2015)), digits=0)

#replacement rates
rate62=(benefit62*12)/ss2$sal[ss2$age==64]
rate65=(benefit65*12)/ss2$sal[ss2$age==64]
rate67=(fullbenefit*12)/ss2$sal[ss2$age==64]
rate70=(benefit70*12)/ss2$sal[ss2$age==64]


#creating consolidated social security benefit data frame
plot <- data.frame(
  year=factor(c(ss2$year[ss2$age==62], ss2$year[ss2$age==65], ss2$year[ss2$age==67], ss2$year[ss2$age==70])),
  type = factor(c("age 62 benefit","age 65 benefit","age 67 benefit", "age 70 benefit")),
  benefit = c(benefit62, benefit65, fullbenefit, benefit70),
  benefit15 = c(realb62, realb65, realb67, realb70),
  rate = c(rate62, rate65, rate67, rate70)
)

#benefit amount, nominal dollars
ggplot(data=plot, aes(x=year, y=benefit, fill=type)) + 
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  geom_text(aes(label=dollar(benefit)), hjust=0.3, vjust=-.1) +
  scale_fill_manual(values=c("#999999", "#E69F00", "navy", "darkgreen")) +
  scale_x_discrete(breaks=c(ss2$year[ss2$age==62], ss2$year[ss2$age==65], 
                              ss2$year[ss2$age==67], ss2$year[ss2$age==70])) +
  scale_y_continuous(labels = dollar) +
  ggtitle("Social Security Monthly Benefit, Nominal Dollars")

#benefit amount, inflation-adjusted 2015 dollars
ggplot(data=plot, aes(x=year, y=benefit15, fill=type)) + 
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  geom_text(aes(label=dollar(benefit15)), hjust=0.3, vjust=-.1) +
  scale_fill_manual(values=c("#999999", "#E69F00", "navy", "darkgreen")) +
  scale_x_discrete(breaks=c(ss2$year[ss2$age==62], ss2$year[ss2$age==65], 
                            ss2$year[ss2$age==67], ss2$year[ss2$age==70])) +
  scale_y_continuous(labels = dollar) +
  ggtitle("Social Security Monthly Benefit, 2015 Dollars")

#replacement rate
ggplot(data=plot, aes(x=year, y=rate, fill=type)) +
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  geom_text(aes(label=percent(rate)), hjust=0.3, vjust=-.1) +
  scale_fill_manual(values=c("#999999", "#E69F00", "navy", "darkgreen")) +
  scale_x_discrete(breaks=c(ss2$year[ss2$age==62], ss2$year[ss2$age==65], 
                            ss2$year[ss2$age==67], ss2$year[ss2$age==70])) +
  scale_y_continuous(labels = percent) +
   ggtitle("Social Security Income Replacement Rate")