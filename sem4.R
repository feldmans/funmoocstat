
install.packages("dplyr")
install.packages("Epi")
library(Epi)
library(dplyr)

a<-read.csv2("C:/Users/sf/Documents/git/funmooc M2MSR/data/smp2.csv")

z<-a$suicide.hr
y<-a$discip
mod<-glm(z ~ y, data=a, family=binomial("logit")) 
summary(mod)
exp(coefficients(mod))
prop.table(table(z))
twoby2(1-y,1-z)
y
