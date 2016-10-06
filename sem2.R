##INTERVALLES DE CONFIANCE

#IC avec la fonction binom.confint (utilise 1.96)
install.packages ("binom")
library (binom)
binom.confint (300,1000,method="all")
binom.confint (300,1000,method="exact")
#avec 300 : nb de personne ayant la maladie 1000:nb de personnes exposées

########exo
smp<-read.csv2("smp2.csv")
enfdep<-subset(smp,dep.cons == 1,n.enfant)
str(enfdep)
mean(enfdep$n.enfant,na.rm=T)
mean(smp[smp$dep.cons==1,"n.enfant"],na.rm=T)

intervalle interquartile
quantile(smp[smp$age<35,"duree"],na.rm=T)
mean(smp[smp$suicide.past == 1, 'dur.interv'], na.rm=TRUE)
mean(smp$dur.interv[smp$suicide.past == 1],na.rm=T)

x<-smp$age
hist(x,nclass=6)
hist(x, breaks=6, prob=TRUE)
hist(x, breaks=6)
which(is.na(smp[c(20, 221, 342, 446, 531),]))
prop.table(table(smp$prof,useNA = "a"))
age<-ifelse(smp$age>=20 & smp$age<=30,"sel",smp$age)
table(age)
