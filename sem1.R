install.packages("gplots")
install.packages("prettyR")

smp.c<-read.csv2("smp1.csv")
str(smp.c)

barplot(table(smp.c$prof))
pie(table(smp.c$prof))
hist(smp.c$age, col="grey", main="age", xlab= "age(année)")

#moustache
boxplot(smp.c$age ~smp.c$rs)

#plot
plot(smp.c$age,smp.c$n.enfant) #pb : un seul point si 2 var identiques
plot(jitter(smp.c$age),jitter(smp.c$n.enfant)) #pour bouger légèrement les points 

#courbe en fonction du temps : plotmeans
repdat<-read.csv2("outils_hdrs.csv")
str(repdat)
library(gplots)
plotmeans(repdat$HDRS ~repdat$VISIT,gap=0,  barcol="black")

#1 courbe par sujet
interaction.plot(repdat$VISIT,repdat$NUMERO,repdat$HDRS, lty=1, legend=F)#variable temporelle, sujet,var d'intéret

#histogrammes et plot
hist(smp.c$age)
plot(smp.c$n.enfant,smp.c$age)

#description mieux organisée que summarise:
library(prettyR)
describe(smp.c, num.desc=c("mean", "sd", "median",  "min","max", "valid.n"))



#############EXO###########
smp<-read.csv2("smp2.csv")
summary(smp)
min(smp$age,na.rm=T)
#renommer des niveaux
z <- smp$scz.cons
z <- factor(z,levels=c(0,1),labels=c("Non","Oui")) #indiquer les levels pour être sûr de l'ordre d'affectation des nouveaux niveaux
z <- factor(z,labels=c("Non","Oui")) #idem

#table
y<- smp$prof
table(y)

# 4. À partir du data frame smp, nous souhaitons recoder l’âge (variable age) en variable catégorielle 
# en considérant 4 intervalles de classe dont les bornes intermédiaires sont définies à partir des 1er, 2ème et 3ème quartiles.
# 
# Les bornes inférieures et supérieures de la première et dernière classe seront naturellement les valeurs minimale et maximale
# observées pour la variable age. À l'exception de la première classe dont les deux bornes d'intervalle seront fermées 
# (c'est-à-dire que les bornes seront inclues dans l'intervalle), les bornes inférieures des classes suivantes (2 à 4) seront ouvertes,
# et les bornes supérieures fermées. Indiquer l'effectif associé à la 3ème classe ainsi constituée. [question difficile]
a <- smp$age 
quant <- quantile(a,na.rm=T)
a <- ifelse (a >= quant[1] & a <= quant[2],1,a)
a <- ifelse (a > quant[2] & a <= quant[3],2,a)
a <- ifelse (a > quant[3] & a <= quant[4],3,a)
a <- ifelse (a > quant[4] & a<= quant[5],4,a)
table(a)

#Correction : 
summary(smp$age)
 smp$age.cat <- cut(smp$age, breaks = c(19, 28, 37, 48, 83),include.lowest = TRUE,right=T) #include lowest inclus 19, right ferme lintervalle sup et ouvre l'inf
 table(smp$age.cat)


quantile(smp$dur.interv,na.rm=T)

#recoder quantitatif en variable binaire
factor(table(smp$n.fratrie >= 5),labels=c('<5', '5+'))#NON
smp$n.fratrie[smp$n.fratrie < 5] <-'<5';smp$n.fratrie[smp$n.fratrie >= 5] <- '5+';table(smp$n.fratrie) #ne marche pas #la première instruction transforme le vecteur en caractère, et 10 devient alors plus petit que 5(ordre "alphabétique")
smp$n.fratrie[smp$n.fratrie >= 5] <- '5+'; smp$n.fratrie[smp$n.fratrie < 5] <-'<5' ;table(smp$n.fratrie) #marche dans ce sens ! 1234 sont bien <5 par ordre alphabétique
smp<-read.csv2("smp2.csv")
smp$n.fratrie<-factor(smp$n.fratrie >= 5, labels=c('<5', '5+')) #oui : booleen=binaire




###########EXO bis###########
sum(complete.cases(smp$age)) #nombre d'individus sans données manquantes pour la variable age
mean(smp$age[1:10])
median(smp$dur.interv[1:300],na.rm=T)
