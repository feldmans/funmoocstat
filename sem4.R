
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

predict(mod)

# 4. Nous considérons l'âge (age) des individus ayant 4 enfants ou plus (n.enfant)
# et dont la catégorie socio-professionnelle (prof) figure parmi les modalités
# suivantes : « sans emploi », « ouvrier », « cadre » et « employé ». 
# Pour ce sous-ensemble de l'échantillon du data frame smp, 
# le rapport entre les deux variances les plus extrêmes dans ces 4 groupes est :

age<-a$age
enfant<-a$n.enfant

sel<-a %>% filter (n.enfant>=4 & prof%in%c("sans emploi","ouvrier","cadre","employe")) %>% select(age,n.enfant,prof) 
ag<-aggregate(sel$age,list(sel$prof),var)
min(ag[,2])/


a[a$n.enfant>=4 & a$prof%in%c("sans emploi","ouvrier","cadre","employe"),c("age","n.enfant","prof")]#?? pourquoi ne marche pas?
table(enfant,prof)
