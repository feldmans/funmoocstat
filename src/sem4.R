
install.packages("dplyr")
install.packages("Epi")
library(Epi)
library(dplyr)

#a<-read.csv2("C:/Users/sf/Documents/git/funmooc M2MSR/data/smp2.csv")
a<- read.csv2("data/smp2.csv")

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

# age<-a$age
# enfant<-a$n.enfant

sel<-a %>% filter (n.enfant>=4 & prof%in%c("sans emploi","ouvrier","cadre","employe")) %>% select(age,n.enfant,prof) 
ag<-aggregate(sel$age,list(sel$prof),var)
max(ag[,2])/min(ag[,2])


a[a$n.enfant>=4 & a$prof%in%c("sans emploi","ouvrier","cadre","employe"),c("age","n.enfant","prof")]#?? pourquoi ne marche pas?


# 5. Nous souhaitons réaliser une ANOVA à un facteur en considérant l'âge (age) comme variable réponse,
# et la taille de la fratrie (n.fratrie) recodée en 3 classes (0-2, 3-4, 5+) comme variable explicative. 
# Les bornes des intervalles sont inclues pour chacune des trois classes. 
# Indiquer le résultatdu test F de Fisher-Snedecor d’égalité des moyennes :
library (car)
#a$n.fratriebis <- recode (a$n.fratrie,"0:2='0-2'; 3:4='3-4'")  
a$n.fratriebis <- a$n.fratrie
a$n.fratriebis <- ifelse(a$n.fratriebis >=5 ,"5+",a$n.fratriebis) #attention mettre cette ligne en premier pour que 10 soit reconnue >5, sinon devient caractère
a$n.fratriebis <- ifelse(a$n.fratriebis %in% c(0:2),"0-2",a$n.fratriebis)
a$n.fratriebis <- ifelse(a$n.fratriebis %in% c(3:4),"3-4",a$n.fratriebis)
table(a$n.fratrie,a$n.fratriebis)

modfrat<-lm(age~n.fratriebis,a)
drop1(modfrat,test="F")
drop1(modfrat,.~.,test="F")

# 6. Nous nous intéressons à la relation entre la variable séparation (separation) et l'âge (age) des individus,
# que l'on modélise à l'aide d'une régression logistique.
# Donner la borne inférieure de l'intervalle de confiance à 95 % pour l'odds-ratio (3 chiffres après la virgule).

