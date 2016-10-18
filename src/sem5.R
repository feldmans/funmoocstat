library(survival)

# 1. On souhaite comparer le risque de rechute de la maladie alcoolique dans deux sous-groupes:
#     le groupe des plus de 50 ans (strictement plus de 50 ans, recodé en “1”) 
#     et le groupe des moins de 50 ans (50 ou moins, recodé en “0”). 
#   Donner la p-value associée au test statistique correspondant (4 chiffres après la virgule):
  
alc <- read.csv2("data/alcool.csv")

alc$SEVRE #1 = rechute
alc$AGE50 <- ifelse (alc$AGE>50,1,0)

#coxph(Surv(t,SEVRE)~AGE50,data=alc) -> NON c'est pour variable quantitative
survdiff(Surv(t,SEVRE)~AGE50, data=alc)

# 2. On souhaite tester l'association entre le risque de rechute de la maladie alcoolique 
#       et les variables SEXE, AGE et l'interaction entre les variables SEXE et AGE. 
#   Donner la p-value associée à l'interaction entre les variables SEXE et AGE
#       dans le test correspondant (2 chiffres après la virgule):
coxph(Surv(t,SEVRE)~SEXE*AGE,data=alc)


smp<-read.csv2("data/smp2.csv")
smp$dur.interv
var<- c("age","n.enfant","dur.interv")
cor(smp[,var],use="pairwise.complete.obs")
cah <- hclust(dist(t(scale(smp[,var]))), method = "ward.D")
plot(cah)
