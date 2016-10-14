###### sem3

smp<-read.csv2("smp2.csv")
by(smp$age,smp$subst.cons,mean,na.rm=T)
by(smp$age,smp$subst.cons,mean,na.rm=T)
tapply(smp$age,smp$subst.cons,mean,na.rm=T) ## idem mais renvoi 1 liste au lieu de 2 listes mais s'extrait pareil, avec [[]]
with(smp, tapply(age, subst.cons, mean, na.rm=TRUE))#idem
aggregate(age~subst.cons,smp,mean) #idem mais renvoi un tableau et na.rm inutile et renvoit un df

t.test(smp$age,smp$subst.cons,var.equal=T) #si on ne précise pas var equal, on a test de welsh par défaut
t.test(age~subst.cons,smp, var.equal=T) #idem mais avec une écriture en formule

boxplot(age~subst.cons,smp)


#EXO

#4. Dans le data frame smp, la durée d'interview médiane chez les personnes diagnostiquées
#comme dépressives (dep.cons = 1) est supérieure à celle des personnes diagnostiquées comme 
#non-dépressives (dep.cons = 0) de plus de :
aggregate(dur.interv~dep.cons,smp,median)
by(smp$dur.interv,smp$dep.cons,median,na.rm=T)
# dep.cons dur.interv
# 1        0         60
# 2        1         65

#5. Donner la borne inférieure d'un intervalle de confiance à 95 % pour la corrélation linéaire
#(Pearson) entre les variables durée d'intervention (dur.interv) et âge (age) (3 chiffres après 
#la virgule).
cor.test(smp$dur.interv,smp$age)

#6. Nous souhaitons vérifier si la durée d'interview (dur.interv) diffère sensiblement selon que
#les individus ont déjà effectué une tentative de suicide dans le passé ou non (suicide.past) à
#l'aide d'un test de Wilcoxon. Le degré de significativité du test est :
wilcox.test(dur.interv~suicide.past,smp,correct=F)
