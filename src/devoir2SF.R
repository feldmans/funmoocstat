#######################
#       DEVOIR 2      #
#######################


sat<- read.csv2("satisfaction_hopital.csv") #je charge la base de données et je la nomme sat

summary(sat) #description de la base de donnée
dim(sat) #534 lignes, 9 variables

#### QUESTION 1 #########

#J'ETUDIE MA VARIABLE sat$recommander
table(sat$recommander,useNA = "a") #useNA="a" pour afficher les missings
# 0    1    2   <NA> 
# 16  120  269  129 
#J'ai une variable à 3 classes 0,1,2 avec respectivement 16,120,269 sujets et 129 missings


#JE LA TRANSFORME EN VARIABLE BINAIRE
#1. Je crée ma variable à partir de recommander (JE fais une copie)
sat$recommander.b <- sat$recommander 

#2. Je la transforme
#Je sélectionne les lignes qui satisfont ma condition(soit 0,1 soit 2) et pour ces lignes, je donne une nouvelle valeur pour la variable recommander.b 
sat[sat$recommander.b %in% c(0,1), "recommander.b"] <- 0  # %in% permet de sélectionner ligne qui ont un résultat soit de 0 soit de 1. Attention, ne pas utiliser == qui va recycler le vecteur c(0,1) 
sat[sat$recommander.b %in% 2, "recommander.b"] <- 1 # %in% permet également de sélectionner les NA


#VERIFICATION 
table(sat$recommander.b,useNA="a")
# 0    1    <NA> 
# 136  269  129 
#Je retombe bien sur mes pieds : 16+120=136 et j'ai toujours 129 NA

#correction:
table(sat$recommander.b,sat$recommander,deparse.level=2,useNA="always")


# DONC REPONSE: 
sat[sat$recommander.b %in% c(0,1), "recommander.b"] <- 0
sat[sat$recommander.b %in% 2, "recommander.b"] <- 1



#### QUESTION 2 #########

install.packages("Epi") #J'installe le package Epi qui a une fonction donnant l'OR
library(Epi) #Je n'oublie pas de charger ensuite la librairie

twoby2(sat$recommander.b,sat$sexe)

fisher.test(sat$recommander.b,sat$sexe)
# Sample Odds Ratio: 1.0837  
# OR est proche de 1, on peut penser que les 2 variables sont indépendantes, mais il faut regarder l'intervalle de confiance

#Intervalle de confiance à 95%: [0.7169 ; 1.6383]
#L'IC comprend 1, je ne met donc pas en évidence d'association entre recommander.b et sexe (la p value>0.05 nous dit bien sûr la même chose)



#### QUESTION 3 #########

#CALCUL DU COEFFICIENT DE CORRELATION entre « score.relation » et « age ».
cor(sat$score.relation,sat$age,use="complete.obs")
#coefficient de corrélation = 0.09596955, ce qui semble très faible, on peut le confirmer par un test


#TEST DE CORRELATION 

####################################
#Attention aux conditions de validité : 1 des 2 variables doit suivre une loi normale

#Je regarde dans quelle mesure la distribution de mes 2 variables s'approche de celle d'une loi normale
hist(sat$score.relation) #complètement asymétrique : n'approche pas une loi normale
hist(sat$age) #courbe en cloche à peu près symétrique, ressemblant donc à une loi normale
#ou:
qqnorm(sat$score.relation) ; qqline(sat$score.relation)#pas normal
qqnorm(sat$age) ; qqline(sat$age) #à peu près normal
#une de mes 2 variable a une distribution suivant une loi normale(age), je peux faire un test de corrélation de pearson


#############################
#test de corrélation de pearson
cor.test(sat$score.relation,sat$age) #garder les NA ne change rien au test
#p-value = 0.07336
#p>0.05, le coefficient de corrélation n'est pas significativement différent de 0,
#je ne peux donc pas rejeter l'hypothèse nulle qui est que le coefficient est égale à 0 (indépendance des 2 variables)



#### QUESTION 4 #########

#Je regarde d'abord mes 2 variables score et sexe
summary(sat$score.relation,useNA = "a") #moyenne globale de 35.22
table(sat$sexe,useNA="a") #environ autant d'homme que de femme et  n>30 par groupe

#MOYENNE PAR SOUS GROUPE homme(0)/femme(1)
aggregate(score.relation~sexe,sat,mean) #na.rm=T inutile avec aggregate, je préfère aggregate à by et tapply car j'obtiens un tableau et non une liste
# sexe score.relation
# 1    0       35.48087
# 2    1       34.92771


#COMPARAISON DE MOYENNE

#######################################
#conditions de validité du test de student: 

#1-distribution de la variable suit une loi normale
hist(sat$score.relation) #n'a pas l'air de suivre une loi normale
qqnorm(sat$score.relation);qqline(sat$score.relation)

#2-variances égales
aggregate(score.relation~sexe,sat,sd)
# sexe score.relation
# 1    0       4.552311
# 2    1       4.697603
#4.5 et 4.69 sont très proches, la condition des variances égales est satisfaite

#La variable ne suit pas une loi normale mais l'effectif est supérieur à 30 par groupe (et les variances sont égales).
#Je considère donc que mes conditions sont validées
#Je choisi donc d'utiliser le test de student plutôt que le test de wilcoxon afin de ne pas me fermer aux tests paramétriques par la suite.

#################################
#test de student
t.test(score.relation~sexe,sat)
#p-value = 0.2657
#le test de comparaison de moyenne est non significatif car >0.05
#Je ne peux donc pas rejeter l'hypothèse d'une égalité de la moyenne des scores chez les hommes et chez les femmes




#correction : pour faire une belle fenêtre avec des graphes
windows(record=TRUE)
par(mfrow=c(2,2))
hist(satis$age, xlab="Age", ylab="Densité", main="Histrogramme pour age",col="grey", ylim=c(0,0.025), freq=FALSE)
# Ajouter une courbe normale théorique en pointillé
curve(dnorm(x, mean=mean(satis$age, na.rm=TRUE), sd=sd(satis$age, na.rm=TRUE)), lty="dotted", add=TRUE)

qqnorm(satis$age, xlab="Quantiles théoriques", ylab="Quantiles de la population age", main="Q-Q Plot", pch=1)
qqline(satis$age)

hist(satis$score.relation, xlab="Score relation", ylab="Densité", main="Histrogramme pour score relation",col="grey", ylim=c(0,0.20), freq=FALSE)
# Ajouter une courbe normale théorique en pointillé
curve(dnorm(x, mean=mean(satis$score.relation, na.rm=TRUE), sd=sd(satis$score.relation, na.rm=TRUE)), lty="dotted", add=TRUE)

qqnorm(satis$score.relation, xlab="Quantiles théoriques", ylab="Quantiles de la population score relation", main="Q-Q Plot", pch=1)
qqline(satis$score.relation)
