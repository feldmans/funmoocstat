#######################
#       DEVOIR 1      #
#######################
library(prettyR)
sat <- read.csv2("satisfaction_hopital.csv")

#### QUESTION 1 #########
#Pour les trois variables catégorielles du fichier, présentez les pourcentages de sujets relevant de chacune des modalités.

#Variables catégorielles : service, sexe, profession (ce sont les variables dont la moyenne n'a aucun sens)
service<-round(prop.table(table(sat$service))*100,2)
sexe<-round(prop.table(table(sat$sexe))*100,2)
profession<-round(prop.table(table(sat$profession))*100,2)

#Explications:
table(sat$service) #table pour les effectifs
prop.table(table(sat$service)) #prop.table pour la proportion de sujet dans chaque classe 
prop.table(table(sat$service))*100 # on multiplie par 100 pour avoir un pourcentage
round(prop.table(table(sat$service))*100,2) #j'arrondie à 2 décimales

#presentation des résultats dans un mini rapport:
#version brute
ls_cat<- list(service,sexe,profession);names(ls_cat)<-c("service","sexe","profession"); ls_cat
#version amélioree :
r1<-paste("\n######Présentation des variables catégorielles######\n\nSexe :") #j'introduis des \n ici car ils sont lu comme des sauts de ligne plus tard par cat()
r2<-paste0("\n  ",c("homme : ","femme : "),sexe," %") #paste0 lit les valeurs d'un vecteur un élément après l'autre. lancer r2 pour mieux comprendre
r3<-"\n\nService :"
r4<-paste0("\n  service ",names(service)," : ",service," %")
r5<-"\n\nProfession :"
r6<-paste0("\n  profession ",names(profession)," : ",profession," %")
cat(r1,r2,r3,r4,r5,r6)

#####Résultat final
cat(r1,r2,r3,r4,r5,r6)




#### QUESTION 2 #########
#Pour les autres variables, donnez de façon synthétique : moyenne, médiane, écart-type, minimum, maximum, nombre de données disponibles (non manquantes).

#vecteur avec les autres variables : 
#je crée un tableau provisoire avec uniquement les variables d'intéret pour la question. ne pas oublier les "" poyr les colonnes
autres <- sat[,c("age","amelioration.sante","amelioration.moral","recommander","score.information","score.relation")]  

#on donne les options voulues dans num.desc
desc_autres<-describe(autres,num.desc=c("mean","median","sd","min","max","valid.n"),xname="non categorial variables") ;desc_autres

#je peux en faire un data_frame pour modifier les titres
df_autres <- data.frame(do.call(rbind,desc_autres[[1]])) #describe donne des listes, il faut donc extraire avec [[]] puis coller les listes qui sont au sein des listes avec do.call(rbind, )...
names (df_autres) <- c("moyenne", "médiane", "écart-type", "minimum", "maximum", "nombre de données disponibles")
df_autres

#######Résultat final
df_autres


#### QUESTION 3 #########
#Faites un histogramme du score de relation (score.relation).
hist(sat$score.relation,col="darkslategray3", main=" score relatif à la qualité des relations\n avec le personnel soignant \n pendant le séjour", xlab= "score de relation (10 à 40)", ylab="Fréquence")
#NB : liste of R colors here : http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf



#### QUESION 4 ##########
#A l’aide de deux « boxplots », représentez côte à côte la distribution du score de relation chez les hommes et les femmes.
data<- list(masc=sat$score.relation[sat$sexe=="0"], fem=sat$score.relation[sat$sexe=="1"])
boxplot(data,main="Score de relation par sexe",ylab="Score", xlab="Sexe",names =c("hommes","femmes"), col=c("darkslategray2","pink"),width=table(sat$sexe)) #width : largeur des boites proportionnelle à l'effectif d'homme et de femmes
