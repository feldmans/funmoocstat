#######################
#       DEVOIR 3      #
#######################

path<-"C:/Users/sf/Documents/git/funmooc M2MSR"
path<-file.path(getwd()) #je stock le chemin où je me trouve dans une variable
sat <- read.csv2(paste0(path,"/data/satisfaction_hopital.csv")) #je charge la base de données et je la nomme sat
#ne pas oublier le / avant data
#### QUESTION 1 #########
# Estimez le modèle de régression linéaire expliquant la variable « score.relation » par les variables 
# « age », « sexe », « score.information », « amelioration.sante », « amelioration.moral », « profession », « service ». 
# (le script doit inclure la vérification éventuelle des conditions de validité de la méthode utilisée)

#0/Attention à regarder préalablement les variables, les variables catégorielles doivent être recodées en facteur:
sat$profession <- factor(sat$profession)
sat$service <- factor(sat$service)
#1/ J'écris mon modèle de régression linéaire multiple
mod<- lm(score.relation~age+sexe+score.information+amelioration.sante+amelioration.moral+profession+service,sat)

#2/ Je vérifie les conditions de validité de mon modèle avant de regarder mes estimations
#La seule condition vérifiable est la normalité du bruit (de la résiduelle) :
resid(mod) #donne les résiduelles du modèle 
hist(resid(mod)) #permet de représenter graphiquement la distribution des résiduelles
#les résiduelles sont distribuées approxiamtivement selon une loi normale, je peux donc interprêter mes coefficients.

#3/ J'affiche les coefficients de corrélation avec leur p value
mod$coefficients
summary(mod)
#les variables "age" "score.information" et "amelioration.moral" sont significativement associées au score de relation,
#toutes autres variables égales par ailleurs.

#4/Je peux regarder si les variables profession et service sont globalement associées au score de relation
drop1(mod,.~.,test="F")
#Pas d'effet de la profession et du seuil sur le score de relation au seuil de 5%

#### QUESTION 2 #########
# Estimez le modèle de régression logistique expliquant la variable « recommander.b » par les variables 
# « age », « sexe », « score.information », « amelioration.sante », « amelioration.moral », « profession »,  « service ».  
# 
# Notons que la variable « recommander.b » est une transformation de la variable « recommander» en une variable binaire où
# « recommander.b » vaut 0 si « recommander» vaut 0 ou 1, et 1 si « recommander» vaut 2.
# 
# (le script doit inclure la vérification éventuelle des conditions de validité de la méthode utilisée)



#1/Je transforme la variable recommander en variable binaire
sat$recommander.b<-ifelse (sat$recommander==2,1,0)
#Je vérfie que le recodage s'est bien déroulé:
table(sat$recommander,sat$recommander.b,useNA = "always")

#1'/Attention à regarder préalablement les variables explicatives, les variables catégorielles doivent être recodées en facteur:
sat$profession <- factor(sat$profession)
sat$service <- factor(sat$service)

#2/Je construis mon modèle de régression logistique avec recommander.b comme variable à expliquer
mod2<- glm(recommander.b~age+sexe+score.information+amelioration.sante+
             amelioration.moral+profession+service, data=sat, family="binomial")

#3/ Je vérifie mes conditions de validité avant d'interprêter les coefficients
#conditions de validité = au moins 5 à 10 évènement par variable explicatives
#Attention, profession et service seront transformés en 7 variables chacune car 8 classes 
#5+7+7=19 variables explicatives
#il faudrait donc au moins entre 19*5 = 95 et 19*10=190 sujets
nrow(sat) #J'ai 534 sujets (1 patient par ligne), les conditions de validité sont donc vérifiées

#4/ estimation des coefficients du modèle
mod2$coefficients
summary(mod2)
#score.information et amelioration.sante sont statistiquement associées à recommander.b
#5/ interprétation avec les odds ratio:
exp(coefficients(mod2)) #exponentiel du coefficient donne l'OR
table(sat$recommander.b) #recommander.b n'est pas rare donc OR ne peut pas être interprété comme un risque relatif
#5/effet global des variables catégorielles profession et service
drop1(mod2,.~.,test="Chisq") #pas d'effet des 2 variables catégorielle sur recommander.b globalement


