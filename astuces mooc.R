#Astuces mooc


#on attach les données pour ne pas remettre l'indication data= dans nos fonctions
attach(sathop) 

#deparse.level=2 pour afficher les noms des vecteurs dans la table
table(sat$recommander,sat$recommander.b,useNA = "always",deparse.level = 2) 

#pour afficher les courbes dans une fenêtre