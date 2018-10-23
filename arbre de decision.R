install.packages("C50")
library(C50)
library(rpart)
tennis=read.table("http://www.grappa.univ-lille3.fr/~gilleron/jeuxdonnees/tennum.txt")
tennis
> tennis
         Ciel Temperature Humidite   Vent Jouer
1  Ensoleille        27.5       85 Faible   Non
2  Ensoleille        25.0       90   Fort   Non
3     Couvert        26.5       86 Faible   Oui
4       Pluie        20.0       96 Faible   Oui
5       Pluie        19.0       80 Faible   Oui
6       Pluie        17.5       70   Fort   Non
7     Couvert        17.0       65   Fort   Oui
8  Ensoleille        21.0       95 Faible   Non
9  Ensoleille        19.5       70 Faible   Oui
10      Pluie        22.5       80 Faible   Oui
11 Ensoleille        22.5       70   Fort   Oui
12    Couvert        21.0       90   Fort   Oui
13    Couvert        25.5       75 Faible   Oui
14      Pluie        20.5       91   Fort   Non

ad.tennis = rpart (Jouer ~ Ciel + Temperature + Humidite + Vent, tennis)##predire l'attribut jouer
#en fonction des attributs ciel,humidite ,vent et temperature
ad.tennis
ad.tennis.cnt=rpart.control (minsplit = 1)##minsplit doit être choisi plus petit. Pour celà, nous allons spécifier le paramètre minsplit :
ad.tennis =rpart (Jouer ~ Ciel + Temperature + Humidite + Vent, tennis, control = ad.tennis.cnt)
#####n= 14 

node), split, n, loss, yval, (yprob)
      * denotes terminal node

 1) root 14 5 Oui (0.3571429 0.6428571)  
   2) Ciel=Ensoleille,Pluie 10 5 Non (0.5000000 0.5000000)  
     4) Humidite>=82.5 5 1 Non (0.8000000 0.2000000)  
       8) Temperature>=20.25 4 0 Non (1.0000000 0.0000000) *
       9) Temperature< 20.25 1 0 Oui (0.0000000 1.0000000) *
     5) Humidite< 82.5 5 1 Oui (0.2000000 0.8000000)  
      10) Temperature< 18.25 1 0 Non (1.0000000 0.0000000) *
      11) Temperature>=18.25 4 0 Oui (0.0000000 1.0000000) *
   3) Ciel=Couvert 4 0 Oui (0.0000000 1.0000000) *
> #####
##cnt=parametre de controle
ad.tennis
plot(ad.tennis)
text(ad.tennis)
plot (ad.tennis, uniform=T)
text (ad.tennis, use.n=T, all=T)
plot (ad.tennis, branch=0)
plot (ad.tennis, branch=.7)
text (ad.tennis, use.n=T)
plot (ad.tennis, branch=.4, uniform=T, compress=T)
text (ad.tennis, all=T,use.n=T)
plot (ad.tennis, branch=.2, uniform=T, compress=T, margin=.1)
text (ad.tennis, all=T, use.n=T, fancy=T)
predict(ad.tennis, tennis)##prédire la classe des données du jeu 
predict(ad.tennis, tennis,"class")
predict(ad.tennis, tennis,"prob")
predict(ad.tennis, tennis,"vector")

install.packages("C50")
library(C50)
essay=C5.0(Sans_titre[1:100,-5],Sans_titre[1:100],5)
treeModel = C5.0(x = churnTrain[, -20], y = churnTrain$churn)
library("C50")
data(churn)
treeModel=C5.0(x = churnTrain[, -20], y = churnTrain$churn)
treeModel
summary(treeModel)
plot (treeModel, uniform=T)
ruleModel=C5.0(churn ~ ., data = churnTrain, rules = TRUE)