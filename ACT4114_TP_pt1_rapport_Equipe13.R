###
### Analyse préliminaire (rapport 1) - Équipe 13
### ACT-4114, Hiver 2024
###

#
#   Ce fichier contient le code d'analyse exploratoire des données,
#   des étapes de pré-traitement des variables et d'utilisation de techniques
#   de classification non-supervisée. Le fichier est séparé en plusieurs
#   sections énumérées ci-dessous.
#
#   1. Importation des données et paquetages
#   2. Pré-traitement des variables
#   3. Analyse exploratoire des données
#   4. ...
#

##
## 1. Importation des données et paquetages
##

# Paquetages requis
liste.paquetage <- c("ggplot2", "dplyr", "CASdatasets")

# On installe les paquetages de la liste qu'on a pas déjà
inst <- liste.paquetage %in% installed.packages()
if(length(liste.paquetage[!inst]) > 0) install.packages(liste.paquetage[!inst])
lapply(liste.paquetage, require, character.only = TRUE)

# Charger les données en R
# Le jeux de données vien du paquetage "CASdatasets"
data("pg15training")

# On va créer un nouvel objet pour éviter de modifier le jeux
# de données original
data.analyse <- pg15training

##
## 2. Pré-traitement des variables
##
str(data.analyse)
summary(data.analyse)

# Polices en double
ids <- data.analyse[duplicated(data.analyse$PolNum), "PolNum"]
data.analyse[data.analyse$PolNum %in% ids[1], ]
data.analyse[data.analyse$PolNum %in% ids[2], ]
# les 21 premières ?
(data.analyse$PolNum %in% ids)[1:22] # oui, on doit les enlever
data.analyse <- data.analyse[-(1:21), ]
any(duplicated(data.analyse$PolNum))
nrow(data.analyse)

##
## 3. Analyse exploratoire des données
##
colnames(data.analyse)

# Numtppd (endogène)

# Exppdays (exposition)

# Variables exclues :
# - Polnum
# - Numtpbi
# - Indtppd
# - Indtpbi

# CalYear
table(data.analyse$CalYear)

# Gender
table(data.analyse$Gender)
ggplot(data.analyse, aes(x=Numtppd))+
    geom_bar(aes(y = after_stat(prop), fill=Gender), position="dodge")+
    scale_y_continuous(labels = scales::percent)
data.analyse %>% group_by(Gender) %>% summarise(Moyenne = mean(Numtppd))

# Type
table(data.analyse$Type)
ggplot(data.analyse, aes(x=Numtppd))+
    geom_bar(aes(y = after_stat(prop), fill=Type), position="dodge")+
    scale_y_continuous(labels = scales::percent)

# Category

# Occupation

# Age

# Group1

# Bonus

summary(data.analyse$Bonus)
summary(as.factor(data.analyse$Bonus))
data.analyse$Bonus[which(data.analyse$Age == 18)] # ceux de 18 ans on 0 comme bonus car premiere année dans le contrat
data.analyse$Age[which(data.analyse$Bonus == 0)] # pas seulement ceux de 18 ans qui on 0 de bonus
# imputation stochastique pour les 18 ans car bonus biaisé ???

# Moyenne de réclamations par bonus
moy_par_bonus <- data.analyse %>% group_by(Bonus) %>% summarise(
  Count=length(Numtppd), Moyenne=mean(Numtppd), dev_std=sd(Numtppd)
)
moy_par_bonus
# IC approximatif 80 % par bonus
moy_par_bonus$low_ic <- moy_par_bonus$Moyenne + qnorm(0.1)*moy_par_bonus$dev_std/sqrt(moy_par_bonus$Count)
moy_par_bonus$upp_ic <- moy_par_bonus$Moyenne + qnorm(0.9)*moy_par_bonus$dev_std/sqrt(moy_par_bonus$Count)
# graphique moyenne d'accidents par bonus avec IC 80 %
ggplot(moy_par_bonus) + geom_point(aes(x=Bonus, y=Moyenne), col="darkblue", size=2.5) +
  geom_errorbar(aes(x=Bonus, ymin=low_ic, ymax=upp_ic), width=0.5) +
  labs(title = "Moyenne de réclamation pour la couverture étudiée\nen fonction du Bonus avec un IC approximatif 80%",
       y="Moyenne des réclamtations")
### très linéaire sauf le 0 a cause des 18 ans

# Poldur

summary(data.analyse$Poldur)
summary(as.factor(data.analyse$Poldur))
# assez de donées pour chaque année

# Moyenne de réclamations par durée de la police
moy_par_bonus <- data.analyse %>% group_by(Poldur) %>% summarise(
  Count=length(Numtppd), Moyenne=mean(Numtppd), dev_std=sd(Numtppd)
)
moy_par_bonus
# IC approximatif 80 % par durée de la police
moy_par_bonus$low_ic <- moy_par_bonus$Moyenne + qnorm(0.1)*moy_par_bonus$dev_std/sqrt(moy_par_bonus$Count)
moy_par_bonus$upp_ic <- moy_par_bonus$Moyenne + qnorm(0.9)*moy_par_bonus$dev_std/sqrt(moy_par_bonus$Count)
# graphique moyenne d'accidents par durée de la police avec IC 80 %
ggplot(moy_par_bonus) + geom_point(aes(x=Poldur, y=Moyenne), col="darkblue", size=2.5) +
  geom_errorbar(aes(x=Poldur, ymin=low_ic, ymax=upp_ic), width=0.5) +
  labs(title = "Moyenne de réclamation pour la couverture étudiée\nen fonction de la durée de la police avec un IC approximatif 80%",
       y="Moyenne des réclamtations")
### 3 catégorie (0, 1-9, 10+) 0 très significatif le reste pas vraiment

# Value

# Adind
summary(data.analyse$Adind) # 0/1

ggplot(data.analyse, aes(x=Numtppd))+
  geom_bar(aes(y = after_stat(prop), fill=as.factor(Adind)), position="dodge")

mean(data.analyse$Numtppd[which(data.analyse$Adind == 0)]) # moyenne pour Adind = 0
mean(data.analyse$Numtppd[which(data.analyse$Adind == 1)]) # moyenne pour Adind = 1
# si tes pas assuré des deux bords tu fais plus d'accident

# SubGroup2

summary(data.analyse$SubGroup2) ## beaucoup troop de niveau on va prendre Group2 à la place

# Group2

# region (interactionna vec densité)
summary(data.analyse$Group2)

ggplot(data.analyse, aes(x=Numtppd))+
  geom_bar(aes(y = after_stat(prop), fill=as.factor(Group2)), position="dodge")


meanGroup2 <- data.frame(
  L = mean(data.analyse$Numtppd[which(data.analyse$Group2 == 'L')]),
  M = mean(data.analyse$Numtppd[which(data.analyse$Group2 == 'M')]),
  N = mean(data.analyse$Numtppd[which(data.analyse$Group2 == 'N')]),
  O = mean(data.analyse$Numtppd[which(data.analyse$Group2 == 'O')]),
  P = mean(data.analyse$Numtppd[which(data.analyse$Group2 == 'P')]),
  Q = mean(data.analyse$Numtppd[which(data.analyse$Group2 == 'Q')]),
  R = mean(data.analyse$Numtppd[which(data.analyse$Group2 == 'R')]),
  S = mean(data.analyse$Numtppd[which(data.analyse$Group2 == 'S')]),
  `T` = mean(data.analyse$Numtppd[which(data.analyse$Group2 == 'T')]),
  U = mean(data.analyse$Numtppd[which(data.analyse$Group2 == 'U')])
) # visualiser la moyenne par categorie
meanGroup2

sdGroup2 <- data.frame(
  L = sd(data.analyse$Numtppd[which(data.analyse$Group2 == 'L')]),
  M = sd(data.analyse$Numtppd[which(data.analyse$Group2 == 'M')]),
  N = sd(data.analyse$Numtppd[which(data.analyse$Group2 == 'N')]),
  O = sd(data.analyse$Numtppd[which(data.analyse$Group2 == 'O')]),
  P = sd(data.analyse$Numtppd[which(data.analyse$Group2 == 'P')]),
  Q = sd(data.analyse$Numtppd[which(data.analyse$Group2 == 'Q')]),
  R = sd(data.analyse$Numtppd[which(data.analyse$Group2 == 'R')]),
  S = sd(data.analyse$Numtppd[which(data.analyse$Group2 == 'S')]),
  `T` = sd(data.analyse$Numtppd[which(data.analyse$Group2 == 'T')]),
  U = sd(data.analyse$Numtppd[which(data.analyse$Group2 == 'U')])
) #visualiser le sd apr categorie
sort(meanGroup2[1,])
sort(sdGroup2[1,])

moy_par_bonus <- data.analyse %>% group_by(Group2) %>% summarise(
  Count=length(Numtppd), Moyenne=mean(Numtppd), dev_std=sd(Numtppd)
)
moy_par_bonus
# IC approximatif 80 % par durée de la police
moy_par_bonus$low_ic <- moy_par_bonus$Moyenne + qnorm(0.1)*moy_par_bonus$dev_std/sqrt(moy_par_bonus$Count)
moy_par_bonus$upp_ic <- moy_par_bonus$Moyenne + qnorm(0.9)*moy_par_bonus$dev_std/sqrt(moy_par_bonus$Count)
# graphique moyenne d'accidents par durée de la police avec IC 80 %
ggplot(moy_par_bonus) + geom_point(aes(x=Group2, y=Moyenne), col="darkblue", size=2.5) +
  geom_errorbar(aes(x=Group2, ymin=low_ic, ymax=upp_ic), width=0.5) +
  labs(title = "Moyenne de réclamation pour la couverture étudiée\nen fonction de la durée de la police avec un IC approximatif 80%",
       y="Moyenne des réclamtations")
### sort by mean serrait beaucoup plus clair

# Density

