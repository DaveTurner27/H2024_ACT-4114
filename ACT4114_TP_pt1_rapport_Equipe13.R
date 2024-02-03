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
ggplot(data.analyse, aes(x=Numtppd))+
    geom_bar(aes(y = after_stat(prop), fill=as.factor(CalYear)), position="dodge")+
    scale_y_continuous(labels = scales::percent)
data.analyse %>% group_by(as.factor(CalYear)) %>% summarise(Moyenne = mean(Numtppd))

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
data.analyse %>% group_by(Type) %>% summarise(Moyenne = mean(Numtppd))

# Category

# Occupation

# Age
table(data.analyse$Age)
plot(table(data.analyse$Age))

# Moyenne de réclamations par age
moy_par_age <- data.analyse %>% group_by(Age) %>% summarise(
    Count=length(Numtppd), Moyenne=mean(Numtppd), dev_std=sd(Numtppd)
    )
moy_par_age
# IC approximatif 80 % par age
moy_par_age$low_ic <- moy_par_age$Moyenne + qnorm(0.1)*moy_par_age$dev_std/sqrt(moy_par_age$Count)
moy_par_age$upp_ic <- moy_par_age$Moyenne + qnorm(0.9)*moy_par_age$dev_std/sqrt(moy_par_age$Count)
# graphique moyenne d'accidents par age avec IC 80 %
ggplot(moy_par_age) + geom_point(aes(x=Age, y=Moyenne), col="darkblue", size=2.5) +
    geom_errorbar(aes(x=Age, ymin=low_ic, ymax=upp_ic), width=0.5) +
    labs(title = "Moyenne de réclamation pour la couverture étudiée\nen fonction de l'age avec un IC approximatif 80%",
         y="Moyenne des réclamtations")
# graphique de l'ecart-type estimé en fct de l'age
ggplot(moy_par_age) + geom_point(aes(x=Age, y=dev_std))

# Group1

# Bonus

# Poldur

# Value

# Adind

# SubGroup2

# Group2

# Density

