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

##
## 3. Analyse exploratoire des données
##
