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
liste.paquetage <- c("ggplot2", "dplyr", "CASdatasets", "MASS")

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

# Variables exclues : ICI
## - Polnum
hist(data.analyse$PolNum)
### Il y a deux bosses, pcq ya deux années hihi fun times.
### numeric discrèteç

ggplot(data.analyse, aes(x = PolNum)) +
    geom_density(aes(fill = CalYear))

summary(data.analyse$PolNum) # pas concluant
### On dirait qu'on a 2001 et 2002 comme début des numéros pour les deux années.

## - Numtpbi
table(data.analyse$Numtpbi)
### grosse masse à 0
### 1, 2, 3 de body injured sinon
### numeric discret

plot(table(data.analyse$Numtpbi))

## - Indtppd
hist(log(data.analyse$Indtppd)) # loi continue, capable de fitter une distrib
summary(data.analyse$Indtppd)
### maximum à 13k$, pas beaucoup ?? revoir loi france ou si limite
### numeric continue

## - Indtpbi
hist(log(data.analyse$Indtpbi))

summary(data.analyse$Indtpbi)
### maximum à 70k$, makes sense plus chere que du materiel
### numeric continu

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
summary(data.analyse$Category)
## Environ les mêmes proportions, un peu moins de small
str(data.analyse$Category) # pas de NA fun times

ggplot(data.analyse, aes(x=Numtppd))+
    geom_bar(aes(y = after_stat(prop), fill=Category), position="dodge")
## Les petites autos font plus de dommages matériels et sont les moins à avoir
## 0 dommage. Plus l'auto est grosse, moins elle fait de dommages matériels. Les
## jeunes ont des petites autos.

## Moyenne du nb accidents des gens qui sont dans la categ large:
mean(data.analyse$Numtppd[which(data.analyse$Category == "Large")])

## Moyenne du nb accidents des gens qui sont dans la categ Medium:
mean(data.analyse$Numtppd[which(data.analyse$Category == "Medium")])

## Moyenne du nb accidents des gens qui sont dans la categ Small:
mean(data.analyse$Numtppd[which(data.analyse$Category == "Small")])

## Comme on avait dit, plus c'est large, moins la moyenne d'Accidents est haute

## catégorielle ordinale

# Occupation
summary(data.analyse$Occupation)
## La plupart sont des employés, 5 catégories
str(data.analyse$Occupation) # pas de NA fun times

ggplot(data.analyse, aes(x=Numtppd))+
    geom_bar(aes(y = after_stat(prop), fill=Occupation), position="dodge")
## les reitred font moins d'accidents, makes sense moins de déplacement, vont pas
## au travail par exemple

## Moyenne du nb accidents des gens qui sont dans la categ Empployed:
mean(data.analyse$Numtppd[which(data.analyse$Occupation == "Employed")])

## Moyenne du nb accidents des gens qui sont dans la categ Housewife:
mean(data.analyse$Numtppd[which(data.analyse$Occupation == "Housewife")])

## Moyenne du nb accidents des gens qui sont dans la categ Retired:
mean(data.analyse$Numtppd[which(data.analyse$Occupation == "Retired")])

## Moyenne du nb accidents des gens qui sont dans la categ Self-employed:
mean(data.analyse$Numtppd[which(data.analyse$Occupation == "Self-employed")])

## Moyenne du nb accidents des gens qui sont dans la categ Unemployed:
mean(data.analyse$Numtppd[which(data.analyse$Occupation == "Unemployed")])

## plus gross moyenne pour les unemployed, makes sense, plus de temps pour rien
## faire et être sur la route. Plus petite pour retired comme dit tantot

# Age

# Group1

# Bonus

# Poldur

# Value
summary(data.analyse$Value)
## Auto entre 1000 et 50k$. Pas tant chere comme auto. Pas de NA hihi

ggplot(data.analyse, aes(x=as.factor(Numtppd), y = Value))+
    geom_boxplot()


hist(data.analyse$Value)
quant <- quantile(data.analyse$Value, c(0.25, 0.5, 0.75))
group1_Value <- data.analyse[data.analyse$Value < quant[1], ]
group2_Value <- data.analyse[data.analyse$Value < quant[2] & data.analyse$Value > quant[1], ]
group3_Value <- data.analyse[data.analyse$Value < quant[3] & data.analyse$Value > quant[2], ]
group4_Value <- data.analyse[data.analyse$Value > quant[3] , ]

df <- data.frame(
    quartile = c("1", "2", "3", "4"),
    moyenne = c(
        mean(group1_Value$Numtppd),
        mean(group2_Value$Numtppd),
        mean(group3_Value$Numtppd),
        mean(group4_Value$Numtppd)
    )
)

ggplot(df, aes(x = quartile, y = moyenne))+
    geom_point()
## On peut voir qu'il y a une tendance croissante, même si le nombre de points
## n'est pas énorme, pas full concluant

# Adind

# SubGroup2

# Group2

# Density
summary(data.analyse$Density)

ggplot(data.analyse, aes(y=as.factor(Numtppd), x = Density))+
    geom_boxplot()
## plus d'accidents quand la densité de population est plus élevé,
## semble être linéaire. moins de données donc moins fiables pour les 3 à droite
table(data.analyse$Numtppd)

## regrouper 5, 6, 7
