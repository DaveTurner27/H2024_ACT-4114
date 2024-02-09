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
## 2. Pré-traitement des variables / Correction d'erreurs
##
str(data.analyse)
summary(data.analyse)

# Polices en double
ids <- data.analyse[duplicated(data.analyse$PolNum), "PolNum"]

data.analyse[data.analyse$PolNum %in% ids[1], ]
data.analyse[data.analyse$PolNum %in% ids[2], ]
# les 21 premières ? voir rapport
(data.analyse$PolNum %in% ids)[1:22] # oui, on doit les enlever
data.analyse <- data.analyse[-(1:21), ]
any(duplicated(data.analyse$PolNum))
nrow(data.analyse)

##
## 3. Analyse exploratoire des données
##
colnames(data.analyse)

# Numtppd (endogène)
(freq <- table(data.analyse$Numtppd))
# Wow 7 accidents !!!
fmp <- freq / 100000
fmp
# Graphique de la fmp
plot(fmp, main="Fonction de masse de probabilité empirique\npour le nombre d'accidents",
     lwd=4, col="red", xlab="Nombre d'accidents", ylab="Proportion")
mean(data.analyse$Numtppd)
var(data.analyse$Numtppd)
summary(data.analyse$Numtppd)

# Exppdays (exposition)
table(data.analyse$Exppdays)
summary(data.analyse$Exppdays)
hist(data.analyse$Exppdays / 365)
# très grande proportion de police complète!

# Variables exclues : ICI
## - Polnum
hist(data.analyse$PolNum)
### On peut remarquer qu'il y a deux bosses sur l'histogramme. Pour l'instant,
### l'hypothèse est que ça représentre possiblement deux années.
### La variable est numérique discrète.

ggplot(data.analyse, aes(x = PolNum)) +
    geom_density(aes(fill = CalYear))

summary(data.analyse$PolNum) # Le sommaire n'est pas concluant.

## - Numtpbi
table(data.analyse$Numtpbi)
### On peur remarquer une grosse masse à 0 qui représente toutes les fois où il
### n'y a pas eu de blessés.
### L'étendue est petit ensuite. Les options sont 1, 2, 3 blessés sinon.
### La variable est numérique dicrète.

plot(table(data.analyse$Numtpbi))

## - Indtppd
hist(log(data.analyse$Indtppd))
### L'histogramme montre une variable continue. On pourrait essayer de trouver
### la distribution appropriée aux données.

summary(data.analyse$Indtppd)
### Le maximum du montant pour les dommages matériels est à 13k$. Ce n'est pas beaucoup.
### On va revoir si ça peut être expliqué par une loi  quelconque en France ou
### si c'est causé par une limite quelconque.
### La variable est numérique continue.

## - Indtpbi
hist(log(data.analyse$Indtpbi))

summary(data.analyse$Indtpbi)
### Le maximum du montant pour les dommages matériels est à 70k$. C'est plus haut
### que pour les dommages matériels, ce qui a du sens.
### La variable est numérique continue.

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
plot(data.analyse %>% group_by(Type) %>% summarise(Moyenne = mean(Numtppd)))

# Category
summary(data.analyse$Category)
## On remarque que les catégories ont environ les mêmes proportions.
## Il y a un peu moins de véhicule dans la catégorie Small.

str(data.analyse$Category) # pas de NA!

ggplot(data.analyse, aes(x=Numtppd))+
    geom_bar(aes(y = after_stat(prop), fill=Category), position="dodge")
## Les petites autos font plus de dommages matériels et sont les moins à avoir
## 0 dommage. Plus l'auto est grosse, moins elle fait de dommages matériels. Les
## jeunes ont souvent des petites autos, ce qui pourrait expliquer le phénomène.

## Moyenne du nb accidents des gens qui sont dans la categ large:
mean(data.analyse$Numtppd[which(data.analyse$Category == "Large")])

## Moyenne du nb accidents des gens qui sont dans la categ Medium:
mean(data.analyse$Numtppd[which(data.analyse$Category == "Medium")])

## Moyenne du nb accidents des gens qui sont dans la categ Small:
mean(data.analyse$Numtppd[which(data.analyse$Category == "Small")])

## Comme dit plus haut, plus c'est large, moins la moyenne d'accidents est haute.

## La variable est catégorielle ordinale.

# Occupation
summary(data.analyse$Occupation)
## La plupart sont des employés et il y a cinq catégories.

str(data.analyse$Occupation) # pas de NA!

ggplot(data.analyse, aes(x=Numtppd))+
    geom_bar(aes(y = after_stat(prop), fill=Occupation), position="dodge")
## Les gens à la retraite font moins d'accidents. C'est logique, puisqu'ils font
## moins de déplacement et ne vont pas au travail à tous les jours par exemple.

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

## La plus grosse moyenne est pour les gens sans emploi. C'est loique, il ont plus
## de temps pour être sur la route. La plus petite moyenne est pour les gens
## retraités comme dit plus haut.

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
# graphique du coef de variation en fct de l'age
ggplot(moy_par_age) + geom_point(aes(x=Age, y=dev_std/Moyenne))
# Très dure è interpréter adéquatement, mais intéressant

# Group1
table(data.analyse$Group1)

# D'accord, bcp de niveaux faisons une analyse similaire à l'age
moy_par_group1 <- data.analyse %>% group_by(Group1) %>% summarise(
    Count=length(Numtppd), Moyenne=mean(Numtppd), dev_std=sd(Numtppd)
)
moy_par_group1

# IC approximatif 80 % par group1
moy_par_group1$low_ic <- moy_par_group1$Moyenne + qnorm(0.1)*moy_par_group1$dev_std/sqrt(moy_par_group1$Count)
moy_par_group1$upp_ic <- moy_par_group1$Moyenne + qnorm(0.9)*moy_par_group1$dev_std/sqrt(moy_par_group1$Count)
# graphique moyenne d'accidents par group1 avec IC 80 %
ggplot(moy_par_group1) + geom_point(aes(x=Group1, y=Moyenne), col="darkblue", size=2.5) +
    geom_errorbar(aes(x=Group1, ymin=low_ic, ymax=upp_ic), width=0.5) +
    labs(title = "Moyenne de réclamation pour la couverture étudiée\nen fonction de 'Group1' avec un IC approximatif 80%",
         y="Moyenne des réclamtations")
# Très linéaire Wow!


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
summary(data.analyse$Value)
## La valeur des autos se trouvent entre 1000 euros et 50 000 euros. La maximum
## semble bas à premièere vue. Il n'y a pas de NA.

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
## n'est pas énorme. Ce n'Est pas très concluant pour l'instant. À retravailler
## avec plus de points.

# Adind
summary(data.analyse$Adind) # 0/1

ggplot(data.analyse, aes(x=Numtppd))+
  geom_bar(aes(y = after_stat(prop), fill=as.factor(Adind)), position="dodge")

mean(data.analyse$Numtppd[which(data.analyse$Adind == 0)]) # moyenne pour Adind = 0
mean(data.analyse$Numtppd[which(data.analyse$Adind == 1)]) # moyenne pour Adind = 1

# si tes pas assuré des deux bords tu fais plus d'accident
(t.test(x = data.analyse$Numtppd[which(data.analyse$Adind == 0)], data.analyse$Numtppd[which(data.analyse$Adind == 1)], var.equal = F))$p.value
### avec la p-value on déduit que les moyennes sont significativement différente

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
summary(data.analyse$Density)

ggplot(data.analyse, aes(y=as.factor(Numtppd), x = Density))+
    geom_boxplot()
## Il y a plus d'accidents quand la densité de population est plus élevée,
## Ça semble être linéaire. Il y a moins de données pour les densités 5 et +,
## donc c'est moins fiables.

table(data.analyse$Numtppd)
## regrouper 5, 6, 7

##
## Interactions
##
plot(data.analyse %>% group_by(Group1) %>% summarise(Moyenne = mean(Age)))
cor(data.analyse$Group1, data.analyse$Age)
cor(data.analyse %>% group_by(Group1) %>% summarise(Moyenne = mean(Age)))
