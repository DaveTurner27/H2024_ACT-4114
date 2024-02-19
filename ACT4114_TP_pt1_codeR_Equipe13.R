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
#   4. Regroupement
#   5. Analyse en composantes principales
#   6. Classification hiérarchique
#   7. Algorithme des k-moyennes
#

##
## 1. Importation des données et paquetages
##

# Paquetages requis
liste.paquetage <- c("ggplot2", "dplyr", "CASdatasets", "MASS", "car", "factoextra", "FactoMineR")

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
# les 21 premières lignes
# on les enlève
(data.analyse$PolNum %in% ids)[1:22]
data.analyse <- data.analyse[-(1:21), ]
any(duplicated(data.analyse$PolNum))
nrow(data.analyse)

##
## 3. Analyse exploratoire des données
##

colnames(data.analyse)

##
## Numtppd (endogène)
##

(freq <- table(data.analyse$Numtppd))
fmp <- freq / 100000
fmp
# Graphique de la fmp
plot(fmp, main="Fonction de masse de probabilité empirique\npour le nombre d'accidents",
     lwd=4, col="red", xlab="Nombre d'accidents", ylab="Proportion")
mean(data.analyse$Numtppd)
var(data.analyse$Numtppd)
summary(data.analyse$Numtppd)
# Très petite étendue

## Exppdays (exposition)
table(data.analyse$Exppdays)
summary(data.analyse$Exppdays)
hist(data.analyse$Exppdays / 365)
# Très grande proportion de police complète!

##
## Variables exclues :
##

# - Polnum
hist(data.analyse$PolNum)
### Il y a deux bosses, pcq il y a deux années
ggplot(data.analyse, aes(x = PolNum, fill=as.factor(CalYear))) +
    geom_density()

# - Numtpbi
table(data.analyse$Numtpbi)
# tout semble okay
plot(table(data.analyse$Numtpbi))

# - Indtppd
hist(log(data.analyse$Indtppd))
summary(data.analyse$Indtppd)
# tout semble okay

# - Indtpbi
hist(log(data.analyse$Indtpbi))
summary(data.analyse$Indtpbi)
# tout semble okay

# - CalYear
table(data.analyse$CalYear)
ggplot(data.analyse, aes(x=Numtppd))+
    geom_bar(aes(y = after_stat(prop), fill=as.factor(CalYear)), position="dodge")+
    scale_y_continuous(labels = scales::percent)
data.analyse %>% group_by(as.factor(CalYear)) %>% summarise(Moyenne = mean(Numtppd))
# tout semble okay

##
## Variables exogènes
##

# - Gender
table(data.analyse$Gender)
ggplot(data.analyse, aes(x=Numtppd))+
    geom_bar(aes(y = after_stat(prop), fill=Gender), position="dodge")+
    scale_y_continuous(labels = scales::percent)
data.analyse %>% group_by(Gender) %>% summarise(Moyenne = mean(Numtppd))
# les hommes sont plus dangereux

# - Type
table(data.analyse$Type)
ggplot(data.analyse, aes(x=Numtppd))+
    geom_bar(aes(y = after_stat(prop), fill=Type), position="dodge")+
    scale_y_continuous(labels = scales::percent)
data.analyse %>% group_by(Type) %>% summarise(Moyenne = mean(Numtppd))
plot(data.analyse %>% group_by(Type) %>% summarise(Moyenne = mean(Numtppd)))
# La variable semble importante et semble ordinale

# - Category
summary(data.analyse$Category)
# Environ les mêmes proportions, un peu moins de small
str(data.analyse$Category)
ggplot(data.analyse, aes(x=Numtppd))+
    geom_bar(aes(y = after_stat(prop), fill=Category), position="dodge")
data.analyse %>% group_by(Category) %>% summarise(Moyenne = mean(Numtppd))
# Les petites autos font plus de dommages matériels et sont les moins à avoir
# 0 dommage. Plus l'auto est grosse, moins elle fait de dommages matériels. Les
# jeunes ont des petites autos.

# - Occupation
summary(data.analyse$Occupation)
# La plupart sont des employés, 5 catégories
str(data.analyse$Occupation)
ggplot(data.analyse, aes(x=Numtppd))+
    geom_bar(aes(y = after_stat(prop), fill=Occupation), position="dodge")
# les reitred font moins d'accidents, makes sense moins de déplacement, vont pas
# au travail par exemple
data.analyse %>% group_by(Occupation) %>% summarise(Moyenne = mean(Numtppd))
# plus gross moyenne pour les unemployed, makes sense, plus de temps pour rien
# faire et être sur la route. Plus petite pour retired comme dit tantot
# regroupement possibles ?

# - Age
table(data.analyse$Age)
plot(table(data.analyse$Age))
# semble y avoir moins de vieilles personnes, majoritairement des personnes d'age moyen

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
# relation non linéaire, mais on constate que les jeunes sont plus dangereux que les vieux

# - Group1
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


# - Bonus
summary(data.analyse$Bonus)
table(data.analyse$Bonus)
data.analyse$Bonus[which(data.analyse$Age == 18)]
# ceux de 18 ans on 0 comme bonus car premiere année dans le contrat
data.analyse$Age[which(data.analyse$Bonus == 0)] # pas seulement ceux de 18 ans qui on 0 de bonus
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
# très linéaire sauf le 0 a cause des 18 ans

# - Poldur
summary(data.analyse$Poldur)
table(data.analyse$Poldur)
# assez de donées pour chaque cat
# Moyenne de réclamations par durée de la police
moy_par_bonus <- data.analyse %>% group_by(Poldur) %>% summarise(
  Count=length(Numtppd), Moyenne=mean(Numtppd), dev_std=sd(Numtppd)
)
moy_par_poldur
# IC approximatif 80 % par durée de la police
moy_par_poldur$low_ic <- moy_par_poldur$Moyenne + qnorm(0.1)*moy_par_poldur$dev_std/sqrt(moy_par_poldur$Count)
moy_par_poldur$upp_ic <- moy_par_poldur$Moyenne + qnorm(0.9)*moy_par_poldur$dev_std/sqrt(moy_par_poldur$Count)
# graphique moyenne d'accidents par durée de la police avec IC 80 %
ggplot(moy_par_poldur) + geom_point(aes(x=Poldur, y=Moyenne), col="darkblue", size=2.5) +
  geom_errorbar(aes(x=Poldur, ymin=low_ic, ymax=upp_ic), width=0.5) +
  labs(title = "Moyenne de réclamation pour la couverture étudiée\nen fonction de la durée de la police avec un IC approximatif 80%",
       y="Moyenne des réclamtations")
# l'age de 18 ans semble encore venir faire des soucis...

# - Value
summary(data.analyse$Value)
## Auto entre 1000 et 50k$. Pas tant chere comme auto.
ggplot(data.analyse, aes(x=as.factor(Numtppd), y = Value))+
    geom_boxplot()
# la variable semble très peu prédictive
hist(data.analyse$Value)
# On sépare les valeurs de voiture en quatre groupe et on fait la moyenne d'accidents
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
# On peut voir qu'il y a une tendance croissante, même si le nombre de points
# n'est pas énorme, pas full concluant + l'échèle est assez petite

# - Adind
summary(data.analyse$Adind) # 0/1
table(data.analyse$Adind)
ggplot(data.analyse, aes(x=Numtppd))+
  geom_bar(aes(y = after_stat(prop), fill=as.factor(Adind)), position="dodge")
mean(data.analyse$Numtppd[which(data.analyse$Adind == 0)]) # moyenne pour Adind = 0
mean(data.analyse$Numtppd[which(data.analyse$Adind == 1)]) # moyenne pour Adind = 1
# si tes pas assuré des deux bords tu fais plus d'accident
(t.test(x = data.analyse$Numtppd[which(data.analyse$Adind == 0)], data.analyse$Numtppd[which(data.analyse$Adind == 1)], var.equal = F))$p.value
# avec la p-value on déduit que les moyennes sont significativement différentes

# - SubGroup2
summary(data.analyse$SubGroup2)
# beaucoup trop de niveau on va prendre Group2 à la place

# - Group2 (region)
# interactionn avec densité ?
summary(data.analyse$Group2)
ggplot(data.analyse, aes(x=Numtppd))+
  geom_bar(aes(y = after_stat(prop), fill=as.factor(Group2)), position="dodge")
# des regroupements sont envisageables
moy_par_group2 <- data.analyse %>% group_by(Group2) %>% summarise(
  Count=length(Numtppd), Moyenne=mean(Numtppd), dev_std=sd(Numtppd)
)
moy_par_group2
# IC approximatif 80 % par durée de la police
moy_par_group2$low_ic <- moy_par_group2$Moyenne + qnorm(0.1)*moy_par_group2$dev_std/sqrt(moy_par_group2$Count)
moy_par_group2$upp_ic <- moy_par_group2$Moyenne + qnorm(0.9)*moy_par_group2$dev_std/sqrt(moy_par_group2$Count)
# graphique moyenne d'accidents par durée de la police avec IC 80 %
ggplot(moy_par_group2) + geom_point(aes(x=Group2, y=Moyenne), col="darkblue", size=2.5) +
  geom_errorbar(aes(x=Group2, ymin=low_ic, ymax=upp_ic), width=0.5) +
  labs(title = "Moyenne de réclamation pour la couverture étudiée\nen fonction de la durée de la police avec un IC approximatif 80%",
       y="Moyenne des réclamtations")
# sort by mean serait beaucoup plus clair, mais des regroupements sont clairement envisageables

# - Density
summary(data.analyse$Density)
ggplot(data.analyse, aes(y=as.factor(Numtppd), x = Density))+
    geom_boxplot()
# plus d'accidents quand la densité de population est plus élevé,
# semble être linéaire. moins de données donc moins fiables pour les 3 à droite
table(data.analyse$Numtppd)

##
## 4. Regroupement
##

## regroupement de Group2 selon
v1 <- data.analyse$Numtppd[which(data.analyse$Group2 == 'O')]
v2 <- data.analyse$Numtppd[which(data.analyse$Group2 == 'P')]
t.test(v1, v2, var.equal = FALSE)$p.value
#on regroupe O et P dans P
data.analyse$Group2[which(data.analyse$Group2 == 'O')] <- 'P'

v1 <- data.analyse$Numtppd[which(data.analyse$Group2 == 'L')]
v2 <- data.analyse$Numtppd[which(data.analyse$Group2 == 'P')]
t.test(v1, v2, var.equal = FALSE)$p.value
#on regroupe PO et L dans P
data.analyse$Group2[which(data.analyse$Group2 == 'L')] <- 'P'

v1 <- data.analyse$Numtppd[which(data.analyse$Group2 == 'U')]
v2 <- data.analyse$Numtppd[which(data.analyse$Group2 == 'P')]
t.test(v1, v2, var.equal = FALSE)$p.value
#pvalue de 4.79%

v1 <- data.analyse$Numtppd[which(data.analyse$Group2 == 'S')]
v2 <- data.analyse$Numtppd[which(data.analyse$Group2 == 'T')]
t.test(v1, v2, var.equal = FALSE)$p.value
#on regroupe S et T dans S
data.analyse$Group2[which(data.analyse$Group2 == 'T')] <- 'S'

v1 <- data.analyse$Numtppd[which(data.analyse$Group2 == 'S')]
v2 <- data.analyse$Numtppd[which(data.analyse$Group2 == 'P')]
t.test(v1, v2, var.equal = FALSE)$p.value
#tres petite pvalue

v1 <- data.analyse$Numtppd[which(data.analyse$Group2 == 'N')]
v2 <- data.analyse$Numtppd[which(data.analyse$Group2 == 'Q')]
t.test(v1, v2, var.equal = FALSE)$p.value
#on regroupe N et Q dans Q
data.analyse$Group2[which(data.analyse$Group2 == 'N')] <- 'Q'

##
## On a donc
## (OPL) = P, (ST) = S, (NQ) = Q
## Pour maintenant 6 classe
# bref :
data.analyse <- data.analyse %>% mutate(
    Group2 = case_when(
        Group2 %in% c("O", "P", "L") ~ "0PL",
        Group2 %in% c("S", "T") ~ "ST",
        Group2 %in% c("N", "Q") ~ "NQ",
        TRUE ~ Group2
    )
)

## regroupement selon occupation

v1 <- data.analyse$Numtppd[which(data.analyse$Occupation == 'Employed')]
v2 <- data.analyse$Numtppd[which(data.analyse$Occupation == 'Housewife')]
t.test(v1, v2, var.equal = FALSE)$p.value
## Rien a regrouper ici

##
## 5. Analyse en composantes principales
##

data.analyse$crisk <- data.analyse$Group1
data.analyse$is.female <- as.numeric(data.analyse$Gender == "Female")
data.pca <- data.analyse[, c("Age", "crisk", "Bonus", "Poldur", "Value", "Density")]
data.pca <- scale(data.pca, center = TRUE, scale = TRUE)
acp <- PCA(data.pca, scale.unit = T)
acp$eig
fviz_screeplot(acp, ncp=6)
weights <- data.frame(acp$var$coord[,1:3])
weights$carac <- rownames(weights)
weights.long <- reshape2::melt(weights)
ggplot(weights.long, aes(x=carac, fill=variable, y=value))+
    geom_bar(stat="identity",position=PositionDodge)+
    facet_grid(~variable)+
    theme(legend.position="top",axis.text.x = element_text(angle = 90))+
    coord_flip()
ggplot(mapping = aes(x=1:6, y=acp$eig[, 3])) + geom_point() + geom_line()

##
## 6. Classification hiérarchique
##

##
## 7. Algorithme des k-moyennes
##
