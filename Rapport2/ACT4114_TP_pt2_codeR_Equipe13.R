###
### Modélisation (rapport 2) - Équipe 13
### ACT-4114, Hiver 2024
###

#
#   Ce fichier contient le code pour ajuster les différents modèles et en faire
#   l'analyse. On sépare le fichier en plusieurs sections énumérées ci-dessous.
#
#   1. Importation des paquetages
#   2. Transformation des données et création des jeux train et test
#   3. Importation de test et train
#
#   4. Ajustement de modèles
#   5. Comparaison des modèles
#   6. Interprétation des meilleurs modèles

##
## 1. Importation des paquetages
##

# Paquetages requis
liste.paquetage <- c(
    "ggplot2", "dplyr", "CASdatasets", "MASS", "pscl", "VGAM", "flexsurv", "caret", "randomForest", "FNN", "flexsurv", "randomForest", "rpart", "rpart.plot"
)

# On installe les paquetages de la liste qu'on a pas déjà
inst <- liste.paquetage %in% installed.packages()
if(length(liste.paquetage[!inst]) > 0) install.packages(liste.paquetage[!inst])
lapply(liste.paquetage, require, character.only = TRUE)

##
## 2. Transformation des données et création des jeux train et test
##    (décommenter seulement si il faut créer train et test)
##

## Charger les données en R
# # Le jeux de données vien du paquetage "CASdatasets"
# data("pg15training")
#
# #
# # # Tranformation
# data <- pg15training
# data <- data[-(1:21), ]
# # Variable Power
# data$Power <- data$Group1
# data$Group1 <- NULL
# # Variable Région
# data$Region <- data$Group2
# data$Group2 <- NULL
# data <- data %>% mutate(
#     Region = case_when(
#         Region %in% c("O", "P", "L") ~ "0PL",
#         Region %in% c("S", "T") ~ "ST",
#         Region %in% c("N", "Q") ~ "NQ",
#         TRUE ~ Region
#     )
# )
# # # Nouvelle variable
#
# data$Is_18 <- as.factor(as.numeric(data$Age == 18))
# # Mettre certaines variables en factor
# data$CalYear <- as.factor(data$CalYear)
# data$Adind <- as.factor(data$Adind)
# # Variable d'exposition
# data$Expp <- data$Exppdays / 365
# data$Exppdays <- NULL
#
# # Création des jeux train et test
# set.seed(42069)
# idx_train <- sample(100000L, 0.85*100000L, replace = FALSE)
# keep_colnames <- c(
#     "Gender", "Type", "Category", "Occupation", "Age",
#     "Bonus", "Poldur", "Value", "Adind", "Density",
#     "Numtppd", "Power", "Region", "Is_18", "Expp"
# )
# train <- data[idx_train, keep_colnames]
# test <- data[-idx_train, keep_colnames]
# write.csv(train, "train.csv")
# write.csv(test, "test.csv")

##
##   3. Importation de test et train
##

train <- read.csv("train.csv")[, -1]
test <- read.csv("test.csv")[, -1]

#####
#####
##### 4. Ajustement de modèles
#####
#####

##
## GLM
##
formula_base <- formula(
    Numtppd ~ Gender + Type + Category + Occupation +
        poly(Age, 6) + Bonus + Is_18 + Poldur +
        Value + Adind + Density + Region + log(Power) + offset(Expp)
)
formula_base_bin <- formula(
    cbind(Numtppd, Expp*365) ~ Gender + Type + Category + Occupation +
        poly(Age, 6) + Bonus + Is_18 + Poldur +
        Value + Adind + Density*Region + log(Power)
)

# Loi de poisson
glm_pois <- glm(
    formula = formula_base,
    family = poisson(link = "log"),
    data = train
)
summary(glm_pois)

# Loi Binomiale Négative
glm_negbin <- glm.nb(
    formula = formula_base,
    link = "log",
    data = train
)
summary(glm_negbin)

# Loi binomiale
glm_bin <- glm(
    formula = formula_base_bin,
    family = binomial(link = "log"),
    data = train
)
summary(glm_bin)

# Loi de Poisson gonflée en zéro (converge difficilement)
glm_pois_gonf <- pscl::zeroinfl(
    formula = formula_base,
    dist = "poisson",
    data = train
)
summary(glm_pois_gonf)
AIC(glm_pois_gonf)

# Continuons avec le modèle Poisson
drop1(object = glm_pois, test = "Chisq")
glm_pois <- update(glm_pois, ~.-Value)
drop1(object = glm_pois, test = "Chisq")
glm_pois <- update(glm_pois, ~.-Category)
drop1(object = glm_pois, test = "Chisq")
glm_pois <- update(glm_pois, ~.-Adind)
drop1(object = glm_pois, test = "Chisq")
summary(glm_pois)
(res <- sum(residuals.glm(glm_pois, type = "pearson")^2))
pchisq(res, df=glm_pois$df.residual)
# Le fit n'est pas incroyable

##
## GLM (avec une régularisation)
##

## POUR LASSO
# modèle
mod.complet <- glm(Numtppd ~.-Expp+offset(Expp), family = poisson, data = train)
summary(mod.complet)

x.train <- model.matrix(mod.complet, data = train)[, -1]
y.train <- train$Numtppd

# lambda
grid.lambda <- 10^seq(3, -2, length = 100)

library(glmnet)
mod.lasso <- glmnet(x.train, y.train, family = "poisson", alpha = 1, lambda = grid.lambda)

set.seed(111)
cv.out <- cv.glmnet(x.train, y.train, alpha = 1, nfolds = 3)
plot(cv.out)
(meilleur.lam.lasso <- cv.out$lambda.min)

predict(mod.lasso, type = "coefficients", s = meilleur.lam.lasso)

x.test <- model.matrix(mod.complet, data = test)[, -1]

pred_lasso <- predict(mod.lasso, newx = x.test, s = meilleur.lam.lasso, type = "response")
# MSE
(mse_lasso <- mean((pred_lasso - test$Numtppd)^2))

# POUR RIDGE
mod.ridge <- glmnet(x.train, y.train, family = "poisson", alpha = 0, lambda = grid.lambda)

set.seed(111)
cv.out2 <- cv.glmnet(x.train, y.train, alpha = 0, nfolds = 3)
plot(cv.out2)
(meilleur.lam.ridge <- cv.out2$lambda.min)

predict(mod.ridge, type = "coefficients", s = meilleur.lam.ridge)

pred_ridge <- predict(mod.ridge, newx = x.test, s = meilleur.lam.ridge, type = "response")
# MSE
(mse_ridge <- mean((pred_ridge - test$Numtppd)^2))


##
## K plus proches voisins
##

trControl <- trainControl(method  = "cv", # validation croisée
                          number  = 2)

fit <- caret::train(Numtppd ~ Age+Bonus+Poldur+Value+Adind+Density+Power+Is_18+offset(Expp),
                    method     = "knn",
                    preProcess = "scale",
                    tuneGrid   = expand.grid(k = c(20,40)),
                    trControl  = trControl,
                    metric     = "RMSE",
                    data       = train)

fit



##
## Arbre de décision
##
tree.control <- rpart.control(cp = 0, minbucket = 100L, xval = 10L)
arbre1 <- rpart(cbind(Expp, Numtppd)~.,
                method = "poisson",
                data = train,
                control = tree.control, parms = list(shrink=1))
plotcp(arbre1)
# err.min <- arbre1$cptable[which.min(arbre1$cptable[,4]),]
# err.max <- err.min[4] + err.min[5]
# cp.pois <- max(arbre1$cptable[arbre1$cptable[,4] <= err.max,1])
cp.pois <- arbre1$cptable[which.min(arbre1$cptable[,4]), 1]
arbre1 <- prune(arbre1, cp = cp.pois)
prev_arbre1 <- predict(arbre1, newdata=test)
mean((test$Numtppd - prev_arbre1)^2)
arbre1$variable.importance

##
## Bagging
##

#On crée l'échantillon de validation manuellement pour tester l'hyperparamètre nodesize
set.seed(131)
ind.val <- sample(nrow(train), 0.2*nrow(train), FALSE)
dat.valid <- train[ind.val,]
dat.non.valid <- train[-ind.val,]

#On cherche le meilleur hyperparamètre en testant sur l'échantillon de validation
minb <- seq(100, 1100, 200)
prev.nodesize <- numeric(length(minb))
set.seed(4757)
idx <- 0

for(i in minb){
    idx <- idx + 1
    foret.node <- distRforest::rforest(
        cbind(Expp, Numtppd)~.-Is_18,
        method="poisson",
        data=dat.non.valid,
        control=rpart.control(
            minbucket = i,
            cp=0,
            xval = 0
        ),
        ncand=12,
        ntrees = 300,
        subsample = 1)

    foret.node.prev <- predict(foret.node, newdata=dat.valid)
    preds <- rowMeans(matrix(unlist(foret.node.prev), ncol=length(foret.node.prev)))
    foret.node.res <- distRforest:::dev_poiss(ytrue = dat.valid$Numtppd, ypred = preds)

    prev.nodesize[idx] <- foret.node.res
}
minb[which.min(prev.nodesize)]
plot(prev.nodesize~minb)

##
## Forêt aléatoire
##

##
## Gradient Boosting
##

#####
#####
##### 5. Comparaison des modèles
#####
#####

#####
#####
##### 6. Interprétation des meilleurs modèles
#####
#####
