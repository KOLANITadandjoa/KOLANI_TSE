##############"  PROJET DE SURVEY SAMPLING ##################

# INSTALLATION DES PACKAGES

#install.packages("tidyverse")
#install.packages("dplyr")
#install.packages("survey")
#install.packages("sampling")
#install.packages("icarus")


# CHARGEMENT DES PACKAGES NECESSAIRES

library(tidyverse)
library(dplyr)
library(survey)
library(sampling)
library(icarus)

#### PARTIE 1: IMPRTATION DE LA BASE ET VERIFICATION DE LA STRUCTURE  ####

##  Importation de la base
salaries <- read.csv("/home/tadandjoa06/salaries.csv")
## Attachement de la base à l'environnement R

attach(salaries)

##  Verifier de la structure de la base de données
str(salaries)
dim(salaries)
N = nrow(salaries); N   # population size


####### PARTIE 2: ANALYSE PRELIMINAIRE #########

## Question 1 :

# La population est les salariés français des années 2000.

# La variable d'interêt y est le salaire mensuel moyen (individuel) des salariés en euros pour l'année 2000 (SAL00)

# La paramètre d'interêt est le salaire moyen de l’ensemble de la population des salairiés français en 2000

#  les variables d'auxilaies sont : le salaire mensuel moyen (individuel) des salariés en euros pour l'année 1999 (SAL99) et la région française (REG) où résident les salariés

# La variable pour la postrafication est la variable région française (REG) où résident les salariés


## Question 2: Calcul du total (vraie) de la population ty

total_y = sum(SAL00)
print(total_y) ## La valeur de ty est 26572068
var_y = var(SAL00)
print(var_y) ## La valeur de la variance est 579989.6
y_bar = total_y / N
print(y_bar) ## La valeur de la moyenne est 1371.249


## Qestion3 :  Un scatterplot de y sur  x

ggplot(salaries, aes(x = SAL99, y = SAL00)) +
  geom_point(color = "blue", alpha = 0.6) +
  labs(title = "Nuage de points de SAL00 sur SAL99",
       x = "Salaire mensuel moyen en 1999 (SAL99)",
       y = "Salaire mensuel moyen en 2000 (SAL00)") +
  theme_minimal()

########### PARTIE 3 : ECHANTILLON UNIQUE ############

## Question 1 : SRSWOR design

# Définition de la graine pour reproduction des résultats plus tard
set.seed(2023)

# Taille de l'échantillon
n <- 80

# Effectuons un échantillonnage SRSWOR
includ_indic_srswor <- srswor(n = n, N = nrow(salaries))

# Implémentation de la procédure d'échantillonnage en utilisant la fonction `svydesign` du package "survey"
srswor_sample <- svydesign(id = ~1,
                           weights = rep(nrow(salaries)/n, n), # poids égaux
                           fpc = rep(nrow(salaries), n),  # correction de population finie
                           data = salaries[includ_indic_srswor == 1, ])

# Estimation de la moyenne de y à partir de l'échantillon
estimated_mean_srswor <- svymean(~ SAL00, srswor_sample)
estimated_mean_srswor ## la moyenne estimée de l'échantillon sélectioné est 1275.7
# Variance
var_estimated_mean_srswor <- attr(estimated_mean_srswor, "var")
var_estimated_mean_srswor # la variance estimée de l'échantillon est 7087.796

# Nous pouvons aussi obtenir les intervalle de confiance par exemple
lb_estimated_mean_srswor <- estimated_mean_srswor[1] - 1.96 * SE(estimated_mean_srswor)
lb_estimated_mean_srswor  # la borne inférieure est 1110.681
ub_estimated_mean_srswor <- estimated_mean_srswor[1] + 1.96 * SE(estimated_mean_srswor)
ub_estimated_mean_srswor # la borne supérieure est 1440.702

## Le  Coefficient de  variation
coefvar_estimated_mean_srswor <- SE(estimated_mean_srswor) / estimated_mean_srswor[1]
coefvar_estimated_mean_srswor # le coeficient de variation vaut 0.06599484


# Question 2: Poststratification

# Définition de la graine pour reproduction des résultats
set.seed(2023)

# Effectuons un échantillonnage SRSWOR
si.rec <- srswor(n, nrow(salaries))

# Implémentation de la procédure d'échantillonnage en tenant compte de la poststratification
ech.si <- svydesign(id = ~ 1,
                    weights = rep(nrow(salaries) / n, n),
                    fpc = rep(n / nrow(salaries), n),
                    data = salaries[si.rec == 1, ])

# Tableau post-stratifié
tot.pop <- table(RG = salaries$RG)
tot.pop
# Objet post-stratifié
ech.post <- postStratify(ech.si, ~ RG, tot.pop, partial = TRUE) # Certains strates sont absents dans l'échantillon
ech.post
# Estimation de la moyenne Horvitz-Thompson post-stratifiée
est.post <- svymean(~ SAL00, ech.post)
est.post  # la moyenne post estimée est 1353.6
## Estimation de la variance post
var_est.post <- attr(est.post, "var")
var_est.post # la variance estimée de l'échantillon est 12609.49

# Nous pouvons aussi obtenir les intervalle de confiance par exemple
lb_est.post <- est.post[1] - 1.96 * SE(est.post)
lb_est.post  # la borne inférieure est 1133.485
ub_est.post <- est.post[1] + 1.96 * SE(est.post)
ub_est.post # la borne supérieure est 1573.67

## Le  Coefficient de  variation
coefvar_est.post <- SE(est.post) / est.post[1]
coefvar_est.post # le coeficient de variation vaut 0.08295941


## Question 2 BIS : Regroupement des régiones de la france en 04 régions:Ile-de-France, Nord-Est, Sud-Ouest, Sud-Est
### L'idée c'est voir l'impact sur la précision dû à l'absence de certaines régions dans la poststratification. Cette classification permet d'obtenir des effecitifs non nuls dans chaque strates.

# Définition de la classification des régions
salaries <- salaries %>%
  mutate(
    RG_Classification = case_when(
      RG %in% c("11") ~ "Ile-de-France",
      RG %in% c("21", "22", "23", "24", "25", "26", "31", "41", "42", "43", "52", "53", "54") ~ "Nord-Est",
      RG %in% c("72", "73", "74") ~ "Sud-Ouest",
      RG %in% c("82", "83", "91", "92") ~ "Sud-Est",
      TRUE ~ "Autre"
    )
  )
attach(salaries)
## La graine
set.seed(2023)
# Effectuons un échantillonnage SRSWOR
si.rec <- srswor(n, nrow(salaries))
# Implémentation de la procédure d'échantillonnage en tenant compte de la poststratification
ech.si <- svydesign(id = ~ 1,
                    weights = rep(nrow(salaries) / n, n),
                    fpc = rep(n / nrow(salaries), n),
                    data = salaries[si.rec == 1, ])


# Tableau post-stratifié

tot.pop <- table(RG_Classification = salaries$RG_Classification)
tot.pop

# Objet post-stratifié
ech.post <- postStratify(ech.si, ~ RG_Classification, tot.pop, partial = TRUE) # Maintenant Ok, tous les strates sont présents
ech.post
# Estimation de la moyenne post-stratifiée
est.post <- svymean(~ SAL00, ech.post)
est.post # la nouvelle moyenne estimée est 1356.4
## Estimation de la variance post
var_est.post <- attr(est.post, "var")
var_est.post # la variance estimée de l'échantillon est 13701.8

# Nous pouvons aussi obtenir les intervalle de confiance par exemple
lb_est.post <- est.post[1] - 1.96 * SE(est.post)
lb_est.post  # la borne inférieure est 1126.932
ub_est.post <- est.post[1] + 1.96 * SE(est.post)
ub_est.post # la borne supérieure est 1585.786

## Le  Coefficient de  variation
coefvar_est.post <- SE(est.post) / est.post[1]
coefvar_est.post # le coeficient de variation vaut 0.08630067


#Constat: Plutôt une perte de précision avec le regroupement de la région. Le fait que certains régions soit absents dans l'échantillon sélection pour la poststratification, on constate une variance plus pétite néamoins. Même s'il faut noter que le regroupemnt à améloirer le biais (dimunition du biais)


# Question 3 : Ratio estimateur

# Définition de la graine pour reproduction des résultats
set.seed(2023)

# Effectuons un échantillonnage SRSWOR
si.rec <- srswor(n, nrow(salaries))

# Implémentation de la procédure d'échantillonnage
ech.si <- svydesign(id = ~ 1,
                    weights = rep(nrow(salaries) / n, n),
                    fpc = rep(n / nrow(salaries), n),
                    data = salaries[si.rec == 1, ])

# Estimation du coefficient de pente R
R.est <- svyratio(~ SAL00, ~ SAL99, design = ech.si)
R.est ## la valeur de R estimé (ratio) estimé est 1.049634 avec SE(R) égale 0.01731561

# Estimation de la moyenne de SAL00 par le ratio estimator
est.ratio_T <- predict(R.est, total = total_y)
est.ratio_T ## estimation du total
est.ratio <- (1 / N) * est.ratio_T$total
est.ratio  ## estimation de la moyenne par ratio estimator est 1439.31

# Calcul de la variance de l'estimateur de ratio
## d'abord calcul de la variance du total
sd_est.ratio_T <-est.ratio_T$se
var_est_ratio_T <- (sd_est.ratio_T)^2
var_est_ratio_T
a <-(1/(N^2))
a
var_est.ratio <- a * var_est_ratio_T
var_est.ratio  ## La variance estimé de la moyenne  par l'estimateur du Ratio est 563.7781

## Les intervales de confiance


# Calcul du coefficient de variation
coef_var_est_ratio <- sqrt(var_est.ratio) / est.ratio
coef_var_est_ratio ## le coefficient de variation vaut 0.0164968


# Question 4 : Regression estimator

# Définition de la graine pour la reproduction des résultats
set.seed(2023)
si.rec <- srswor(n, N)
# Implémentation de la procédure d'échantillonnage
ech.si <- svydesign(id = ~ 1,
                    weights = rep(nrow(salaries) / n, n),
                    fpc = rep(n / nrow(salaries), n),
                    data = salaries[si.rec == 1, ])

# Tatal de la variable auxiliaire
N <- length(salaries$SAL99)
t.x0 <- sum(salaries$SAL99)

# Estimation de la moyenne de y par Estimateur de régression
ech.si.cal <- calibrate(ech.si, ~ SAL99, c(N, t.x0))
est_reg <-svymean(~ SAL00, ech.si.cal)
est_reg  ## la moyenne estimée de l'échantillon sélectioné est 1387.3
# Estimation de la Variance
var_est_reg <- attr(est_reg, "var")
var_est_reg  # la variance estimée de l'échantillon est 744.7529

# Nous pouvons aussi obtenir les intervalle de confiance par exemple
lb_est_reg <- est_reg[1] - 1.96 * SE(est_reg)
lb_est_reg  # la borne inférieure est 1333.79
ub_est_reg <- est_reg[1] + 1.96 * SE(est_reg)
ub_est_reg # la borne supérieure est 1440.767

## Le  Coefficient de  variation
coefvar_est_reg <- SE(est_reg) / est_reg[1]
coefvar_est_reg # le coeficient de variation vaut 0.01967172


# Note: Variance faible pour estimation par regresion

# Question 5 : Calibration

#Explication :
# La calibration est une méthode statistique utilisée pour ajuster les poids d'échantillonnage dans une enquête afin de garantir que les estimations issues de cette enquête coïncident avec les totaux ou les pourcentages connus de la population cible, provenant de sources externes fiables telles que le recensement ou d'autres enquêtes de référence.

#Dans le contexte de l'utilisation du package Icarus, la calibration implique l'ajustement des poids des échantillons en fonction de certaines variables clés afin d'améliorer la précision et la fiabilité des estimations obtenues à partir de l'échantillon. Ce processus est essentiel pour minimiser les erreurs de non-réponse, les erreurs de couverture et d'autres biais potentiellement présents dans l'échantillon d'enquête.


# Chargement du package icarus #

library(icarus)

######### Calibration pénalisée ####
# Calcul de la moyenne selon l'estimateur de Horvitz Thompson
N <- nrow(salaries) # population totale
ht_mean <- weightedMean(salaries$SAL00, rep(1, nrow(salaries)), N) # Vérification des poids
ht_mean
### Calcul des totaux de SAL00 par région (21 régions)
RG_T <-aggregate(salaries$SAL00, by = list(salaries$RG), FUN = sum)
#view(RG_T)
# Définition des marges pour la calibration
## moyenne de SAL00 par region
mar1 <- c("RG", 21, 5518552.3, 719461.3, 748112.5, 920701.5, 1108176.3, 644446.2, 1012529.0, 1583076.8, 994308.5, 1009358.1, 825772.3, 1448357.7, 1383689.0, 926462.9, 1263543.6, 696585.1, 575555.1, 2356847.0, 579106.6, 745481.8, 1511944.8) # Valeurs de SAL00 pour les 21 régions
## SAL99
mar2 <- c("SAL99",0,25582267,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)  # total de SAL99

margins <- rbind(mar1, mar2)

# Création d'une variable de poids avec des poids égaux pour toutes les observations
salaries$poids <- rep(1, nrow(salaries))
#salaries$poids <- rep(10, nrow(salaries))
#salaries$poids <- rep(1.5, nrow(salaries))

# Calcul des poids calibrés avec la méthode de calibrage pénalisée
costs <- c(1, 1)
gap <- 1.5
wCal_penalized <- calibration(data = salaries, marginMatrix = margins, colWeights = "poids", method = "penalized", costs = costs, gap = gap, description = FALSE, maxIter = 5000)
# Calcul de la moyenne calibrée
calibrated_mean_penalized <- weightedMean(salaries$SAL00, wCal_penalized, N)
cat("Moyenne calibrée (calibrated_mean_penalized) :", calibrated_mean_penalized, "\n")
ht_mean

## poids = (1, 1.5, 10,0.9, 1.05)
## poids = 1 , CP = 1371.249
## poids = 0.9 , CP = 1234.124
## poids = 1.05 , CP = 1439.812
## poids = 1.5 , CP = 2056.874
## poids = 10 , CP = 13712.49


# Création du graphique de calibration
calibration_plot <- ggplot(salaries, aes(x = SAL00, y = SAL00 * wCal_penalized)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "Graphique de Calibration pénalisée pour poids égaux = 1, gap =1.5 et costs  = c(1,1)",
       x = "Valeurs Observées (SAL00)",
       y = "Valeurs Calibrées")

# Affichage du graphique
print(calibration_plot)

########### Calibration linéaire ######
# Calcul des poids calibrés avec la méthode de calibrage linéaire
wCal_linear <- calibration(data = salaries, marginMatrix = margins, colWeights = "poids", method = "linear", description = FALSE)

# Calcul de la moyenne calibrée
calibrated_mean_linear <- weightedMean(salaries$SAL00, wCal_linear, N)
cat("Moyenne calibrée (calibrated_mean_linear) :", calibrated_mean_linear, "\n")
## Poids = 1, CL = 188244.4


# PARTIE 4; SIMULATIONS MONTE CARLO

# Horvitz - Thompson


# Simulations for SRSWOR
# Initialisation
set.seed(2023)
nsimu<-1000
nb.simul<-matrix(1:nsimu,nsimu,1);
tu.esti<-matrix(1,nsimu,1);
var.esti.tu<-matrix(1,nsimu,1);
# Simulations
for(i in 1:nsimu)
{
  si.salaries<-srswor(n,N)
  ech.si <- svydesign(id=~1, weights=rep(N/n,n),
                      fpc = rep(N, n),data=salaries[which(si.salaries==1),])
  a<-svymean(~SAL00, ech.si)
  tu.esti[i]<-a[1]
  var.esti.tu[i]<-SE(a)^2
}
#Monte Carlo moyenne et  écart type
mean(tu.esti) ## la moyenne MC est 1367.753
sqrt(var(tu.esti))
var(tu.esti) ## la variance  MC est 7417.69
sqrt(var(tu.esti))/mean(tu.esti) #le CVE MC est 0.06296897
# Histogram
hist(tu.esti)
## Biais
biais <-mean(tu.esti) - y_bar  ## le biais est -3.495904
biais
## EQM
EQM <- function (a, b) {
  EQM = b + a^2
  return(EQM)
}
EQM(7087.796, biais) # l'EQM est 50236849



# poststratified


set.seed(2023)
nsimu <- 1000
est.post <- matrix(1, nsimu, 1)
for (i in 1:nsimu) {
  si.salaries <- srswor(n, N)
  ech.si <- svydesign(id = ~1, weights = rep(N / n, n),
                      fpc = rep(n / N, n), data = salaries[si.salaries == 1, ])
  tot.pop <- table(RG = salaries$RG)
  ech.post <- postStratify(ech.si, ~RG, tot.pop, partial = TRUE)
  est.post[i] <- svymean(~SAL00, ech.post)[1]
}
mean(est.post) # la moyenne MC est 1371.271
var(est.post)  ## la variance  MC est 9062.168
sqrt(var(est.post)) / mean(est.post)  # le CVE MC est 0.0694213
# Histogram
hist(est.post)
## Biais
biais <-mean(est.post) - y_bar  ## le biais est 0.02170327
biais
## EQM
EQM <- function (a, b) {
  EQM = b + a^2
  return(EQM)
}
EQM(9062.168, biais) # l'EQM est 82122889



## Ratio


# Définition de la graine pour la reproductibilité
set.seed(2023)

# Initialisation des variables
nsimu <- 1000
est.ratio <- matrix(1, nsimu, 1)

# Boucle pour les simulations
for (i in 1:nsimu) {
  si.salaries <- srswor(80, N) # Modifier le paramètre de la taille de l'échantillon en conséquence
  ech.si <- svydesign(id = ~1, weights = rep(N/n, n), fpc = rep(n/N, n), data = salaries[si.salaries == 1, ])
  est.R <- svyratio(~SAL00, ~SAL99, design = ech.si)
  est.ratio[i] <- predict(est.R, total = sum(salaries$SAL00))$total
}

# Moyenne de de MC par estimateur Ratio
moy_MC_est.ratio <-(1/N) * mean(est.ratio) # pour obtenir la moyenne et non le total
moy_MC_est.ratio  # la moyenne MC est 1425.536

# Variance de MC par l'estimateur de ratio
var_MC_esti.ratio <- (1/N^2)*var(est.ratio) # pour obtenir la variance de la moyenne et du total
var_MC_esti.ratio # la variance MC est 1303.149
# Coefficient de variation de MC par l'estimateur de ratio
cv.ratio <- sqrt(var_MC_esti.ratio) / moy_MC_est.ratio
cv.ratio #  le CV est 0.02532322
# Histogram
est.ratio_ok <-(1/N) * est.ratio
hist(est.ratio_ok)
## Biais
biais <-mean(est.ratio_ok) - y_bar  ## le biais est 54.28653
biais
## EQM
EQM <- function (a, b) {
  EQM = b + a^2
  return(EQM)
}
EQM(563.7781, biais) # l'EQM est 317900



# Regression

set.seed(2023)
est.slr<- matrix(1,nsimu,1)
for (i in 1:nsimu)
{si.salaries <- srswor(n,N)
ech.si <- svydesign(id=~1, weights=rep(N/n,n),
                    fpc=rep(n/N,n),data=salaries[which(si.salaries==1),])
ech.si.cal<-calibrate(ech.si,~SAL99,c(N,t.x0))
est.slr[i] <-svymean(~SAL00, ech.si.cal)
}


mean(est.slr) # la moyenne MC est 1373.119

var(est.slr)  # la variance MC est 1044.554

cv.regression <- sqrt(var(est.slr))/mean(est.slr)
cv.regression # le CVE  MC est 0.02353733
# Histogram

hist(est.ratio_ok)
## Biais
biais <-mean(est.slr) - y_bar  ## le biais est 1.869958
biais
## EQM
EQM <- function (a, b) {
  EQM = b + a^2
  return(EQM)
}
EQM(744.7529, biais) # l'EQM est 554658.8

#########  GRAPHIQUE DES ESTIMATIONS MONTE CARLO ###
#Données pour chaque estimateur
data <- data.frame(
  Estimate = c(tu.esti, est.post, est.ratio_ok, est.slr),
  Method = rep(c("HT_SRSWOR", "Poststratified", "Ratio", "Regression"), each = nsimu)
)

# Créer l'histogramme
histogram <- ggplot(data, aes(x = Estimate, fill = Method)) +
  geom_histogram(position = "identity", alpha = 0.7, bins = 150, color = "white") +
  geom_vline(xintercept = y_bar, color = "black", linetype = "dashed", size = 1) +
  labs(title = "Histogramme des différentes estimations par Monte Carlo",
       x = "Valeurs des moyennes",
       y = "Fréquence") +
  scale_fill_manual(values = c("HT_SRSWOR" = "blue", "Poststratified" = "green", "Ratio" = "orange", "Regression" = "red")) +
  theme_minimal()

# Afficher l'histogramme
print(histogram)


############# FIN DU CODE R #################
