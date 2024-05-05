
#-------------------------Tache 1 : Importation des données----------------------------

seed = read.table(file=file.choose(),header=TRUE,sep=",",dec=".")
#En consultant le fichier seed.txt on remarque que la première ligne est 
#consacrée pour les noms des variable, la séparation entre les données est","
#et les valeurs décimales sont avec des "." .

str(seed)
#Un résumé contenant le nombre d’observations, nombre des variables ainsi que
leurs types.
#On a 185 observations , 8 variables : 7 variables quantitatives 
#et une variable qualitative
#6 variables explicatifs et deux variables à expliquer (variete+length of kernel groove)

summary(seed)
#Un résumé statistique plus détaillé contenant les valeurs de référence 
#statistiques de chaque variable(valuer minimale, premier quartile, médiane,moyenne, troisiéme
#quartile et la valeur maximale).

View(seed)
#Visualisation de notre jeu de données
attach(seed)

#----------------------------Tache 2 : Pré-traitement des données----------------------

#----------------Suppréssion des valuers manquantes dans notre cible varietie----------

Percent_MV_varieties = (nrow(subset(seed,varietie == "" ))/nrow(seed))*100
Percent_MV_varieties #La pourcentage en valeur manquante dans la colonne cible varietie
			   #est égale à 3.243243 % qui est négligable
			   #Seulement 6 lignes parmi 185 obsérvations
		       #Suppréssion des lignes ou il y'a des valeurs manquantes dans notre target.


seed=subset(seed,varietie != "" )
unique(seed$varietie)

#------------------------------2.1 Valeurs aberantes-----------------------------------
#Installation des packages

install.packages("rAmCharts")
library(rAmCharts)

#----------------------------------1.Colonne Area--------------------------------------

#Visualisation des outliers dans la colonne Area
amBoxplot(A~varietie,data=seed,xlab="Boxplot de la variable Area", export = TRUE)
#On remarque qu'il existe des outliers pour la variable Area pour la modalité kama

#On remplace les valeurs aberrantes par des Nan
summary(seed$A[seed$varietie=="Kama wheat"])

Q1=17.99 
Q3=19.14 
Vmin=Q1-1.5*(Q3-Q1)
Vmax=Q3+1.5*(Q3-Q1)
min_outliers=which(seed$A[seed$varietie=="Kama wheat"]<Vmin) 
max_outliers=which(seed$A[seed$varietie=="Kama wheat"]>Vmax)
Area_outliers=c(min_outliers,max_outliers)
for (a in Area_outliers) {
  seed$A[seed$varietie=="Kama wheat"][a]=NA
}

#Verification de la non existance des outliers dans la colonne Area
install.packages("outliers")
library(outliers) 

grubbs.test(seed$A[seed$varietie=="Kama wheat"], type = 11,two.sided = FALSE)

#H0: Il n'existe pas d'outlier dans notre colonne A.
#H1: Il existe d'outlier dans notre colonne A.
#p-value=0.2918>0.05 : On accepte H0 et on rejette H1


#--------------------------------2.Colonne Pressure------------------------------------

#-------------Visualisation des outliers dans la colonne Pressure----------------------

amBoxplot(P~varietie,data=seed,xlab="Boxplot de la variable Pressure", export = TRUE)

#On remarque qu'il existe un outlier pour la variable Pressure pour la modalité kama

#On remplace les valeurs aberrantes par des Nan
summary(seed$P[seed$varietie=="Kama wheat"])
Q1=15.86 
Q3=16.57 
Vmin=Q1-1.5*(Q3-Q1)
Vmax=Q3+1.5*(Q3-Q1)
min_outliers=which(seed$P[seed$varietie=="Kama wheat"]<Vmin) 
max_outliers=which(seed$P[seed$varietie=="Kama wheat"]>Vmax)
Pressure_outliers=c(min_outliers,max_outliers)
for (p in Pressure_outliers) {
  seed$P[seed$varietie=="Kama wheat"][p]=NA
}

#Verification de la non existance des outliers dans la colonne Pressure
grubbs.test(seed$P[seed$varietie=="Kama wheat"], type = 11,two.sided = FALSE)

#H0: Il n'existe pas d'outlier dans notre colonne P.
#H1: Il existe d'outlier dans notre colonne P.
#p-value=1>0.05 : On accepte H0 et on rejette H1


#-----------------------------3.Colonne Compactness------------------------------------

#Visualisation des outliers dans la colonne Compactness
amBoxplot(C~varietie,data=seed,xlab="Boxplot de la variable Compactness", export = TRUE)
#On remarque qu'il n'y a pas d'outliers pour la variable Compactness.

#---------------------------4.Colonne length of kernel---------------------------------

#Visualisation des outliers dans la colonne length of kernel
amBoxplot(Lk~varietie,data=seed,xlab="Boxplot de la variable length of kernel", export = TRUE)
#On remarque qu'il existe des outliers pour la variable length of kernel pour la modalité kama

#On remplace les valeurs aberrantes par des Nan
summary(seed$Lk[seed$varietie=="Kama wheat"])
Q1=5.980
Q3=6.315 
Vmin=Q1-1.5*(Q3-Q1)
Vmax=Q3+1.5*(Q3-Q1)
min_outliers=which(seed$Lk[seed$varietie=="Kama wheat"]<Vmin) 
max_outliers=which(seed$Lk[seed$varietie=="Kama wheat"]>Vmax)
Lk_outliers=c(min_outliers,max_outliers)

for (lk in Lk_outliers) {
  seed$Lk[seed$varietie=="Kama wheat"][lk]=NA
}


#Verification de la non existance des outliers dans la colonne length of kernel
grubbs.test(seed$Lk[seed$varietie=="Kama wheat"], type = 11,two.sided = FALSE)

#H0: Il n'existe pas d'outlier dans notre colonne length of kernel .
#H1: Il existe d'outlier dans notre colonne length of kernel.
#p-value=1>0.05 : On accepte H0 et on rejette H1


#--------------------------5.Colonne width of kernel-----------------------------------

#Visualisation des outliers dans la colonne width of kernel
amBoxplot(Wk~varietie,data=seed,xlab="Boxplot de la variable width of kernel", export = TRUE)
#On remarque qu'il n'existe pas d'outliers pour la variable width of kernel.


#-----------------------6.Colonne asymmetry coefficient------------------------------

#Visualisation des outliers dans la colonne asymmetry coefficient
amBoxplot(Ac~varietie,data=seed,xlab="Boxplot de la variable asymmetry coefficient", export = TRUE)
#On remarque qu'il existe des outliers pour la variable asymmetry coefficient pour les modalités canadian et rosa

####################Pour les outliers dans la modalité Canadian Wheat##########
#On remplace les valeurs aberrantes par des Nan
summary(seed$Ac[seed$varietie=="Canadian wheat"])
Q1=1.8355
Q3=3.1990 
Vmin=Q1-1.5*(Q3-Q1)
Vmax=Q3+1.5*(Q3-Q1)
min_outliers=which(seed$Ac[seed$varietie=="Canadian wheat"]<Vmin) 
max_outliers=which(seed$Ac[seed$varietie=="Canadian wheat"]>Vmax)
Ac_outliers=c(min_outliers,max_outliers)

for (ac in Ac_outliers) {
  seed$Ac[seed$varietie=="Canadian wheat"][ac]=NA
}

#Verification de la non existance des outliers dans la colonne asymmetry coefficient
grubbs.test(seed$Ac[seed$varietie=="Canadian wheat"], type = 11,two.sided = FALSE)

#H0: Il n'existe pas d'outlier dans notre colonne length of kernel .
#H1: Il existe d'outlier dans notre colonne length of kernel.
#p-value=1>0.05 : On accepte H0 et on rejette H1

####################Pour les outliers dans la modalité Rosa Wheat###############

summary(seed$Ac[seed$varietie=="Rosa wheat"])
Q1=4.032 
Q3=5.470 
Vmin=Q1-1.5*(Q3-Q1)
Vmax=Q3+1.5*(Q3-Q1)
min_outliers=which(seed$Ac[seed$varietie=="Rosa wheat"]<Vmin) 
max_outliers=which(seed$Ac[seed$varietie=="Rosa wheat"]>Vmax)
Ac_outliers=c(min_outliers,max_outliers)

for (ac in Ac_outliers) {
  seed$Ac[seed$varietie=="Rosa wheat"][ac]=NA
}

#Verification de la non existance des outliers dans la colonne asymmetry coefficient
grubbs.test(seed$Ac[seed$varietie=="Rosa wheat"], type = 11,two.sided = FALSE)

#H0: Il n'existe pas d'outlier dans notre colonne length of kernel .
#H1: Il existe d'outlier dans notre colonne length of kernel.
#p-value=1>0.05 : On accepte H0 et on rejette H1

#---------------------------7.length of kernel groove---------------------------

#Visualisation des outliers dans la colonne length of kernel groove

amBoxplot(Lkg~varietie,data=seed,xlab="Boxplot de la variable length of kernel groove", export = TRUE)

#On remarque qu'il existe des outliers pour la variable length of kernel groove pour les modalités canadian et kama

####################Pour les outliers dans la modalité Canadian Wheat############

#On remplace les valeurs aberrantes par des Nan
summary(seed$Lkg[seed$varietie=="Canadian wheat"])
Q1=4.870
Q3=5.219 
Vmin=Q1-1.5*(Q3-Q1)
Vmax=Q3+1.5*(Q3-Q1)
min_outliers=which(seed$Lkg[seed$varietie=="Canadian wheat"]<Vmin) 
max_outliers=which(seed$Lkg[seed$varietie=="Canadian wheat"]>Vmax)
Lkg_outliers=c(min_outliers,max_outliers)
for (lkg in Lkg_outliers) {
  seed$Lkg[seed$varietie=="Canadian wheat"][lkg]=NA
}
grubbs.test(seed$Lkg[seed$varietie=="Canadian wheat"], type = 11,two.sided = FALSE)
#H0: Il n'existe pas d'outlier dans notre colonne length of kernel .
#H1: Il existe d'outlier dans notre colonne length of kernel.
#p-value=1>0.05 : On accepte H0 et on rejette H1

####################Pour les outliers dans la modalité Kama Wheat###############


summary(seed$Lkg[seed$varietie=="Kama wheat"])
Q1=5.879
Q3=6.197 
Vmin=Q1-1.5*(Q3-Q1)
Vmax=Q3+1.5*(Q3-Q1)
min_outliers=which(seed$Lkg[seed$varietie=="Kama wheat"]<Vmin) 
max_outliers=which(seed$Lkg[seed$varietie=="Kama wheat"]>Vmax)
Lkg_outliers=c(min_outliers,max_outliers)
for (lkg in Lkg_outliers) {
  seed$Lkg[seed$varietie=="Kama wheat"][lkg]=NA
}

#Verification de la non existance des outliers dans la colonne asymmetry coefficient

grubbs.test(seed$Lkg[seed$varietie=="Kama wheat"], type = 11,two.sided = FALSE)
#H0: Il n'existe pas d'outlier dans notre colonne length of kernel .
#H1: Il existe d'outlier dans notre colonne length of kernel.
#p-value=0.7437>0.05 : On accepte H0 et on rejette H1


#---------------------------2.2 : Les valeurs manquantes------------------------

#----------------------------Etude des valeurs manquantes------------------------

#-----------------Pourcentage des valeurs manquantes dans notre database---------
df_sans_varietie=subset(seed,select = -c(varietie))
Taux=sum(is.na(df_sans_varietie))/prod(dim(df_sans_varietie))
Taux
#Le taux des valeurs manquantes est 1.9% de la totalité des valeurs dans la base, 
#on le considère comme un taux insiginifiant qui est < 5%,
#donc on peut les eliminer de la base mais on a pensée à l’imputation 
#pour ne pas avoir une perte d'information.

#------------------------Nb valeurs manquantes par colonne---------------------

colSums(is.na(seed))

#-----------Visualisation du pourcentage en valeurs manquantes par colonnes-----

library(naniar)
gg_miss_var(seed,show_pct = TRUE)
install.packages("VIM")
library(VIM)
mice_plot <- aggr(seed, col=c('navyblue','yellow'),
                    numbers=TRUE, sortVars=TRUE,
                    labels=names(seed), cex.axis=.7,
                    gap=3, ylab=c("Missing data","Pattern"))

#----------------On établit le label encoding sur la variable varietie------------
#L'algorithme missForest nécessite l'encodage des variables qualitatifs car il
#ne comprend que les v	ariables quantitatifs.

seed$varietie <- as.numeric(factor(seed$varietie))

#-------------------------------------missForest----------------------------------
install.packages("missForest")
library(missForest)
seed_mf <- missForest(seed)

#verification des valeurs imputées
seed_imputed=seed_mf$ximp
seed_imputed
#le taux d'erreur d'imputation
seed_mf$OOBerror
#NRMSE is normalized mean squared error.
#Le taux d'erreur est trés faible = 0.083 => C'est une bonne technique d'imputation
#de données manquantes

#Visualisation de notre base de données aprés l'imputation des données manquantes
seed_imputed
View(seed_imputed)
gg_miss_var(seed_imputed,show_pct = TRUE)


#-----------------------------------------KNN-----------------------------------------
#install.packages("VIM")
#library(VIM)
#seed_knn <- kNN(seed,impNA=TRUE,k=5)
#seed_knn=subset(seed_knn, select = -c(A_imp ,P_imp,C_imp ,Lk_imp,Wk_imp,Ac_imp ,Lkg_imp,varietie_imp))
#head(seed_knn)


#-----------------------------Tache 3 : Analyse univariée-----------------------------

#-------------3.1- Etude graphique de la normalité des variables quantitatifs-----------

#-------------------------------Par des histogrammes-------------------------------
#Les variables quantitatives
colnames(seed_imputed)
str(seed_imputed)

install.packages("ggpubr")
library("ggpubr")

par(mfrow=c(3,3))
numeric_seed = subset(seed_imputed, select = -c(varietie) )
hist(seed_imputed$A, prob = TRUE, main = "Histogramme de l'Area", xlab = "Area", ylab = "Densité de la distribution", col = "lightblue") # Histogramme en densité
lines(density(seed_imputed$A),col="red")
hist(seed_imputed$P, prob = TRUE, main = "Histogramme du perimeter", xlab = "perimeter", ylab = "Densité de la distribution", col = "lightblue") # Histogramme en densité
lines(density(seed_imputed$P),col="red")
hist(seed_imputed$C, prob = TRUE, main = "Histogramme du compactness", xlab = "compactness", ylab = "Densité de la distribution", col = "lightblue") # Histogramme en densité
lines(density(seed_imputed$C),col="red")
hist(seed_imputed$Lk, prob = TRUE, main = "Histogramme du length of kernel", xlab = "length of kernel", ylab = "Densité de la distribution", col = "lightblue") # Histogramme en densité
lines(density(seed_imputed$Lk),col="red")
hist(seed_imputed$Wk, prob = TRUE, main = "Histogramme du width of kernel", xlab = "width of kernel", ylab = "Densité de la distribution", col = "lightblue") # Histogramme en densité
lines(density(seed_imputed$Wk),col="red")
hist(seed_imputed$Lkg, prob = TRUE, main = "Histogramme du length of kernel groove", xlab = "length of kernel groove", ylab = "Densité de la distribution", col = "lightblue") # Histogramme en densité
lines(density(seed_imputed$Lkg),col="red")
hist(seed_imputed$Ac, prob = TRUE, main = "Histogramme de l'asymmetry coefficient", xlab = "asymmetry coefficient", ylab = "Densité de la distribution", col = "lightblue") # Histogramme en densité
lines(density(seed_imputed$Ac),col="red")
#On remarque que la distribution de toutes les variables quantitatives sauf Ac n'ont
#pas la forme bell shape caractéristique de la loi normale 
#Ac suit la loi normale
#A,P,C,Lk,Wk,Lkg ne suivent pas la loi normale


#--------------------------------Par la droite d'henry--------------------------------

#define plotting region
par(mfrow=c(3,3)) 

#create Q-Q plot for both datasets
qqnorm(seed_imputed$A, main='Area')
qqline(seed_imputed$A)
qqnorm(seed_imputed$P, main='perimeter')
qqline(seed_imputed$P)
qqnorm(seed_imputed$C, main='compactness')
qqline(seed_imputed$C)
qqnorm(seed_imputed$Lk, main='length of kernel')
qqline(seed_imputed$Lk)
qqnorm(seed_imputed$Wk, main='width of kernel')
qqline(seed_imputed$Wk)
qqnorm(seed_imputed$Ac, main='asymmetry coefficient')
qqline(seed_imputed$Ac)
qqnorm(seed_imputed$Lkg, main='length of kernel groove')
qqline(seed_imputed$Lkg)

#Ac est la seule variable qui présente une conformité entre les quantiles theoriques
#de la loi normale et sa distribution : la normalité pour Ac est visuellement verifiée.

#------------------------------2- Par le Test de shapiro---------------------------

#----------------------------------Pour la colonne Area----------------------------
#H0 : A suit une normale
#H1 : A ne suit pas une normale
shapiro.test(seed_imputed$A)
#p_value<0.05 : on rejette H0 et on accepte H1 
#La variable A ne suit pas la loi normale

#--------------------------------Pour la colonne perimeter-------------------------
#H0 : P suit une normale
#H1 : P ne suit pas une normale
shapiro.test(seed_imputed$P)
#p_value<0.05 : on rejette H0 et on accepte H1 
#La variable P ne suit pas la loi normale


#---------------------------------Pour la colonne compactness-----------------------
#H0 : C suit une normale
#H1 : C ne suit pas une normale
shapiro.test(seed_imputed$C)
#p_value<0.05 : on rejette H0 et on accepte H1 
#La variable C ne suit pas la loi normale


#------------------------------Pour la colonne length of kernel--------------------
#H0 : Lk suit une normale
#H1 : Lk ne suit pas une normale
shapiro.test(seed_imputed$Lk)
#p_value<0.05 : on rejette H0 et on accepte H1
#La variable Lk ne suit pas la loi normale
 

#------------------------------Pour la colonne width of kernel---------------------
#H0 : Wk suit une normale
#H1 : Wk ne suit pas une normale
shapiro.test(seed_imputed$Wk)
#p_value<0.05 : on rejette H0 et on accepte H1
#La variable Wk ne suit pas la loi normale


#----------------------------Pour la colonne asymmetry coefficient-----------------
#H0 : Ac suit une normale
#H1 : Ac ne suit pas une normale
shapiro.test(seed_imputed$Ac)
#p_value>0.05 : on accepte H0 et on rejette H1
#La variable Ac suit la loi normale

#---------------------------Pour la colonne length of kernel groove----------------
#H0 : Lkg suit une normale
#H1 : Lkg ne suit pas une normale
shapiro.test(seed_imputed$Lkg)
#p_value<0.05 : on rejette H0 et on accepte H1
#La variable Lkg ne suit pas la loi normale


#---------------------3.2- Etude de la modatlité de la variable varietie-------------

unique(varietie)
table(varietie)
by(seed_imputed,seed$varietie,nrow)

#on a trois modalité :
#"Canadian wheat" avec 58 observations
#"Kama wheat" avec 53 observations
#"Rosa wheat" avec 68 observations
#On a une database balanced : les modalités sont réparties de facon équitable

 
#---------------------------------Tâche 4: Analyse bivariée-------------------------

#Etude de la relation de dépendance entre les variables quantitatives deux à deux

#------------------------------1- Représentation graphique-------------------------

library(ggplot2)
install.packages("ggpointdensity")
library(ggpointdensity)

#densité couleur
#ggplot(seed_imputed, aes(x=seed_imputed$A, y=seed_imputed$P)) + geom_pointdensity() + scale_color_viridis_c()

my_cols <- c("#00AFBB", "#E7B800", "#FC4E07")  
pairs(seed_imputed[,1:7], pch = 19,  cex = 0.5,
      col = my_cols[],
      lower.panel=NULL)

#=> A-P/A-Lk/A-Wk/A-Lkg/P-Lk/P-Wk/P-Lkg/Lk-Lkg /Wk-Lkg=> Relation linéaire positive
#:le nuage de points suit une droite linéaire avec une pente positive
#=> A-C /P-C /C-Lk/C-Wk/Lk-Wk =>Liaison monotone positive non linéaire
#=> Ac-p/A/C/Lk/Wk/Lkg:Les ploits sont éparpillés
#=> Abscence de liaison 
#=> La variation de Ac n'influe pas la variation des autres variables quantitatives

#---------------------------2- Calcul des indicateurs statistiques------------------------

#Normalité non vérifiée pour toutes les variables quantitatives sauf Ac 
#on va calculer le coefficient de corrélation avec "Spearman" qui n'exige pas la normalité

install.packages("corrplot")
library(corrplot)
corrplot(cor(seed_imputed[,1:7],method="s"), type = "lower", method = "number")

#coefficient de corrélation de spearman: coefS
#si -0.5<coefS<0.5 ==>liaison faible
#si coefS=0 ==> pas de liaison
#si (-1 < coef < -0.5) ou (0.5 < coef < 1)==> liaison forte


#-----------------------------------3- Test de corrélation--------------------------------

#H0:r=0 => absence de dépendance
#H1:r<>0 => présence de dépendance

cor.test(seed_imputed$A,seed_imputed$P,method="spearman")

#p-value=2.2e-16<<<<<0.05 => p-value est négligeable=>accepter H1
#=>A et P sont fortement corrélées

cor.test(seed_imputed$Ac,seed_imputed$Lk,method="spearman")

#p-value= 0.0003664 <0.05 =>accepter H1
#=>Ac et Lk sont faiblement corrélées car p-value est proche de 0.05 



#Etude de la relation de dépendance entre les variables quantitatives et qualitatives

#---------------------------------1- Représentation graphique--------------------------------
par(mfrow=c(3,3)) 
boxplot(seed_imputed$A ~ seed_imputed$varietie)
boxplot(seed_imputed$P ~ seed_imputed$varietie)
boxplot(seed_imputed$C ~ seed_imputed$varietie)
boxplot(seed_imputed$Lk ~ seed_imputed$varietie)
boxplot(seed_imputed$Wk ~ seed_imputed$varietie)
boxplot(seed_imputed$Ac ~ seed_imputed$varietie)
boxplot(seed_imputed$Lkg ~ seed_imputed$varietie)

#On remarque la liaison entre les variables quantitatives et la variable qualitative varietie 
#en changant la modalité de la variable varietie,on remarque un changement 
#au niveau des valeurs de référence des autres variables,
#Alors il existe un effet de la variable cible varietie sur toutes les variables quantitatives.

#---------------------------------------2- Test statistique--------------------------------

#On est dans le cas de deux variables une quantitative A/P/C/Lk/Wk/Ac/Lkg
#et une qualitative varietie

#------------------------------------------A & varietie-------------------------------------

tapply(seed_imputed$A,seed_imputed$varietie,shapiro.test)

#pvalue = 0.944 > 0.05, alors on a la normalité pour le groupe "canadian wheat".
#pvalue = 0.0006571 < 0.05, alors n'on pas la normalité pour le groupe "kama wheat".
#pvalue = 0.0785 > 0.05, alors a la normalité pour le groupe "Rosa wheat".
#==> on n'a pas la normalité 

#la variable qualitative varietie possède plus que deux modalités (3)
#=>On applique le test de Kruskall-wallis
#H0:variables indépendantes
#H1:variables liées(les distributions des échantillons ne sont pas les mêmes)

kruskal.test(seed_imputed$A ~ seed_imputed$varietie)

#p-value=2.2e-16 <<<< 0.05
#on accepte H1 : les distributions des échantillons ne sont pas les mêmes, 
#il existe alors une différence entre les différentes modalités
#donc l’effet est présent et par la suite 
#on ne peut pas ignorer la relation entre les deux variables varietie et A.

#---------------------------------------P et varietie----------------------------------------

tapply(seed_imputed$P,seed_imputed$varietie,shapiro.test)
#==> on n'a pas la normalité 
kruskal.test(seed_imputed$P ~ seed_imputed$varietie)
#p-value=2.2e-16 <<<< 0.05==>On accepte H1

#----------------------------------------Wk et variete---------------------------------

tapply(seed_imputed$Wk,seed_imputed$varietie,shapiro.test)
#==> on n'a pas la normalité
kruskal.test(seed_imputed$Wk ~ seed_imputed$varietie)
#p-value=2.2e-16 <<<< 0.05==>On accepte H1

#------------------------------------------C et varietie-----------------------------------

tapply(seed_imputed$C,seed_imputed$varietie,shapiro.test)
#on a pas la normalité
kruskal.test(seed_imputed$C ~ seed_imputed$varietie)
#p-value=2.2e-16 <<<< 0.05==>On accepte H1

#-------------------------------------------Lk et varietie---------------------------------

tapply(seed_imputed$Lk,seed_imputed$varietie,shapiro.test)
#on a la normalité
bartlett.test(seed_imputed$Lk ~ seed_imputed$varietie)
# p-value = 8.11e-06<<0.05=>On accepte H1
kruskal.test(seed_imputed$Lk ~ seed_imputed$varietie)
#p-value=2.2e-16 <<<< 0.05==>On accepte H1

#---------------------------------------Ac et variete----------------------------------

tapply(seed_imputed$Ac,seed_imputed$varietie,shapiro.test)
#on a la normalité

##########################################################################################
#Utlisation des indicateurs Kurtosis et skewness pour l'étude de l'applatissement et l'assymétrie
install.packages('moments')
library(moments)

#####skewness de la distribution de Ac
skewness(seed_imputed$Ac)
#le coefficient d'asymétrie (skewness en anglais) correspond à une mesure de 
#l’asymétrie de la distribution d’une variable aléatoire réelle.
#la Skewness de Ac est supérieur à 0 (0.26) , alors le dataset est légérement skewed sur la droite.

#####kurtosis de la distribution de Ac
#le kurtosis , aussi traduit par coefficient d’acuité, coefficient d’aplatissement 
et degré de voussure, est une mesure directe de l’acuité et une mesure 
#indirecte de l'aplatissement de la distribution d’une variable aléatoire réelle.
#Il refléte de drgrée de présence des outliers dans le dataset.
kurtosis(seed_imputed$Ac)
#La distribution de Ac est pointue
##########################################################################################

bartlett.test(seed_imputed$Ac ~ seed_imputed$varietie)
#p-value = 0.5013>0.05 =>On accepte H0

#==> On peut appliquer le test statistique ANOVA
#H0: variables indépendantes
#H1: présence de dépendance entre les deux varibles
fit <- aov(seed_imputed$Ac ~ seed_imputed$varietie)
summary(fit)
#p-value<2e-16 <0.001
# On accepte H1
# au moins un niveau(groupe) avec une moyenne significativement différente
# Il existe un effet de varietie sur Ac

#--------------------------------------LKG et varietie------------------------------------

tapply(seed_imputed$Lkg,seed_imputed$varietie,shapiro.test)
#on a la normalité
bartlett.test(seed_imputed$Lkg ~ seed_imputed$varietie)
#p-value = 0.001164 < 0.05=>On accepte H1
#On ne peut pas appliquer l’ANOVA=>On applique le test de Kruskal Wallis
kruskal.test(seed_imputed$Lkg ~ seed_imputed$varietie)
#p-value=2.2e-16 <<<< 0.05==>On accepte H1


#------------------------------------Tâche 5 : Régression linéaire-------------------------

#--------------5.1)Régression de la variable cible quantitative Lkg en fonction des autres

D=lm(Lkg ~ A+P+C+Lk+Wk+Ac ,data=seed_imputed)
summary(D)

## R-squared:  0.9236
#=>92,36% de la variabilité de Lkg est expliquée par les autres variables
#==>C'est un bon modèle 

#--------------5.2)Amélioration de la performance du modèle de régression.----------------

#On va éliminer les variables les moins significatives qui sont les variables 
#avec la valeur du pvalue la plus élevée
#c’est le paramètre qui est le plus susceptible d’être nul.
 
#Le paramêtre relatif à la variable Wk présente la valeur p-value= 0.165  
#la plus importante, alors la variable Wk est de variabilité minimale.
#=>on va opter à éliminer la variable Wk et définir le nouveau modèle

#-------------------------------------En eliminant Wk------------------------------------------

DSWk=lm(Lkg ~ A+P+C+Lk+Ac ,data=seed_imputed)
summary(DSWk)

#Le modèle ne perd pas de qualité
#=> on a toujours R2=0.9229 
#le même taux d’information expliquée de la variabilité de Lkg.
#la variable éliminée n'a pas d'importance

#----------------------------------------En eliminant Lk-----------------------------------

DSLk=lm(Lkg ~ A+P+C+Ac ,data=seed_imputed)
summary(DSLk)
# R2:  0.9132 =>le modèle perd de qualité 
#=>la varible Lk est significative 
#=>On va considérer le modèle précédant DSWk comme modèle optimal 


#--------5.3)-----------------------l’analyse en composantes principales-------------------
#------------------------------------------------PCA---------------------------------------

install.packages("FactoMineR")
library(FactoMineR)
install.packages("factoextra")
library(factoextra)

data_pca <- subset(seed_imputed,select=-c(Wk,Lkg,varietie))
results <- prcomp(data_pca, scale = TRUE)

results$rotation

fviz_pca_var(results ,
             col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     
             )

#Plus l’angle entre deux variables est petit
#plus la corrélation est proche de 1.
#Si l’angle est droit, la corrélation est proche de 0.
#Si l’angle est de 180° (les flèches sont opposées)
# alors la corrélation est proche de -1. 

#--Visualisation des valeurs propres.
fviz_eig(results)
 
#ce graphe représente les pourcentages des variances expliquées par chaque axe principal.
#D'après le graphe, on va garder les trois premières dimensions


#Régression de la variable Lkg en fonction des autres variables de la nouvelle base

NEW_DATA=results$x[, 1:3]
NEW_MODEL=lm(seed_imputed$Lkg ~ NEW_DATA )
summary(NEW_MODEL)

#R-squared: 0.9032
#90.32% de la variation de Lkg est expliquée par les variables résultantes
#de la PCA 
#Malgré la diminution de R2, il reste un bon modèle 


#-------------------------------Tâche 6 :  Régression linéaire généralisée-----------------

#Regression Poisson : On n'a pas utilisé la regression de poisson puisque notre variable dépendante Lkg 
#possède des valeurs continues positives et non entières 
#Regression Logistique : On n'a pas utilisé la régression logistique 
#car la variable LKG est quantitative et non qualitative


GENERALIZED_MODEL <- glm(Lkg ~ A+P+C+Lk+Wk+Ac, data=seed_imputed,family=Gamma())
#gamma : valeurs des variables dépendantes continue positifs

summary(GENERALIZED_MODEL)
AIC(GENERALIZED_MODEL)
#AIC(GENERALIZED_MODEL)= -198.1217
AIC(NEW_MODEL)
#AIC(NEW_MODEL)= -160.9094

#AIC(GENERALIZED_MODEL)< AIC(NEW_MODEL) 
#==> Le modèle linéaire généralisé a une qualité d'ajustement meillsseure
#que le modèle obtenu par la modélisation linéaire des données obtenues
#de la PCA

#--------------------------------------Classification------------------------------------

create_train_test <- function(data, size = 0.8, train = TRUE) {
    n_row = nrow(data)
    total_row = size * n_row
    train_sample <- 1: total_row
    if (train == TRUE) {
        return (data[train_sample, ])
    } else {
        return (data[-train_sample, ])
    }
}

data_train <- create_train_test(seed_imputed, 0.8, train = TRUE)
data_test <- create_train_test(seed_imputed, 0.8, train = FALSE)
dim(data_train)
dim(data_test)

install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)
fit <- rpart(varietie ~ ., data = data_train, method = 'class')

predict_unseen <- predict(fit, data_test, type = 'class')
table_mat <- table(data_test$varietie, predict_unseen)
table_mat
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
accuracy_Test

