######TP1#####
#Lecture du fichier data.txt dans un dataFrame donnees
donnees = read.table("data.txt", header = T)
#Vérification de l'importation
head(donnees)
#Comparaison des deux types de read.table()
dim(donnees)
#200   11
donnees = read.table("data.txt", header = T,sep = ",")
dim(donnees)
#200   1
donnees = read.table("data.txt", header = T,sep = "\t")
dim(donnees)
#200   11
#Donc par défaut séparateur "\t”
#------->Projection
#Affichage de la troisième colonne de données 1e syntaxe
donnees[,3]
#Affichage de la deuxième colonne de données 1e syntaxe
donnees$x.1
#------->Selection
#Affichage du nombre de ligne de la colonne x.1
length(donnees$x.1)
#Même chose que la 1e dimension de données qui est le nombre de ligne
#Nombre de lignes ou la variable x1 >= 1
sum(donnees$x.1>=1)
length(donnees[donnees$x.1<1,"x.1"])
#Nombre de colonnes ou la variable x1 <= 1
sum(donnees$x.1<=1)
#------->Map & Reduce
#la plus grande valeur de la colonne x.5
max(donnees$x.5)
#Afficher le produit par 10 de la colonne x.5
donnees$x.5 * 10
#la plus grande valeur du produit par 10 de la colonne x.5
max(donnees$x.5 * 10)
# Tests de comparaison de moyennes
help("wilcox.test")
# wilcox = mann-whithney
head(donnees)
#PAs de données qualitatives
breastc = read.csv("breast_cancer.csv")
head(breastc)
library(RColorBrewer)
help("hist")
hist(breastc$radius_mean,
     xlab = "radius mean",
     main = paste("Histogram of radius_mean"), 
     col = colorRampPalette(c("blue", "violet", "red"))(20))
#Maligne
df_malignant <- subset(breastc, diagnosis == 'M')
df_benign <- subset(breastc, diagnosis == 'B')

hist(breastc[breastc$diagnosis=="M",]$radius_mean,
     xlab = "radius mean",
     main = paste("Histogram of radius_mean"),
     breaks = 15,
     xlim = c(min(breastc$radius_mean), max(breastc$radius_mean)),
     ylim = c(0, max(hist(breastc$radius_mean, plot = FALSE)$counts)),
     col = colorRampPalette(c("blue", "violet", "red"))(20))
hist(df_benign$radius_mean, 
     col = colorRampPalette(c("green", "orange", "yellow"))(20), 
     border = "black",
     breaks = 15,
     add = TRUE)
#Boîtes à moustache


boxplot(radius_mean ~
          diagnosis, data = breastc, main = "Boîte à moustaches - Rayon moyen en fonction du dignosis", ylab = "radius_mean", xlab = "Diagnostic")

#######
library(tidyverse)
ggplot(data=breastc, aes(x = diagnosis, y = radius_mean)) +
  geom_boxplot()
help("shapiro.test")
dataM <- breastc$texture_mean[breastc$diagnosis == "M"]
dataB <- breastc$texture_mean[breastc$diagnosis == "B"]
shapiro.test(dataM)
shapiro.test(dataB)
wilcox.test(dataM,dataB)
##Texture_mean est un facteur qui  différencie les deux types de cellules
dataM <- breastc$radius_mean[breastc$diagnosis == "M"]
dataB <- breastc$radius_mean[breastc$diagnosis == "B"]

shapiro.test(dataM)
shapiro.test(dataB)
wilcox.test(dataM,dataB)
var.test(dataM,dataB)
help("shapiro.test")
##Radius_mean est un facteur qui différencie les deux types de cellules
dataM <- breastc$symmetry_se[breastc$diagnosis == "M"]
dataB <- breastc$symmetry_se[breastc$diagnosis == "B"]

shapiro.test(dataM)
shapiro.test(dataB)
wilcox.test(dataM,dataB)
#Ne différencie pas
col <- breastc$radius_mean
med <- median(col)
med
breastc$isHigherMedian = col > med

table(breastc$diagnosis,breastc$isHigherMedian)
#Homogénéité
chisq.test(breastc$diagnosis,breastc$isHigherMedian)
#Les variables sont dépendantes

#Test d'adéquation
table_contingency <- table(breastc$isHigherMedian)
#Il faut soummetre au test du chi-2 une table de contingence ainsi que la probabilité souhaitée
chisq.test(table_contingency, p = c(0.5, 0.5))

ggplot(breastc, aes(x = col)) +
  geom_histogram()
fisher.test(breastc$diagnosis,breastc$isHigherMedian)

ggplot(breastc, aes(x = concavity_mean,y = radius_mean)) +
  geom_point(aes(color = breastc$diagnosis))

hist(breastc$radius_mean,
     xlab = "radius mean",
     main = paste("Histogram of radius_mean"),
     breaks = 15,
     xlim = c(min(breastc$radius_mean), max(breastc$radius_mean)),
     ylim = c(0, max(hist(breastc$radius_mean, plot = FALSE)$counts)),
     col = colorRampPalette(c("blue", "violet", "red"))(20))
hist(breastc$concavity_mean, 
     col = colorRampPalette(c("green", "orange", "yellow"))(20), 
     border = "black",
    
     )

cor.test(breastc$radius_mean, breastc$concavity_mean, method = "spearman")
help(cor.test)
#Pas corrélées
breastc$diagnosis <- ifelse(breastc$diagnosis == "M", 0, 1)
plot(breastc$diagnosis ~ breastc$radius_mean)
ggplot(breastc, aes(x = texture_se,y = radius_mean)) +
  geom_point(aes(color = breastc$diagnosis))




