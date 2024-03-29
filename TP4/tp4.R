library(dplyr)
library(sf)
library(spdep)
library(RColorBrewer)
library(ggplot2)
library(classInt)

rm(list = ls())

iris <- st_read("./fonds/iris_franceentiere_2021.shp", options = "ENCODING=WINDOWS-1252" )
data <- read.csv2("./donnee/BASE_TD_FILO_DISP_IRIS_2018.csv", sep=";", dec=".")

marseille <- iris %>% filter(substr(depcom,1,3)=="132") %>% left_join(data %>%  select(code=IRIS,DISP_MED18), by="code")

# Conversion du système de projection en Lambert-93
marseille <- st_transform(marseille, crs = st_crs(2154))

# Résumé statistique de la variable de revenu médian
summary(marseille$DISP_MED18)

# Boxplot du revenu moyen en fonction des arrondissements
boxplot(DISP_MED18 ~ depcom, data = marseille, main = "Revenu moyen par arrondissement à Marseille")
ggplot(data = marseille, aes(x = depcom, y = DISP_MED18)) +
  geom_boxplot() + theme_bw() + 
  labs(title = "Revenu moyen par arrondissement à Marseille", x = "Arrondissement", y = "Revenu moyen")

# Suppression des valeurs manquantes
marseille <- na.omit(marseille)

# Représentation de la carte de Marseille en fonction des revenus
# Utilisation de la méthode de discrétisation automatique (breaks)
plot(marseille["DISP_MED18"], breaks = "jenks", main = "Revenu médian à Marseille")

ggplot(data = marseille, aes(fill = cut(DISP_MED18, 
                                        breaks = classIntervals(marseille$DISP_MED18, 5, style = "jenks")$brks,
                                        include.lowest = TRUE, right=FALSE))) +
  geom_sf() +
  scale_fill_manual(values = brewer.pal(5, "Blues"), name = "Revenu médian") +
  labs(title = "Distribution des revenus médians à Marseille") +
  theme_void()

# Création de la permutation aléatoire des revenus médians par iris
marseille$DISP_MED18_ALEA <- sample(marseille$DISP_MED18)

# Représentation de la distribution géographique de la variable aléatoire
plot(marseille["DISP_MED18_ALEA"],breaks = "jenks", main = "Distribution aléatoire des revenus médians à Marseille")

ggplot(data = marseille, aes(fill = cut(DISP_MED18_ALEA, 
                                        breaks = classIntervals(marseille$DISP_MED18_ALEA, 5, style = "jenks")$brks,
                                        include.lowest = TRUE, right=FALSE))) +
  geom_sf() +
  scale_fill_manual(values = brewer.pal(5, "Blues"), name = "Revenu médian aléatoire") +
  labs(title = "Distribution aléatoire des revenus médians à Marseille") +
  theme_void()

# Extraction de la liste des voisins de chaque Iris
voisins <- poly2nb(marseille, queen = TRUE)

# Résumé de l'objet en sortie
summary(voisins)

# Nombre de voisins du quatrième élément de la liste
voisins[[4]]

# Création d'une liste de poids à partir de la liste de voisins
ponderation <- nb2listw(voisins, zero.policy = TRUE)

# Affichage de la structure de l'objet créé
str(ponderation, max.level = 1)

# Résumé de la liste de poids
summary(ponderation)

# Vérification que la somme des pondérations associées à chaque iris est égale à 1
ponderation$weights[4]
all(sapply(ponderation$weights, sum) ==1)

# Création de la variable des revenus disponibles centrés réduits
marseille$DISP_MED18_STD <- scale(marseille$DISP_MED18)

# Vérification que la nouvelle variable est bien centrée (moyenne = 0) et réduite (SD = 1)
mean(marseille$DISP_MED18_STD)
sd(marseille$DISP_MED18_STD)

moran.plot(as.numeric(marseille$DISP_MED18_STD), listw=ponderation, labels = marseille$libelle)
# Interprétation des quatre cadrans du diagramme de Moran :
# - Quadrant supérieur droit : Autocorrélation positive (valeurs hautes entourées de valeurs hautes)
# - Quadrant supérieur gauche : Autocorrélation négative (valeurs basses entourées de valeurs hautes)
# - Quadrant inférieur droit : Autocorrélation négative (valeurs hautes entourées de valeurs basses)
# - Quadrant inférieur gauche : Autocorrélation positive (valeurs basses entourées de valeurs basses)

# Interprétation du diagramme de Moran :
# Si la plupart des points se trouvent dans les quadrants supérieurs (droit et gauche), il y a une forte autocorrélation spatiale positive.
# Si la plupart des points se trouvent dans les quadrants inférieurs (droit et gauche), il y a une forte autocorrélation spatiale négative.

# Calcul de l'indice I de Moran et sa significativité
moran_result <- moran.test(marseille$DISP_MED18_STD, ponderation, randomisation = TRUE)

# Affichage du résultat
moran_result

# Calcul des LISA
mars_rev_lisa <- localmoran(x = as.vector(marseille$DISP_MED18_STD), listw = ponderation)

# Étude de l'objet obtenu
class(mars_rev_lisa)
str(mars_rev_lisa, max.level = 1)
summary(mars_rev_lisa)

# Calcul de la moyenne des indicateurs locaux (Ii)
mean(mars_rev_lisa[,1])

# Nombre d'indicateurs locaux négatifs
table(mars_rev_lisa[, 1] < 0)
sum(mars_rev_lisa[, 1] < 0)
table(mars_rev_lisa[mars_rev_lisa[,5]< 0.05, "Ii"] < 0)

# Ajout des LISA comme nouvelle variable du fond des iris
marseille$LISA <- mars_rev_lisa[, 1]

plot(marseille["LISA"], breaks = "jenks")
# Interprétation de ce que l'on voit sur la carte :
# Les zones avec des LISA positifs sont celles où les revenus des Iris sont similaires à ceux de leurs voisins, tandis que les zones avec des LISA négatifs sont celles où les revenus des Iris sont différents de ceux de leurs voisins.

# Création de la variable LISA_PVAL contenant les valeurs p du test associé
marseille$LISA_PVAL <- mars_rev_lisa[, 5]

# Nombre de LISA significativement différents de zéro pour un niveau de confiance à 95%
sum(marseille$LISA_PVAL < 0.05)
table(mars_rev_lisa[,5] < 0.05)

# Représentation sur une carte des p-valeurs des LISA
plot(marseille["LISA_PVAL"], breaks = c(0, 0.01, 0.05, 0.1, 1), main = "P-valeur des LISA")
