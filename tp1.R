# tp1
rm(list= ls())

library(sf)
library(dplyr)

# 1. Importer le fond communal
communes <- st_read("fonds/commune_francemetro_2021.shp",
                 options = "ENCODING=WINDOWS-1252")
print("Informations sur la console :")
print(communes)

# 2. Résumé/descriptif du contenu
summary(communes)

# 3. Afficher les dix premières lignes et regarder la dernière colonne
View(head(communes, 10))
print("Dernière colonne de la table :")
print(communes[, ncol(communes)])

# 4. Afficher le système de projection
st_crs(communes)

# 5. Créer une table "communes_Bretagne"
communes_Bretagne <- communes %>%
  filter(dep %in% c("22", "29", "35", "56")) %>%
  select(code, libelle, epc, dep, surf)

# 6. Assurer que la nouvelle table est un objet sf
class(communes_Bretagne)

# 7. Appliquer la fonction plot sur la table
plot(communes_Bretagne, lwd = 0.1)

# 8. Faire la question précédente en utilisant st_geometry dans votre plot
plot(st_geometry(communes_Bretagne), lwd = 0.5)

# 9. Créer une variable de surface appelée "surf2"
communes_Bretagne$surf2 <- st_area(communes_Bretagne$geometry)
# mètres carrés

# 10. Convertir la variable surf2 en km2
communes_Bretagne$surf2 <- units::set_units(communes_Bretagne$surf2, "km^2")

# 11. Vérifier si les variables surf et surf2 sont égales
# FAUX 

# 12. Créer une table départementale "dept_bretagne" sans doublons
dept_bretagne <- communes_Bretagne %>%
  group_by(dep) %>%
  summarise(surf = sum(surf))

plot(st_geometry(dept_bretagne))

# 13. Constituer un fond départemental avec st_union
fond_dept <- communes_Bretagne %>%
  group_by(dep) %>%
  summarise() %>%
  st_union()

plot(fond_dept)

# 14. Créer une table "centroid_dept_bretagne"
centroid_dept_bretagne <- dept_bretagne %>%
  st_centroid()

# 14.b. Représenter les départements et leurs centroïdes
plot(fond_dept)
plot(st_geometry(centroid_dept_bretagne), add = TRUE)

# 14.c. Ajouter le nom du département dans le fond de centroïdes
centroid_dept_bretagne$dept_lib <- dept_bretagne$dep
centroid_dept_bretagne_with_names <- merge(centroid_dept_bretagne, dept_bretagne, by = "dep")

# 14.d. Récupérer les coordonnées des centroïdes
centroid_coords <- st_coordinates(centroid_dept_bretagne)
centroid_coords_df <- as.data.frame(centroid_coords)
centroid_dept_bretagne_with_names <- bind_cols(centroid_coords_df, centroid_dept_bretagne_with_names)

# 14.e. Représenter les départements, leurs centroïdes et leurs noms
plot(fond_dept)
plot(st_geometry(centroid_dept_bretagne_with_names), add = TRUE)
text(st_geometry(centroid_dept_bretagne_with_names), labels = centroid_dept_bretagne_with_names$dept_lib, cex = 0.8)

# 15. Retrouver dans quelle commune se situe le centroïde de chaque département breton
communes_Bretagne_with_dept <- st_join(communes_Bretagne, centroid_dept_bretagne)

# 16. Utiliser st_intersection() et st_within()
communes_Bretagne_with_dept_intersection <- st_intersection(communes_Bretagne, centroid_dept_bretagne)
communes_Bretagne_with_dept_within <- st_within(centroid_dept_bretagne, communes_Bretagne)

# 17. Calculer la distance entre les centroïdes des départements et leurs chefs-lieux
chefs_lieux <- communes_Bretagne %>%
  filter(dep %in% c("22", "29", "35", "56") & code %in% c("22070", "29105", "35238", "56260")) %>%
  st_centroid()

distances <- st_distance(centroid_dept_bretagne, chefs_lieux)

# 18.a. Créer une zone de 20 km autour des centroïdes
buffer_20km <- st_buffer(st_geometry(centroid_dept_bretagne), dist = 20000)

# 18.b. Représenter la géométrie obtenue
plot(buffer_20km)

# 18.c. Récupérer les communes comprises dans les tampons
communes_in_buffer <- st_intersection(communes_Bretagne, buffer_20km)

# 18.d. Nombre de communes concernées par département
communes_count_by_dept <- communes_in_buffer %>%
  group_by(dep) %>%
  summarise(count = n())

# 19.a. Changer le système de projection des communes bretonnes
communes_Bretagne_WGS84 <- st_transform(communes_Bretagne, crs = 4326)

# 19.b. Représenter le fond ainsi produit
plot(communes_Bretagne_WGS84)

# 20. Recalculer l'aire des communes pour créer une variable surf3
communes_Bretagne_WGS84$surf3 <- st_area(st_geometry(communes_Bretagne_WGS84)) / 1e6