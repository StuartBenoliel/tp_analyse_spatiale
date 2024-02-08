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
str(communes)
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

ggplot() + geom_sf(data= communes_Bretagne, fill ="steelblue") + theme_void()

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
  st_union(by_feature = TRUE)

plot(st_geometry(fond_dept))

# 14. Créer une table "centroid_dept_bretagne"
centroid_dept_bretagne <- dept_bretagne %>%
  st_centroid()
st_geometry_type(centroid_dept_bretagne)

# 14.b. Représenter les départements et leurs centroïdes
plot(st_geometry(fond_dept))
plot(st_geometry(centroid_dept_bretagne), add = TRUE)

# 14.c. Ajouter le nom du département dans le fond de centroïdes
dept_names <- data.frame(dep = unique(communes_Bretagne$dep), dept_lib = c("Côtes-d'Armor", "Finistère", "Ille-et-Vilaine", "Morbihan"))
centroid_dept_bretagne <- merge(centroid_dept_bretagne, dept_names, by = "dep")

# 14.d. Récupérer les coordonnées des centroïdes
centroid_coords <- st_coordinates(centroid_dept_bretagne)
centroid_coords <- bind_cols(centroid_coords, select(centroid_dept_bretagne, dep, dept_lib))
centroid_coords <- st_drop_geometry(centroid_coords)

# 14.e. Représenter les départements, leurs centroïdes et leurs noms
plot(st_geometry(fond_dept))
plot(st_geometry(centroid_dept_bretagne), add = TRUE)
text(centroid_coords$X, centroid_coords$Y- 10000, labels = centroid_coords$dept_lib)

# 15. Retrouver dans quelle commune se situe le centroïde de chaque département breton
intersects_result <- st_intersects(centroid_dept_bretagne, communes_Bretagne)

# 16. Utiliser st_intersection() et st_within()
intersection_result <- st_intersection(centroid_dept_bretagne, communes_Bretagne)
within_result <- st_within(centroid_dept_bretagne, communes_Bretagne)

# 17. Calculer la distance entre les centroïdes des départements et leurs chefs-lieux
chefs_lieux <- communes_Bretagne %>%
  filter(code %in% c("22278", "29232", "35238", "56260")) %>%
  st_centroid()

distances <- st_distance(centroid_dept_bretagne, chefs_lieux)

# 18.a. Créer une zone de 20 km autour des centroïdes
buffer_20km <- st_buffer(st_geometry(centroid_dept_bretagne), dist = units::set_units(20, km))

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
plot(st_geometry(communes_Bretagne), lwd = 0.5)
plot(st_geometry(communes_Bretagne_WGS84), lwd = 0.5)

# 20. Recalculer l'aire des communes pour créer une variable surf3
communes_Bretagne_WGS84$surf3 <- st_area(st_geometry(communes_Bretagne_WGS84)) %>% 
  units::set_units("km^2")
