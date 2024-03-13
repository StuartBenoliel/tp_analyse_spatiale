rm(list=ls())
library(DBI)
library(sf)
source(file = "TP5/connexion_db.R")

conn<-connecter()
DBI::dbListTables(conn)

DBI::dbListFields(conn,"popnaiss_com")

query <- paste0(
  'CREATE TABLE bpe21_metro',
  '( id INT PRIMARY KEY,',
  types_vars_metro,
  'geometry GEOMETRY(POINT, 2154));',
  collapse =''
)

dbSendQuery(conn, 'SELECT * FROM popnaiss_com')
popnaiss_com <- dbGetQuery(conn, 'SELECT * FROM popnaiss_com')

dbGetQuery(conn, "SELECT * FROM popnaiss_com WHERE CODGEO = '35047'")

query <- paste0(
  "SELECT * FROM bpe21_metro ",
  "INNER JOIN popnaiss_com ON bpe21_metro.depcom = popnaiss_com.codgeo ",
  "WHERE CODGEO = '35047';",
  collapse = " "
)
dbGetQuery(conn, query)

library(dplyr)
library(dbplyr)

# Connexion à la table popnaiss
popnaiss<-tbl(conn,"popnaiss_com")
str(popnaiss) # ! ce n'est pas un data.frame

# Reprise de la question 5
popnaiss %>% 
  filter(codgeo=="35047") %>% 
  show_query()

pop_bruz <- popnaiss %>% 
  filter(codgeo=="35047") %>% 
  collect()
str(pop_bruz)

bpe21_metro <- tbl(conn,"bpe21_metro")
str(bpe21_metro)

popnaiss %>% inner_join(bpe21_metro, join_by(codgeo ==  depcom)) %>% filter(codgeo=="35047") %>% 
  collect()

bpe_dep50 <- bpe21_metro %>% filter(dep == "50") %>% 
  select(id, depcom, dom, sdom, typequ, geometry) %>% 
  collect()
str(bpe_dep50)
st_crs(bpe_dep50)
st_read(conn, query = "SELECT id, depcom, dom, sdom, typequ, geometry FROM bpe21_metro WHERE dep = '50';")
dbGetQuery(conn, "SELECT DISTINCT(ST_SRID(geometry)) FROM bpe21_metro;")
dbGetQuery(conn, "SELECT DISTINCT(ST_SRID(geometry)) FROM bpe21_04;")

query <- paste0(
  "SELECT reg, COUNT(id) FROM bpe21_metro ",
  "WHERE typequ = 'D107' ",
  "GROUP BY reg ",
  "ORDER BY COUNT(id) DESC ;",
  collapse = " "
)
dbGetQuery(conn, query)

bpe21_metro %>% filter(typequ =="D107") %>% group_by(reg) %>% 
  summarise(count_id =count(id)) %>% arrange(desc(count_id)) %>% collect()

cinema_bpe <- st_read(conn, query ="SELECT * FROM bpe21_metro WHERE TYPEQU='F303';")

# On construit un buffer de 1km (une zone tampon) autour de la sorbonne
# df des coordonnées
sorbonne_buffer <- data.frame(x=2.34297,y=48.84864) %>% 
  #qu'on transforme en objet sf (systeme de proj WGS84 => crs=4326)
  st_as_sf(coords = c("x","y"), crs = 4326) %>% 
  # on reprojette en LAMBERT-93 (crs=2154)
  st_transform(2154) %>% 
  # on crée la zone tampon autour du point (l'unité est le mètre ici)
  st_buffer(1000) 

str(sorbonne_buffer) # le buffer est constitué d'un unique polygône
plot(sorbonne_buffer %>% st_geometry()) # qui s'avère être un cercle

cinema_1km_sorbonne <- cinema_bpe  %>% st_intersection(sorbonne_buffer)

library(leaflet)
# Optionnel :
# On récupère une icone spécifique sur https://ionic.io/ionicons (mot clé film)
cinemaIcons <- makeIcon(iconUrl = "images/film-sharp.png", 18,18)

leaflet() %>% 
  setView(lat = 48.84864, lng = 2.34297, zoom = 15) %>% 
  addTiles() %>% 
  addMarkers(lat = 48.84864, lng = 2.34297) %>% 
  addCircles(
    lat = 48.84864, lng = 2.34297, weight = 1, radius = 1000
  ) %>% 
  addMarkers(data = cinema_1km_sorbonne %>% st_transform(4326))


# Remarque : 1000m en LAMBERT-93 ce n'est pas exactement 1000m en WGS84 (zoomez sur la carte suivante)
leaflet() %>%
  setView(lat = 48.84864, lng = 2.34297, zoom = 15) %>%
  addTiles() %>%
  addCircles(
    lat = 48.84864, lng = 2.34297, weight = 1, radius = 1000
  ) %>%
  addPolygons(data=sorbonne_buffer %>% st_transform(4326), col = "red")

boulodrome <- st_read(conn, query ="SELECT * FROM bpe21_metro WHERE TYPEQU='F102';")

paca <- st_read(conn, query ="SELECT * FROM regions_metro WHERE code='93';")
str(paca)

query <- paste0(
  "SELECT bpe.* FROM bpe21_metro AS bpe, regions_metro AS regions ",
  "WHERE ST_Contains(regions.geometry, bpe.geometry) AND bpe.typequ='F102' ",
  "AND regions.code = '93' ;",
  collapse = " "
)

boulodromes_paca <- st_read(conn, query = query)
boulodromes_paca_bis <- st_read(conn, query = "SELECT id, typequ, dep, qualite_xy, geometry FROM bpe21_metro WHERE typequ = 'F102' and dep in ('04','05','06','13','83','84');")
diff <- boulodromes_paca_bis %>% mutate(version_bis = TRUE) %>% 
  st_join(
    boulodromes_paca %>% mutate(version_orig = TRUE) %>% select(-typequ), by = "id"
  ) %>% 
  filter((is.na(version_bis) | (is.na(version_orig))))
# A cause de la jointure -> regions_metro ne doit pas contenir certains boulodromes

# Ici, on prend pour exemple les maternités
mater <- sf::st_read(conn, query = "SELECT * FROM bpe21_metro WHERE TYPEQU='D107';") %>%
  slice(1)

# On récupère ses coordonnées 
mater_coords <- st_coordinates(mater) %>% as.numeric

sf_reg_metro <- st_read(conn, query = "SELECT * FROM regions_metro")
plot(st_geometry(sf_reg_metro))
points(x = mater_coords[1], y = mater_coords[2], pch = 4, lwd = 2, cex = 1.5, col = "red")

# On transforme ses coordonnées en WGS84 (epsg=4326)
mater_coords <- st_coordinates(mater %>% st_transform(4326)) %>% as.numeric

leaflet() %>% 
  setView(lng = mater_coords[1], lat = mater_coords[2], zoom = 14) %>% 
  addTiles() %>% 
  addMarkers(lng = mater_coords[1], lat = mater_coords[2])

# install.packages("osrm")
# Attention, cela peut prendre quelques minutes
iso <- osrm::osrmIsochrone(
  loc = mater_coords, # coordonnées du point de référence
  breaks = seq(0,60,10), # valeurs des isochrones à calculer en minutes
  res = 100 # détermine le nombre de points utilisés (res*res) pour dessiner les isochornes 
)
str(iso)

bks <-  sort(unique(c(iso$isomin, iso$isomax)))
pals <- hcl.colors(n = length(bks) - 1, palette = "Red-Blue", rev = TRUE)
plot(iso["isomax"], breaks = bks, pal = pals, 
     main = "Isochrones (in minutes)", reset = FALSE)
points(x = mater_coords[1], y = mater_coords[2], pch = 4, lwd = 2, cex = 1.5)

leaflet() %>% 
  setView(lng = mater_coords[1], lat = mater_coords[2], zoom = 8) %>% 
  addTiles() %>% 
  addMarkers(lng = mater_coords[1], lat = mater_coords[2]) %>% 
  addProviderTiles(
    providers$CartoDB.DarkMatter,
    options = providerTileOptions(opacity = 0.4)) %>%
  addPolygons(
    data=iso, 
    fillColor = pals,
    smoothFactor = 0.3,
    weight = 1,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.65
  ) %>% 
  addLegend(
    position="bottomleft",
    colors=pals,
    labels=rev(c("50-60","40-50",
                 "30-40","20-30","10-20", "0-10")),
    opacity = 0.6,
    title="Temps de trajet par la route (en minutes)")