library(dplyr)
library(ggplot2)
library(sf)
library(glue)

bucket_name <- "" #à remplacer par le nom d'utilisateur sspcloud
  
# --- Iris shapefile ---
url <- "https://data.iledefrance.fr/api/explore/v2.1/catalog/datasets/iris/exports/geojson?refine=dep%3A75"
tmp <- tempfile(fileext = ".geojson")
download.file(url, tmp, mode = "wb")

iris_paris <- st_read(tmp)%>%
  st_make_valid() %>%
  st_transform(2154)

nrow(iris_paris)   # 992

ggplot(iris_paris) + geom_sf(fill = "grey", color = "white", size = 0.1) + theme_void()

# --- Arbres de la Ville de Paris (points) ---
arbres <- st_read("https://opendata.paris.fr/api/explore/v2.1/catalog/datasets/les-arbres/exports/geojson")

# --- Espaces verts (polygones) ---
ev <- st_read("https://opendata.paris.fr/api/explore/v2.1/catalog/datasets/espaces_verts/exports/geojson")

### 2. MEME PROJECTION POUR TOUT LE MONDE
# On passe tout en 2154 : unités en mètres, donc les surfaces sont correctes.
arbres <- st_transform(arbres, 2154)
ev     <- st_transform(ev, 2154) %>% st_make_valid()

st_crs(iris_paris) == st_crs(arbres)

### 3. NOMBRE D'ARBRES PAR IRIS (jointure spatiale, comme pour les restaurants)
iris_arbres <- st_join(x = iris_paris,
                       y = arbres,
                       join = st_contains,
                       left = TRUE)

arbres_counts <- iris_arbres %>%
  st_drop_geometry() %>%
  group_by(code_iris) %>%
  summarise(n_arbres = sum(!is.na(idbase)))   

iris_paris <- iris_paris %>%
  left_join(arbres_counts, by = "code_iris") %>%
  mutate(
    surface_ha     = as.numeric(st_area(geometry)) / 10000,
    arbres_par_ha  = n_arbres / surface_ha
  )

hist(iris_paris$arbres_par_ha)

### 4. SURFACE D'ESPACES VERTS PAR IRIS
#Eviter deux fois les chevauvements
ev_union <- st_union(ev)

ev_par_iris <- st_intersection(iris_paris %>% dplyr::select(code_iris), ev_union) %>%
  mutate(surf_ev_m2 = as.numeric(st_area(geometry))) %>%
  st_drop_geometry() %>%
  group_by(code_iris) %>%
  summarise(surf_ev_m2 = sum(surf_ev_m2))

iris_paris <- iris_paris %>%
  left_join(ev_par_iris, by = "code_iris") %>%
  mutate(
    surf_ev_m2 = coalesce(surf_ev_m2, 0),
    part_ev    = 100 * surf_ev_m2 / (surface_ha * 10000)   # % de la surface de l'IRIS
  )

### 5. AJOUT DE LA POPULATION (INSEE, base infracommunale "Population")
# A télécharger sur insee.fr : base-ic-evol-struct-pop-XXXX (fichier csv, séparateur ";")

pop <- aws.s3::s3read_using(FUN = read.csv2, 
                                      object = "base-ic-evol-struct-pop-2022.CSV", 
                                      bucket = glue("{bucket_name}"), 
                                      opts = list("region" = "",
                                                  "colClasses" = c(IRIS = "character")))



iris_paris <- iris_paris %>%
  left_join(pop %>% dplyr::select(IRIS, P22_POP, P22_POP65P, P22_POP0014),
            by = c("code_iris" = "IRIS"))

sum(is.na(iris_paris$P22_POP))   # IRIS qui n'ont pas trouvé de correspondance

#Arbres par hectare (on coupe en quantiles car la distribution est très asymétrique)
ggplot() +
  geom_sf(data = iris_paris,
          aes(fill = arbres_par_ha),
          color = "white",
          size = 0.05) +
  scale_fill_distiller(type = "seq",
                       palette = "Greens",
                       direction = 1,
                       trans = "sqrt") +
  theme_void() +
  labs(fill = "Arbres / ha",
       title = "Densité d'arbres par IRIS à Paris",
       caption = "")
# Part de la surface en espaces verts
ggplot() +
  geom_sf(data = iris_paris,
          aes(fill = part_ev),
          color = "white",
          size = 0.05) +
  scale_fill_distiller(type = "seq",
                       palette = "Greens",
                       direction = 1) +
  theme_void() +
  labs(fill = "% espaces verts",
       title = "Part de la surface en espaces verts par IRIS")

# Export
st_write(iris_paris, "processed-data/iris_paris_vert.geojson",
         driver = "GeoJSON", delete_dsn = TRUE)
