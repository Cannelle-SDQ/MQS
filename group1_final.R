library(tidyverse)
library(sf)
library(glue)


###############################
# DATA ---
###############################

# --- Iris shapefile ---
url <- "https://data.iledefrance.fr/api/explore/v2.1/catalog/datasets/iris/exports/geojson?refine=dep%3A75"
tmp <- tempfile(fileext = ".geojson")
download.file(url, tmp, mode = "wb")

iris <- st_read(tmp) %>%
  st_transform(crs = 2154)

# --- Housing values: selling prices ---
# Year of flat sellings
year <- 2021

# Download Paris data for this year
url <- glue("https://files.data.gouv.fr/geo-dvf/latest/csv/{year}/departements/75.csv.gz")
dvf <- read_csv(url, show_col_types = FALSE)

# Keep flat sellings only (otherwise transactions may include commercial locals, garage... and we cannot isolate the housing price)
appts <- dvf %>%
  filter(nature_mutation == "Vente") %>%
  group_by(id_mutation) %>%
  filter(sum(type_local == "Appartement", na.rm = TRUE) == 1,
         !any(type_local %in% c("Maison",
                                "Local industriel. commercial ou assimilé"),
              na.rm = TRUE)) %>%
  ungroup() %>%
  filter(type_local == "Appartement",
         !is.na(longitude), !is.na(latitude)) %>%
  # compute price per squared meter
  mutate(price_m2 = valeur_fonciere / surface_reelle_bati)

# --- Vegetation ---
# Trees (points)
trees <- st_read("https://opendata.paris.fr/api/explore/v2.1/catalog/datasets/les-arbres/exports/geojson") %>%
  st_transform(crs = 2154)

# Green spaces (polygons)
gs <- st_read("https://opendata.paris.fr/api/explore/v2.1/catalog/datasets/espaces_verts/exports/geojson") %>%
  st_transform(crs = 2154)


###############################
# DATA PROCESSING ---
###############################

# --- IRIS boundaries ---
# Compute area
iris <- iris %>%
  mutate(iris_area = as.numeric(st_area(geometry)) / 10000,)

# --- Housing values data ---
# Geocode selling location points
appts <- appts %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(crs = 2154) 

# Compute median price per squared meter at the IRIS level
price_iris <- appts %>%
  st_join(iris) %>%
  st_drop_geometry() %>%
  group_by(code_iris) %>%
  summarise(n_sellings = n(),
            price_m2_median = median(price_m2), .groups = "drop") %>%
  # keep IRIS with at least 10 transactions for the median to have a meaning
  filter(n_sellings >= 10)

# Join at the IRIS level
iris_joined <- iris %>%
  left_join(price_iris)

# --- Trees data ---
# Join at the IRIS level and compute trees density
trees_iris <- iris %>%
  st_join(trees, join = st_contains, left = TRUE) %>%
  st_drop_geometry() %>%
  group_by(code_iris, iris_area) %>%
  summarise(n_trees = sum(!is.na(idbase)), .groups= "drop") %>%
  mutate(
    trees_density  = n_trees / iris_area
  ) %>%
  # Put geometry again by rejoining at the IRIS level
  right_join(iris)

iris_joined <- iris_joined %>%
  left_join(trees_iris)

# --- Green spaces ---
# Eliminate overlaps across polygons
gs_union <- st_union(gs)

# Compute share of IRIS area covered by green spaces
gs_iris <- st_intersection(iris %>% dplyr::select(code_iris, iris_area), gs_union) %>%
  mutate(area_gs_m2 = as.numeric(st_area(geometry))) %>%
  st_drop_geometry() %>%
  group_by(code_iris, iris_area) %>%
  # compute green space area
  summarise(area_gs_m2 = sum(area_gs_m2), .groups = "drop") %>%
  ungroup() 

# Spatialize data again and compute IRIS share of green space
iris_joined <- iris_joined %>%
  left_join(gs_iris) %>%
  mutate(
    area_gs_m2 = ifelse(is.na(area_gs_m2), 0, area_gs_m2),     # replace missing values by 0
    share_gs    = 100 * area_gs_m2 / (iris_area * 10000)   
  )

###############################
# MAPS ---
###############################

# --- Housing values as proportional symbols ---
symbols <- iris_joined %>%
  filter(!is.na(price_m2_median)) %>%
  # place the symbol at each iris' centroid
  st_centroid()

# Compute quintiles
bounds <- quantile(symbols$price_m2_median,
                   probs = seq(0, 1, 0.2), na.rm = TRUE)

fmt <- function(x) format(round(x, -1), big.mark = " ", trim = TRUE)
# Label the symbols with the values bounding the quintiles
labels <- paste(fmt(head(bounds, -1)), "to", fmt(tail(bounds, -1)))

# Identify the quintile each iris falls in
symbols <- symbols %>%
  mutate(classe = cut(price_m2_median,
                      breaks = bounds,
                      labels = labels,
                      include.lowest = TRUE))

# TREES DENSITY
ggplot() +
  geom_sf(data = iris_joined,
          aes(fill = trees_density),
          color = "white",
          size = 0.05) +
  scale_fill_distiller(type = "seq",
                       palette = "Greens",
                       direction = 1,
                       trans = "sqrt",
                       name = "Trees density (Count per 100m2)") +
  geom_sf(data = symbols,
          aes(size = classe, color = classe),
          shape = 21, fill = NA, stroke = 0.7) +
  scale_size_manual(values = c(0.8, 1.6, 2.6, 3.8, 5.2),   
                    name = "Median price (€/m²)") +
  scale_color_manual(values = c("#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d"),
                     name = "Median price (€/m²)")+
  theme_void()

# GREEN SPACE SHARE
ggplot() +
  geom_sf(data = iris_joined,
          aes(fill = share_gs),
          color = "white",
          size = 0.05) +
  scale_fill_distiller(type = "seq",
                       palette = "Greens",
                       direction = 1, 
                       name = "Green space area (% of IRIS area)") +
  geom_sf(data = symbols,
          aes(size = classe, color = classe),
          shape = 21, fill = NA, stroke = 0.7) +
  scale_size_manual(values = c(0.8, 1.6, 2.6, 3.8, 5.2),   
                    name = "Median price (€/m²)") +
  scale_color_manual(values = c("#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d"),
                     name = "Median price (€/m²)")+
  theme_void()


