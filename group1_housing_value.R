library(tidyverse)
library(sf)
library(glue)

# --- Data ---
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

# Load Iris shapefile
url <- "https://data.iledefrance.fr/api/explore/v2.1/catalog/datasets/iris/exports/geojson?refine=dep%3A75"
tmp <- tempfile(fileext = ".geojson")
download.file(url, tmp, mode = "wb")

iris <- st_read(tmp)

# Geocode selling locations and join an the IRIS level
pts <- appts %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(st_crs(iris)) %>%
  st_join(iris["code_iris"])

# Compute median price per squared meter at the IRIS level
price_iris <- pts %>%
  st_drop_geometry() %>%
  group_by(code_iris) %>%
  summarise(n_sellings = n(),
            price_m2_median = median(price_m2)) %>%
  # keep IRIS with at least 10 transactions for the median to have a meaning
  filter(n_sellings >= 10)

price_iris_joined <- iris %>% left_join(price_iris, by = "code_iris")

# --- Maps ---

# CHOROPLETH MAP
ggplot(price_iris_joined) +
  geom_sf(aes(fill = price_m2_median), color = "white", linewidth = 0.1) +
  scale_fill_viridis_c(name = "Median Price (€/m²)", na.value = "grey85") +
  theme_void()

# PROPORTIONAL SYMBOL MAP
symbols <- price_iris_joined %>%
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

# Proportional symbol map with size and color varying by quintile
ggplot() +
  geom_sf(data = iris, fill = "grey92", color = "white", linewidth = 0.1) +
  geom_sf(data = symbols,
          aes(size = classe, color = classe),
          shape = 21, fill = NA, stroke = 0.7) +
  scale_size_manual(values = c(0.8, 1.6, 2.6, 3.8, 5.2),   
                    name = "Median price (€/m²)") +
  scale_color_manual(values = c("#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d"),
                     name = "Median price (€/m²)")+
  theme_void() 


# --- 3D MAP (fully made with AI)---
library(mapgl)   

pal <- viridisLite::magma(5, begin = 0.2, end = 0.9)

iris3d <- price_iris_joined %>%
  st_transform(4326) %>%                     
  mutate(
    quintile = cut(price_m2_median,
                   breaks = quantile(price_m2_median,
                                     probs = seq(0, 1, 0.2), na.rm = TRUE),
                   labels = 1:5, include.lowest = TRUE) %>% as.integer(),
    height = replace_na(quintile, 0) * 300,   
    color  = coalesce(pal[quintile], "#d9d9d9"),   
    info   = paste0("<b>IRIS ", code_iris, "</b><br>",
                    "Median price: ", round(price_m2_median), " €/m²<br>",
                    "Quintile: ", quintile, "<br>",
                    "Sellings: ", n_sellings)
  )

m <- maplibre(style = carto_style("positron"),
              center = c(2.34, 48.86), zoom = 11,
              pitch = 40, bearing = -20) |>
  add_fill_extrusion_layer(
    id = "iris_price",
    source = iris3d,
    fill_extrusion_height  = list("get", "height"),
    fill_extrusion_color   = list("get", "color"),
    fill_extrusion_opacity = 0.9,
    tooltip = "info"
  ) |>
  add_legend("Median price quintile",
             values = c("1 (lowest)", "2", "3", "4", "5 (highest)"),
             colors = pal, type = "categorical")

m
htmlwidgets::saveWidget(m, "map_3d.html", selfcontained = TRUE) # we then take a screenshot of this

