library(tidyverse)
library(janitor)
library(sf)

# DSA

data_dir <- Sys.getenv("JIAF_SIMU_DATA_DIR")

df_sectoral <- read_csv(
  file.path(
    data_dir,
    "Raw Data",
    "Quantitative Data",
    "Somalia - HNO2022 (Pin and Severity).csv"
  )
)

df_som <- read_excel(
  file.path(
    data_dir,
    "Raw Data",
    "Quantitative Data",
    "DTM Somalia Somaliland MT B2.xlsx"
  ),
  sheet = "Somaliland MT B2 Dataset",
  skip = 2
)

df_punt <- read_excel(
  file.path(
    data_dir,
    "Raw Data",
    "Quantitative Data",
    "DTM SOMALIA_B2 Puntland_2021.xlsx"
  ),
  sheet = "B2 Puntland 2021",
  skip = 2
)

df_bounds <- st_read(
  file.path(
    data_dir,
    "Raw Data",
    "Quantitative Data",
    "som-administrative-divisions-shapefiles",
    "Som_Admbnda_Adm2_UNDP.shp"
  )
) %>%
  st_transform(
    crs = 4326
  )

df_dtm <- rbind(df_punt, df_som) %>%
  mutate(
    lat = as.numeric(`Y Coordinate (Latitude)`),
    long = as.numeric(`X Coordinate (Longitude)`)
  ) %>%
  filter(!is.na(lat), !is.na(long))

# checking which areas don't align with OCHA boundaries
df <- st_as_sf(df_dtm, coords = c("long", "lat"))

df <- df %>%
  st_set_crs(4326) %>%
  st_join(df_bounds) %>%
  filter(
    `District name` != admin2Name
  ) %>%
  as.data.frame() %>%
  select(
    `Region name`,
    `District name`,
    admin1Name,
    admin2Name
  ) %>%
  unique()

df_dtm <- df_dtm %>%
  group_by(
    `Admin 1` = `Region name`,
    `Admin 1 Pcode` = `Region code`,
    `Admin 2` = `District name`,
    `Admin 2 Pcode` = `District code`
  ) %>%
  summarise(
    `Total number of Residents (Ind)` =
      sum(`Total number of Residents (Ind)`, na.rm = TRUE),
    `Total number of IDPs in sites (ind)` =
      sum(`Total number of IDPs in sites (ind)`, na.rm = TRUE),
    `Total number of IDPs outside of sites (Ind)` =
      sum(`Total number of IDPs outside of sites (Ind)`)
  ) %>%
  mutate( # the pcodes are the same as OCHA bounds but has an extra "M"
    `Admin 1 Pcode` = gsub("SOM", "SO", `Admin 1 Pcode`),
    `Admin 2 Pcode` = gsub("SOM", "SO", `Admin 2 Pcode`)
  )

write_csv(
  df_dtm,
  file = file.path(
    data_dir,
    "Aggregated Files",
    "Somalia DTM data.csv"
  )
)
