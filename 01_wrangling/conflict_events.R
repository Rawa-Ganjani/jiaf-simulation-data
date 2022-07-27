library(tidyverse)
library(janitor)
library(sf)

data_dir <- Sys.getenv("JIAF_SIMU_DATA_DIR")

df_sectoral <- read_csv(
  file.path(
    data_dir,
    "Raw Data",
    "Quantitative Data",
    "Somalia - HNO2022 (Pin and Severity).csv"
  )
)

df_conf <- read_csv(
  file.path(
    data_dir,
    "Raw Data",
    "Quantitative Data",
    "conflict_data_som.csv"
  )
) %>%
  filter(
    row_number() > 1,
    year == 2021
  ) %>%
  type.convert() %>%
  mutate(
    adm1 = gsub(" region", "", adm_1),
    adm2 = gsub(" district", "", adm_2),
  ) %>%
  select(
    adm1, adm2, deaths_a, deaths_b, deaths_civilians, deaths_unknown,
    latitude,
    longitude
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

df <- st_as_sf(df_conf, coords = c("longitude", "latitude")) %>%
  st_set_crs(4326) %>%
  st_join(df_bounds)

output <- df %>%
  as.data.frame() %>%
  mutate(
    `Admin 1` = case_when(
      adm1 == "Galgudud" ~ "Galgaduud",
      is.na(admin1Name) ~ adm1,
      TRUE ~ admin1Name
    ),
    `Admin 2` = case_when(
      adm2 == "Kismayo" ~ "Kismaayo",
      adm2 == "Abudwaq" ~ "Cabudwaaq",
      adm2 == "El Waq" ~ "Ceel Waaq",
      adm2 == "Burao" ~ "Burco",
      is.na(admin2Name) ~ adm2,
      TRUE ~ admin2Name
    )
  ) %>%
  group_by(
    `Admin 1`,
    `Admin 2`
  ) %>%
  summarize(
    deaths_a = sum(deaths_a, na.rm = TRUE),
    deaths_b = sum(deaths_b, na.rm = TRUE),
    deaths_civilians = sum(deaths_civilians, na.rm = TRUE),
    deaths_unknown = sum(deaths_unknown, na.rm = TRUE)
  ) %>%
  filter(!is.na(`Admin 1`)) %>%
  left_join(
    df_bounds %>%
      select(
        admin1Pcod,
        admin2Pcod,
        admin2Name,
        admin1Name
      ),
    by = c(
      "Admin 2" = "admin2Name",
      "Admin 1" = "admin1Name"
    )
  ) %>%
  as.data.frame() %>%
  transmute(
    `Admin 1`,
    `Admin 1 Pcode` = admin1Pcod,
    `Admin 2`,
    `Admin 2 Pcode` = admin2Pcod,
    `Deaths of Side A` = deaths_a,
    `Deaths of Side B` = deaths_b,
    `Deaths of Side Civilinas` = deaths_civilians,
    `Deaths of Side Unknown people` = deaths_unknown
  )

write_csv(
  output,
  file = file.path(
    data_dir,
    "Aggregated Files",
    "Political conflict death cases.csv"
  )
)
