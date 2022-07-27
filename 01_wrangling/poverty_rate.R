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

df_pov <- read_excel(
  file.path(
    data_dir,
    "Raw Data",
    "Quantitative Data",
    "somalia-poverty-rate.xlsx"
  ),
  sheet = 1
) %>%
  filter(
    !is.na(Region)
  ) %>%
  mutate(
    District = case_when(
      District == "Baidoa" ~ "Baydhaba",
      District == "Kismayo" ~ "Kismaayo",
      District == "Bandarbayla" ~ "Bandarbeyla",
      District == "Garowe" ~ "Garoowe",
      TRUE ~ District
    )
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
  as.data.frame()

df <- df_pov %>%
  left_join(
    df_bounds,
    by = c(
      "Region" = "admin1Name",
      "District" = "admin2Name"
    )
  ) %>%
  transmute(
    `Admin 1` = Region,
    `Admin 1 Pcode` = admin1Pcod,
    `Admin 2` = District,
    `Admin 2 Pcode` = admin2Pcod,
    `Total population estimate`,
    `Population in Poverty` = `Poverty Rate`
  )

write_csv(
  df,
  file = file.path(
    data_dir,
    "Aggregated Files",
    "Somalia population in poverty.csv"
  )
)
