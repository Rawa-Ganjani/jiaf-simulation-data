library(tidyverse)
library(readxl)
library(janitor)

data_dir <- Sys.getenv("JIAF_SIMU_DATA_DIR")

df_sectoral <- read_csv(
  file.path(
    data_dir,
    "Raw Data",
    "Quantitative Data",
    "Somalia - HNO2022 (Pin and Severity).csv"
  )
)

df_msna <- read_excel(
  file.path(
    data_dir,
    "Raw Data",
    "Quantitative Data",
    "REACH_SOM2101_JMCNA.xlsx"
  ),
  sheet = 1
) %>%
  filter(
    row_number() > 1,
    Area != "Zdummy"
  ) %>%
  mutate(
    adm2_name = case_when(
      Area == "Baidoa" ~ "Baydhaba",
      Area == "Kismayo" ~ "Kismaayo",
      Area == "Bandarbayla" ~ "Bandarbeyla",
      Area == "Garowe" ~ "Garoowe",
      TRUE ~ gsub("_", " ", Area)
    ),
    pop_group = ifelse(`Population group` == "HC", "Non-Displaced", "Displaced")
  ) %>%
  select(-c(uuid, KEY, Area, `Population group`))

df_msna_summarized <- df_msna %>%
  pivot_longer(
    cols = -c(adm2_name, pop_group),
    names_to = "indicator",
    values_to = "inneed"
  ) %>%
  mutate(
    inneed = ifelse(inneed > 2, "Yes", "No")
  ) %>%
  group_by(
    adm2_name,
    pop_group,
    indicator
  ) %>%
  summarize(
    perc_inneed = sum(inneed == "Yes", na.rm = TRUE) / n(),
    .groups = "drop"
  )

df_msna_sectoral <- df_sectoral %>%
  select(`Admin 1`:`Affected Population`) %>%
  right_join(
    df_msna_summarized,
    by = c(
      "Admin 2" = "adm2_name",
      "Population Group" = "pop_group"
    )
  ) %>%
  pivot_wider(
    names_from = indicator,
    values_from = perc_inneed
  )

df_msna <- df_sectoral %>%
  select(`Admin 1`:`Population Group`) %>%
  right_join(
    df_msna,
    by = c(
      "Admin 2" = "adm2_name",
      "Population Group" = "pop_group"
    )
  ) %>%
  type.convert()

df <- list(
  "Household Data" = df_msna,
  "% affected at Admin 2" = df_msna_sectoral
)

openxlsx::write.xlsx(
  df,
  file = file.path(
    data_dir,
    "Aggregated Files",
    "REACH_JMSNA.xlsx"
  )
)
