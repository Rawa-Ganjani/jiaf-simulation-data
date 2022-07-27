library(tidyverse)
library(janitor)
library(readxl)
library(openxlsx)
library(sf)

data_dir <- Sys.getenv("JIAF_SIMU_DATA_DIR")

df_dsa <- read_excel(
  file.path(
    data_dir,
    "Raw Data",
    "Quantitative Data",
    "reach_som_dataset_dsa_somalia_february-2022.xlsx"
  ),
  sheet = "Settlement level dataset"
)

df_questions <- read_excel(
  file.path(
    data_dir,
    "Raw Data",
    "Quantitative Data",
    "reach_som_dataset_dsa_somalia_february-2022.xlsx"
  ),
  sheet = "Survey questions"
) %>%
  type.convert() %>%
  clean_names() %>%
  select(
    question_code = code,
    type,
    question_name = name,
    question_label = label_english_en
  )

df_choices <- read_excel(
  file.path(
    data_dir,
    "Raw Data",
    "Quantitative Data",
    "reach_som_dataset_dsa_somalia_february-2022.xlsx"
  ),
  sheet = "Survey choices"
) %>%
  type.convert() %>%
  clean_names() %>%
  select(
    choice_code = code,
    list_name,
    choice_name = name,
    choice_label = label_english_en
  )

df_questionnaire <- df_questions %>%
  mutate(
    is_multiple = grepl("select_multiple", type),
    list_name = ifelse(
      grepl("select", type),
      gsub("select_(one|multiple) ", "", type),
      ""
    )
  ) %>%
  filter(
    !list_name %in% c("region", "district")
  ) %>%
  left_join(
    df_choices
  )

df <- df_dsa %>%
  pivot_longer(
    cols = -c(
      localisation_region_label:district,
      contains("cccm_populationestimates")
    ),
    names_to = "Question",
    values_to = "Response",
    values_transform = as.character
  ) %>%
  filter(
    !Question %in% unique(df_questionnaire$question_name[
      df_questionnaire$is_multiple
    ]),
    !Question %in% unique(df_questionnaire$question_name[
      df_questionnaire$type == "calculate"
    ]),
    !Question %in% c("cccm_idps_origin_first", "cccm_district_origin_first")
  ) %>%
  group_by(
    across(-contains("cccm_populationestimates"))
  ) %>%
  summarize(
    across(
      .cols = contains("cccm_populationestimates"),
      .fns = ~ sum(., na.rm = TRUE)
    ),
    frequency = n(),
    .groups = "drop"
  ) %>%
  filter(
    !is.na(Response)
  ) %>%
  separate(
    Question,
    sep = "[.]",
    into = c("Question", "Choice"),
    fill = "right",
    extra = "merge"
  ) %>%
  filter(
    !(!is.na(Choice) & Response == "0"),
    Response != "NC"
  ) %>%
  mutate(
    Choice = ifelse(is.na(Choice), Response, Choice)
  ) %>%
  left_join(
    df_questionnaire,
    by = c(
      "Question" = "question_name",
      "Choice" = "choice_name"
    )
  ) %>%
  arrange(
    localisation_region,
    district,
    question_code,
    choice_code
  ) %>%
  transmute(
    `Admin 1` = localisation_region_label,
    `Admin 1 Pcode` = localisation_region,
    `Admin 2` = localisation_district_label,
    `Admin 2 Pcode` = district,
    Question = ifelse(
      is.na(question_label),
      df_questionnaire$question_label[match(
        Question,
        df_questionnaire$question_name
      )],
      question_label
    ),
    Response = ifelse(
      is.na(choice_label),
      Response,
      choice_label
    ),
    `# of responses` = frequency,
    `# of shelters affected` = cccm_populationestimates_shelters,
    `# of families affected` = cccm_populationestimates_families,
    `# of people affected` = cccm_populationestimates_individuals
  )

wb <- loadWorkbook(
  file.path(
    data_dir,
    "Raw Data",
    "Quantitative Data",
    "reach_som_dataset_dsa_somalia_february-2022.xlsx"
  )
)

removeWorksheet(wb, "Aggregation process")
addWorksheet(
  wb = wb,
  sheetName = "Aggregated at admin 2"
)

writeData(
  wb,
  sheet = "Aggregated at admin 2",
  startCol = 1,
  startRow = 1,
  x = df,
  col.names = TRUE
)

saveWorkbook(wb,
  file = file.path(
    data_dir,
    "Aggregated Files",
    "REACH-DSA.xlsx"
  )
)
