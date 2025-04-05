library(pxweb)
library(dplyr)
# Установка рабочей директории
setwd("C:/R")
# ---- Reading from px-axis
#--- pxweb_interactive() -- to fetch 
pxweb_query_list <- list(
  "Maakunta" = c("SSS"),  # Выбор всех регионов (если нужно)
  "Kieli" = "*",           # Выбор всех языков
  "Vuosi" = "*",           # Выбор всех годов
  "Tiedot" = c("vaesto")   # Выбор данных о населении
)

# Download data
px_data1 <- pxweb_get_data(
  url = "https://statfin.stat.fi/PXWeb/api/v1/en/StatFin/vaerak/statfin_vaerak_pxt_11rl.px",
  query = pxweb_query_list
)

# Filter and arrange data
tulos <- px_data1 %>%
  select(-Region) %>%
  filter(!Language %in% c(
    "TOTAL", "Total", "Other language", "DOMESTIC LANGUAGES, TOTAL",
    "FOREIGN LANGUAGES, TOTAL", "NATIONAL LANGUAGES, TOTAL",
    "Finnish", "Swedish", "Unknown", "Sami"
  )) %>%
  mutate(
    Year = as.integer(Year),
    Language = factor(Language)
  ) %>%
  arrange(desc(`Population 31 Dec`), desc(Year)) %>%
  group_by(Year) %>%
  mutate(Rank = as.integer(rank(-`Population 31 Dec`))) %>%
  select(Rank, everything())

# Save and test the result
saveRDS(tulos, "..//R//tulos.rds")
test <- readRDS("..//R//tulos.rds")
identical(tulos, test)