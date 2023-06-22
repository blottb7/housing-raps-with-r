# Libraries
library(dplyr)
library(purrr)
library(readxl)
library(stringr)
library(janitor)

# set working directory
# setwd("C://Users//Ben//Documents//projects//raps_with_r//housing//")

# The url below points to an Excel file
# hosted on the book’s github repository
url <- "https://is.gd/1vvBAc"

raw_data <- tempfile(fileext = ".xlsx")

download.file(url, raw_data,
              method = "auto",
              mode = "wb")

sheets <- excel_sheets(raw_data)

read_clean <- function(..., sheet){
  read_excel(..., sheet = sheet) |>
    mutate(year = sheet)
}

raw_data <- map(
  sheets,
  ~read_clean(raw_data,
              skip = 10,
              sheet = .)
                   ) |>
  bind_rows() |>
  clean_names()

raw_data <- raw_data |>
  rename(
    locality = commune,
    n_offers = nombre_doffres,
    average_price_nominal_euros = prix_moyen_annonce_en_courant,
    average_price_m2_nominal_euros = prix_moyen_annonce_au_m2_en_courant,
    average_price_m2_nominal_euros = prix_moyen_annonce_au_m2_en_courant
  ) |>
  mutate(locality = str_trim(locality)) |>
  select(year, locality, n_offers, starts_with("average"))

# Clean for foreign letters
raw_data <- raw_data |>
    mutate(
        locality = ifelse(grepl("Luxembourg-Ville", locality),
                          "Luxembourg",
                          locality),
        locality = ifelse(grepl("P.tange", locality),
                          "Pétange",
                          locality)
    ) |>
    mutate(across(starts_with("average"),
                  as.numeric))

# Remove rows stating sources
raw_data <- raw_data |>
    filter(!grepl("Source", locality))

# Keep commune level data
commune_level_data <- raw_data |>
    filter(!grepl("nationale|offres", locality),
           !is.na(locality))

# Create data set with national level data
country_level <- raw_data |>
    filter(grepl("nationale", locality)) |>
    select(-n_offers)

offers_country <- raw_data |>
    filter(grepl("Total d.offres", locality)) |>
    select(year, n_offers)

country_level_data <- full_join(country_level, offers_country) |>
    select(year, locality, n_offers, everything()) |>
    mutate(locality = "Grand-Duchy of Luxembourg")

# Scrape and save a list of all possible communes
current_communes <- "https://w.wiki/6nPu" |>
    rvest::read_html() |>
    rvest::html_table() |>
    purrr::pluck(1) |>
    janitor::clean_names()

# Check if all communes are present in our data
setdiff(unique(commune_level_data$locality),
        current_communes$commune)

# Harmonize previous and current commune spellings and get current communes
former_communes <- "https://w.wiki/_wFe7" |>
    rvest::read_html() |>
    rvest::html_table() |>
    purrr::pluck(3) |>
    janitor::clean_names() |>
    dplyr::filter(year_dissolved > 2009)

former_communes

# Combine current and former communes and harmoize their names
communes <- unique(c(former_communes$name,
                     current_communes$commune))

# Different spelling of these communes between wikipedia and the data
communes[which(communes == "Clemency")] <- "Clémency"
communes[which(communes == "Redange")] <- "Redange-sur-Attert"
communes[which(communes == "Erpeldange-sur-Sûre")] <- "Erpeldange"
communes[which(communes == "Luxembourg-City")] <- "Luxembourg"
communes[which(communes == "Käerjeng")] <- "Kaerjeng"
communes[which(communes == "Petange")] <- "Pétange"

# Retest if all communes are present
setdiff(unique(commune_level_data$locality),
        communes)

# save the data (uncomment if you need to save)
# you may need to create the `datasets` folder first
write.csv(commune_level_data, "datasets/commune_level_data.csv", row.names = TRUE)
write.csv(country_level_data, "datasets/country_level_data.csv", row.names = TRUE)
