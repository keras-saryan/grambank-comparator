#title: "Grambank Comparator Preprocessing"
#author: "Keras Saryan"

library("magrittr")
library("tidyverse")
library("arrow")
library("rnaturalearth")
library("rnaturalearthhires")
library("sf")
library("jsonlite")

grambank.parameters <- read.csv(
  "https://raw.githubusercontent.com/grambank/grambank/master/cldf/parameters.csv",
  header = TRUE,
  stringsAsFactors = FALSE,
  encoding = "UTF-8"
) %>%
  select(ID, Name, Description) %>%
  rename(
    Parameter_ID = ID,
    Parameter_Name = Name,
    Parameter_Description = Description
  )

grambank.codes <- read.csv(
  "https://raw.githubusercontent.com/grambank/grambank/master/cldf/codes.csv",
  header = TRUE,
  stringsAsFactors = FALSE,
  encoding = "UTF-8"
) %>%
  select(-ID) %>%
  rename(Parameter_Value = Name, Parameter_Value_Long = Description)

grambank.languages <- read.csv(
  "https://raw.githubusercontent.com/grambank/grambank/master/cldf/languages.csv",
  header = TRUE,
  stringsAsFactors = FALSE,
  encoding = "UTF-8"
) %>%
  select(ID, Name, Macroarea, Latitude, Longitude, Family_name, level) %>%
  rename(
    Language_ID = ID,
    Language_Name = Name,
    Language_Macroarea = Macroarea,
    Language_Latitude = Latitude,
    Language_Longitude = Longitude,
    Language_Family_Name = Family_name,
    Language_Level = level
  )

grambank.values <- read.csv(
  "https://raw.githubusercontent.com/grambank/grambank/master/cldf/values.csv",
  header = TRUE,
  stringsAsFactors = FALSE,
  encoding = "UTF-8"
) %>%
  select(Language_ID, Parameter_ID, Value, Comment) %>%
  rename(Parameter_Value = Value, Parameter_Comment = Comment)

#grambank.contributors <- read.csv("https://raw.githubusercontent.com/grambank/grambank/master/cldf/contributors.csv", header=TRUE, stringsAsFactors=FALSE, encoding="UTF-8")

#grambank.families <- read.csv("https://raw.githubusercontent.com/grambank/grambank/master/cldf/families.csv", header=TRUE, stringsAsFactors=FALSE, encoding="UTF-8")

grambank_coordinates <- grambank.languages %>%
  select(Language_ID, Language_Latitude, Language_Longitude) %>%
  rename(
    ID = Language_ID,
    Latitude = Language_Latitude,
    Longitude = Language_Longitude
  )

grambank_languages <- right_join(
  grambank.languages,
  grambank.values,
  by = "Language_ID"
)

grambank_parameters <- right_join(
  grambank.parameters,
  grambank.codes,
  by = "Parameter_ID"
) %>%
  mutate(Parameter_Value = as.character(Parameter_Value))

grambank_parameters_slim <- grambank_parameters %>%
  select(-Parameter_Value, -Parameter_Value_Long)

grambank <- left_join(
  grambank_languages,
  grambank_parameters_slim %>% distinct(Parameter_ID, .keep_all = TRUE),
  by = "Parameter_ID"
)

grambank <- full_join(
  grambank,
  grambank_parameters,
  by = c("Parameter_ID", "Parameter_Value")
)

# grambank_parameter_categories <- read_tsv("grambank_categories_simple.tsv")
#
# grambank_parameter_categories %<>%
#   left_join(grambank.parameters, by = "Parameter_ID") %>%
#   select(Parameter_ID, Parameter_Name, Parameter_Category, Parameter_Subcategory)
#
# write.table(grambank_parameter_categories, "grambank_parameter_categories.tsv", quote = FALSE, sep = "\t", row.names = FALSE, col.names = TRUE, fileEncoding = "UTF-8")

rm(
  grambank.languages,
  grambank.values,
  grambank.parameters,
  grambank.codes,
  grambank_languages,
  grambank_parameters,
  grambank_parameters_slim,
  grambank_parameter_categories
)

grambank %<>%
  mutate(
    Parameter_Value_Long = case_when(
      Parameter_Value_Long == "both." ~ "both",
      TRUE ~ Parameter_Value_Long
    )
  )

grambank_wide <- grambank %>%
  select(
    Language_ID,
    Language_Name,
    Language_Macroarea,
    Language_Family_Name,
    Language_Level,
    Parameter_ID,
    Parameter_Value_Long
  ) %>%
  pivot_wider(names_from = Parameter_ID, values_from = Parameter_Value_Long) %>%
  rowwise() %>%
  mutate(Parameters_Coded = sum(!is.na(c_across(GB020:GB522)))) %>%
  ungroup() %>%
  relocate(
    Language_ID,
    Language_Name,
    Language_Macroarea,
    Language_Family_Name,
    Language_Level,
    Parameters_Coded
  )

rm(grambank)

grambank_wide %>%
  group_by(Language_Macroarea) %>%
  write_dataset(path = "grambank_wide", format = "parquet")

un_m49_json <- fromJSON(
  "https://raw.githubusercontent.com/lukert33/united-nations-geoscheme-subregions-json/refs/heads/master/un-geoscheme-subregions-countries.json",
  flatten = TRUE
)

un_m49_df <- bind_rows(
  lapply(names(un_m49_json), function(continent) {
    subregions <- un_m49_json[[continent]]
    bind_rows(
      lapply(names(subregions), function(subregion) {
        countries_raw <- subregions[[subregion]]
        countries_clean <- countries_raw %>%
          gsub("^c\\(|\\)$", "", .) %>%
          str_split(",") %>%
          unlist() %>%
          str_replace_all('["]', "") %>%
          str_trim()
        tibble(
          continent = continent,
          subregion = subregion,
          country = countries_clean
        )
      })
    )
  })
) %>%
  filter(country != "NULL") %>%
  mutate(
    country = case_when(
      country == "Bolivia (Plurinational State of)" ~ "Bolivia",
      country == "Brunei Darussalam" ~ "Brunei",
      country == "Cote d'Ivoire" ~ "Côte d'Ivoire",
      country == "Czech Republic" ~ "Czechia",
      country == "Democratic Republic of the Congo" ~ "DR Congo",
      country == "Iran (Islamic Republic of)" ~ "Iran",
      country == "Lao People's Democratic Republic" ~ "Laos",
      country == "Micronesia (Federated States of)" ~ "Micronesia",
      country == "The former Yugoslav Republic of Macedonia" ~ "North Macedonia",
      country == "Russian Federation" ~ "Russia",
      country == "Democratic People's Republic of Korea" ~ "South Korea",
      country == "Syrian Arab Republic" ~ "Syria",
      country == "United Republic of Tanzania" ~ "Tanzania",
      country == "United Kingdom of Great Britain and Northern Ireland" ~ "United Kingdom",
      country == "Venezuela (Bolivarian Republic of)" ~ "Venezuela",
      country == "Viet Nam" ~ "Vietnam",
      TRUE ~ country
    )
  ) %>%
  unique()

rm(un_m49_json)

grambank_sf <- grambank_coordinates %>%
  filter(!is.na(Latitude), !is.na(Longitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

sf::sf_use_s2(FALSE)

countries <- ne_countries(scale = "large", returnclass = "sf")

nearest_idx <- st_nearest_feature(grambank_sf, countries)

grambank_map <- grambank_sf %>%
  mutate(
    country = countries$name[nearest_idx],
    country = case_when(
      country == "Bosnia and Herz." ~ "Bosnia and Herzegovina",
      country == "Central African Rep." ~ "Central African Republic",
      country == "Cook Is." ~ "Cook Islands",
      country == "Cyprus U.N. Buffer Zone" ~ "Cyprus",
      country == "Dem. Rep. Congo" ~ "DR Congo",
      country == "Eq. Guinea" ~ "Equatorial Guinea",
      country == "Faeroe Is." ~ "Faeroe Islands",
      country == "Fr. Polynesia" ~ "French Polynesia",
      country == "Indian Ocean Ter." ~ "Indian Ocean Territory",
      country == "Marshall Is." ~ "Marshall Islands",
      country == "N. Cyprus" ~ "Cyprus",
      country == "N. Mariana Is." ~ "Northern Mariana Islands",
      country == "S. Sudan" ~ "South Sudan",
      country == "Solomon Is." ~ "Solomon Islands",
      country == "Wallis and Futuna Is." ~ "Wallis and Futuna Islands",
      TRUE ~ country
    )
  ) %>%
  unique()

rm(countries, nearest_idx)

grambank_map %<>%
  left_join(un_m49_df, by = "country") %>%
  mutate(
    continent = case_when(
      country == "Kosovo" ~ "EU",
      country == "Taiwan" ~ "AS",
      country == "Tajikistan" ~ "AS",
      country == "Turkmenistan" ~ "AS",
      country == "Uzbekistan" ~ "AS",
      TRUE ~ continent
    ),
    subregion = case_when(
      country == "Kosovo" ~ "Southern Europe",
      country == "Taiwan" ~ "Eastern Asia",
      country == "Tajikistan" ~ "Southern Asia",
      country == "Uzbekistan" ~ "Southern Asia",
      TRUE ~ subregion
    ),
    country = case_when(
      ID == "coco1260" ~ "Australia",
      TRUE ~ country
    ),
    subregion = case_when(
      ID == "coco1260" ~ "Australia and New Zealand",
      TRUE ~ subregion
    ),
    continent = case_when(
      ID == "coco1260" ~ "OC",
      TRUE ~ continent
    )
  ) %>%
  select(ID, geometry, country, subregion, continent) %>%
  rename(
    Geometry = geometry,
    Country = country,
    Subregion = subregion,
    Continent = continent
  )

grambank_map %<>%
  st_drop_geometry()

grambank_coordinates <- full_join(
  grambank_coordinates,
  grambank_map,
  by = "ID"
) %>%
  filter(!is.na(Continent)) %>%
  select(-Country) %>%
  unique()

rm(grambank_map, grambank_sf, grambank_wide, un_m49_df)

grambank_coordinates %>%
  group_by(Continent) %>%
  write_dataset(path = "grambank_coordinates", format = "parquet")

rm(grambank_coordinates)
