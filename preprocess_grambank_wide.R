#title: "Grambank Comparator Preprocessing"
#author: "Keras Saryan"

library("magrittr")
library("tidyverse")
library("arrow")

grambank.parameters <- read.csv("https://raw.githubusercontent.com/grambank/grambank/master/cldf/parameters.csv", header=TRUE, stringsAsFactors=FALSE, encoding="UTF-8") %>%
  select(ID, Name, Description) %>%
  rename(Parameter_ID = ID, Parameter_Name = Name, Parameter_Description = Description)

grambank.codes <- read.csv("https://raw.githubusercontent.com/grambank/grambank/master/cldf/codes.csv", header=TRUE, stringsAsFactors=FALSE, encoding="UTF-8") %>%
  select(-ID) %>%
  rename(Parameter_Value = Name, Parameter_Value_Long = Description)

grambank.languages <- read.csv("https://raw.githubusercontent.com/grambank/grambank/master/cldf/languages.csv", header=TRUE, stringsAsFactors=FALSE, encoding="UTF-8") %>%
  select(ID, Name, Macroarea, Latitude, Longitude, Family_name, level) %>%
  rename(Language_ID = ID, Language_Name = Name, Language_Macroarea = Macroarea, Language_Latitude = Latitude, Language_Longitude = Longitude, Language_Family_Name = Family_name, Language_Level = level)

grambank.values <- read.csv("https://raw.githubusercontent.com/grambank/grambank/master/cldf/values.csv", header=TRUE, stringsAsFactors=FALSE, encoding="UTF-8") %>%
  select(Language_ID, Parameter_ID, Value, Comment) %>%
  rename(Parameter_Value = Value, Parameter_Comment = Comment)

#grambank.contributors <- read.csv("https://raw.githubusercontent.com/grambank/grambank/master/cldf/contributors.csv", header=TRUE, stringsAsFactors=FALSE, encoding="UTF-8")

#grambank.families <- read.csv("https://raw.githubusercontent.com/grambank/grambank/master/cldf/families.csv", header=TRUE, stringsAsFactors=FALSE, encoding="UTF-8")

grambank_coordinates <- grambank.languages %>%
  select(Language_ID, Language_Latitude, Language_Longitude) %>%
  rename(ID = Language_ID, Latitude = Language_Latitude, Longitude = Language_Longitude)

grambank_coordinates %>%
  write_dataset(path = "grambank_coordinates", format = "parquet")

grambank_languages <- right_join(grambank.languages, grambank.values, by = "Language_ID")

grambank_parameters <- right_join(grambank.parameters, grambank.codes, by = "Parameter_ID") %>%
  mutate(Parameter_Value = as.character(Parameter_Value))

grambank_parameters_slim <- grambank_parameters %>%
  select(-Parameter_Value, -Parameter_Value_Long)

grambank <- left_join(grambank_languages, grambank_parameters_slim %>% distinct(Parameter_ID, .keep_all = TRUE), by = "Parameter_ID")

grambank <- full_join(grambank, grambank_parameters, by = c("Parameter_ID", "Parameter_Value"))

rm(grambank.languages, grambank.values, grambank.parameters, grambank.codes, grambank_languages, grambank_parameters, grambank_parameters_slim)

grambank %<>%
  mutate(Parameter_Value_Long = case_when(
    Parameter_Value_Long == "both." ~ "both",
    TRUE ~ Parameter_Value_Long))

grambank_wide <- grambank %>%
  select(Language_ID, Language_Name, Language_Macroarea, Language_Family_Name, Language_Level, Parameter_ID, Parameter_Value_Long) %>%
  pivot_wider(names_from = Parameter_ID, values_from = Parameter_Value_Long) %>%
  rowwise() %>%
  mutate(Parameters_Coded = sum(!is.na(c_across(-Language_ID)))) %>%
  ungroup() %>%
  relocate(Language_ID, Language_Name, Language_Macroarea, Language_Family_Name, Language_Level, Parameters_Coded)

rm(grambank)

grambank_wide %>%
  group_by(Language_Macroarea) %>%
  write_dataset(path = "grambank_wide", format = "parquet")
