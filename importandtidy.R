library(readr)
library(dplyr)
library(readxl)
library(ggplot2)

hazeglife <- read_excel("~/Desktop/hazinggreeklife/Hazing Database 4.25 presentation.xlsx")
hazeglife
summary(hazeglife)

hazeglifeorg <- hazeglife$'Organization'

unique(minwage$chain)
unique(hazeglife$`Resulting Action`)
filter(hazeglife, Organization == "Sigma Alpha Epsilon")
filter(minwage, chain == "burgerking") 
# filter(minwage, chain == "kfc") 
# filter(minwage, chain == "roys")
# unique(minwage$location)
# filter(minwage, location == "PA")
# # filter(minwage, location == "centralNJ")
# # filter(minwage, location == "northNJ")
# # filter(minwage, location == "shoreNJ")
# # filter(minwage, location == "southNJ")
# tb2_cases <- filter(table2, type == "cases")$'count'
# tb2_country <- filter(table2, type == "cases")$'country'
# tb2_year <- filter(table2, type == "cases")$'year'
# tb2_population <- filter(table2, type == "population")$'count'
# table2_clean <- tibble(country = tb2_country,
# year = tb2_year,
# rate = tb2_cases / tb2_population)
# table2_clean

orgsyr <- tribble(
  ~year, ~noforgs,
  "2015", 16,
  "2016", 4,
  "2017", 239,
  "2018", 54
)
orgsaction <- tribble(
  ~sanction, ~noforgs,
  "cease and desist", 16,
  "disciplinary probation", 5,
  "revocation", 7,
  "social probation", 46,
  "suspension", 239
)
orgssus <- tribble(
  ~organization, ~suslength,
  "Pi Kappa Alpha", 307.75,
  "Sigma Alpha Epsilon", 287.25,
  "Theta Kappa Epsilon", 146.5,
  "Alpha Phi Alpha", 132,
  "Theta Delta Chi", 125
)
ggplot(data = orgssus) +
  geom_bar(mapping = aes(x = organization, y = suslength), stat = "identity")
ggplot(data = orgsaction) +
  geom_bar(mapping = aes(x = sanction, y = noforgs), stat = "identity")
ggplot(data = orgsyr) +
  geom_bar(mapping = aes(x = year, y = noforgs), stat = "identity")

orgscamp <- tribble(
  ~institution, ~norgs,
  "FSU", 52,
  "PSU", 45,
  "OSU", 36,
  "IU", 34,
  "TSU", 30
)
ggplot(data = orgscamp) +
  geom_bar(mapping = aes(x = institution, y = norgs), stat = "identity")