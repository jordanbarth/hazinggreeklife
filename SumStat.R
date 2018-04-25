minwage
unique(minwage$chain)
filter(minwage, chain == "wendys")
filter(minwage, chain == "burgerking") 
# filter(minwage, chain == "kfc") 
# filter(minwage, chain == "roys")
# unique(minwage$location)
# filter(minwage, location == "PA")
# # filter(minwage, location == "centralNJ")
# # filter(minwage, location == "northNJ")
# # filter(minwage, location == "shoreNJ")
# # filter(minwage, location == "southNJ")

tb2_cases <- filter(table2, type == "cases")$'count'
tb2_country <- filter(table2, type == "cases")$'country'
tb2_year <- filter(table2, type == "cases")$'year'
tb2_population <- filter(table2, type == "population")$'count'
table2_clean <- tibble(country = tb2_country,
                       year = tb2_year,
                       rate = tb2_cases / tb2_population)
table2_clean