library(readr)
library(dplyr)
library(readxl)
library(ggplot2)
library(lubridate)
library(maps)
library(ggmap)
library(mapdata)

# Import and Tidy

hazeglife <- read_excel("Desktop/hazinggreeklife/Hazing Database (updated).xlsx", 
                        col_types = c("text", "text", "text", "text", "date", 
                                      "text", "numeric", "text", "text", "date",
                                      "numeric", "numeric", "numeric", 
                                      "numeric"))
hazeglife
summary(hazeglife)

greekorg <- hazeglife$'Organization'
fratsoro <- hazeglife$'FraternitySorority'
colluniv <- hazeglife$'CollegeUniversity'	
collst <- hazeglife$'State'
startdate <- hazeglife$'Date'
month <- hazeglife$'Month'
yr <- hazeglife$'Year'
investsanc <- hazeglife$'Sanctioned'
ussanction <- hazeglife$'Resulting Action'
enddate <- hazeglife$'End Date'
camplat <- hazeglife$'CampusLat'
camplon <- hazeglife$'CampusLong'
hazeglife$Duration <- difftime(hazeglife$'End Date', hazeglife$Date, 
                               units = c("days"))
hazeglife$Duration
sancdur <- hazeglife$'Duration'
groupglife <- hazeglife$'Group'
orderglife <- hazeglife$'Order'

# Summary Statistics

# Number of fraternities and sororities
fratsororbreak <- hazeglife %>%
  count(FraternitySorority) %>%
  arrange(desc(n))
fratsororbreak
ggplot(data = fratsororbreak) +
  geom_bar(mapping = aes(x = FraternitySorority, y = n), stat = "identity")

# When were institutions sanctioning greek organizations the most for hazing?

ByYear <- hazeglife %>%
  count(Year) %>%
  arrange(desc(n))
ByYear
ggplot(data = ByYear) +
  geom_bar(mapping = aes(x = Year, y = n), stat = "identity")

# tibble for Summary Statistics - # of Sanctions by Organization
orgsanc <-hazeglife %>%
  count(Organization) %>%
  arrange(desc(n))
filter(orgsanc, n >= 14)
orgsanc

# Tibble for colleges with most sanctioned greek life

CollUniv_tib <- hazeglife %>%
  count(CollegeUniversity) %>%
  arrange(desc(n))
CollUniv_tib

# Which sanctions are most popular/likely?

sanctions <- hazeglife %>%
  count(`Resulting Action`) %>%
  arrange(desc(n))
sanctions

orgsaction <- tribble(
  ~sanction, ~noforgs,
  "cease and desist", 201,
  "Suspension", 171,
  "Social Probation", 63,
  "Revocation", 19,
  "Disciplinary Probation", 17
)
ggplot(data = orgsaction) +
  geom_bar(mapping = aes(x = sanction, y = noforgs), stat = "identity")

# mapping

usa <- map_data("usa") 
ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group), fill=NA, color="red") + 
  coord_fixed(1.3)

greekmap <- ggplot() + 
  geom_polygon(data = usa, aes(x=long, y = lat, group = group), fill = NA, color = "red") + 
  coord_fixed(1.3)
greekmap
labs <- data.frame(
  long = camplon,
  lat = camplat,
  stringsAsFactors = FALSE
)  
greekmap + 
  geom_point(data = labs, aes(x = long, y = lat), color = "black", size = 2)

# detailed map

greek_states <- map_data("state")
ggplot(data = greek_states) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = NA, color = "black") + 
  coord_fixed(1.3)
labs <- data.frame(
  long = camplon,
  lat = camplat,
  stringsAsFactors = FALSE
)  
greek_states + 
  geom_point(data = labs, aes(x = long, y = lat), color = "red", size = 2)
# gave me an error when trying to put together the nicer graph -- would you mind
# commenting with suggesstions on how to resolve?

# Also, wasn't able to figure out the chropleth map due to getting dates and duration
# of suspension. Any recommendations on how I should best go about it with my data set?
