library(readr)
library(dplyr)
library(readxl)
library(ggplot2)
library(lubridate)
library(maps)
library(ggmap)
library(mapdata)
library(stringr)

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
states <- map_data("state")
greekstates <- ggplot() + 
  geom_polygon(data = states, aes(x=long, y = lat, group = group), fill = NA, color = "black") + 
  coord_fixed(1.3)
greekstates
labs <- data.frame(
  long = camplon,
  lat = camplat,
  stringsAsFactors = FALSE
)  
greekstates + 
  geom_point(data = labs, aes(x = long, y = lat), color = "red", size = 2)

#breaking down into regions

northeast <- subset(states, region %in% c("california", "oregon", "washington"))
midatlantic <- subset(states, region %in% c("california", "oregon", "washington"))
south <- subset(states, region %in% c("california", "oregon", "washington"))
midwest <- subset(states, region %in% c("california", "oregon", "washington"))
west <- subset(states, region %in% c("california", "oregon", "washington"))
northwest <- subset(states, region %in% c("california", "oregon", "washington"))

Getting the california data is easy:
  
ca_df <- subset(states, region == "california")
head(ca_df)

Now, let’s also get the county lines there

counties <- map_data("county")
ca_county <- subset(counties, region == "california")

head(ca_county)

#Plot the state first but let’s ditch the axes gridlines, and gray background by using the super-wonderful theme_nothing().

ca_base <- ggplot(data = ca_df, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "gray")
ca_base + theme_nothing()

# Now plot the county boundaries in white:
  
ca_base + theme_nothing() + 
  geom_polygon(data = ca_county, fill = NA, color = "white") +
  geom_polygon(color = "black", fill = NA)  # get the state border back on top

#Get some facts about the counties

# make a data frame
x <- readLines("data/ca-counties-wikipedia.txt")
pop_and_area <- str_match(x, "^([a-zA-Z ]+)County\t.*\t([0-9,]{2,10})\t([0-9,]{2,10}) sq mi$")[, -1] %>%
  na.omit() %>%
  str_replace_all(",", "") %>% 
  str_trim() %>%
  tolower() %>%
  as.data.frame(stringsAsFactors = FALSE)

# give names and make population and area numeric
names(pop_and_area) <- c("subregion", "population", "area")
pop_and_area$population <- as.numeric(pop_and_area$population)
pop_and_area$area <- as.numeric(pop_and_area$area)

head(pop_and_area)

We now have the numbers that we want, but we need to attach those to every point on polygons of the counties. This is a job for inner_join from the dplyr package

cacopa <- inner_join(ca_county, pop_and_area, by = "subregion")

And finally, add a column of people_per_mile:
  
  cacopa$people_per_mile <- cacopa$population / cacopa$area

head(cacopa)

Now plot population density by county

# prepare to drop the axes and ticks but leave the guides and legends
# We can't just throw down a theme_nothing()!
  ditch_the_axes <- theme(
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank()
  )

elbow_room1 <- ca_base + 
  geom_polygon(data = cacopa, aes(fill = people_per_mile), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  theme_bw() +
  ditch_the_axes
elbow_room1 

# This is a job for a scale transformation. Let’s take the log-base-10 of the population density.

elbow_room1 + scale_fill_gradient(trans = "log10")
