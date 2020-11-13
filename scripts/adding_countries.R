# Preparing the Beer Data
# The data was produced for a study by goeurop (no omio.de) from 2015

library(dplyr)
library(readr)
library(maps) #needed for loading cities and countrys
library(stringr)

beer_data <- read_delim("raw-data/word_wide_beer_prices.csv", delim = ";" , locale = locale(encoding = 'ISO-8859-2')) %>% 
  mutate(year=2015) # the data was produced in 2015

# loading the city/country data
wc_data <- force(world.cities) %>% 
  select(c(name, country.etc, pop)) %>% # only interested in name (city), country and popuation
  rename("CITY" = "name") %>% 
  rename("country" = "country.etc")
  
  
# changing letters for joining 
beer_data$CITY <- str_replace(beer_data$CITY, "ó", "o")
beer_data$CITY <- str_replace(beer_data$CITY, "á", "a")

# Join of the two datasets
beer_data_2 <- left_join(beer_data, wc_data)

# Some city names are not unique. Keeping only those with the highest population
# Lösung von https://stackoverflow.com/questions/24011246/deleting-rows-that-are-duplicated-in-one-column-based-on-the-conditions-of-anoth
beer_data_2 <- arrange(beer_data_2, CITY, -pop) 
beer_data_2 <- beer_data_2[!duplicated(beer_data_2$CITY),]

# Some adjustment by hand
beer_data_2 <- beer_data_2 %>% 
  mutate(country = case_when(CITY=="Bali" ~ "Indonesia",
                              CITY=="Belgrade" ~ "Serbia",
                              CITY=="Hong Kong" ~ "Hong Kong",
                              CITY=="Krakow" ~ "Poland",
                              CITY=="Luxembourg" ~ "Luxembourg",
                              CITY=="Seville" ~ "Spain",
                              CITY=="Tel Aviv" ~ "Israel",
                              CITY=="The Hague" ~ "Netherland",
                              TRUE ~ (country)))

# save
write.csv2(beer_data_2, "processed-data/wwbp_with_contries.csv")
