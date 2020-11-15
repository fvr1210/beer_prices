# Preparing the Beer Data
# The data was produced for a study by goeurop (now omio.de) from 2015

library(dplyr)
library(readr)
library(maps) #needed for loading cities and countrys
library(stringr)

beer_data <- read_delim("raw-data/world_wide_beer_prices.csv", delim = ";" , locale = locale(encoding = 'ISO-8859-2')) %>% 
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
                              CITY=="The Hague" ~ "Netherlands",
                              TRUE ~ (country)))


# adding continents
# https://stackoverflow.com/questions/47510141/get-continent-name-from-country-name-in-r
library(countrycode)

# America is one continent in the function countrycode therefore it is maybe better to use region, however ther central Asia and Europ are combined...

beer_data_2$continent <- countrycode(sourcevar = beer_data_2[["country"]],
                            origin = "country.name",
                            destination = "continent")

beer_data_2$region <- countrycode(sourcevar = beer_data_2[["country"]],
                                     origin = "country.name",
                                     destination = "region")

beer_data_2$code <- countrycode(sourcevar = beer_data_2[["country"]],
                                  origin = "country.name",
                                  destination = "iso3c")

beer_data_2$cc <- paste(beer_data_2$CITY, " (", beer_data_2$code, ")", sep = "")


# calculating the ration bar prices to supermarkt prices 
beer_data_2$markup <- beer_data_2$`BAR PRICE $`/beer_data_2$`AVERAGE SUPERMARKET PRICE $`






# save
write_delim(beer_data_2, "processed-data/wwbp_add_var.csv", delim = ";")
