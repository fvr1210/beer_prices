# Preparing the Beer Data
# The data was produced for a study by goeurop (now omio.de) from 2015

library(dplyr)
library(readr)
library(maps) #needed for loading cities and countrys
library(stringr)

bd <- read_delim("raw-data/world_wide_beer_prices.csv", delim = ";" , locale = locale(encoding = 'ISO-8859-2')) %>% 
  mutate(year=2015) # the data was produced in 2015

# loading the city/country data
wc_data <- force(world.cities) %>% 
  select(c(name, country.etc, pop)) %>% # only interested in name (city), country and popuation
  rename("CITY" = "name") %>% 
  rename("country" = "country.etc")
  
  
# changing letters for joining 
bd$CITY <- str_replace(bd$CITY, "ó", "o")
bd$CITY <- str_replace(bd$CITY, "á", "a")

# Join of the two datasets
bd_2 <- left_join(bd, wc_data)

# Some city names are not unique. Keeping only those with the highest population
# Lösung von https://stackoverflow.com/questions/24011246/deleting-rows-that-are-duplicated-in-one-column-based-on-the-conditions-of-anoth
bd_2 <- arrange(bd_2, CITY, -pop) 
bd_2 <- bd_2[!duplicated(bd_2$CITY),]

# Some adjustment by hand
bd_2 <- bd_2 %>% 
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

bd_2$continent <- countrycode(sourcevar = bd_2[["country"]],
                            origin = "country.name",
                            destination = "continent")

bd_2$region <- countrycode(sourcevar = bd_2[["country"]],
                                     origin = "country.name",
                                     destination = "region")

bd_2$code <- countrycode(sourcevar = bd_2[["country"]],
                                  origin = "country.name",
                                  destination = "iso3c")

bd_2$cc <- paste(bd_2$CITY, " (", bd_2$code, ")", sep = "")


# calculating the ration bar prices to supermarkt prices 
bd_2$markup <- bd_2$`BAR PRICE $`/bd_2$`AVERAGE SUPERMARKET PRICE $`



# calculating prices on country and region level (https://stackoverflow.com/questions/11562656/calculate-the-mean-by-group)
bdc <- bd_2 %>% group_by(country) %>% 
  mutate(`AVERAGE SUPERMARKET PRICE $` = round(mean(`AVERAGE SUPERMARKET PRICE $`), 2)) %>% 
  mutate(`BAR PRICE $` = round(mean(`BAR PRICE $`), 2)) %>% 
  mutate(`OVERALL PRICE $` = round(mean(`OVERALL PRICE $`), 2)) %>% 
  distinct (country, .keep_all = T) %>%  
  ungroup %>% 
  select(-c(CITY))

bdr <-  bd_2 %>% group_by(region) %>% 
  mutate(`AVERAGE SUPERMARKET PRICE $` = round(mean(`AVERAGE SUPERMARKET PRICE $`), 2)) %>% 
  mutate(`BAR PRICE $` = round(mean(`BAR PRICE $`), 2)) %>% 
  mutate(`OVERALL PRICE $` = round(mean(`OVERALL PRICE $`), 2)) %>% 
  distinct (region, .keep_all = T) %>%  
  ungroup %>% 
  select(-c(CITY, country))


# save
write_delim(bd_2, "processed-data/wwbp_city_add_var.csv", delim = ";") #city level
write_delim(bdc, "processed-data/wwbp_country_add_var.csv", delim = ";") #country level
write_delim(bdr, "processed-data/wwbp_region_add_var.csv", delim = ";") #region level