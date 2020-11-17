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


# The beer consumption per Capita data seems not soo trust worthy... vietnam for example has a really high consumption (higher than germany). 
# other Data on the internet don't show this. Data from the Global Health Observatory (GHO) is added (no data for Hong-Kong)
bc_gho <- read_delim("raw-data/alcahol_consumption_capita_gho.csv", delim = ";" , locale = locale(encoding = 'ISO-8859-2')) %>% 
  select(c(`GHO (DISPLAY)`, `YEAR (DISPLAY)`, `COUNTRY (CODE)`, `COUNTRY (DISPLAY)`, `ALCOHOLTYPE (DISPLAY)`, `Display Value`)) %>% 
  filter(`ALCOHOLTYPE (DISPLAY)`=="Beer") %>%  # select only beer values
  rename("Description"=`GHO (DISPLAY)`, "year"=`YEAR (DISPLAY)`, "code" = `COUNTRY (CODE)`, "country" =`COUNTRY (DISPLAY)`, "Alcoholtype"=`ALCOHOLTYPE (DISPLAY)`, "Value_GHO" = `Display Value`) %>% 
  spread(year, Value_GHO) #long to wide format


# left_join with goEurop data
bd_2_gho <- left_join(bd_2, bc_gho, by="code") 


# Scraping data from wikipedia
library(XML)
library(rvest)
library(robotstxt)

# Testen ob scrapping erlaubt ist ----
paths_allowed(
  paths = c("https://en.wikipedia.org/wiki/List_of_countries_by_beer_consumption_per_capita")
)

#Ist erlaubt

url <- "https://en.wikipedia.org/wiki/List_of_countries_by_beer_consumption_per_capita"


# Scraping der Wikipediaseite über Schweizergemeinden (Stand:2019) ----
beer.table = url %>%
  read_html() %>%
  html_node(xpath = '//*[@id="mw-content-text"]/div/table[1]') %>%
  html_table(fill = TRUE) 

# has a lot of empty or not usefull cells, usfull infromation from row 63 to row 123 and from the first to the sixth colum
beer.table <- beer.table[63:123,] 
beer.table <- beer.table[,1:6] 
names(beer.table) <- beer.table[1,]
bd_w <- beer.table

bd_w$code <- countrycode(sourcevar = bd_w[["Country"]],
                         origin = "country.name",
                         destination = "iso3c")
 
# leftjoin with dataset from goEurop and GHO, Wiki has no data for United Arab Emirates, Paraguay, Greece, Colombia, Argentina, 
# Egypt, Ukraine, Peru, Luxembourg, Philippines, Russia, Norway, Iceland, Chile, Sweden
bd_2_gho_w <- left_join(bd_2_gho, bd_w, by="code") 


# add rank based on the diffrent values
bd_2_gho_w$r_ge <- rank(-bd_2_gho_w$`BEER PER CAPITA/YEAR (LITERS)`, ties.method = "max")
bd_2_gho_w$r_gho <- rank(-bd_2_gho_w$`2015`, ties.method = "max") # for the GHO Data I chose the data from year 2015
bd_2_gho_w$r_wiki <- rank(-as.numeric(bd_2_gho_w$`Consumptionper capita[1](litres per year)`), ties.method = "max")

# cleaning and renaming the bd_2_gho_w dataset to final dataset
df_f <- bd_2_gho_w %>% 
  rename("city" = "CITY",  "asmp" = `AVERAGE SUPERMARKET PRICE $`, "abp" = `BAR PRICE $`, "aop" =`OVERALL PRICE $`, 
         "bpc_ge" = `BEER PER CAPITA/YEAR (LITERS)`, "country" = country.x, 
         "bpc_gho" = `2015`, "bpc_wiki" = `Consumptionper capita[1](litres per year)`) %>% 
  select(c(country, code, city, cc, region, asmp, abp, markup, aop, bpc_ge,  bpc_gho, bpc_wiki, r_ge, r_gho, r_wiki ))



# calculating prices on country and region level (https://stackoverflow.com/questions/11562656/calculate-the-mean-by-group)
bdc <- df_f %>% group_by(country) %>% 
  mutate(asmp = round(mean(asmp), 2)) %>% 
  mutate(abp = round(mean(abp), 2)) %>% 
  mutate(aop = round(mean(aop), 2)) %>% 
  distinct (country, .keep_all = T) %>%  
  ungroup %>% 
  select(-c(city))

bdr <-  df_f %>% group_by(region) %>% 
  mutate(asmp = round(mean(asmp), 2)) %>% 
  mutate(abp = round(mean(abp), 2)) %>% 
  mutate(aop = round(mean(aop), 2)) %>% 
  distinct (region, .keep_all = T) %>%  
  ungroup %>% 
  select(-c(city, country))




# save
write_delim(df_f, "processed-data/wwbp_city_add_var.csv", delim = ";") #city level
write_delim(bdc, "processed-data/wwbp_country_add_var.csv", delim = ";") #country level
write_delim(bdr, "processed-data/wwbp_region_add_var.csv", delim = ";") #region level
