# Preparing the Beer Data
# The data was produced for a study by goeurop (now omio.de) from 2015

library(dplyr)
library(readr)
library(maps) #needed for loading cities and countrys
library(stringr)
library(tidyr)
library(readxl)

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

# bd_2$flag_code <- countrycode(sourcevar = bd_2[["country"]],
#                          origin = "country.name",
#                          destination = "iso2c") # is needed later visualasing with flags... not needed so far
bd_2$flag_code <- tolower(bd_2$flag_code)

bd_2$cc <- paste(bd_2$CITY, " (", bd_2$code, ")", sep = "")


# calculating the ration bar prices to supermarkt prices 
bd_2$markup <- round(bd_2$`BAR PRICE $`/bd_2$`AVERAGE SUPERMARKET PRICE $`,1)


# The beer consumption per Capita data seems not soo trust worthy... vietnam for example has a really high consumption (higher than germany). 
# other Data on the internet don't show this. Data from the Global Health Observatory (GHO) is added (no data for Hong-Kong)
bc_gho <- read_delim("raw-data/alcahol_consumption_capita_gho.csv", delim = ";" , locale = locale(encoding = 'ISO-8859-2')) %>% 
  select(c(`GHO (DISPLAY)`, `YEAR (DISPLAY)`, `COUNTRY (CODE)`, `COUNTRY (DISPLAY)`, `ALCOHOLTYPE (DISPLAY)`, `Display Value`)) %>% 
  filter(`ALCOHOLTYPE (DISPLAY)`=="Beer") %>%  # select only beer values
  rename("Description"=`GHO (DISPLAY)`, "year"=`YEAR (DISPLAY)`, "code" = `COUNTRY (CODE)`, "country" =`COUNTRY (DISPLAY)`, "Alcoholtype"=`ALCOHOLTYPE (DISPLAY)`, "Value_GHO" = `Display Value`) %>% 
  spread(year, Value_GHO) #long to wide format


# left_join with goEurop data
bd_2_gho <- left_join(bd_2, bc_gho, by="code") 


# Scraping data from wikipedia, since this data could change it is saved and the scraping is only done once 
# library(XML)
# library(rvest)
# library(robotstxt)
# 
# # Testen ob scrapping erlaubt ist ----
# paths_allowed(
#   paths = c("https://en.wikipedia.org/wiki/List_of_countries_by_beer_consumption_per_capita")
# )
# 
# #Ist erlaubt
# 
# url <- "https://en.wikipedia.org/wiki/List_of_countries_by_beer_consumption_per_capita"
# 
# 
# # Scraping der Wikipediaseite über Schweizergemeinden (Stand:2019) ----
# beer.table = url %>%
#   read_html() %>%
#   html_node(xpath = '//*[@id="mw-content-text"]/div/table[1]') %>%
#   html_table(fill = TRUE) 
# 
# # has a lot of empty or not usefull cells, usfull infromation from row 63 to row 123 and from the first to the sixth colum
# beer.table <- beer.table[63:123,] 
# beer.table <- beer.table[,1:6] 
# names(beer.table) <- beer.table[1,]
# bd_w <- beer.table[-1,]
# bd_w$`Consumptionper capita[1](litres per year)` <- as.numeric(bd_w$`Consumptionper capita[1](litres per year)`)
# 
# bd_w$code <- countrycode(sourcevar = bd_w[["Country"]],
#                          origin = "country.name",
#                            destination = "iso3c")
# 
# write_delim(bd_w, "raw-data/beer_consumption_wiki.csv", delim = ";")

bd_w <- read_delim("raw-data/beer_consumption_wiki.csv", delim = ";" , locale = locale(encoding = 'ISO-8859-2')) %>% 
  mutate(year=2015)

 
# leftjoin with dataset from goEurop and GHO, Wiki has no data for United Arab Emirates, Paraguay, Greece, Colombia, Argentina, 
# Egypt, Ukraine, Peru, Luxembourg, Philippines, Russia, Norway, Iceland, Chile, Sweden
bd_2_gho_w <- left_join(bd_2_gho, bd_w, by="code") 



# Add GNI (Gross national income) per capita from 2015 to the dataset as proxy for average income 

gni <- read_delim("raw-data/gni_per_capita_wordlbank.csv", delim = "," , locale = locale(encoding = 'ISO-8859-2')) %>% 
  mutate(`2015 [YR2015]` = as.numeric(`2015 [YR2015]`)) %>% 
  drop_na %>% 
  select(-c(`Series Code`)) %>%  # spread does not work otherwise
  spread(`Series Name`, `2015 [YR2015]`) %>%   #long to wide format
  rename("code" = "Country Code") %>% 
  select(-c(`GNI per capita (constant 2010 US$)`)) # for swizterland and island no entry and not much differenze by the rank for the other countries so that I only use atlas method
  
bd_2_gho_w_gni <- bd_2_gho_w <- left_join(bd_2_gho_w, gni, by="code")

# cleaning and renaming the bd_2_gho_w_gni dataset ----
df_cr <- bd_2_gho_w_gni %>% 
  rename("city" = "CITY",  "asmp" = `AVERAGE SUPERMARKET PRICE $`, "abp" = `BAR PRICE $`, "aop" =`OVERALL PRICE $`, 
         "bpc_ge" = `BEER PER CAPITA/YEAR (LITERS)`, "country" = country.x, 
         "bpc_gho" = `2015`, "bpc_wiki" =`Consumptionper capita[1](litres per year)`, # choocing the year 2015 for the GHO data because the study from goEurop is 2015
          "gni_atlas" = `GNI per capita, Atlas method (current US$)`) %>% 
  select(c(country, code, city, cc,  region, asmp, abp, aop, markup, bpc_ge,  bpc_gho, bpc_wiki, gni_atlas ))


# add average net hourly earnings from UBS data ---  
av_earning <- read_excel("raw-data/UBS_PricesAndEarnings_OpenData.xlsx") %>% 
  filter(Year==2015) %>%  # choosing again 2015
  filter(`Main Section`=="Earnings: Average hourly (net)") %>% 
  rename("year" = "Year") %>% 
  rename("city" = "City") %>%  # for the joing with wc_data
  rename("N_H_E" = "Value") %>% 
  mutate(N_H_E = round(as.numeric(N_H_E, 2)))%>% 
  mutate(city = str_replace(city, "ó", "o")) %>% 
  mutate(city = str_replace(city, "á", "a")) %>% 
  select(c(city, N_H_E)) 

df_cr_ae <- left_join(df_cr, av_earning) # no data for abu Dhabi, Ascuncion, bali, belgrad, boston, carpe town, delhi, dubrovnik, edinburgh, ho chi minh city, krakow, leeds, liverpool, malaga, nice, reykjavik, san francisco, santiago, seville, singapore, strasbourg, the hague, venice


# calculate how long you have to work for a bear (Beer Index)
df_cr_ae <- df_cr_ae %>% 
  mutate(smbi = round(asmp/N_H_E*60)) %>%  # Supermarket beer index in Minutes
  mutate(bbi = round(abp/N_H_E*60)) %>%  # Supermarket beer index
  mutate(obi = round(aop/N_H_E*60))

# make long form for bar plot to compare beer index 
df_cr_ae_long <- df_cr_ae %>% 
  gather(source, minutes, smbi, bbi, obi) %>% 
  filter(!is.na(minutes)) %>%  # use only those with data
  mutate(source = recode(source, bbi = "at the hotel bar", smbi = "in the supermarket", obi = "Overall"))  # recode for plot

  
# save data on city level ----
df_city_wide <- df_cr_ae %>% select(-c(bpc_ge,  bpc_gho, bpc_wiki, gni_atlas)) #not needed on city level
df_city_long <- df_cr_ae_long %>% select(-c(bpc_ge,  bpc_gho, bpc_wiki, gni_atlas)) 

write_delim(df_city_wide, "processed-data/beer_city_add_wide.csv", delim = ";") #city level
write_delim(df_city_long, "processed-data/beer_city_add_long.csv", delim = ";") #city level


# calculating prices on country level and remove the cities (55 countries) (https://stackoverflow.com/questions/11562656/calculate-the-mean-by-group)
bdc <- df_cr %>% group_by(country) %>% 
  mutate(asmp = round(mean(asmp), 2)) %>% 
  mutate(abp = round(mean(abp), 2)) %>% 
  mutate(aop = round(mean(aop), 2)) %>% 
  distinct (country, .keep_all = T) %>%  
  ungroup %>% 
  select(-c(city, cc))

# # calculate price rank - most expensive rank 1 
# bdc$r_asmp <- rank(-bdc$asmp, ties.method = "min")
# bdc$r_abp <- rank(-bdc$abp, ties.method = "min") 
# bdc$r_aop <- rank(-bdc$aop, ties.method = "min")
# 
# 
# # calculate consumption rank - most consumption rank 1
# bdc$r_ge <- rank(-bdc$bpc_ge, ties.method = "min")
# bdc$r_gho <- rank(-bdc$bpc_gho, ties.method = "min") 
# bdc$r_wiki <- rank(as.numeric(-bdc$bpc_wiki, na.rm = TRUE), ties.method = "min", na.last = TRUE)
# 

# add calculations: how much beer you can buy at average (gni/price)
bdc <- bdc %>% 
  mutate(be_gni_sm = round(gni_atlas/asmp)) %>% 
  mutate(be_gni_bp = round(gni_atlas/abp)) %>% 
  mutate(be_gni_op = round(gni_atlas/aop)) 


# save
write_delim(bdc, "processed-data/beer_country_add_wide.csv", delim = ";") #country level

