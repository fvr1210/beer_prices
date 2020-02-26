library(tidyverse)

beerprices <- read_delim("raw-data/word_wide_beer_prices.csv", delim=";", locale = locale(encoding = 'ISO-8859-2'))

beerprices$margin <- beerprices$`BAR PRICE $`- beerprices$`AVERAGE SUPERMARKET PRICE $`
                         