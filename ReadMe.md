<p><h1>Beerboard</h1></p>
<p>Beer and Money, two of my favorite things on this world… just kidding, I only like money because you can buy beer with it. And in Switzerland you need a lot of money to buy beer.</p>
<p>Do you? Yes beer is expensive in restaurants and bars in Switzerland but I don't think it is in the supermarket, where you find half liter beer cans for 0.50$. While visiting other countries I rarely found cheaper beer in supermarkets or small shops.</p>
<p>A few months ago I found data about beer prices. This data was collected by GoEuro (now Omio), a travel website, where you can find, compare and buy tickets for trains, buses or flights for European countries (now also for the USA and Canada, that’s probably the reason why they changed names).  This data includes supermarket, hotel bar and overall prices for a 0.33L beer in different cities.</p>
<p>So what do you do when you are interested in economics, data and beer? Right you build a Beer Data Dashboard aka Beerboard. &nbsp;</p>
With this Beerboard I now can answer questions like:
<ul>
<li>
&laquo;Is Switzerland really an expensive place to drink beer?&rdquo; (Spoiler: yes it is, especially as a tourist in a hotel bar but not really as citizen when you compare earnings&hellip;)
</li>
<li>
&laquo;Do people drink more beer when it's cheaper?&raquo;
</li>
<li>
&laquo;Where can inhabitants buy the most beer with their income?&raquo;
</li>
</ul>
If you like my Beerboard and you want to have your own dashboard for your data or you want to give me suggestions for improvement or other observations please contact me over my <a href="www.linkedin.com/in/flavio-von-rickenbach-12103b">LinkedIn</a> or via <a href="mailto:%20beerboard@hotmail.com">Mail.</a>
Created by: Flavio von Rickenbach, CC-BY 4.0
<p><h2>Data Sources</h2></p>
<p>For this Beerboard I use data from different sources and this sources sometimes also use further sources&hellip; Showing all this in the graphs would make the them really messy. Therefore I created this section. Following I state where if found the data I use for my graphs and describe it.</p>
<p><h3>GoEuro Data </h3></p>
<strong>Name of the file:</strong> world_wide_beer_prices.csv
<p><strong>Source:</strong></p> The data was collected in 2015. I can&rsquo;t find a link for the raw data anymore but <a href="https://graphics.wsj.com/table/BEER_062415">here</a> is a table with the data you could probably scrap and <a href="https://www.businessinsider.com/go-euros-beer-price-index-shows-the-most-expensive-places-for-beer-2015-7?op=1&amp;r=US&amp;IR=T">here</a> you find a graphic produced by GoEuro with additional information about the data in the footnotes. If this two links are not working, use your favorite search engine and search for &ldquo;goeuro beer index 2015&rdquo; and then choose images.
<p><strong>Variables:</strong></p>
<ul>
<li>
<em>CITY</em> <em>&ndash; </em>75 different cities for which the data was collected
</li>
<li>
<em>AVERAGE SUPERMARKET PRICE $ &ndash;</em> The prices of beer in a supermarket was calculated as the average cost of a 33cl bottle of several worldwide brands and a major local brand in a regular discount store.
</li>
<li>
<em>BAR PRICE $</em> <em>&ndash; </em>The price of beer in a bar was calculated using average price of local and imported 33cl draught beer in several major hotel chains worldwide.
</li>
<li>
<em>OVERALL PRICE $ &ndash; </em>The mean of the supermarket and the hotel bar price.
</li>
<li>
<em>BEER PER CAPITA/YEAR (LITERS) &ndash; </em>Data from World Health Organization
</li>
<li>
<em>AVERAGE SPENT ON BEER PER PERSON PER YEAR $ &ndash; </em>Average based on the World Health Organisation data and GoEuro Beer Price Index
</li>
</ul>
<p><strong>Remarks:</strong></p>
<ul>
<li>
All prices were converted in USD using the Bloomberg exchange rate on June 2015.
</li>
<li>
Where beers are sold in 12ou (35.3cl) varieties vs the European standard of 33cl, GoEuro normalized prices to be equivalent to a 33cl bottle.
</li>
</ul>
It would have be good if they have stated more prominent and not only in a footnote that the bar prices were collected at hotel bars. The prices seem in general a little high for me and that they were collected at a hotel bar could be a reason. In my opinion they also should have used an average exchange rate to converting to USD not just take one on a somehow random day.
Unfortunately, I don&rsquo;t have the exact source for the consumption data. I doubt that the numbers are for 2015 since such data is mostly uploaded with delay. I&rsquo;m also not sure if this data has some faulty numbers, especially the high consumption in Vietnam seems odd. This is way I added two more sources for consumption data to the raw data base.
The data set has a European bias. This is probably due to the fact, that it was collected by a European focused website.
<p><h3>GHO consumption data</h3></p>
<strong>Name of the file:</strong> beer-consumption-per-person-GHO.csv
<strong>Source: </strong>The data is from the World Health Organization (WHO) data base &ldquo;Global Health Observatory&rdquo; (GHO) and can be found <a href="https://apps.who.int/gho/data/node.main.A1039?lang=en">here</a>. I choose to use the consumption data for the year 2015.
<p><strong>Variables (the dataset includes many variables, I only describe the ones I use):</strong></p>
<ul>
<li>
<em>GHO (DISPLAY) &ndash; </em>Alcohol, recorded per capita (15+) consumption (in litres of pure alcohol)
</li>
<li>
<em>Year</em>
</li>
<li>
<em>Country (CODE) &ndash; </em>The three letter iso3c country code for each country
</li>
<li>
<em>Country (Display) &ndash; </em>The English name for each country
</li>
<li>
<em>Display Value &ndash; </em>The amount of Alcohol, recorded per capita (15+) consumption (in litres of pure alcohol)
</li>
<li>
ALCOHOLTYPE (DISPLAY) &ndash; 4 different types: Other alcohol beverages, Spirits, Wine, Beer, All typs
</li>
</ul>
<p><strong>Remarks:</strong></p>
Due to my experience I know that you must be cautious when comparing consumption data. You find for example at least three numbers for meat consumption in Switzerland depending on the source and calculation methods (if you want to know more about this check <a href="https://novanimal.ch/2018/10/01/produktion-und-konsum-von-fleisch-in-der-schweiz/">this</a> out). So when it comes to cross country data you have to be even more careful even if they are published from one Organization. Don&rsquo;t use this numbers as exact values! Look at them as an approximation and if two countries have only small differences there is maybe no difference in the real world. The sources for the data and the calculation methods for the GHO data can be found <a href="https://www.who.int/data/gho/indicator-metadata-registry/imr-details/462">here</a>.
<p><h3>Wikipedia consumption data</h3></p>
<strong>Name of the file:</strong> beer_consumption_wiki.csv
<strong>Source: </strong>The data is scrapped from <a href="https://en.wikipedia.org/wiki/List_of_countries_by_beer_consumption_per_capita">this</a> Wikipedia side. As in the nature of Wikipedia the source of the consumption data for the countries varies. Most data is from the Kirin Beer University Report: &ldquo;Global Beer Consumption by Country in 2018&rdquo; which can be found <a href="https://www.kirinholdings.co.jp/english/news/2019/1224_01.html">here</a> (note to myself, there is a Beer University! Have to find out how to get a job there!). <a href="https://en.wikipedia.org/wiki/Kirin_Company">Wikipedia</a> says: &ldquo;Kirin Brewery Company, Limited is a Japanese integrated beverages company. It is a subsidiary of Kirin Holdings Company, Limited. Its major operating units include Kirin Brewery Company, Limited, <a href="https://en.wikipedia.org/wiki/Mercian_Corporation">Mercian Corporation</a> and Kirin Beverages Company, Limited. Kirin is a member of the <a href="https://en.wikipedia.org/wiki/Mitsubishi">Mitsubishi Group</a>.&raquo;. There are a lot of other data sources:
<p>1&nbsp; <a href="https://www.kirinholdings.co.jp/english/news/2019/1224_01.html">"Kirin Beer University Report Global Beer Consumption by Country in 2018 | News Releases | Kirin Holdings"</a>. www.kirinholdings.co.jp.</p>
<p>2 &nbsp;sectoresonline.com (September 2, 2011). <a href="https://web.archive.org/web/20140508082709/http:/www.sitioandino.com/nota/6920-una-mala-para-el-sector-vitivinicola-en-argentina-se-consume-mas-cerveza-que-vino/">"Una mala para el sector vitivin&iacute;cola: en Argentina se consume m&aacute;s cerveza que vino"</a>. Andino (in Spanish). Archived from <a href="http://www.sitioandino.com/nota/6920-una-mala-para-el-sector-vitivinicola-en-argentina-se-consume-mas-cerveza-que-vino/">the original</a> on May 8, 2014<em>. Retrieved </em><em>September 2,</em><em> 2011</em>.</p>
<p>3 <a href="http://kinhdoanh.vnexpress.net/tin-tuc/hang-hoa/nguoi-viet-tieu-thu-gan-3-8-ty-lit-bia-3527545.html">"Vietnamese consume 3.8 billion liters of beers in 2016"</a>. VnExpress. January 12, 2017<em>. Retrieved </em><em>January 12,</em><em> 2017</em>.</p>
<p>4&nbsp; <a href="https://www.statista.com/statistics/742249/apac-beer-consumption-per-capita-by-country/">"Beer consumption per capita in the Asia Pacific region in 2015, by country (in liters)"</a>. statista: the statistics portal<em>. Retrieved </em><em>2018-09-02</em>.</p>
<p>5&nbsp; <a href="https://www.statista.com/statistics/447089/france-volume-beer-consumption-per-capita/">"Annual volume of beer consumed per capita in France from 2008 to 2016"</a><em>. Retrieved </em><em>August 9,</em><em> 2018</em>.</p>
<p>6&nbsp; <a href="http://www.assobirra.it/wp-content/uploads/2017/11/annual_report_2016.pdf">"AssoBirra annual report 2016"</a> <em>(PDF)</em><em>. Retrieved </em><em>January 8,</em><em> 2017</em>.</p>
<p>7&nbsp; <a href="https://www.statista.com/outlook/10010000/117/beer/china#market-volume">"Beer - China"</a>. statista: the statistics portal<em>. Retrieved </em><em>2018-09-02</em>.</p>
<p>8&nbsp; <a href="https://www.statista.com/outlook/10010000/118/beer/hong-kong#market-volumePerCapita">"Beer - Hong Kong"</a>. statista: the statistics portal<em>. Retrieved </em><em>2018-09-02</em>.</p>
<p>9&nbsp; <a href="https://www.helgilibrary.com/indicators/beer-consumption-per-capita/israel/">"Beer Consumption Per Capita in Israel"</a><em>. </em><em>Retrieved </em><em>2018-09-04</em>.</p>
<p>10&nbsp; <a href="http://www.ntvmsnbc.com/id/24958632">"Kriz geldi, alkol t&uuml;ketimi y&uuml;zde 20 artti"</a>. NTVMSNBC. NTV. 2008.</p>
<p>11 &nbsp;Barigaba, Julius (August 17, 2009). <a href="https://www.theeastafrican.co.ke/business/-/2560/640576/-/view/printVersion/-/lohe75/-/index.html">"Uganda brewers toast to higher sales, thanks to low-end products"</a>. EastAfrican<em>. Retrieved </em><em>October 13,</em><em> 2010</em>.</p>
<p>12&nbsp; <a href="http://www.flex-news-food.com/console/PageViewer.aspx?page=13818&amp;amp%3Bstr=Beer">"Heineken Partners with Efes Breweries in Uzbekistan, Serbia and Kazakhstan"</a>. FlexNews. January 1, 2008<em>. Retrieved </em><em>October 13,</em><em> 2010</em>.</p>
<p>13&nbsp; <a href="http://www.island.lk/2009/08/16/business3.html">"Lion seeks to reduce gearing through Rs. 1.2 bn. cash infusion"</a>. Sunday Island. August 16, 2009<em>. Retrieved </em><em>October 13,</em><em> 2010</em>.</p>
<p><strong>Variables:</strong></p>
<em>Country </em>&ndash; 60 different countries
<em>Consumption per capita (liters per year)</em> &ndash; Beer consumption measured in liters
<em>2018 change (liters per year)</em> &ndash; increase or decrease of beer consumption
<em>Total national consumption (million liters per year)</em> &ndash; national beer consumption
<em>Year</em>
<em>Source</em> &ndash; Source of the data (see above)
<p><strong>Remarks:</strong></p>
Since the data is cross country and from different sources, we should be even more careful when analyzing and interpreting it.
<p><h3>UBS earning Data</h3></p>
<strong>Name of the file:</strong> UBS_PricesAndEarnings_OpenData.csv
<strong>Source: </strong>The data for the average hourly earning is from the UBS global cities ranking which includes many other economic data for 77 cities worldwide (data found <a href="https://www.ubs.com/minisites/prices-earnings/en/open-data/">here</a>).
<p><strong>Variables:</strong></p>
<ul>
<li>
<em>City &ndash; &nbsp;</em>The English name for the included cities
</li>
<li>
<em>Year &ndash;</em>
</li>
<li>
<em>Main Section &ndash; </em>A lot of different categories about earnings, prices, exchange rates and general economic data
</li>
<li>
<em>Sub-Section &ndash; </em>measuring unite for the chosen Main Section variable
</li>
<li>
<em>Value &ndash; </em>
</li>
</ul>
<p><strong>Remarks:</strong></p>
Quoting from the UBS methodology explanations (here) wages were calculated the following way:
&ldquo;For the study's second main ingredient, earnings, we created a reference profile of 15 professions to represent the structure of the working population in Europe. We included profiles of workers of varying ages, family status, work experience and education levels.
We collected tax and social security information using PwC&rsquo;s &ldquo;Worldwide Tax Summaries,&rdquo; supplemented by information from local tax authorities, and corroborated our results with survey participants. Taxes were averaged for each profession, weighted and then calculated as a percentage. The social security contributions of employees and employers were gathered from PwC and KPMG sources.
Our net wages are calculated by deducting these social security contributions from gross wages and then subtracting taxes. Hourly wages in individual cities were calculated by dividing average annual gross or net wages by the average number of hours worked per year.
Public sector spending and social security systems in individual countries (and in individual cities) vary greatly, so the percentage of deductions ranges widely.&rdquo;
Here you see again that there are a lot of calculations and assumptions in variables like average net earning and the numbers shouldn&rsquo;t be used as if they were natural science results. 
<h2>Final Data</h2>
From the different data I discribed in the data source section I created three data sets. Two to compare beer data on the city level and one to compare the data on country level.
<p><h3>City level wide</h3></p>
<strong>Name of the file:</strong> beer_city_add_wide.csv
<p><strong>Variables:</strong></p>
<ul>
<li>
<em>country &ndash; </em>The English name for the included countries
</li>
<li>
<em>code &ndash; </em>The three letter iso3c country code for each country. Created with the R package maps based on the country name.
</li>
<li>
<em>city &ndash; </em>The English name for the included cities
</li>
<li>
cc <em>&ndash; </em>City name with country code in brackets
</li>
<li>
<em>region &ndash;</em>Created with the R package maps based on the country name. Values are: East Asia &amp; Pacific, Europe &amp; Central Asia, Latin America &amp; Caribbean, Middle East &amp; North Africa, North America, South Asia, Sub-Saharan Africa.
</li>
<li>
<em>asmp &ndash; </em>Average beer price at the supermarket
</li>
<li>
<em>abp &ndash;</em> Average beer price at the hotel bar
</li>
<li>
<em>aop &ndash;</em> Overall price
</li>
<li>
<em>markup &ndash;</em> This variable is calculated by the fraction asp/abp. It gives the ratio how much more a beer costs in a hotel bar than in the supermarket.
</li>
<li>
<em>N_H_E &ndash; </em>average net earning. No data for Abu Dhabi, Ascuncion, Bali, Belgrad, Boston, Cape Town, Delhi, Dubrovnik, Edinburgh, Ho Chi Minh City, Krakow, Leeds, Liverpool, Malaga, Nice, Reykjavik, San Francisco, Santiago, Seville, Singapore, Strasbourg, The Hague, Venice
</li>
<li>
<em>Smbi &ndash; </em>Supermarket Beer Index: How many minutes do you have to work in this city to buy a 33cl beer at the supermarket. Calculated by the fraction asp/N_H_E.
</li>
<li>
<em>bbi &ndash; </em>Hotel Bar Beer Index: How many minutes do you have to work in this city to buy a 33cl beer at the Hotel Bar. Calculated by the fraction abp/N_H_E.
</li>
<li>
<em>obi &ndash; </em>Overall Beer Index: How many minutes do you have to work in this city to buy a 33cl at the mean of supermarket and hotel bar prices. Calculated by the fraction aop/N_H_E.
</li>
</ul>
<p><h3>City level long</h3></p>
<strong>Name of the file:</strong> beer_city_add_long.csv
<p><strong>Variables:</strong></p>
<ul>
<li>
<em>country &ndash; </em>The English name for the included countries
</li>
<li>
<em>code &ndash; </em>The three letter iso3c country code for each country. Created with the R package maps based on the country name.
</li>
<li>
<em>city &ndash; </em>The English name for the included cities
</li>
<li>
cc <em>&ndash; </em>City name with country code in brackets
</li>
<li>
<em>region &ndash;</em> Created with the R package maps based on the country name. Values are: East Asia &amp; Pacific, Europe &amp; Central Asia, Latin America &amp; Caribbean, Middle East &amp; North Africa, North America, South Asia, Sub-Saharan Africa.
</li>
<li>
<em>asmp &ndash; </em>Average beer price at the supermarket
</li>
<li>
<em>abp &ndash;</em> Average beer price at the hotel bar
</li>
<li>
<em>aop &ndash;</em> Overall price
</li>
<li>
<em>markup &ndash;</em> This variable is calculated by the fraction asp/abp. It gives the ratio how much more a beer costs in a hotel bar than in the super market.
</li>
<li>
<em>N_H_E &ndash; </em>average net earning. No data for Abu Dhabi, Ascuncion, Bali, Belgrad, Boston, Cape Town, Delhi, Dubrovnik, Edinburgh, Ho Chi Minh City, Krakow, Leeds, Liverpool, Malaga, Nice, Reykjavik, San Francisco, Santiago, Seville, Singapore, Strasbourg, The Hague, Venice
</li>
<li>
<em>source &ndash; </em>Source of the minutes values (see below): in the Supermarket, at the Hotelbar, Overall
</li>
<li>
<em>minutes &ndash; </em>How long you have to work to buy a beer at the place given by the source
</li>
</ul>
<p><h3>Country level wide</h3></p>
<strong>Name of the file:</strong> beer_country_add_wide.csv
<p><strong>Variables:</strong></p>
<ul>
<li>
<em>country &ndash; </em>The English name for the included countries
</li>
<li>
<em>code &ndash; </em>The three letter iso3c country code for each country. Created with the R package maps based on the country name.
</li>
<li>
<em>region &ndash; </em>Created with the R package maps based on the. Created with the R package maps based on the country name. Values are: East Asia &amp; Pacific, Europe &amp; Central Asia, Latin America &amp; Caribbean, Middle East &amp; North Africa, North America, South Asia, Sub-Saharan Africa.
</li>
<li>
<em>asmp &ndash; </em>Average beer price at the supermarket. These values were calculated by taking the mean when there was more than one city for a country otherwise it is the same value as for the city of this country which was in the city dataset.
</li>
<li>
<em>abp &ndash;</em> Average beer price at the hotel bar. Same calculation method as in asmp.
</li>
<li>
<em>aop &ndash;</em> Overall price. Same calculation method as in asmp.
</li>
<li>
<em>markup &ndash;</em> This variable is calculated by the fraction asp/abp. It gives the ratio how much more a beer costs in a hotel bar than in the supermarket. Same calculation method as in asmp.
</li>
<li>
<em>bpc_ge &ndash; </em>Beer consumption per capita in liters beer. Values from GoEurop.
</li>
<li>
<em>bpc_gho &ndash; </em>Beer consumption in litres of pure alcohol. Values from GHO.
</li>
<li>
<em>bpc_gho &ndash; </em>Beer consumption in litres of pure alcohol. Values from Wikipedia.
</li>
<li>
<em>be_gni_sm &ndash; </em>Beer buying Power Hotel Bar: How many beers could you buy when you spend the 2015 average GNI at a hotel bar.
</li>
<li>
<em>be_gni_sm &ndash; </em>Beer buying Power Supermarket: How many beers could you buy when you spend the 2015 average GNI in a supermarket.
</li>
<li>
<em>be_gni_sm &ndash; </em>Beer buying Overall: How many beers could you buy when you spend the 2015 average GNI overall (spend half in a hotel bar and half in a supermarket).
</li>
</ul>
