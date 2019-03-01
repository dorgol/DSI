# TO DO
#
# Size legend for pizza plotly (package bug removes it? Colorbar legend is fine)
# Dynamic version as pizzas delivered showing when state polls close
#
# references: 
# https://github.com/MEDSL
# https://electionlab.mit.edu/research#reports
# https://bipartisanpolicy.org/library/improving-the-voter-experience-reducing-polling-place-wait-times-by-measuring-lines-and-managing-polling-place-resources/ "The Bipartisan Policy Center’s project to promote the recommendations of the PCEA has developed a procedure that relies on collecting information about the number of people in line on an hourly basis. This procedure was used by 88 jurisdictions in the 2016 general election, and it proved to be easy to implement."
# Most other existing research on wait times seems to be based on survey data
# http://www.stephenpettigrew.com/articles/pettigrew-2017-psq.pdf
# https://medium.com/mit-election-lab/insights-into-voting-wait-time-from-the-2016-elections-performance-index-6693576e9b99
# https://healthofstatedemocracies.org/factors/waittime.html
# https://twitter.com/openelex?lang=en
# -----------------------------------------------------------------------------------------------

# 0.Setup ####
pizza = read.csv("~/Documents/DSI/pizza_2018.csv", stringsAsFactors = F)
head(pizza)
dim(pizza)

library(ggplot2)
#devtools::install_github("dkahle/ggmap") 
library(ggmap)
library(dplyr)
library(ggrepel)
library(sf)
library(USAboundaries) #has state, county, congressional district and other shape files
library(plotly)
library(stringr)
library(grDevices)
library(emojifont)
library(showtext)
#library(ggimage)

#ggmap v2.7 allows the user to specify a Google Maps API key through the
#register_google() function. v2.7 is not on CRAN yet, so you have to use
#devtools::install_github("dkahle/ggmap") to install it. After updating to that
#version and setting my API Key register_google(key = "my_api_key"), the same
#batch worked in one run as expected.

# also had to enable geocoding 
# https://github.com/dkahle/ggmap/issues/213
# https://console.cloud.google.com/google/maps-apis/apis/geocoding-backend.googleapis.com/metrics?project=my-project-1539884956519&duration=PT1H
# and enable call without signature (not sure if that made a difference)
# geocodeQueryCheck() to check remaining queries of 5000

# Data processing ####
pizza = rename(pizza, polling_place_address =  What.s.the.address.of.the.polling.place.)
tmp.pizza = pizza %>% group_by(polling_place_address) %>% do(lat_lon = (geocode(.$polling_place_address)))
lat_lon = do.call("rbind", tmp.pizza$lat_lon)
# WHY DOES THE PREVOIUS CODE ARRANGE HE LAT_LON BY POLLING PLACE ADDRESS? had to adjust for that
pizza = cbind(pizza%>%  arrange(polling_place_address), lon=lat_lon$lon, lat=lat_lon$lat)
#write.csv(pizza, "~/Documents/DSI/DSI_repo/proposition_fest/pizza.csv")

# Read in pizza (don't reprocess) ----
pizza = read.csv("~/Documents/DSI/DSI_repo/proposition_fest/pizza.csv")
pizza = filter(pizza, !is.na(lat)) #removes 13 rows

# Filter by date to only midterms  ----

# use range 11/02 to 11/05 (early voting) and 11/06 (day of)
# Drops it from 2534 to 2496 entries
# Distribution of entries by date, all other date have only a few
# 2018-11-02 2018-11-03 2018-11-04 2018-11-05 2018-11-06
# 26         37         51        121       2261 

# The pizza data has a Timestamp (of delivery? report?). Estimated_wait_time var is mostly missing

#First fix coding errors I saw later
levels(pizza$Timestamp)[which(levels(pizza$Timestamp)=="11/2")] = "11/02/18"
levels(pizza$Timestamp)[which(levels(pizza$Timestamp)=="11/8")] = "11/08/18"

pizza = pizza %>% mutate(Timestamp_date = as.Date(Timestamp, "%m/%d/%y"),
                         #ones with year 2020 bc had different coding and were converted incorrectly
                         Timestamp_date = as.Date(gsub(x = Timestamp_date, "2020", "2018"), "%Y-%m-%d"),
                         Timestamp_hour =  as.POSIXlt(Timestamp, format = "%m/%d/%y %I:%M %p")$hour)

table(pizza$Timestamp_date, useNA = "always")
filter(pizza, is.na(pizza$Timestamp_date))
filter(pizza, is.na(pizza$Timestamp_hour))  

pizza = filter(pizza, Timestamp_date  >= "2018-11-02" & Timestamp_date <= "2018-11-06") %>% 
  mutate(Timestamp_date = as.factor(Timestamp_date))

#write.csv(pizza, "~/Documents/DSI/intro_to_interactive/pizza_midterms.csv")

# Create a clustering variable to prevent plot overlap ----

pizza.dist = dist(pizza[,c("lat","lon")])
pizza.clust = hclust(pizza.dist, method = "complete")
length(unique(pizza$lat)) #1172
h = 1 #then farthest apart is about a mile
pizza$cluster = cutree(pizza.clust, h = h)
length(unique(pizza$cluster))
sort(table(pizza$cluster))



# -------------------------------------------- A. Plotting ---------------------------------------------------

# 1. intro plots ----
# plot(pizza$lon, pizza$lat)

pizza.grouped.all = pizza %>% group_by(lat, lon) %>% 
  summarize(pizzas_delivered = sum(Number.of.Pizzas, na.rm = T),
            polling_place = first(polling_place_address),
            city = first(City))

#remove USA from polling place address
pizza.grouped.all$polling_place = gsub(", USA", "", pizza.grouped.all$polling_place)
pizza.grouped.all$image = "/Users/jac/Documents/DSI/DSI_repo/proposition_fest/pizza_icon_small.png"
pizza.grouped.all$pizza = emoji('pizza')

plot1 = ggplot(pizza.grouped.all,
        aes(x = lon, y = lat, size = pizzas_delivered)) + 
        geom_point(color = "red", alpha = .5) + 
        ggtitle("Delivery locations (By unique polling places -- some overlap)")

#plot with polling places clustered to show delivery geography  ----
pizza.grouped = pizza %>% group_by(cluster) %>% 
  summarize(pizzas_delivered = sum(Number.of.Pizzas, na.rm = T), 
            lat = mean(lat), 
            lon = mean(lon),
            city = first(City))

plot2 = ggplot(pizza.grouped, aes(x = lon, y = lat, size = pizzas_delivered)) + 
       geom_point(color = "red", alpha = .5) + 
        ggtitle("Delivery locations (Clustered polling places to limit overlap)")

cowplot::plot_grid(plot1, plot2)

ggmap()

# 2. plot with map ----

us.map.watercolor <-get_map(location='united states', zoom=3, maptype = "watercolor",
             source='google',color='color')
us.map.toner <-get_map(location='united states', zoom=4, maptype = "toner-lite",
                            source='google',color='color')
ggmap(us.map.watercolor) + geom_point(data=pizza.grouped, aes(x = lon, y = lat, size = pizzas_delivered), color = "red", alpha = .8) + 
  ggtitle("Delivery locations (Clustered polling places to limit overlap)")

# 3. plot with shapefiles ----

us.state = us_boundaries(type = "state")
us.congressional = us_boundaries(type = "congressional")

# Note the zero pizzas point in the plot were NA in the data

plot.map1 = ggplot() + 
  ylim(24,75) + xlim(-175, -67) +
  geom_sf(data = us.congressional, lwd = .1) +   
  geom_sf(data = us.state, lwd = .3, fill = NA) +
  geom_text(data = pizza.grouped.all, family='EmojiOne',
             #incude bad label aesthetics so they'll show up in the tool tip
             aes(x = lon, y = lat, color = pizzas_delivered, #size = pizzas_delivered, 
                 label = pizza, label1 = city, label2 = polling_place), alpha = 1, size = 2) +
  #geom_text(family='fontawesome-webfont') +
  scale_color_gradient(low = "red", high = "black") +
  #scale_color_gradientn(colors = heat.colors(6), values = c(.1,.2,.3,.4,.5, 1) + guide = "legend") <- puts both in circle legend for ggplot
  theme_bw() + 
  theme(#axis.text.x = element_blank(), axis.text.y = element_blank(),
    axis.title.x = element_blank(), axis.title.y = element_blank()) +
  ggtitle("Delivery locations")
  
plot.map1

ggplotly(plot.map1, tooltip = c("pizzas_delivered", "city", "polling_place")) 


# image approach didnt' work
#geom_image(data = pizza.grouped.all, 
#           #incude bad label aesthetics so they'll show up in the tool tip
#           aes(x = lon, y = lat, size = pizzas_delivered, color = pizzas_delivered, 
#               image = image,
#               label = city, label2 = polling_place), size = .01, alpha = 1) +

# 4. plot with emoji ----

# This was a pain to get working. Most of the emoji fonts I tried didn't work. Only OpenSansEmoji did.
# https://guangchuangyu.github.io/2015/12/use-emoji-font-in-r/ tipped me off
load.emojifont("OpenSansEmoji.ttf") #i put this downloaded file in the working directory 
font_families()
x = 1:10;y=1:10
plot(x, y, cex=0)
text(x, y, labels=emoji('pizza'), cex=1, col='red', family='OpenSansEmoji')

# Emoji

# Pizza is part of the label so it doesn't show up as a legend

plot.map.emoji = ggplot() + 
  ylim(24,75) + xlim(-175, -67) +
  geom_sf(data = us.congressional, lwd = .1) +   
  geom_sf(data = us.state, lwd = .3, fill = NA) +
  #geom_point(data = pizza.grouped.all,
  #          aes(x = lon, y = lat, size = pizzas_delivered/10)) +
  geom_text(data = pizza.grouped.all, 
            #incude bad label aesthetics so they'll show up in the tool tip
            aes(x = lon, y = lat, size = pizzas_delivered, #color = pizzas_delivered, 
                label = pizza, label1 = city, label2 = polling_place), alpha = 1) +
  theme_bw() + 
  theme(#axis.text.x = element_blank(), axis.text.y = element_blank(),
    axis.title.x = element_blank(), axis.title.y = element_blank(),
    panel.background = element_rect(fill = "#FEEDAB")) +
  ggtitle("Delivery locations")

g = ggplotly(plot.map.emoji, tooltip = c("pizzas_delivered", "city", "polling_place"))


# 6. add crosstalk ----

# add plots about locations on the side
library(crosstalk)
bscols(widths = c(9, 3), g, g)

# 7. plotly with emoji ? ----

# Try building 4 (ggplotly) directly with plotly -- helps some things hurts others

plot_ly(pizza.grouped.all) %>%
  add_sf(data = us.congressional, width = 1, showlegend = FALSE) %>%
  add_sf(data = us.state, width = 1, showlegend = FALSE) %>%
  add_trace(data = pizza.grouped.all, type = "scatter",
              x = ~lon, y = ~lat, size = .1, 
              text = ~paste("delivered", pizzas_delivered, "city", city, "polling_place", polling_place)) %>%
  add_text(data = pizza.grouped.all, type = "scatter",
           x = ~lon, y = ~lat, text.size = ~pizzas_delivered/100, 
           text = ~pizza) %>% #pizza.grouped.all$city, pizza.grouped.all$polling_place), 
  layout(yaxis = list(range = c(24,75)), xaxis = list(range = c(-175, -67)))
         
######################### Overflow from intro_to_interactive.Rmd ###########################################

# Linked time plot ####
# Do static plots first
ggplot(pizza_hourly) +
  geom_bin2d(aes(x = lon, y = Timestamp_hour))

plot(pizza_hourly$Timestamp_hour, pizza_hourly$lat)

# Process data ----
pizza_hourly = pizza %>% filter(!is.na(Number_of_pizzas)) %>%
  group_by(Timestamp_hour) %>% 
  arrange(Timestamp_date, Number_of_pizzas) %>%
  mutate(n_hour = 1:n()) %>%
  ungroup()

shared_pizza_hourly = SharedData$new(pizza_hourly)

# Map plot ----

plot.map.point = ggplot() + 
  # map
  geom_sf(data = us.state, lwd = .5, fill = NA) +
  geom_sf(data = us.congressional, lwd = .1) +  
  # points
  geom_point(data = shared_pizza_hourly, 
             #non-functioning aes label1 and label2 to pass these variables to plotly for the tooltip
             aes(x = lon, y = lat, size = Number_of_pizzas, color = Timestamp_hour,
                 label1 = City, label2 = Polling_place_address), alpha = .5) +
  ylim(24,75) + xlim(-175, -67) +
  theme_bw() + 
  scale_color_distiller(palette = "YlGnBu", direction = -1) +
  scale_size_continuous(range = c(1,1)) +
  guides(color = "none") +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  ggtitle("Delivery locations")

plotly.map.point = ggplotly(plot.map.point) %>% highlight(on = "plotly_selected", off = "plotly_deselect") %>%
  layout(legend = list(orientation = 'h'))

#display.brewer.all()

# Dot plot ----
# Only show actual deliveries in the "dotplot"

pizza_hourly_ggplot = ggplot(shared_pizza_hourly) + 
  geom_point(aes(x = Timestamp_hour, y = n_hour, color = Timestamp_date, size = Number_of_pizzas), shape = 20) +
  scale_size_continuous(range = c(1,1)) +
  scale_color_brewer(type = "seq", palette = "Spectral") +
  theme_bw()

pizza_hourly_ggplotly = ggplotly(pizza_hourly_ggplot, tooltip = c("Number_of_pizzas")) %>%
  highlight(on = "plotly_selected", off = "plotly_deselect") 

# Grouped plot ----
bscols(widths = c(6,6), plotly.map.point, pizza_hourly_ggplotly)



###########################  Addtional pizza exploration (beyond workshop)  #################################


# Integrate outside election data to geographic plot

# election data ----
# codebook: https://github.com/MEDSL/2018-elections-unoffical/blob/master/election-context-2018.md
election_context_2018 = read.csv("2018-elections-unoffical-master/election-context-2018.csv")
names(election_context_2018)
# the fips code in the election context data set is a two-digit state code (with possible first 0 cut off) + 3-digit county code
# the fips  code in election_context_2018 may have first 0 cutof
# Reformate both so we can join
us.county$fips = paste(us.county$statefp, us.county$countyfp, sep="")
election_context_2018$fips = sprintf("%05d", election_context_2018$fips)
us.county = left_join(us.county, election_context_2018, by = "fips")
us.county = us.county %>% mutate(ID = paste(name, state_name, sep = " county, "))

# geography with plotly ----
# https://moderndata.plot.ly/visualizing-geo-spatial-data-with-sf-and-plotly/

# 1. Create smaller shated data objects ----
us.county.shared = SharedData$new(us.county %>%
                                    select(1:15, "total_population", "nonwhite_pct", "rural_pct", "ID"),
                                  key = ~ID, group = "pizza_election")
us.county.demo.shared =  SharedData$new(data.frame(us.county) %>%
                                          select("total_population", "nonwhite_pct", "rural_pct", "median_hh_inc", "ID") %>%
                                          mutate(nonwhite_pct = round(nonwhite_pct),
                                                 rural_pct = round(rural_pct)),
                                        key = ~ID, group = "pizza_election")

# 2. Main plot with shared and election data ----

#### ####
plot.map.emoji.election = ggplot() + 
  # map
  geom_sf(data = us.county.shared, lwd = .1, fill = "gray") + #aes(fill = nonwhite_pct, label1 = total_population, label2 = rural_pct )) + 
  #scale_fill_gradient(low = "white", high = "black", name = "County percent non-white") +
  #geom_sf(data = us.congressional, lwd = .2, fill = NA, color = "blue") +
  #geom_sf(data = us.state, lwd = .2, fill = NA) +
  # points
  geom_text(data = shared_pizza_delivered, 
            #non-functioning aes label1 anpd label2 to pass these variables to plotly for the tooltip
            aes(x = lon, y = lat, size = Pizzas_delivered, label = pizza, label1 = City, label2 = Polling_place),
            alpha = 1) +
  ylim(24,75) + xlim(-175, -67) +
  theme_bw() + 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  ggtitle("Delivery locations with counties shown")

plotly.map.emoji.election = ggplotly(plot.map.emoji.election, height = 500) %>%
  highlight(on = "plotly_click", off = "plotly_doubleclick", opacityDim = .1, color = "red")

plotly_json(plotly.map.emoji.election)
plotly_data(plotly.map.emoji.election, id = 1)

plotly.map.emoji.election = plotly.map.emoji.election %>%
  style(traces=1, hoveron = "fills")

plotly.map.emoji.election

# 3. Scatter ----
pizza.scatter.election = ggplotly( ggplot(us.county.demo.shared) + 
                                     geom_point(aes(x = nonwhite_pct, y = rural_pct)) +
                                     theme_bw(), toolip = c("x","y")) %>% 
  highlight(on = "plotly_selected", off = "plotly_doubleclick", opacityDim = .1, color = "red") 

# 4.datatable ----
pizza.datatable.election = datatable(us.county.demo.shared, options = list(pageLength = 10),
                                     selection = "multiple", rownames = T)

# 5. Panel plot ----
pizza_linked_election = subplot(widths = c(.6, .4), plotly.map.emoji.election, pizza.scatter.election)

htmltools::save_html(pizza_linked_election, file = "pizza_linked_election.html")
```

```{r election_data, echo = F, eval = F}

# NOT the elections package on cran - https://github.com/MEDSL/elections/blob/master/README.md
# make sure devtools is updated
# if (!require('devtools', quietly = TRUE)) install.packages('devtools')
# devtools::install_github('MEDSL/elections') 

# The package makes available the following datasets:

# presidential_precincts_2016
# senate_precincts_2016
# house_precincts_2016
# state_precincts_2016
# local_precincts_2016

library(elections)

# Show percent dem by state for 2016 presidential election:

data("presidential_precincts_2016"); head(presidential_precincts_2016)

pres_by_state_returns_2016 = presidential_precincts_2016 %>% 
  group_by(state_postal, party) %>%
  summarize(party_votes = sum(votes))

state_2party = left_join(pres_by_state_returns_2016 %>% filter(grepl(party, pattern="[D|d]emocrat")),
                         pres_by_state_returns_2016 %>% filter(grepl(party, pattern="[R|r]epublican")), by = "state_postal")

state_2party = state_2party %>% 
  group_by(state_postal) %>% 
  summarize(votes.dem = sum(party_votes.x), votes.rep = sum(party_votes.y))

us.state.dem = left_join(us.state, state_2party, by = c("state_abbr" =  "state_postal")) %>% 
  mutate(percent_dem = votes.dem/(votes.dem + votes.rep))


plot3 = ggplot(data = pizza.grouped, aes(x = lon, y = lat, size = Pizzas_delivered)) + 
  ylim(24,75) + xlim(-175, -67) +
  geom_sf(data = us.state.dem, aes(fill =  percent_dem), inherit.aes = FALSE) +
  scale_fill_gradient(low = "white", high = "black", limits = c(0,1)) + 
  geom_point(color = "red", alpha = .5) + 
  ggtitle("Delivery locations (By unique polling places -- some overlap)")

plot3

# Show percent dem by congressional district for 2016 presidential election:

data("house_precincts_2016"); head(house_precincts_2016)


head(us_congressional())

# lm ----
#polling places listed by state
View(pizza.grouped %>% group_by(state) %>% summarize(n()))
View(filter(pizza.grouped, Status == "Delivered") %>% group_by(state) %>% summarize(n()))
View(filter(pizza.grouped, Status == "Delivered") %>% group_by(state) %>% summarize(sum(Pizzas_delivered)))

pizza.grouped.lm = left_join(pizza.grouped, us.state.dem[,c("state_abbr", "percent_dem")], by = c("state"= "state_abbr")) %>% ungroup() %>% filter(!is.na(percent_dem), Status == "Delivered")
lm1 = lm(Pizzas_delivered ~ percent_dem + state, data = pizza.grouped.lm)
summary(lm1)

```
