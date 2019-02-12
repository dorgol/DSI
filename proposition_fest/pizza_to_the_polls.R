# TO DO
#
# Size legend for pizza plotly (package bug removes it? Colorbar legend is fine)
# Dynamic version as pizzas delivered showing when state polls close
# -----------------------------------------------------------------------------------------------

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

###
pizza = rename(pizza, polling_place_address =  What.s.the.address.of.the.polling.place.)
tmp.pizza = pizza %>% group_by(polling_place_address) %>% do(lat_lon = (geocode(.$polling_place_address)))
lat_lon = do.call("rbind", tmp.pizza$lat_lon)
# WHY DOES THE PREVOIUS CODE ARRANGE HE LAT_LON BY POLLING PLACE ADDRESS? had to adjust for that
pizza = cbind(pizza%>%  arrange(polling_place_address), lon=lat_lon$lon, lat=lat_lon$lat)
#write.csv(pizza, "~/Documents/DSI/DSI_repo/proposition_fest/pizza.csv")
#pizza = read.csv("~/Documents/DSI/DSI_repo/proposition_fest/pizza.csv")
pizza = filter(pizza, !is.na(lat)) #removes 13 rows

# create a clustering variable to prevent plot overlap

pizza.dist = dist(pizza[,c("lat","lon")])
pizza.clust = hclust(pizza.dist, method = "complete")
length(unique(pizza$lat)) #1172
h = 1 #then farthest apart is about a mile
pizza$cluster = cutree(pizza.clust, h = h)
length(unique(pizza$cluster))
sort(table(pizza$cluster))
#pizza.grouped = pizza %>% group_by(cluster) %>% summarize(lat_range = max(lat)-min(lat), lon_range = max(lon)-min(lon))

# plot ----

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

# 5. add crosstalk ----

# add plots about locations on the side
library(crosstalk)
bscols(widths = c(9, 3), g, g)

# 6. plotly with emoji ? ----

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
         


