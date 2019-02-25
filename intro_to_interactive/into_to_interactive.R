# R script companion to intro_to_interactive.Rmd
# Jane Carlen, March 2019

library(dplyr)
library(ggplot2)
library(plotly)
library(crosstalk)
library(emojifont)
library(USAboundaries) #has state, county, congressional district and other shape files
library(USAboundariesData)
library(sf)
library(forcats)
library(listviewer)
library(DT)

setwd("~/Documents/DSI/intro_to_interactive") #<- navigate to your intro_to_interactive folder
pizza = read.csv("pizza.csv") #2534 x 13
names(pizza) # We're interested in a subset of these fields

pizza.grouped = pizza %>% group_by(lat, lon, Status) %>% 
  summarize(Pizzas_delivered = sum(Number_of_pizzas, na.rm = T),
            Polling_place = first(Polling_place_address),
            City = first(City), # note there are some errors in city and state
            State = first(State))

# remove USA from polling place address
pizza.grouped$Polling_place = gsub(", USA", "", pizza.grouped$Polling_place)
pizza.grouped$pizza = emoji(ifelse(pizza.grouped$Status == "Delivered", "pizza", "grey_question"))



# Shared data ----

pizza.grouped$ID = interaction(pizza.grouped$Polling_place, pizza.grouped$Status)
shared_pizza = SharedData$new(pizza.grouped, key = ~ID, group = "pizza_shared")
shared_pizza_deliveries = SharedData$new(filter(pizza.grouped, Status == "Delivered"), key = ~ID, group = "pizza_shared")
shared_pizza_non_deliveries = SharedData$new(filter(pizza.grouped, Status != "Delivered"), key = ~ID, group = "pizza_shared")

