# Comparison of interactive plotting libraries in R
# Jane Carlen, January 2019
# Notes for Duncan ----
# One thing I've wanted more control over in plotly and rbokeh is the legend location, e.g. to move it to the margin outside the plot. 
# Data is static in all of these? Benefit of shiny?
# Other things to look into ----
# https://rdrr.io/cran/ggvis/man/linked_brush.html
# https://ggvis.rstudio.com/
# https://rstudio.github.io/crosstalk/shiny.html

# 1. Install & load as necessary ####

workshop.packages = c("highcharter", "plotly", "rbokeh", "dplyr", "ggplot2", "ggmosaic")
workshop.install = workshop.packages[! workshop.packages %in% (installed.packages())[,1]]
sapply(workshop.install, install.packages)
sapply(workshop.packages, require, character.only = TRUE)
data("mtcars")

# Load Crosstalk and other packages we'll use with it from source
devtools::install_github("rstudio/crosstalk")
# d3 scatter plot widget
devtools::install_github("jcheng5/d3scatter")
# development-version of leaflet package
devtools::install_github("rstudio/leaflet")

library(crosstalk)
library(d3scatter)
library(leaflet)


# 2. Overall comparison ####

# - rbokeh has a very intuitive and controllable interface, but is built around a 2-D cartesian framework
# - highcharter strength is the range of templates for non-cartesian plots, e.g. pie, heatmaps, treemaps, pyramid (http://jkunst.com/highcharter/highcharts.html)
# - plotly is the easiet to convert from ggplot, so you can layer in interactivity

######################################################################
# i. Highcharter ####

## References ----

# https://cran.r-project.org/web/packages/highcharter/vignettes/charting-data-frames.html
# https://api.highcharts.com/highcharts/

## Examples ----

# data
data(mpg, package = "ggplot2")

data(diamonds, package = "ggplot2")
dfdiam <- diamonds %>% 
  group_by(cut, clarity) %>%
  summarize(price = median(price))

# plots 
h <- highchart() %>% 
  hc_add_series(mpg, "point", hcaes(x = displ, y = cty))

h
class(h) #"highchart"  "htmlwidget"

hchart(mpg, "point", hcaes(x = displ, y = cty))
hchart(dfdiam, "heatmap", hcaes(x = cut, y = clarity, value = price)) 

## It all begins with highchart() or hchart####

## ggplot2 syntax elements:
## hchart function (like qplot)
## hcaes (like aes)

## Then we add layers, hc_add_series ####
#https://api.highcharts.com/highcharts/series


######################################################################
# ii. Plotly ####
# Major pro: Easy to transition existing ggplots with ggplotly()
#  Can build the plot in ggplot then decide to add inerteracitivity
# An interactive visualization library that can be used with Python or R (rbokeh)
# ggplot like layers, but uses ~ (different evaluation method?)
## References ----
#https://plot.ly/r/reference/
#https://plot.ly/ggplot2/

## Examples ----

p <- plot_ly(data = mtcars, x = ~mpg, y = ~cyl, type = "scatter")
p
class(p) #"plotly"   "htmlwidget"

plot_ly(data = mtcars, x = ~cyl) %>%
  add_boxplot(y = ~mpg) %>%
  add_markers(y = ~mpg)

## It all begins with plot_ly  ----
# plot_ly maps R objects to plotly.js, an (MIT licensed) web-based interactive charting library.

p <- plot_ly(data = mtcars)
p

p <- plot_ly(data = mtcars, x = ~cyl)

## Then we add layers, add_----

p <- p %>% add_boxplot(y = ~mpg) %>% add_markers(y = ~mpg)
p

## We can add layout elements ----

#https://plot.ly/r/reference/#layout

p <- p %>% layout(
    title = "Building the plot",
    font = list(size = 14, family = "serif")
)

# contolling lines and points ----

trace_0 <- rnorm(100, mean = 5)
trace_1 <- rnorm(100, mean = 0)
trace_2 <- rnorm(100, mean = -5)
x <- c(1:100)

data <- data.frame(x, trace_0, trace_1, trace_2)

p <- plot_ly(data, x = ~x, y = ~trace_0, name = 'trace 0', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~trace_1, name = 'trace 1', mode = 'lines+markers') %>%
  add_trace(y = ~trace_2, name = 'trace 2', mode = 'markers')

p

## Interactivity ----

p <- p %>% 

# Can you control which buttons appear?
plot_ly(data = mtcars, x = ~cyl) %>%
  add_boxplot(y = ~mpg) %>%
  add_markers(y = ~mpg)
# control tooltip direcly?


## Convert from ggplot ---
g <- ggplot(data = mtcars, aes(x = mpg, y = cyl)) + geom_point()
ggplotly(g, tooltip = "x")


p %>% layout (
    updatemenus = list(
    list(
      buttons = list(
        
        list(method = "restyle",
             args = list("type", "scatter"),
             label = "Scatter"),
        
        list(method = "restyle",
             args = list("type", "histogram2d"),
             label = "2D Histogram")
        )
      )
   )
)
######################################################################
# iii. RBokeh ####

# Like Plotly, Bokeh is an interactive visualization library that can be used with Python or R (rbokeh)
## References ----

## Examples ----

p <- figure() %>%
  ly_points(Sepal.Length, Sepal.Width, data = iris,
            color = Species, glyph = Species,
            hover = list(Sepal.Length, Sepal.Width))


z <- lm(dist ~ speed, data = cars)
p <- figure(width = 600, height = 600) %>%
  ly_points(cars, hover = cars) %>%
  ly_lines(lowess(cars), legend = "lowess") %>%
  ly_abline(z, type = 2, legend = "lm")
p

## It all begins with figure() ----

p <- figure()
class(p) #"rbokeh"   "htmlwidget"

#vs. 
g <- ggplot()
class(g)

## Then we add layers, ly_* ----

# layer functions:

# http://hafen.github.io/rbokeh/rd.html

# data is explicitly specified for each layer.
## unlike ggplot2, we do not attach a default data set when we call figure(), 

## Interactivity ---- 
# Bokeh has many tools available for different types of interaction. 
# Tools can be easily added to a plot either through the tools
# argument to figure(), in which case a vector of tool names is provided, or
# through any of the various tool_ functions. In the latter case, some tools have
# additional parameters that give us finer control over the behavior of the tool.

figure(tools = "pan") %>%
  ly_points(Sepal.Length, Sepal.Width, data = iris, color = Species) %>%
  tool_box_select() %>%
  tool_lasso_select()

figure() %>% ly_crect(data = mtcars, x = gear, y = cyl)

######################################################################
# iv. Crosstalk ####

library(crosstalk)
library(d3scatter)
library(dplyr)
library(leaflet)

## References ----
# https://rstudio.github.io/crosstalk/using.html
# html widgets for R: http://www.htmlwidgets.org/showcase_leaflet.html
## Linked brushing ----

d3scatter(iris, ~Petal.Length, ~Petal.Width, ~Species)

shared_iris <- SharedData$new(iris)
d3scatter(shared_iris, ~Petal.Length, ~Petal.Width, ~Species)

# "Things become more interesting when we pass the same SharedData instance to two separate widgets"
# Enables cross-plot selection

shared_iris <- SharedData$new(iris)
bscols(
  d3scatter(shared_iris, ~Petal.Length, ~Petal.Width, ~Species, width="100%", height=300),
  d3scatter(shared_iris, ~Sepal.Length, ~Sepal.Width, ~Species, width="100%", height=300)
)

# Any Crosstalk-compatible widget can be linked with any other.

shared_quakes <- SharedData$new(quakes[sample(nrow(quakes), 100),])
bscols(
  leaflet(shared_quakes, width = "100%", height = 300) %>%
    addTiles() %>%
    addMarkers(),
  d3scatter(shared_quakes, ~depth, ~mag, width = "100%", height = 300)
)

## Filters ----

shared_mtcars <- SharedData$new(mtcars)
bscols(widths = c(3,NA,NA),
       list(
         filter_checkbox("cyl", "Cylinders", shared_mtcars, ~cyl, inline = TRUE),
         filter_slider("hp", "Horsepower", shared_mtcars, ~hp, width = "100%"),
         filter_select("auto", "Automatic", shared_mtcars, ~ifelse(am == 0, "Yes", "No"))
       ),
       d3scatter(shared_mtcars, ~wt, ~mpg, ~factor(cyl), width="100%", height=250),
       d3scatter(shared_mtcars, ~hp, ~qsec, ~factor(cyl), width="100%", height=250)
)

## Groups ----

#You can provide a group argument to the SharedData constructor to assign it to a specific group.

row.names(mtcars) <- NULL
sd_mtcars_all <- SharedData$new(mtcars, group = "mtcars_subset")
sd_mtcars_auto <- SharedData$new(mtcars[mtcars$am == 0,], group = "mtcars_subset")
sd_mtcars_manual <- SharedData$new(mtcars[mtcars$am == 1,], group = "mtcars_subset")

bscols(widths = c(8, 4),
       d3scatter(sd_mtcars_all, ~hp, ~mpg, ~factor(cyl),
                 x_lim = ~range(hp), y_lim = ~range(mpg),
                 width = "100%", height = 400),
       list(
         d3scatter(sd_mtcars_auto, ~hp, ~mpg, ~factor(cyl),
                   x_lim = range(mtcars$hp), y_lim = range(mtcars$mpg),
                   width = "100%", height = 200),
         d3scatter(sd_mtcars_manual, ~hp, ~mpg, ~factor(cyl),
                   x_lim = range(mtcars$hp), y_lim = range(mtcars$mpg),
                   width = "100%", height = 200)
       )
)

######################################################################
# iv. Review ####

# A range of layer-based syntaxes, some more like ggplot, some more like qplot
# highcharter - Good for non-cartestian templates, e.g. treemaps and pyramids
# plotly - extenstive library but weaker documentation
# rbokeh - intuitive syntax good for 2-D plots in a cartesian framework
# crosstalk - works with html widgets, which includes highcharter, plotly, rbokeh

# Package comparison examples----
## Scatter plot ---- 

# base
plot(x = mtcars$mpg, y = mtcars$cyl)
# ggplot
ggplot(mtcars) + geom_point(aes(x = mpg, y = cyl)) #or ggplot(mtcars, aes(x = mpg, y = cyl)) + geom_point()
qplot(data=mtcars, x = mpg, y = cyl)
#highcharter
highchart() %>% 
  hc_add_series(mtcars, "point", hcaes(x = mpg, y = cyl))
hchart(mtcars, "point", hcaes(x = mpg, y = cyl)) 
#rbokeh
figure() %>% ly_points(x = mpg, y = cyl, data = mtcars)
#plotly

## Heatmap ---- 

# add a fill variable with number of cars in a gear x cyl group
mtcars.heatmap = mtcars %>% group_by(gear, cyl) %>% summarize(fill = n()) %>% ungroup()
# base
image(as.matrix(mtcars.heatmap))
# ggplot
ggplot(mtcars.heatmap) +
    geom_tile(aes(x = gear, y = cyl, fill = fill))
# highcharter
hchart(mtcars.heatmap, "heatmap", hcaes(x = gear, y = cyl, value = fill))
# rbokeh
figure(width = 600,legend_location = "top_left") %>% ly_crect(data = mtcars.heatmap, x = gear, y = cyl, color = fill)

# Treemap/mosaic ---- 
library("ggmosaic")
# base - skip
# ggplot
ggplot(mtcars.new) + 
  geom_mosaic(aes(weight = fill, x = product(gear), fill = as.factor(fill))) # fill needs to be a factor
# highcarter
hchart(mtcars.new, "treemap", hcaes(x = paste(gear,cyl,sep=","), value = fill, color = fill))
# rbokeh - last I checked, bokeh doesn't support moasic plots directly (Bhttps://stackoverflow.com/questions/53797007/how-to-display-statsmodels-mosaics-directly-with-bokeh)
hchart(mtcars.new %>% group_by(gear) %>% summarize(fill = n()), "pyramid", hcaes(y = gear, name = gear))
       
