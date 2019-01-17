# 1. Install & load as necessary ####

workshop.packages = c("highcharter","rbokeh", "dplyr", "ggplot2")
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
# - highcharter may be best for non-cartestian type plots, e.g. pie, heatmaps, treemaps (http://jkunst.com/highcharter/highcharts.html)


######################################################################
# i. Highcharter ####

# refs:

# https://cran.r-project.org/web/packages/highcharter/vignettes/charting-data-frames.html

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
hchart(dfdiam, "treemap", hcaes(x = cut, y = clarity, value = price)) 
hchart(dfdiam, "pie", hcaes(x = cut, value = price)) 

## It all begins with highchart() or hchart####

## ggplot2 syntax elements:
## hchart function (like qplot)
## hcaes (like aes)

## hcmap ####

hcmap("countries/us/us-all") %>%
  hc_title(text = "USA")

get_data_from_map(download_map_data("countries/us/us-all"))
######################################################################
# ii. Plotly ####

######################################################################
# iii. RBokeh ####

# Bokeh is an interactive visualization library that can be used with Python or R (rbokeh)

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

# refs:
# https://rstudio.github.io/crosstalk/using.html



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

# basic points plot -- 

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

# heatmap -- 

# base and ggplot require reformatting the data
mtcars.new = mtcars %>% group_by(gear, cyl) %>% summarize(fill = n()) %>% ungroup()
# base
image(as.matrix(mtcars %>% group_by(gear, cyl) %>% summarize(fill = n()) %>% ungroup()))
# ggplot requires creating the fill variable
ggplot(mtcars.new) +
    geom_tile(aes(x = gear, y = cyl, fill = fill))
# highcharter
hchart(mtcars.new, "heatmap", hcaes(x = gear, y = cyl, value = fill))
# rbokeh
figure(width = 600,legend_location = "top_left") %>% ly_crect(data = mtcars.new, x = gear, y = cyl, color = fill, )

# Notes for Duncan
# One thing I've wanted more control over in plotly and rbokeh is the legend location, e.g. to move it to the margin outside the plot. 

       