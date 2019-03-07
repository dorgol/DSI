# Introduction to principles of plotting and ggplot
# Based off Nick Ulle's notes from Stat 141A
# Below should take between 60 and 90 minutese
# Jane Carlen
# March 7, 2019
#######################################################################################
# Principles of plotting ----

# The Trifecta Checkup (https://junkcharts.typepad.com/junk_charts/junk-charts-trifecta-checkup-the-definitive-guide.html):

# What is the QUESTION?
# What does the DATA say?
# What does the VISUAL say?

# When you see a plot ask yourself:
# What it is trying to convey? 
      # https://junkcharts.typepad.com/.a/6a00d8341e992c53ef01a73dcbe630970d-pi
      # https://junkcharts.typepad.com/.a/6a00d8341e992c53ef01a511c0b358970c-pi
# Did it do so effectively? 
      # https://junkcharts.typepad.com/junk_charts/2006/05/the_crossover_l.html
# Could it have been simpler? 
      # https://junkcharts.typepad.com/junk_charts/2011/10/the-massive-burden-of-pie-charts.html
      # https://junkcharts.typepad.com/junk_charts/2014/04/conventions-novelty-and-the-double-edge.html
# Are the graphics self-sufficient?
      # https://junkcharts.typepad.com/junk_charts/sufficiency/
# Small multiples:
      # https://junkcharts.typepad.com/junk_charts/small_multiples/

# Sometimes a plot fails because the data was insufficient
      # https://junkcharts.typepad.com/junk_charts/2014/04/conventions-novelty-and-the-double-edge.html

#######################################################################################
# Dogs example ----

#   Setup ----

# Install package on machine (do one time)
# if it's already install a pop-up message will appear and you can cancel the installation
install.packages("ggplot2") 

# Load a package (do every time you restart R)
library("ggplot2")

# Adjust the next line for your system. 
# Put in quotes the path to the folder where you have the data file dogs_full.rds stored
setwd("~/Documents/DSI/intro_to_ggplot") 

# Read in the data and save as dogs
dogs = readRDS("dogs_full.rds")

# Explore what's in the data
head(dogs)
names(dogs)

# Example plot with ggplot
ggplot(dogs, aes(x = datadog, y = popularity, color = group)) +
  geom_point()
# data dog is the composite variable used as the x axis here: https://informationisbeautiful.net/visualizations/best-in-show-whats-the-top-data-dog/

#   Let's create a chart that shows how dogs stack up by popularity ----

#       Layer 1: DATA -- supply data to plot ----

# This lays the base for a plot by supplying data. But nothing appears yet.
ggplot(dogs)

#       Layer 2: GEOMetry -- shapes to represent data ----

# This returns an error. 
ggplot(dogs) + geom_point()
# We need to tell ggplot what variables to use to create points, which we do next.

#       Layer 3: AESthetic -- "wires" between geometry and data ----

# Now we give it an x variable and a y variable:
ggplot(dogs) + geom_point(aes(x = popularity_all, y = breed))

# Does the last plot really need to be a plot? We can get the same information from an ordered list. 
# List of dogs by popularity (most popular at the top)
dogs$breed[order(dogs$popularity_all)]

# What do we lose in a list? Comparability

# In our plot we can look up dogs by name AND we can see which are most popular by looking at a certain region of the x axis.

# We want the alphabetical ordering to have A on top. 
# To do that we're change the data slightly. Specifically, we change the breed variable to be a "factor" and re-order its levels (don't worry if you're not sure what that means yet)
dogs$breed = factor(dogs$breed, levels = rev(sort(unique(dogs$breed))))

# Now the plots is ordered by name alphabetically from top to bottom
ggplot(dogs) + geom_point(aes(x = popularity_all, y = breed))

#       Layer 4: SCALE -- controls how data numbers are transformed to screen ----
# We probably want the most popular dogs at the right, not the left.
# Visually, people are more likely to assume things are sorted worst to best
# We use the scale_x_reverse argument to do this

ggplot(dogs) + geom_point(aes(x = popularity_all, y = breed)) +
  scale_x_reverse()

# Do we need to separate names and points? NO.
# Separating them makes it harder to read.
# Let's add another geom to add breed names to the plot at the points

ggplot(dogs) + 
  geom_point(aes(x = popularity_all, y = breed)) +
  geom_text(aes(x = popularity_all, y = breed, label = breed)) +
  scale_x_reverse()

# Now we have superfluous text, let's remove that using the THEME layer next

#       Layer 5: THEME -- overall style of the plot ----
#
# Use themes when you want to change default colors, shapes, etc. throughout the plot.

# We turn off the extra y-axis labels using the theme function
# We also add vertical lines so it's easier to see groups of popularity
ggplot(dogs) + 
  geom_point(aes(x = popularity_all, y = breed)) +
  geom_text(aes(x = popularity_all, y = breed, label = breed)) +
  scale_x_reverse() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        panel.grid.major.x = element_line(colour = "gray", size=0.75, linetype = "dotted"))

#       More adjustments for readability ----
# What we have so far is hard to read. 
# Let's make the text smaller
# Add size as an arguments to the text geom OUTSIDE the aesthetics (since it doesn't depend on the data set).

ggplot(dogs) + 
  geom_point(aes(x = popularity_all, y = breed)) +
  geom_text(aes(x = popularity_all, y = breed, label = breed), size = 3) +
  scale_x_reverse() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        panel.grid.major.x = element_line(colour = "gray", size=0.75, linetype = "dotted"))

# The "alpha" changes point transparency (0 is most transparent, 1 is least)
# vjust changes text position, and "inward" makes it so that text isn't cut off at plot edges. vjust also accepts a number. 
# size and color do what you'd expect

ggplot(dogs) + 
  geom_point(aes(x = popularity_all, y = breed), alpha = .5, color = "red") +
  geom_text(aes(x = popularity_all, y = breed, label = breed), size = 3, vjust = "inward") +
  scale_x_reverse() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        panel.grid.major.x = element_line(colour = "gray", size=0.75, linetype = "dotted"))

# Use the limits argument within scale_x_reverse to give some padding on the left and right side by expanding the x range.
ggplot(dogs) + 
  geom_point(aes(x = popularity_all, y = breed), alpha = .5, color = "red") +
  geom_text(aes(x = popularity_all, y = breed, label = breed), size = 3, vjust = "inward") +
  scale_x_reverse(limits = c(180,-10)) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        panel.grid.major.x = element_line(colour = "gray", size=0.75, linetype = "dotted"))

#       Layer 6: LABELS -- they should be meaningful ----

# Add x and y labels and title using the labs argument
ggplot(dogs) + 
  geom_point(aes(x = popularity_all, y = breed), alpha = .5, color = "red") +
  geom_text(aes(x = popularity_all, y = breed, label = breed), size = 3, vjust = "inward") +
  scale_x_reverse(limits = c(180,-10)) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        panel.grid.major.x = element_line(colour = "gray", size=0.75, linetype = "dotted")) +
  labs(title = "Dog breeds organized by popularity ranking", x = "Popularity rank, lower rank (further right) is better")

  # Other label functions:
  #
  # ggtitle() -- title
  # xlab() -- x label
  # ylab() -- y label

# The plot we just made is about as good as we can do for a chart that shows how dogs stack up by popularity. We can look up dogs by name because they're sorted alphabetically from top to bottom. We can also look at specific groups of dogs by popularity since the x-axis position is determined by popularity. For example, it's easy to see which breeds are in the top 50. Our plots has a clear title and x and y labels, and we used the x label to explain the scale.

# The information in this chart could also be conveyed with two lists of dogs, one by popularity and one alphabetically with rank printed. Then users can look up by name or by rank. Whether you choose to use two lists or a plot probably depends on wiht who and where you're sharing the information. 

# However, the plot is not very user-friendly because there are so many names. Next we'll start to sub-group the dogs using the other variables in the data. This will give readers less of a word soup, and help us explore relationships and produce plots with fewer data points.

#   Let's explore relationships between popularity and other variables ----

#   Let's start with a relationship between dog type and popularity ----
#       We'll do this by coloring the points and labels to correspond to dog type
#       Add a color aesthetic to a geometry ----

# note that color = group goes INSIDE aes() and we removed color = "red"
ggplot(dogs) + 
  geom_point(aes(x = popularity_all, y = breed, color = group), alpha = .5) +
  geom_text(aes(x = popularity_all, y = breed, label = breed, color = group), size = 3, vjust = "inward") +
  scale_x_reverse(limits = c(180,-10)) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        panel.grid.major.x = element_line(colour = "gray", size=0.75, linetype = "dotted")) +
  labs(title = "Dog breeds organized by popularity ranking", x = "Popularity rank, lower rank (further right) is better")

# Those colors aren't great. We can customize them. 
# Adjust color palette -- we'll use a darker one that's easier to read
ggplot(dogs) + 
  geom_point(aes(x = popularity_all, y = breed, color = group), alpha = .5) +
  geom_text(aes(x = popularity_all, y = breed, label = breed, color = group), size = 3, vjust = "inward") +
  scale_x_reverse(limits = c(180,-10)) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        panel.grid.major.x = element_line(colour = "gray", size=0.75, linetype = "dotted")) +
  labs(title = "Dog breeds organized by popularity ranking", x = "Popularity rank, lower rank (further right) is better") +
  scale_color_brewer(palette = "Dark2") # set color palette

# Or supply the colors
my_colors = c("red", "blue", "green", "orange", "purple", "yellow", "cyan")

ggplot(dogs) + 
  geom_point(aes(x = popularity_all, y = breed, color = group), alpha = .5) +
  geom_text(aes(x = popularity_all, y = breed, label = breed, color = group), size = 3, vjust = "inward") +
  scale_x_reverse(limits = c(180,-10)) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        panel.grid.major.x = element_line(colour = "gray", size=0.75, linetype = "dotted")) +
  labs(title = "Dog breeds organized by popularity ranking", x = "Popularity rank, lower rank (further right) is better") +
  scale_color_manual(values = my_colors) # set custom color palette

#       Layer 7: GUIDES -- legends ----
  
# A legend appeared by default when we added colors because ggplot assumed (correctly) that we'd want to know how the colors related to the group types. We can customize the appearance of the legend using the guides function. Below we change the size and label (removing the "a") in the legend.
ggplot(dogs) + 
  geom_point(aes(x = popularity_all, y = breed, color = group), alpha = .5) +
  geom_text(aes(x = popularity_all, y = breed, label = breed, color = group), size = 3, vjust = "inward") +
  scale_x_reverse(limits = c(180,-10)) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        panel.grid.major.x = element_line(colour = "gray", size=0.75, linetype = "dotted")) +
  labs(title = "Dog breeds organized by popularity ranking", x = "Popularity rank, lower rank (further right) is better") +
  scale_color_brewer(palette = "Dark2") +
  guides(color = guide_legend(title = "Dog Type", override.aes = list(size = 7, alpha = .5, label = "")))


#       Layer 8: ANNOTATE -- additional geoms that are not mapped to data ----

# Just to show how this works, here we add a year at the top left with the annotate function
# Annotations don't depend on the data
ggplot(dogs) + 
  geom_point(aes(x = popularity_all, y = breed, color = group), alpha = .5) +
  geom_text(aes(x = popularity_all, y = breed, label = breed, color = group), size = 3, vjust = "inward") +
  scale_x_reverse(limits = c(180,-10)) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        panel.grid.major.x = element_line(colour = "gray", size=0.75, linetype = "dotted")) +
  labs(title = "Dog breeds organized by popularity ranking", x = "Popularity rank, lower rank (further right) is better") +
  scale_color_brewer(palette = "Dark2") +
  guides(color = guide_legend(title = "Dog Type", override.aes = list(size = 7, alpha = .5, label = ""))) +
  annotate("text", label = "2019", hjust = "left", x = 180, y = 170)
#          ^-- name of a geom    ^--- aesthetic for the geom

#       Layer 9: FACET -- sub-divide plots ----

# On first glance it doesn't seem like there's any relationship between type and popularity. However, the plot has so much text it's hard to see anything. Let's divide the plot into sub-plots using a facet argument.

# Two functions to make side-by-side plots:
#   * facet_wrap()
#   * facet_grid()

ggplot(dogs) + 
  geom_point(aes(x = popularity_all, y = breed, color = group), alpha = .5) +
  geom_text(aes(x = popularity_all, y = breed, label = breed, color = group), size = 3, vjust = "inward") +
  scale_x_reverse(limits = c(180,-10)) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        panel.grid.major.x = element_line(colour = "gray", size=0.75, linetype = "dotted")) +
  labs(title = "Dog breeds organized by popularity ranking", x = "Popularity rank, lower rank (further right) is better") +
  scale_color_brewer(palette = "Dark2") +
  guides(color = guide_legend(title = "Dog Type", override.aes = list(size = 7, alpha = .5, label = ""))) +
  facet_wrap(.~group, nrow = 7, scales = "free_y")
# scales = "free_y" tells ggplot that the y axis can be different for each plot. If I wanted the x-axis to also be plot-specific I'd set scales = "free"

# The coloring is now superfluous. Also, there is so much text here that I still don't have a good sense of whether there is a general relationship between popularity and dog type.

# We can ask ourselves whether facets were necessary here. We could have re-ordered the dogs by group in the original non-facetted plot. Facets can be very useful in some cases though. 

#   Consider summary statistics ----

# The amount of data and text in our last plot hurt our ability to see patterns. It's better to look at summary statistics to see broader patterns. 

# We'll introduce a new geometry to do this, geom_boxplot, which summarizes numeric data into a boxplot (https://en.wikipedia.org/wiki/Box_plot).

# We look at the distribution of popularity by dog type
ggplot(dogs) + geom_boxplot(aes(x = group, y = popularity))

# We we want to make the y-axis scale more interpretable. More popular should be higher.
# Let's also add meaningful labels
ggplot(dogs) + geom_boxplot(aes(x = group, y = popularity)) + 
  scale_y_reverse() +
  labs(y = "Popularity of breed type; lower rank (higher position) is more popular", x = "Dog type", title = "Distributions of breed popularity by breed type")

# Now we can see that terriers are generally les popular, but overall popularity is fairly even across dog types

# Lastly, we can further subdivide the distributions by breed size to see if there is any interaction btween type and size. Maybe for certain dog types certain sizes are preferred. 

# We use color to divide dog types visually
ggplot(dogs) + geom_boxplot(aes(x = interaction(size, group), y = popularity_all, fill = group)) 

# Following the small multiples principle of junk chars, 
# now facetting may actually be hepful in making comparisons. 
# It also allows us to remove color 
ggplot(dogs) + geom_boxplot(aes(x = size, y = popularity_all)) + 
  facet_wrap(~group) +
  scale_y_reverse() +
  labs(y = "Popularity of breed type; lower rank (higher position) is more popular", title =          "Distributions of breed popularity by breed type and size")

# Now we can see that larger hounds are more popular while smaller terriers are more popular.

#   Note about storing and saving plots ####

# We can also build up plot by storing a base plot and adding to it, e.g. 
g = ggplot(dogs) + geom_boxplot(aes(x = size, y = popularity_all)) 
g + facet_wrap(~group) + scale_y_reverse() +
  labs(y = "Popularity of breed type; lower rank (higher position) is more popular", title =          "Distributions of breed popularity by breed type and size")

# You can save the last plot you created to your working directory with:
ggsave("dogs_plot.png")

#######################################################################################
# Summary of ggplot layers
#######################################################################################
# Layer       | Description
# ----------  | -----------
# data        | A data frame to visualize
# geometry    | Geometry to represent the data visually
# statistics  | An alternative to geometry
# aesthetics  | The map or "wires" between data and geometry
# scales      | How numbers in data are converted to numbers on screen
# labels      | Titles and axis labels
# guides      | Legend settings
# annotations | Additional geoms that are not mapped to data
# facets      | Side-by-side panels
