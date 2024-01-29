# TYPES OF GEOMETRIES ("GEOMs")
# 
# Single Variable
#   Distributions of continuous
#     geom_histogram() <- histogram (distribution of continuous variable)
#     geom_density() <- Generic curve of distribution, good for comparing things of different sizes (but can be misleading!)
#   Distributions of discrete
#     geom_bar() <- when used with just one categorical, makes a super basic bar chart (counts of discretes)
# 
# Two Variables
#   Continuous by categories
#     geom_bar() <- when used with both x and y, summarizes continuous (y) by category (x)
#     geom_violin() <- combines distribution and relative position of continuous by category
#   Continuous by Continuous
#     geom_point() <- scatter plot, standard "show the relationship between x and y" chart
#     geom_line() <- basic line graph. Most useful: show changes over time or across a cumulative population.  
# 
# MAPPINGS (AESTHETICS, defined inside the aes() )
#   These are ways of "mapping" variables to characteristics of your chart to columns in your dataset.
#     Aside from establishing the main (usually x,y) data driving your plot, additional aesthetic mappings
#     can be SUPER helpful in highlighting other relevant groupings or trends from other columns.
#     Example: You set x=height and y=weight for a scatter plot, but then "map" sex to define each point's color
# 
# Important aesthetics (mappings)
#   x, y <- Specify which columns should be used for x and y. At least one is required, usually two.
#   color, fill <- most useful for overlaying additional categories
#   alpha, shape, labels, size  <- Can be useful, but I don't use them nearly as much
#   
# ATTRIBUTES (static features of individual Geoms)
#   Don't confuse these with aesthetic mappings! The key difference is that aesthetics are "mapped" to columns
#     in the dataset such that different values in those columns create differences in the appearance of the
#     aesthetic. Attributes, on the other hand, are typically static aspects used to customize a given geom. 
#     Example: Income bracket is mapped to the color aesthetic such that lower incomes are dark blue and higher
#       incomes are light blue. But then size is provided as ATTRIBUTES of the geom_point, which simply sets the 
#       size of the scatter plot points (i.e., all points would be the same size).
# 
# General attributes useful across most geoms:
#   position <- defines how to arrange groups that overlap.
#       Most relevant: identity, stack, dodge
#   color <- defines the color of the elements produced by that geom
#   alpha <- defines the opacity (or "see-through-ness") of the elements produced by that geom
# FACETS
#   Facets allow you to use categories within your data to line up similar
#   charts next to each other, which can be a really useful way to highlight
#   differences between groups.
# 
# The two main facet functions are:
#   facet_wrap() <- usually best used to show a series of plots across a 
#                  single category variable, with you specify with `~varable`.
#                  This will show one plot per level of the category.
#   facet_grid() <- My favorite to show a "grid" of facets, with rows and 
#                  columns defined by two different variables. This is specified
#                  using the facet formula `var1~var2`
#       Note: Pay attention to the `scales` parameter in these facet functions, 
#             where you can specify whether you want to hold the x and/or y
#             axis scales constant across all of the facets.
# 

library(tidyverse)

penguins <- read_csv('https://www.dropbox.com/s/65jmvgoed4i0r57/penguins.csv?dl=1')

penguins %>% 
  glimpse

# First: one-variable plots

# histogram with body_mass_g
# add species as fill color, pay attention to position



# density curves are sometimes nicer. Let's do the above but with 
# a density curve. Alpha is nice here.



# bar charts with categoricals: quick, easy counts summary
# stacked vs. side-by-side






# Second: two-variable plots (relationships)
# Numeric + categorical

# Violin plots are cool. Lets look at body mass across islands
# Notice what happens when we add color now


# Bar plots with both x and  y are a bit more flexible.
# summary will give us means






# Numeric + numeric

# The classic scatter
# Important: attributes vs aesthetics. Inheritance





# More inheritance general vs. local aesthetics: if we have time
# flipper vs. body mass generally, species mapped to color, then add a smoother


# versus: mapping the color specific to the scatter points only


