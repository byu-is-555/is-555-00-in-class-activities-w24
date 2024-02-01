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


# Facets and multi-lines --------------------------------------------------------------------------------

tips <- read_csv('https://www.dropbox.com/s/rydxlxdarjdoj7a/tips.csv?dl=1')

# Let's plot tip_percentage vs. total_bill,
# then split that across lots of categories




econ <- read_csv('https://www.dropbox.com/s/8bq9rw0rk46hru2/econ.csv?dl=1')

# Let's plot two measures over time: savings rate & unemployment weeks
# It's easiest if we pivot to make this work

