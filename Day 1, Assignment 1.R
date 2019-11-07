# Search for Questions with Strg + F "!!!".

"
Project:  Data Visualizations in R
Author:   Philipp Kollenda

Script:   Day 1, Assignment 1
"

# --- Step 0: Settings -----------------------------------------------------------------------------
# R Options
rm(list = ls())
options(scipen = 10, stringsAsFactors = FALSE)

# Selective Execution

# Decisions

# Libraries
library(tidyverse)
library(gapm)

# --- Question 1: Simple Scatter Plot --------------------------------------------------------------
#a
ggplot(midwest, aes(x = percollege, y = percadultpoverty)) + geom_point()
#d
ggplot(midwest, aes(x = percollege, y = percadultpoverty, color = percadultpoverty, shape = state)) + geom_point()
#e
# Fix a color, assign color = "coral" in the geom_point() statement.
ggplot(midwest, aes(x = percollege, y = percadultpoverty)) + geom_point(color = "coral") + facet_grid(rows = vars(state))
# Fix a different color scheme: scale_color_gradient(low = ..., high = ...)
ggplot(midwest, aes(x = percollege, y = percadultpoverty, color = percadultpoverty)) + geom_point() + scale_color_gradient(low = "seagreen", high = "royalblue2") + facet_grid(rows = vars(state))


# --- Question 2: Simple Scatter Plot --------------------------------------------------------------
#a
ggplot(data = gapminder) + geom_point(mapping = aes(x = gdpPercap, y = lifeExp), color = "blue")

#e (Other options for scale_x are scale_x_sqrt() and scale_x_reverse())
ggplot(data = gapminder, mapping = aes(x = gdpPercap, y = lifeExp)) + geom_point(alpha = 0.4) + geom_smooth() +
  scale_x_log10()

# Mapping color to coninent, but really I only care about the points, not the smoothed line.
ggplot(data = gapminder, mapping = aes(x = gdpPercap, y = lifeExp)) + geom_point(aes(color = factor(continent)), alpha = 0.4) + geom_smooth() +
  scale_x_log10() + facet_wrap(vars(continent)) + theme(legend.position = "bottom")


# --- Question 3: Grouping line plots --------------------------------------------------------------
# Insight: facet_wrap automatically splits into rows and columns. facet_grid needs me to specify either
# all on the columns or all on the rows. The old fomulation is var ~ . for rows and . ~ var for cols.
# If you specify var ~ var then you facet on rows and columns giving a nxn design.
ggplot(data = gapminder, mapping = aes(x = year, y = gdpPercap)) + geom_line(aes(group = country)) +
  facet_grid(cols = vars(continent))

# The secrets of bar plots: The geom_bar() by default calls stat_count() which calculats the count and proportion of the x variable.
# geom_bar() by default plots count and we can get proportions with aes(y = ..prop..)
ggplot(data = gss_sm, mapping = aes(x = bigregion)) + geom_bar()

labels = scales::percent()



# --- Question 4: Bar Plots ------------------------------------------------------------------------
ggplot(data = gss_sm, mapping = aes(x = religion, color = religion)) + geom_bar()
# guides(fill = F) disables the legend for the fill aesthetic 
ggplot(data = gss_sm, mapping = aes(x = religion, fill = religion)) + geom_bar() + guides(fill = F)


ggplot(data = gss_sm, mapping = aes(x = bigregion, fill = religion)) + geom_bar(position = "fill")



# The secrets of histograms:
ggplot(data = midwest, mapping = aes(x = area, fill = state, color = state)) + geom_histogram()
ggplot(data = midwest, mapping = aes(x = area, fill = state, color = state)) + geom_density(alpha = 0.4)
ggplot(data = midwest, mapping = aes(x = area, fill = state, color = state)) + geom_freqpoly()

# Two dimensional distributions
ggplot(data = midwest, mapping = aes(x = percollege, y = percadultpoverty)) + geom_density_2d() + geom_point(alpha = 0.5)

ggplot(data = midwest, mapping = aes(x = percollege, y = percadultpoverty)) + geom_density_2d() + geom_point(alpha = 0.5) + 
  coord_cartesian(xlim = c(5, 30))




