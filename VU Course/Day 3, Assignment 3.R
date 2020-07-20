# Search for Questions with Strg + F "!!!".

"
Project:  Data Visualizations in R
Author:   Philipp Kollenda

Script:   Day 3, Assignment 3
"

# --- Step 0: Settings -----------------------------------------------------------------------------
# R Options
rm(list = ls())
options(scipen = 10, stringsAsFactors = FALSE)

# Selective Execution

# Decisions

# Libraries
library(tidyverse)
library(plotly)
library(ggforce)

# Import
survey <- readRDS("survey_data")

# --- Chapter 1  ---------------------------------------------------------------------
m <- highlight_key(mpg)
p <- ggplot(m, aes(displ, hwy)) + geom_point()
gg <- highlight(ggplotly(p), "plotly_selected")
crosstalk::bscols(gg, DT::datatable(m))


plot_ly(diamonds, x = ~cut)
plot_ly(diamonds, x = ~cut, y = ~clarity)
plot_ly(diamonds, x = ~cut, color = ~clarity, colors = "Accent")

diamonds %>%
  plot_ly(x = ~cut) %>% 
  add_histogram() %>%
  group_by(cut) %>%
  summarise(n = n()) %>%
  add_text(
    text = ~n, y = ~n, 
    textposition = "top middle", 
    cliponaxis = FALSE
  )

ggplot(diamonds, aes(x = log(price), color = clarity)) + 
  geom_density()
ggplotly(p)

ggplot(mpg, aes(x=drv, y=hwy, color=drv)) +
  ggforce::geom_sina(alpha = 0.7)



# --- Chapter 3 ---------------------------------------------------------------------
data(economics, package = "ggplot2")

# sort economics by psavert, just to 
# show difference between paths and lines
p <- economics %>%
  arrange(psavert) %>%
  plot_ly(x = ~date, y = ~psavert)

add_paths(p)
g <- add_lines(p)

p <- economics %>% arrange(psavert) %>% ggplot(aes(x = date, y = psavert)) + geom_line()
ggplotly(p, dynamicTicks = TRUE)


# --- Chapter 4 ---------------------------------------------------------------------
Sys.setenv('MAPBOX_TOKEN' = 'pk.eyJ1IjoicGtvbGxlbmRhIiwiYSI6ImNrNDJybXFwOTAwOXcza215cGR0bXYwb2IifQ.MP2xBLwlZdvppIJJSx4dfw')

plot_mapbox(maps::canada.cities) %>%
  add_markers(
    x = ~long, 
    y = ~lat, 
    size = ~pop, 
    color = ~country.etc,
    colors = "Accent",
    text = ~paste(name, pop),
    hoverinfo = "text"
  )

layout(
  plot_mapbox(), 
  mapbox = list(style = "satellite")
)


# --- Chapter 8 ---------------------------------------------------------------------
plot_ly(mpg, x = ~cty, y = ~hwy, z = ~cyl) %>%
  add_markers(color = ~cyl)


# --- Chapter 10 ---------------------------------------------------------------------
saveWidget(g, file = "test.html")


