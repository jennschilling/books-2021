# Books I Read in 2021
# Author: Jenn Schilling
# December 2021

#### Libraries ####

library(here)
library(tidyverse)
library(extrafont)
library(ggtext)
library(janitor)
library(scales)
library(patchwork)

#### Data ####

books <- read_csv("2021_book_list.csv", lazy = FALSE) %>% clean_names()

books <- books %>%
  mutate(month_finished = factor(month_finished,
                                 levels = c("January", "February", "March",
                                            "April", "May", "June",
                                            "July", "August", "September",
                                            "October", "November", "December")))

#### Formatting ####

font <- "Franklin Gothic Book"
titlefont <- "Bookman Old Style"
fontcolor <- "gray30"
bcolor <- "#DFDFDF"

theme_set(theme_minimal(base_size = 12, base_family = font))

theme_update(
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  
  panel.background = element_rect(fill = bcolor, color = NA),
  plot.background = element_rect(fill = bcolor, color = NA),
  
  axis.title = element_text(size = 10, color = fontcolor),
  axis.text = element_text(size = 12, color = fontcolor),
  axis.ticks = element_line(color = fontcolor),
  
  axis.line = element_line(color = fontcolor),
  
  strip.text = element_text(size = 20, color = fontcolor, hjust = 0),
  
  legend.text = element_text(size = 10, color = fontcolor),
  legend.title = element_text(size = 10, color = fontcolor),
  
  plot.title.position = "plot",
  plot.title = element_markdown(size = 30, color = fontcolor, family = titlefont),
  
  plot.subtitle = element_markdown(size = 20, color = fontcolor, family = titlefont),
  
  plot.caption.position = "plot",
  plot.caption = element_markdown(size = 10, color = fontcolor),
  
  plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
)

#### Plot ####

# Test book stack plot

ggplot(data = books,
       mapping = aes(x = month_finished,
                     y = pages,
                     fill = title,
                     label = title)) +
  geom_col(color = bcolor) +
  geom_text(position = position_stack(vjust = 0.5),
            family = font) +
  scale_fill_manual(values = books$spine_color,
                    limits = books$title) +
  guides(fill = "none") 
