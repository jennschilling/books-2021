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
library(forcats)
library(ggfittext)

#### Data ####

books <- read_csv("2021_book_list.csv", lazy = FALSE) %>% clean_names()

books <- books %>%
  mutate(month_finished = factor(month_finished,
                                 levels = c("January", "February", "March",
                                            "April", "May", "June",
                                            "July", "August", "September",
                                            "October", "November", "December")),
         title_size = ifelse(title == "W.E.B. Du Bois's Data Portraits: Visualizing Black America" |
                             title == "The Wall Street Journal Guide to Information Graphics" |
                             title == "The Body Is Not An Apology", 1, 2),
         title_wrap = ifelse(title == "W.E.B. Du Bois's Data Portraits: Visualizing Black America" |
                             title == "The Wall Street Journal Guide to Information Graphics" |
                             title == "The Body Is Not An Apology",
                             str_wrap(title, width = 24), str_wrap(title, width = 18)))

#### Formatting ####

font <- "Franklin Gothic Book"
titlefont <- "Bookman Old Style"
fontcolor <- "gray10"
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

genre_pal <- c("#3A4B5B", # fantasy
               "#89B3A7", # fiction
               "#E48F7B", # historical fiction
               "#A93343", # horror
               "#2B409F", # memoir
               "#698DA7", # mystery
               "#828284", # nonfiction
               "#C7453E", # poetry
               "#D3ACB6", # romance
               "#DF913E", # science fiction
               "#486344", # self help
               "#418472") # young adult

#### Plot ####

# Book Stack

ggplot(data = books,
       mapping = aes(x = month_finished,
                     y = pages,
                     fill = genre,
                     label = title)) +
  geom_col(color = bcolor) +
  geom_fit_text(reflow = TRUE,
                contrast = TRUE,
                position = "stack",
                family = font,
                lineheight = 0.8) +
  scale_fill_manual(values = genre_pal) +
  guides(size = "none",
         fill = "none") +
  coord_cartesian(expand = FALSE,
                  clip = "off") +
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank())

# Genre Bar + Legend

ggplot(data = books,
       mapping = aes(y = fct_rev(fct_infreq(genre)),
                     fill = genre)) +
  geom_bar() +
  geom_bar_text(contrast = TRUE,
                stat = "count", 
                aes(label = ..count..),
                family = font) +
  scale_fill_manual(values = genre_pal) +
  coord_cartesian(expand = FALSE,
                  clip = "off") +
  guides(fill = "none") +
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.title = element_blank())
