##-- Packages ----
##-- + Handling ----
library(dplyr)
library(stringr)
##-- + Shiny ----
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyBS)
library(shinyjs)
library(shinycssloaders)
##-- + Plots ----
library(ggplot2)
library(ggthemes)
##-- + md Files ----
library(markdown)
##-- + Others ----
library(mosaic) #devtools::install_version(package = "mosaic", version = "0.10.0")

##-- Sources ----
source("R/utils.R")
source("R/functions.R")

##-- General app ----
##-- + Box colors ----
colors <- tableau_color_pal(palette = "Tableau 10")(10)
col_axis <- grey(0.4)
col_title <- grey(0.2)
col_back <- "white"
size_point <- 2
size_line <- 1.3

gg_tema <- theme_stata() +
  theme(axis.title = element_text(size = 12, margin = margin(10, 10, 0, 0)),
        axis.text = element_text(size = 12, colour = col_axis),
        panel.background = element_rect(fill = col_back),
        plot.background = element_rect(fill = col_back))

##-- + uis e servers ----
uis <- list.files(path = "tabs", pattern = "ui", recursive = T, full.names = T)
uis_order <- str_count(string = uis, pattern = "/") %>% 
  order(decreasing = T)

lapply(uis[uis_order], source)

##-- + globals ----
source("tabs/gibbs/gibbs_global.R")
