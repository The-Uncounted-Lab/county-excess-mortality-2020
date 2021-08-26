rm(list = ls())
library(tidyverse)
library(RColorBrewer)
library(scales)
library(maps)
library(socviz)
library(ggthemes)
library(ggpmisc)
library(gridExtra)
library(ggpubr)
library(kableExtra)

datapath <- "../final_data/"
outpath <- "../output/"

blue_palette <- c("#bdd7e7", "#6baed6", "#2171b5")
blue_palette4 <- c("#bdd7e7", "#6baed6", "#3182bd", "#08519c")

plot_theme <- function() {
  color.background <- "white"
  theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = 9),
      axis.text=element_text(size=9),
      axis.title=element_text(size=12)
    ) 
}

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
