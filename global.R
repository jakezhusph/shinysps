
library(plotly)
library(shiny)
library(knitr)
library(spatstat)
library(concaveman)
library(sf)
library(sp)
library(ggpubr)
library(viridis)
library(shinydashboard)
library(dashboardthemes)
library(shinyBS)

theme_set(theme_bw())

source("utilies_func.R")
source("theme_func.R")

vv = NULL