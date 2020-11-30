###################
# ui.R
# 
# Initializes the ui. 
# Used to load in your header, sidebar, and body components.
###################
library(shinydashboard)

library("rtweet")
library("reactable")
library("glue")
library("stringr")
library("httpuv")
library("dplyr")
library("purrr")

library("shiny")
library("shinythemes")

source('./components/header.R')
source('./components/sidebar.R')
source('./components/body.R')


ui <- dashboardPage(
  skin = "black",
  
  header = header,
  sidebar =  sidebar,
  body = body)
