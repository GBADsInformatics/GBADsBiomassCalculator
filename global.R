library(shiny)
library(dplyr)
library(fresh)
library(shinyWidgets)
library(shinyBS)
library(shinyjs)
library(jsonlite)
library(markdown)
library(EnvStats)
library(ggplot2)
library(ggh4x)

source("./rpert.R")

profiles <- read_json("./Data/profiles.json")

#TLU ratios adopted from: http://www.fao.org/3/t0828e/T0828E07.htm
ratios <- data.frame(
  item = c("Asses", "Camels", "Cattle", "Chickens","Goats", "Horses", "Mules and hinnies", "Sheep", "Cattle and Buffaloes", "Poultry Birds", "Sheep and Goats", "Swine / pigs"),
  tlu_ratio = c(0.5, 1.25, 0.7, 0.01, 0.1, 0.8, 0.7, 0.1, 0.7, 0.01, 0.1, 0.2)
)

pntInputForm <- function(){
  list(
    numericInput("pntPop", "Popualtion:", min = 0, max = 10000000, step = 100000, value = NULL),
    selectInput("pntSpecies", "Select Species:", choices = unique(ratios$item)),
    align = "center"
  )
}

titleAndHelp <- function(pageName) {
  fluidRow(
    column(
      width = 12,
      div(
        style = "display: inline-block; vertical-align: top;",
        h2("Biomass Calculator")
      ),
      div(
        style = "display: inline-block; vertical-align: top; margin-left: 10px;",
        actionBttn(paste(pageName,"Help", sep = ""), icon("circle-info"), size = "xs", style = "pill", color = "warning")
      )
    )
  )
}

m3_input <- function(label, sex, age, status) {
  fluidRow(
    column(2,
           h5(strong(label), style = "margin-top:30px; text-align: right;")
    ),
    column(2,
           numericInput(paste0(sex,"_",age,"_",status,"_pop"), "Population", min = 0, max = 1000000000, step = 100000, value = NULL),
           style = "padding: 2px;"
    ),
    column(7,
           column(4,
                  numericInput(paste0(sex,"_",age,"_",status,"_lw_mode"), "Mode", min = 0, max = 15000, step = 100, value = NULL),
                  style = "padding: 2px"
           ),
           column(4,
                  numericInput(paste0(sex,"_",age,"_",status,"_lw_min"), "Min", min = 0, max = 15000, step = 100, value = NULL),
                  style = "padding: 2px"
           ),
           column(4,
                  numericInput(paste0(sex,"_",age,"_",status,"_lw_max"), "Max", min = 0, max = 15000, step = 100, value = NULL),
                  style = "padding: 2px"
           )
    ),
    column(1, 
           uiOutput(paste0(sex,"_",age,"_",status,"_check"), style = "margin-top:30px;")
           )
  )
}

