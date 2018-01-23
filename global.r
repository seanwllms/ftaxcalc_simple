library(shiny)
library(tidyverse)

#read in standard deduction info
stded <- as_tibble(list(
  status = rep(c("Married Filing Jointly","Single","Head of Household","Married Filing Separately"), 2),
  basealt = c(rep("Base", 4), rep("Alt", 4)),
  standard_deduction = c(
    #2018 Amounts (November Forecast)
    13000, #MFJ
    6500,  #S
    9550,  #HOH
    6500,  #MFS
    #2018 Amounts
    24000, #MFJ
    12000,  #S
    18000,  #HOH
    12000
  )
))


childcr <- as_tibble(list(
  status = rep(c("Married Filing Jointly","Single","Head of Household","Married Filing Separately"), 2),
  basealt = c(rep("Base", 4), rep("Alt", 4)),
  ccred_phaseout = c(
    #2018 Amounts (old law)
    110000, #MFJ
    75000,  #S
    75000,  #HOH
    55000,  #MFS
    #2018 Amounts
    400000, #MFJ
    200000,  #S
    200000,  #HOH
    200000
  )
))

#read in pease 
pease_limits <- as_tibble(list(
  status = c("Married Filing Jointly","Single","Head of Household","Married Filing Separately"),
  limit = c(320000, 266700,293350,160000)
))

#read in bracket information
brackets <- read_csv("brackets.csv")

#function to calculate tax from taxable income
calculate_tax <- function(filingstatus, fti, scen) {
  
  for (br in 1:7) {
    
    params <- filter(brackets, bracket == br, status == filingstatus, scenario == scen)
    bot <- pull(params, br_bot)
    top <- pull(params, br_top)
    rate <- pull(params, rate)
    
    tax <- max(0,min(fti-bot, top-bot)*rate)
    #print(paste0("Bracket ", as.character(br), " Tax:", as.character(tax)))
    varname <- paste0("taxb", br)
    assign(varname, tax)
    
  }
  
  tax <- taxb1 + taxb2 + taxb3 + taxb4 + taxb5 + taxb6 + taxb7
  #print(paste0("Total Tax: ", as.character(tax)))
  tax
}

#read in capital gains brackets
capgains <- read_csv("capgains.csv")

#state itemized deduction limitation
state_id_lim <- as_tibble(list(
  status = rep(c("Married Filing Jointly","Single","Head of Household","Married Filing Separately"), 2),
  basealt = c(rep("Base", 4), rep("Alt", 4)),
  id_lim = c(
    #2018 Amounts (November Forecast)
    190050, #MFJ
    190050,  #S
    190050,  #HOH
    95025,  #MFS
    #2018 Amounts
    190050, #MFJ
    190050,  #S
    190050,  #HOH
    95025  #MFS
  )
))

#function to capital gains rate from taxable income
capgains_rate <- function(filingstatus, fti, scen) {
  
  value <- filter(capgains, br_bot < fti & br_top > fti) %>% 
    filter(status == filingstatus & scenario == scen) %>% 
    pull(capgains)
  
  value
  
}

mnbrackets <- read_csv("mnbrackets.csv")

#function to calculate tax from taxable income
calculate_mntax <- function(filingstatus, fti, scen) {
  
  for (br in 1:4) {
    
    params <- filter(mnbrackets, bracket == br, status == filingstatus, scenario == scen)
    bot <- pull(params, br_bot)
    top <- pull(params, br_top)
    rate <- pull(params, rate)
    
    tax <- max(0,min(fti-bot, top-bot)*rate)
    #print(paste0("Bracket ", as.character(br), " Tax:", as.character(tax)))
    varname <- paste0("taxb", br)
    assign(varname, tax)
    
  }
  
  tax <- taxb1 + taxb2 + taxb3 + taxb4 

  tax
}

