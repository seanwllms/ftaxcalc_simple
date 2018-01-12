library(shiny)
library(tidyverse)

stded <- as_tibble(list(
  status = rep(c("Married Filing Jointly","Single","Head of Household","Married Filing Separately"), 2),
  basealt = c(rep("Base", 4), rep("Alt", 4)),
  standard_deduction = c(
    #2017 Amounts
    12700, #MFJ
    6350,  #S
    9350,  #HOH
    6350,  #MFS
    #2018 Amounts
    24000, #MFJ
    12000,  #S
    18000,  #HOH
    12000
  )
))

brackets <- read_csv("brackets.csv")


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

