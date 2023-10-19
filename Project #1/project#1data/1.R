install.packages("dplyr")
install.packages("plyr")
install.packages("readr")
install.packages("data.table")
install.packages("XML")

library("XML")
library("methods")
library("dplyr")
library("plyr")
library("readr")
library(data.table)


library("dplyr")												
library("plyr")												
library("readr")
library("readxl")

gfg_data <- list.files(path = "C:\Users\aholl\Desktop\Data Science Portfolio\Project #1\project#1data",	
                       pattern = "*.xlsx",
                       full.names = TRUE) %>%
  lapply(read_excel) %>%										
  bind_rows													

gfg_data														
