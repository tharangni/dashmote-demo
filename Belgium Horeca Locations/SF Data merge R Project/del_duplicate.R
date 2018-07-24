# rm(list = ls(all=TRUE))
# setwd("~/Dashmote/Belgium Horeca Locations/SF Data merge R Project/")

library(readxl)
library(dplyr)
set.seed(123) #123, 111, 500

file <- read_excel(path = "Strongbow Excercise of scope sent to dashmote.xlsx", sheet = "DM30k")

file <- file[!duplicated(file$`FB ID`),]

head(file)
