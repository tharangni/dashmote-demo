rm(list = ls(all=TRUE))
# setwd("~/Dashmote/Belgium Horeca Locations/SF Data merge R Project/")

library(readxl)

set.seed(123) #123, 111, 500

read_file <- read_excel(path = "matched_AM_DB.xlsx", sheet = "Sheet1")
temp_m <- subset(read_file, fbid == "No Match", select = colnames(read_file))
temp_2m <- temp_m[sample(1:nrow(temp_m), 20, replace=FALSE),]
