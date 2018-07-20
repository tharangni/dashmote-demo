# rm(list = ls(all=TRUE))
set.seed(123)
read_file <- read_excel(path = "Belgium Horeca Locations/SF Data merge R Project/matched_AM_DB.xlsx", sheet = "Sheet1")
temp <- subset(read_file, fbid == "No Match", select = colnames(read_file))
temp2 <- temp[sample(1:nrow(temp), 20, replace=FALSE),]
