# rm(list = ls(all=TRUE))
# setwd("../Belgium Horeca Locations/SF Data merge R Project/")

library(readxl)
library(tidyr)

# read_file <- read_excel(path = "matched_AM_DB_30k.xlsx", sheet = "Sheet1")
# temp_m <- subset(read_file, fbid == "No Match", select = colnames(read_file))
# temp_2m <- temp_m[sample(1:nrow(temp_m), 40, replace=FALSE),]

set.seed(123) #123, 111, 500

DM <- read_excel("Strongbow Excercise of scope sent to dashmote.xlsx", 
                 sheet = "DM30k")
DM <- DM[!duplicated(DM$`FB ID`),]


dummy <- DM[1, c(1:length(DM))]
colnames(dummy)[colnames(dummy)=="category1"] <- "category"
dummy$category2 <- NULL
dummy$category3 <- NULL
dummy <- dummy[FALSE,]
d <- dummy


json_df <- data.frame()

#First JSON object creation

g_js <- fromJSON("search.json", flatten = TRUE)
gg <- g_js$data #17 cols - after flattening 33

json_df <- rbind(json_df, gg) 

#Second JSON object creation

g_js <- fromJSON("dummy.json", flatten = TRUE)
gg <- g_js$data #15 cols - after flattening 23

ifelse(ncol(json_df) > ncol(gg), 
       absent <-  setdiff(colnames(json_df), colnames(gg)),
       absent <-  setdiff(colnames(gg), colnames(json_df)))
gg[, absent] <- NA

json_df <- rbind(json_df, gg)


# df.lst <- lapply(df.lst, function(df) {
#   df[, setdiff(col, names(df))] <- NA
#   df
# })


check_null <- function(value) {
  if (!is.null(value)) { return (value) }
  else { return (NA) }
}

# check_rs <- function(walkins, waiter, takeout, reserve, pickup, outdoor, kids, groups, catering, delivery) {
#    
# }

# pass argument as df[i,]
check_rs <- function(irow) {
  walkins <- check_null(irow$restaurant_services.walkins)
  waiter <- check_null(irow$restaurant_services.waiter)
  takeout <- check_null(irow$restaurant_services.takeout)
  reserve <- check_null(irow$restaurant_services.reserve)
  pickup <- check_null(irow$restaurant_services.pickup)
  outdoor <- check_null(irow$restaurant_services.outdoor)
  kids <- check_null(irow$restaurant_services.kids)
  groups <- check_null(irow$restaurant_services.groups)
  catering <- check_null(irow$restaurant_services.catering)
  delivery <- check_null(irow$restaurant_services.delivery)
  services <- paste("walkins:", walkins, ";waiter:", waiter, ";takeout:", takeout, ";reserve:", reserve, ";pickup:", pickup, ";outdoor:", outdoor, ";kids:", kids, ";groups:", groups, ";catering:", catering, ";delivery:", delivery, sep="", collapse = "")
  services_list <- list("services" = services, "walkins" = walkins, "waiter" = waiter, "takeout"= takeout, "reserve" = reserve, "pickup" = pickup, "outdoor" = outdoor, "kids" = kids, "groups" = groups, "catering" = catering, "delivery" = delivery)
  return (services_list)
}

check_hours <- function(hours) {
  if(!is.null(hours)) {
    return (paste(hours$key, hours$value, ", ", sep = " ", collapse = ""))
  }
  else { return (NA) }
}

# names(gg) <- c("Name", "category_list", "checkins", "Likes", "FB page link", "City", "Country", "Latitude", "Longitude", "Street", "Zip", "Rating Count", "Address", "FB ID", "description", "hours", "Rating", "phone", "website") 

if (is.null(nrow(gg))) {next}

# for (i in 1:nrow(gg)) {
#   id <- paste("#",i+sample(30164:40164, 1, replace=FALSE),sep = "")
#   fb_id <- check_null(gg$id[i])
#   name <- check_null(gg$name[i])
#   fb_link <- check_null(gg$link[i])
#   checkins <- check_null(gg$checkins[i])
#   likes <- check_null(gg$engagement$count[i])
#   tot_eng <- checkins + likes
#   rating_count <- check_null(gg$rating_count[i])
#   rating <- check_null(gg$overall_star_rating[i])
#   category <- gg$category_list[[i]]$name %>% paste(';', sep = '', collapse = '')
#   type <- check_null(gg$type[i])
#   address <- check_null(gg$single_line_address[i])
#   phone <- check_null(gg$phone[i])
#   about <- check_null(gg$about[i])
#   website <- check_null(gg$website[i])
#   emails <- check_null(gg$emails[i])
#   hours <- check_hours(gg$hours[[i]])
#   weekday_opening_hours <- check_null(gg$hours[[i]][1,]$value)
#   weekday_closing_hours <- check_null(gg$hours[[i]][2,]$value)
#   weekend_opening_hours <- check_null(gg$hours[[i]][nrow(gg$hours[[i]])-1,]$value)
#   weekend_closing_hours <- check_null(gg$hours[[i]][nrow(gg$hours[[i]]),]$value)
#   price_range <- check_null(gg$price_range[i])
#   description <- check_null(gg$description[i])
#   services <- check_rs(gg$restaurant_services[i,])
#   walkins <- check_null(gg$restaurant_services[i,]$walkins)
#   waiter <- check_null(gg$restaurant_services[i,]$waiter)
#   takeout <- check_null(gg$restaurant_services[i,]$takeout)
#   reserve <- check_null(gg$restaurant_services[i,]$reserve)
#   pickup <- check_null(gg$restaurant_services[i,]$pickup)
#   outdoor <- check_null(gg$restaurant_services[i,]$outdoor)
#   kids <- check_null(gg$restaurant_services[i,]$kids)
#   groups <- check_null(gg$restaurant_services[i,]$groups)
#   catering <- check_null(gg$restaurant_services[i,]$catering)
#   delivery <- check_null(gg$restaurant_services[i,]$delivery)
#   latitude  <- check_null(gg$location$latitude[i])
#   longitude  <- check_null(gg$location$longitude[i])
#   city  <- check_null(gg$location$city[i])
#   country  <- check_null(gg$location$country[i])
#   zip  <- check_null(gg$location$zip[i])
#   
#   dummy[i,] <- c(id, fb_id, name, fb_link, checkins, likes, tot_eng, rating_count, rating, category, type, address, phone, about, website, emails, hours, weekday_opening_hours, weekday_closing_hours, weekend_opening_hours, weekend_closing_hours, price_range, description, services, walkins, waiter, takeout, reserve, pickup, outdoor, kids, groups, catering, delivery, latitude, longitude, city, country, zip)
# }
# d <- rbind(d, dummy)