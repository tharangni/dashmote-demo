# rm(list = ls(all=TRUE)) #!caution
# setwd("../Belgium Horeca Locations/SF Data merge R Project/")

library(xml2)
library(httr)
library(dplyr)
library(readxl)
library(stringr)
library(jsonlite)
library(urltools)
library(stringdist)

set.seed(123) #123, 111, 500


# file should already exist in directory. else download from onedrive: https://1drv.ms/x/s!AreiVpMzAPHtiKw6bOkp1F46k0V5gQ
DM <- read_excel("Strongbow Excercise of scope sent to dashmote.xlsx", 
                 sheet = "DM30k")
DM <- DM[!duplicated(DM$`FB ID`),]

read_file <- read_excel(path = "matched_AM_DB_30k.xlsx", sheet = "Sheet1")
no_match <- subset(read_file, fbid == "No Match", select = colnames(read_file))

# no_match$latitude <- str_split_fixed(no_match$Shipping_GeoLocation, ",", 2)[,1]
# no_match$longitude <- str_split_fixed(no_match$Shipping_GeoLocation, ",", 2)[,2]

temp <- no_match[sample(1:nrow(no_match), 25, replace=FALSE),]

user_access_token = "EAAFDEAzmizcBAG7sZAu0ZCD0HuZBRyKrw3ezkHVYBZBoY17DDPwGpZATHm8wwhDvOaVkXc251j5ZA8e2I6PbSY7ZCA9Y4jmANpWFOApK2cF1BZCh2LkijOxSJXCHSdsjRoyg9TmJBVasQSRZCZApTF3NkTDjcwivB6JZAmDrwn45XPqAjEr4dBekUZAf"

dummy <- DM[1, c(1:length(DM))]
colnames(dummy)[colnames(dummy)=="category1"] <- "category"
dummy$category2 <- NULL
dummy$category3 <- NULL
dummy <- dummy[FALSE,]
d <- dummy
# rm(DM)



###### FUNCTIONS NECESSARY FOR MANIPULATION ######

check_null <- function(value) {
  if (is.null(value) || is.na(value)) { return (NA) }
  else { return (value) }
}

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
  if(is.null(hours) || is.na(hours)) {
    hours <- NA
    w_op <- NA
    w_ed <- NA
    wn_op <- NA
    wn_ed <- NA
    hour_list <- list("hours" = hours, "w_op" = w_op, "w_ed" = w_ed, "wn_op" = wn_op, "wn_ed" = wn_ed)
    return (hour_list) }
  else { 
    # tryCatch(df$hours[[2]][1,]$value, error = function(x){return(0)})
    w_op <- check_null(df$hours[[i]][1,]$value)
    w_ed <- check_null(df$hours[[i]][2,]$value)
    wn_op <- check_null(df$hours[[i]][nrow(df$hours[[i]])-1,]$value)
    wn_ed <- check_null(df$hours[[i]][nrow(df$hours[[i]]),]$value)
    hours <- paste(hours$key, hours$value, ", ", sep = " ", collapse = "")
    hour_list <- list("hours" = hours, "w_op" = w_op, "w_ed" = w_ed, "wn_op" = wn_op, "wn_ed" = wn_ed)
    return (hour_list) 
  }
}


###### RUN EXTRACTION FIRST ######


##facebook extracting part

json_df <- data.frame()

for (name in temp$`Account Name`) {
  converted_name <- url_encode(name)
  gps_full <- temp$Shipping_GeoLocation[temp$`Account Name`==name]
  print(paste(name, " ", converted_name, gps_full))
  h1 <- handle('')
  graph_url <- paste("https://graph.facebook.com/v3.0/search?type=place&q=",converted_name,"&center=",gps_full,"&distance=500&fields=name,category_list,checkins,description,fan_count,engagement,hours,link,location,overall_star_rating,phone,photos,price_range,rating_count,restaurant_services,restaurant_specialities,website,single_line_address&access_token=",user_access_token,sep="")
  # print(graph_url)
  # result_html <- tryCatch(read_html(graph_url),error=function(x){return(0)})
  # if(is.numeric(result_html)){next}
  
  # result_html <- GET(url = graph_url, handle = h1, add_headers(c("user-agent"='Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/64.0.3282.186 Safari/537.36')))
  
  graph_json <- fromJSON(graph_url, flatten = TRUE)
  gg <- graph_json$data
  print(length(colnames(gg)))
  # print(length(setdiff(colnames(gg), colnames(json_df))))
  # print(length(setdiff(colnames(json_df), colnames(gg))))
  
  if (is.null(nrow(gg))) {next}
  else if (nrow(json_df) == 0) {
    json_df <- rbind(json_df, gg)
    next }
  
  if(ncol(json_df) > ncol(gg)){
    # print("1-if")
    absent <-  setdiff(colnames(json_df), colnames(gg))
    gg[, absent] <- NA
    
  }
  
  if(ncol(json_df) < ncol(gg)){
    # print("2-if")
    absent <-  setdiff(colnames(gg), colnames(json_df))
    json_df[, absent] <- NA
  }
  
  
  # to deal with multiple results
  if(nrow(gg) > 1) {
    
    gps <- str_split(gps_full,",")
    latMUL <- gps[[1]][1] %>% as.numeric()
    longMUL <- gps[[1]][2] %>% as.numeric()
    
    del_lat <- 0.00165
    del_long <- 0.00165
    
    xp <- latMUL + del_lat
    xn <- latMUL - del_lat
    yp <- longMUL + del_long
    yn <- longMUL - del_long
    
    # make an object which contains the gps coords that only lie within the specific range from temp table
    pos <- which(gg$location.longitude < yp & gg$location.longitude > yn &  gg$location.latitude < xp &  gg$location.latitude > xn)
    close_locations <- gg[pos,]
    gg <- close_locations
    # rm(close_locations)
  }
  
  json_df <- rbind(json_df, gg)
}

saveRDS(json_df, file="data25.Rda")



###### RUN MANIPULATING PART AFTER SAVING THE EXTRACTED DATA ######

## manipulating part

# df <- readRDS(file="data.Rda")
# 
# for (i in 1:nrow(df)) {
#   id <- paste("#",i+sample(30164:40164, 1, replace=FALSE),sep = "")
#   fb_id <- check_null(df$id[i])
#   name <- check_null(df$name[i])
#   fb_link <- check_null(df$link[i])
#   checkins <- check_null(df$checkins[i])
#   likes <- check_null(df$engagement.count[i])
#   tot_eng <- checkins + likes
#   rating_count <- check_null(df$rating_count[i])
#   rating <- check_null(df$overall_star_rating[i])
#   category <- df$category_list[[i]]$name %>% paste(';', sep = '', collapse = '')
#   type <- check_null(df$type[i])
#   address <- check_null(df$single_line_address[i])
#   phone <- check_null(df$phone[i])
#   about <- check_null(df$about[i])
#   website <- check_null(df$website[i])
#   emails <- check_null(df$emails[i])
# 
#   # putting the 4 hours variables in a function where only hours is TRUE
#   hours_list <- check_hours(df$hours[[i]])
#   hours <- hours_list$hours
#   weekday_opening_hours <- hours_list$w_op
#   weekday_closing_hours <- hours_list$w_ed
#   weekend_opening_hours <- hours_list$wn_op
#   weekend_closing_hours <- hours_list$wn_ed
# 
#   price_range <- check_null(df$price_range[i])
#   description <- check_null(df$description[i])
# 
#   # Extract a list of serivices
#   services_list <- check_rs(df[i,])
#   services <- services_list$services
#   walkins <- services_list$walkins
#   waiter <- services_list$waiter
#   takeout <- services_list$takeout
#   reserve <- services_list$reserve
#   pickup <- services_list$pickup
#   outdoor <- services_list$outdoor
#   kids <- services_list$kids
#   groups <- services_list$groups
#   catering <- services_list$catering
#   delivery <- services_list$delivery
# 
#   latitude  <- check_null(df$location.latitude[i])
#   longitude  <- check_null(df$location.longitude[i])
#   city  <- check_null(df$location.city[i])
#   country  <- check_null(df$location.country[i])
#   zip  <- check_null(df$location.zip[i])
#   # dummy[i,] <- c("id", "fb_id", "name", "fb_link", "checkins", "likes", "tot_eng", "rating_count", "rating", "category", "type", "address", "phone", "about", "website", "emails", "hours", "weekday_opening_hours", "weekday_closing_hours", "weekend_opening_hours", "weekend_closing_hours", "price_range", "description", "services", "walkins", "waiter", "takeout", "reserve", "pickup", "outdoor", "kids", "groups", "catering", "delivery", "latitude", "longitude", "city", "country", "zip")
#   dummy[i,] <- c(id, fb_id, name, fb_link, checkins, likes, tot_eng, rating_count, rating, category, type, address, phone, about, website, emails, hours, weekday_opening_hours, weekday_closing_hours, weekend_opening_hours, weekend_closing_hours, price_range, description, services, walkins, waiter, takeout, reserve, pickup, outdoor, kids, groups, catering, delivery, latitude, longitude, city, country, zip)
# }
# d <- rbind(d, dummy)