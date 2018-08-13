# rm(list = ls(all=TRUE))
# setwd("~/Dashmote/Belgium Horeca Locations/SF Data merge R Project/")
# TODO: LATER
library(readxl)
library(dplyr)
set.seed(123) #123, 111, 500

file <- read_excel(path = "Strongbow Excercise of scope sent to dashmote.xlsx", sheet = "DM30k")

file <- file[!duplicated(file$`FB ID`),]

head(file)


dummy <- DM[1, c(1:length(DM))]
colnames(dummy)[colnames(dummy)=="category1"] <- "category"
dummy$category2 <- NULL
dummy$category3 <- NULL
dummy <- dummy[FALSE,]

ggg <- fromJSON("dummy.json", flatten = FALSE)
gg <- ggg$data

# names(gg) <- c("Name", "category_list", "checkins", "Likes", "FB page link", "City", "Country", "Latitude", "Longitude", "Street", "Zip", "Rating Count", "Address", "FB ID", "description", "hours", "Rating", "phone", "website")
for (i in 1:nrow(gg)) {
    fb_id <- check_null(gg$id[i])
    name <- check_null(gg$name[i])
    fb_link <- check_null(gg$link[i])
    checkins <- check_null(gg$checkins[i])
    likes <- check_null(gg$engagement$count[i])
    tot_eng <- checkins + likes
    rating_count <- check_null(gg$rating_count[i])
    rating <- check_null(gg$overall_star_rating[i])
    category <- gg$category_list[[i]]$name %>% paste('; ', sep = '', collapse = '')
    type <- check_null(gg$type[i])
    address <- check_null(gg$single_line_address[i])
    phone <- check_null(gg$phone[i])
    about <- check_null(gg$about[i])
    website <- check_null(gg$website[i])
    emails <- check_null(gg$emails[i])
    hours <- paste(gg$hours[[i]]$key, gg$hours[[i]]$value, "; ", sep = " ", collapse = "")
    weekday_opening_hours <- ""
    weekday_closing_hours <- ""
    weekend_opening_hours <- ""
    weekend_closing_hours <- ""
    price_range <- check_null(gg$price_range[i])
    description <- check_null(gg$description[i])
    services <- check_null(gg$services[i])
    walkins <- check_null(gg$restaurant_services[i,]$walkins)
    waiter <- check_null(gg$restaurant_services[i,]$waiter)
    takeout <- check_null(gg$restaurant_services[i,]$takeout)
    reserve <- check_null(gg$restaurant_services[i,]$reserve)
    pickup <- check_null(gg$restaurant_services[i,]$pickup)
    outdoor <- check_null(gg$restaurant_services[i,]$outdoor)
    kids <- check_null(gg$restaurant_services[i,]$kids)
    groups <- check_null(gg$restaurant_services[i,]$groups)
    catering <- check_null(gg$restaurant_services[i,]$catering)
    delivery <- check_null(gg$restaurant_services[i,]$delivery)
    latitude  <- check_null(gg$location$latitude[i])
    longitude  <- check_null(gg$location$longitude[i])
    city  <- check_null(gg$location$city[i])
    country  <- check_null(gg$location$country[i])
    zip  <- check_null(gg$location$zip[i])
  dummy[i,] <- c("id", "fb_id", "name", "fb_link", "checkins", "likes", "tot_eng", "rating_count", "rating", "category", "type", "address", "phone", "about", "website", "emails", "hours", "weekday_opening_hours", "weekday_closing_hours", "weekend_opening_hours", "weekend_closing_hours", "price_range", "description", "services", "walkins", "waiter", "takeout", "reserve", "pickup", "outdoor", "kids", "groups", "catering", "delivery", "latitude", "longitude", "city", "country", "zip")
}


https://graph.facebook.com/v3.0/search?type=place&q=de&zandloper&center=51.329349605353705,3.1807566854648464&distance=200&fields=name,category_list,checkins,description,fan_count,engagement,hours,link,location,overall_star_rating,phone,photos,price_range,rating_count,restaurant_services,restaurant_specialities,website,single_line_address&access_token=EAAPNYkdHBmkBANWNcZAHezVEYYRbZA5jNea5BZABNEZChoYxg73ZAtlEKp3R3zdCGE07VODqXIAmPvyXaO2nw40iw5liTYTTnEzR4LFslSfeJOgJl2QoLfRlT4dCNiZAaTuBTi0KE3ZBCI52BtnSOvC64dhbIt8IA2SbZBWbbVEzegZDZD

check_rs <- function(value) {
  if(!is.null(value)) {
    temp <- as.character()
    for (i in ncol(value)) {
      text <- toString(colnames(value[i]))
      v <- toString(value[i])
      y <- paste(text, v, ";", sep=":", collapse="")
      temp <- append(temp, y)
    }
    return (temp) }
  else { return (NA) }
}
