rm(list = ls(all=TRUE))
# setwd("../Belgium Horeca Locations/SF Data merge R Project/")

# library(readxl)

# read_file <- read_excel(path = "matched_AM_DB_30k.xlsx", sheet = "Sheet1")
# temp_m <- subset(read_file, fbid == "No Match", select = colnames(read_file))
# temp_2m <- temp_m[sample(1:nrow(temp_m), 40, replace=FALSE),]

set.seed(123) #123, 111, 500

DM <- read_excel("Strongbow Excercise of scope sent to dashmote.xlsx", 
                 sheet = "DM30k")

dummy <- DM[1, c(1:length(DM))]
colnames(dummy)[colnames(dummy)=="category1"] <- "category"
dummy$category2 <- NULL
dummy$category3 <- NULL
dummy <- dummy[FALSE,]
d <- dummy
ggg <- fromJSON("d.json", flatten = FALSE)
gg <- ggg$data

check_null <- function(value) {
  if (!is.null(value)) { return (value) }
  else { return (NA) }
}

# names(gg) <- c("Name", "category_list", "checkins", "Likes", "FB page link", "City", "Country", "Latitude", "Longitude", "Street", "Zip", "Rating Count", "Address", "FB ID", "description", "hours", "Rating", "phone", "website") 

if (is.null(nrow(gg))) {next}

for (i in 1:nrow(gg)) {
  id <- paste("#",i+sample(30164:40164, 1, replace=FALSE),sep = "")
  fb_id <- gg$id[i]
  name <- gg$name[i]
  fb_link <- gg$link[i]
  checkins <- gg$checkins[i]
  likes <- gg$engagement$count[i]
  tot_eng <- checkins + likes
  rating_count <- gg$rating_count[i]
  rating <- gg$overall_star_rating[i]
  category <- gg$category_list[[i]]$name %>% paste('; ', sep = '', collapse = '')
  type <- ""
  address <- gg$single_line_address[i]
  phone <- gg$phone[i]
  about <- ""
  website <- check_null(gg$website[i])
  emails <- ""
  hours <- paste(gg$hours[[i]]$key, gg$hours[[i]]$value, "; ", sep = " ", collapse = "")
  weekday_opening_hours <- "" 
  weekday_closing_hours <- ""
  weekend_opening_hours <- ""
  weekend_closing_hours <- ""
  price_range <- ""
  description <- check_null(gg$description[i])
  services <- ""
  walkins <- ""
  waiter <- ""
  takeout <- ""
  reserve <- ""              
  pickup <- ""
  outdoor <- ""
  kids <- ""                 
  groups <- ""
  catering <- ""
  delivery <- ""   
  latitude  <- gg$location$latitude[i]
  longitude  <- gg$location$longitude[i]
  city  <- gg$location$city[i]
  country  <- gg$location$country[i]
  zip  <- gg$location$zip[i]
  t <- c(id, fb_id, name, fb_link, checkins, likes, tot_eng, rating_count, rating, category, type, address, phone, about, website, emails, hours, weekday_opening_hours, weekday_closing_hours, weekend_opening_hours, weekend_closing_hours, price_range, description, services, walkins, waiter, takeout, reserve, pickup, outdoor, kids, groups, catering, delivery, latitude, longitude, city, country, zip)
  names(t) <- c("id", "fb_id", "name", "fb_link", "checkins", "likes", "tot_eng", "rating_count", "rating", "category", "type", "address", "phone", "about", "website", "emails", "hours", "weekday_opening_hours", "weekday_closing_hours", "weekend_opening_hours", "weekend_closing_hours", "price_range", "description", "services", "walkins", "waiter", "takeout", "reserve", "pickup", "outdoor", "kids", "groups", "catering", "delivery", "latitude", "longitude", "city", "country", "zip")
  # dummy = rbind(dummy, t)
  
  dummy[i,] <- c(id, fb_id, name, fb_link, checkins, likes, tot_eng, rating_count, rating, category, type, address, phone, about, website, emails, hours, weekday_opening_hours, weekday_closing_hours, weekend_opening_hours, weekend_closing_hours, price_range, description, services, walkins, waiter, takeout, reserve, pickup, outdoor, kids, groups, catering, delivery, latitude, longitude, city, country, zip)
}
d <- rbind(d, dummy)