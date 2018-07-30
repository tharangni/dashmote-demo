# rm(list = ls(all=TRUE)) #!caution

library(xml2)
library(httr)
library(dplyr)
library(readxl)
library(stringr)
library(jsonlite)
library(stringdist)

set.seed(123) #123, 111, 500

DM <- read_excel("Strongbow Excercise of scope sent to dashmote.xlsx", 
                 sheet = "DM30k")

read_file <- read_excel(path = "matched_AM_DB_30k.xlsx", sheet = "Sheet1")
no_match <- subset(read_file, fbid == "No Match", select = colnames(read_file))

# no_match$latitude <- str_split_fixed(no_match$Shipping_GeoLocation, ",", 2)[,1]
# no_match$longitude <- str_split_fixed(no_match$Shipping_GeoLocation, ",", 2)[,2]

temp <- no_match[sample(1:nrow(no_match), 5, replace=FALSE),]

user_access_token = "EAACD8nPksS4BAFYthqubRjJTO0n6RCu0iZCnzQoL9t7MZAwh7PaRid45LfD5FAfREAXgGoftiJ6RqCR6kvIdIZBMPbXU3LQTZBvfPFOjtFiA58bpjTqeDcomDgoGPx27pVvZA4i8OAlbU8tufW528rtG7BDGyIdDUx8fykJkwndU0teOa3lKh7GyVn8W9onGZBCZCQm1B3MKAZDZD"

dummy <- DM[1, c(1:length(DM))]
colnames(dummy)[colnames(dummy)=="category1"] <- "category"
dummy$category2 <- NULL
dummy$category3 <- NULL
dummy <- dummy[FALSE,]
d <- dummy
# rm(DM)

# facebook part
for (name in temp$`Account Name`) { 
  converted_name <- name %>% gsub("[^a-zA-Z0-9]", "&", .) %>% tolower()
  print(paste(name, " ", converted_name, temp$Shipping_GeoLocation[temp$`Account Name`==name]))
  h1 <- handle('')
  graph_url <- paste("https://graph.facebook.com/v3.0/search?type=place&q=",urltools::url_encode(name),"&center=",temp$Shipping_GeoLocation[temp$`Account Name`==name],"&distance=200&fields=name,category_list,checkins,description,fan_count,engagement,hours,link,location,overall_star_rating,phone,photos,price_range,rating_count,restaurant_services,restaurant_specialities,website,single_line_address&access_token=",user_access_token,sep="")

  # result_html <- tryCatch(read_html(graph_url),error=function(x){return(0)})
  # if(is.numeric(result_html)){next}
  
  # result_html <- GET(url = graph_url, handle = h1, add_headers(c("user-agent"='Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/64.0.3282.186 Safari/537.36')))
  
  graph_json <- fromJSON(graph_url, flatten = FALSE)
  gg <- graph_json$data
  
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
    if (!is.null(gg$phone[i])){
      phone <- gg$phone[i] }
    else {phone <- ""}
    about <- ""
    if (!is.null(gg$website[i])){
      website <- gg$website[i] }
    else {website <- ""}
    emails <- ""
    hours <- paste(gg$hours[[i]]$key, gg$hours[[i]]$value, "; ", sep = " ", collapse = "")
    weekday_opening_hours <- "" 
    weekday_closing_hours <- ""
    weekend_opening_hours <- ""
    weekend_closing_hours <- ""
    price_range <- ""
    if (!is.null(gg$description[i])){
      description <- gg$description[i] }
    else {description <- NA}
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
    dummy[i,] <- c(id, fb_id, name, fb_link, checkins, likes, tot_eng, rating_count, rating, category, type, address, phone, about, website, emails, hours, weekday_opening_hours, weekday_closing_hours, weekend_opening_hours, weekend_closing_hours, price_range, description, services, walkins, waiter, takeout, reserve, pickup, outdoor, kids, groups, catering, delivery, latitude, longitude, city, country, zip)
  }
  
  d <- rbind(d, dummy)

  # split into 2 loops to reduce GET calls
  # if(result_html$status_code==200){
  #   # print("ok")
  #   content_list <- content(result_html, "parsed")
  #   dummy_json <- toJSON(content_list, pretty = TRUE, auto_unbox = TRUE)
  #   clear_json <- gsub(pattern = '^\\{|\\}$|\"data\"\\:|\\[|\\]', replacement = "", x = dummy_json)
  #   
  #   e <- tryCatch(fromJSON(clear_json), error = function(x){return(0)})
  #   if(is.numeric(e)) { next }
  #   
  #   true_json <- fromJSON(clear_json) 
  #   print(true_json)
  # }
  
  }
# print(paste("Results found for: ",nrow(temp)-c,"/",nrow(temp)))
