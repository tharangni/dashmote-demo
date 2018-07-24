library(stringr)
library(dplyr)
library(stringdist)

library(readxl)
# options(digits = 16)
AM <- read_excel("Strongbow Excercise of scope sent to dashmote.xlsx", 
                 sheet = "ALL DATA SF")

DM <- read_excel("Strongbow Excercise of scope sent to dashmote.xlsx", 
                 sheet = "DM30k")



# {
#   for(id in DM$`FB ID`){
#     if(!is.na(DM$Name[DM$`FB ID`==id])){
#       next
#     }
#     graph_link <- paste("https://graph.facebook.com/",id,"?fields=name&access_token=EAACEdEose0cBALBSehfi6YZAWJt9ZCxyYbK5N4kyg8vJjtmYptRbZB9XT1QU2izhqT6HrZAjFcYPL5eQTp1jMNUxImZAI7xZCQRjrNOcMsPQJq9ZALahugBZC3EnRwmJVAQGfNdC03SEqu2ituBrrcsSXS5b1URUztG2bLAiq0taVpJqZCas7IZB6x6UjfDXk7NfcZD",sep="")
#     html <- tryCatch(xml2::read_html(graph_link),error=function(x){return(0)})
#     if(is.numeric(html)){
#       next
#     }
#     #html <- httr::GET(url=graph_link,httr::add_headers(c("user-agent"='Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/64.0.3282.186 Safari/537.36')))
#     #Sys.sleep(max(0,rnorm(n = 1,mean = 3,sd = 1)))
#     
#     
#     json <- jsonlite::fromJSON(rvest::html_text(html))
#     DM$Name[DM$`FB ID`==id] <- json$name
#     print(paste(json$name,id))
#     
#   }
# }
# 
# ##getting facebook names
# {
#   for(link in DM$`FB page link`){
#     if(!is.na(DM$Name[DM$`FB page link`==link])){
#       next
#     }
#     h1 <- httr::handle('')
#     html <- httr::GET(url=link,handle = h1,httr::add_headers(c("user-agent"='Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/64.0.3282.186 Safari/537.36')))
#     #Sys.sleep(max(0,rnorm(n = 1,mean = 3,sd = 1)))
#     if(html$status_code==200){
#       content <- rawToChar(html$content) %>% xml2::read_html()
#       captcha <- rvest::html_nodes(content,xpath="//div[@class='captcha_interstitial']")
#       if(length(captcha)>0){
#         error()
#       }
#       
#       json <- rvest::html_nodes(content,xpath="//script[@type='application/ld+json']")
#       if(length(json)>0){
#         json <- jsonlite::fromJSON(rvest::html_text(json[1]))
#         DM$Name[DM$`FB page link`==link] <- json$name
#         print(paste(json$name,link))
#       } else {
#         print("Not found")
#       }
#     }
#   }
# }



#algorithm: checking all the DM locations within 50m of an AM location
#compare names of locations found. If there is a close enough match, it matches.
#If no locations are found, then we assume there is no match possible
#in case there are multiple similar names, a human supervision is required
library(geosphere)

AM$`Account Name`[is.na(AM$`Account Name`)] <- ""
AM$`Shipping_GeoLocation`[is.na(AM$Shipping_GeoLocation)] <- ""
AM$fbid <- ""

library(parallel)
cl <- makeCluster(detectCores() - 1)
temp <- DM[,c('longitude','latitude')]
clusterExport(cl, "temp")
counter = 0

for(placei in 1:length(AM$Shipping_GeoLocation)){
  if(placei %% 100 == 0) {
    print(paste(placei, "/", length(AM$Shipping_GeoLocation), "completed"))
    print(paste("Number of matches found: ", counter))
  }
  if(AM$fbid[placei]!=""){
    next
  }
  
  place <- AM$Shipping_GeoLocation[placei]
  gps <- str_split(place,",")
  latAM <- gps[[1]][1] %>% as.numeric()
  lonAM <- gps[[1]][2] %>% as.numeric()
  clusterExport(cl, c("lonAM","latAM"))
  
  dist_list <- parLapply(cl,1:NROW(temp),function(x){ 
    geosphere::distHaversine(c(lonAM, latAM), c(temp$longitude[x], temp$latitude[x]))
  }) %>% unlist()
  
  # dist_list <- apply(DM[,c('longitude','latitude')],1,function(x){ 
  #   distm(c(lonAM, latAM), c(x['longitude'], x['latitude']), fun = distHaversine)
  #   })
  close_locations <- which(dist_list<100)
  if(length(close_locations)>0){
    name_am <- AM$`Account Name`[placei] %>% gsub("[^a-zA-Z]", "", .) %>% tolower()
    names_dm <- DM$Name[close_locations] %>% gsub("[^a-zA-Z]", "", .) %>% tolower()
    
    name_similarity <- stringdist::stringdist(name_am, names_dm, method = "jaccard", q=2)
    name_similarity[is.na(name_similarity)] <- 1
    
    if(sum(name_similarity<=0.75)==1){#assume it is a perfect match
      index <- close_locations[which(name_similarity<=0.75)]
      print(paste("Merging: ",DM$Name[index],"with", name_am))
      AM$fbid[placei] <- DM$`FB ID`[index]
      counter <- counter + 1
    } else if(sum(name_similarity<=0.75)>1){
      #error()
      print(paste("Distance:",dist_list[close_locations],"Name AM:", name_am,"Name DM:",names_dm, name_similarity))
      best_match_mask <- which(name_similarity==min(name_similarity))
      index <- close_locations[best_match_mask]
      print(paste("Merging: ",DM$Name[index],"with", name_am))
      AM$fbid[placei] <- DM$`FB ID`[index]
      counter <- counter + 1
    }
  } else {
    print(paste("Match not found",AM$`Account Name`[placei]))
    AM$fbid[placei] <- "No Match"
  }
  
}
AM$fbid[AM$fbid==""] <- "No Match"
stopCluster(cl)
save(file="AM.RData",DM,AM)

# xlsx::write.xlsx(AM,file="matched_AM_DB.xlsx")
xlsx::write.xlsx(AM,file="matched_AM_DB_temp.xlsx")
