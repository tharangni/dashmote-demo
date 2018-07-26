rm(list = ls(all=TRUE)) #!caution

library(stringr)
library(dplyr)
library(stringdist)

library(readxl)

AM <- read_excel("Strongbow Excercise of scope sent to dashmote.xlsx", 
                 sheet = "ALL DATA SF")

DM <- read_excel("Strongbow Excercise of scope sent to dashmote.xlsx", 
                 sheet = "DM30k")

DM <- DM[!duplicated(DM$`FB ID`),]
AM <- AM[!duplicated(AM$`Account Name`),]


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
AM$reason <- ""
AM$score<- ""

# library(parallel)
# cl <- makeCluster(detectCores() - 1)
temp <- DM[,c('longitude','latitude')]
counter = 0

# clusterExport(cl, "temp")

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
  # clusterExport(cl, c("lonAM","latAM"))

  # dist_list <- parLapply(cl,1:NROW(temp),function(x){ 
  #   geosphere::distHaversine(c(lonAM, latAM), c(temp$longitude[x], temp$latitude[x]))
  # }) %>% unlist()
  
  # dist_list <- apply(DM[,c('longitude','latitude')],1,function(x){ 
  #   distm(c(lonAM, latAM), c(x['longitude'], x['latitude']), fun = distHaversine)
  #   })
  # close_locations <- which(dist_list<100)

  # re-implement version
  
  del_lat <- 0.0009
  del_long <- 0.0015
  xp <- latAM + del_lat
  xn <- latAM - del_lat
  yp <- lonAM + del_long
  yn <- lonAM - del_long
  
  # make an object which contains the gps coords that only lie within the specific range from temp table
  pos <- which(temp$longitude < yp & temp$longitude > yn & temp$latitude < xp & temp$latitude > xn)
  close_locations <- temp[pos,]
  
  if(nrow(close_locations)>0){
    name_am <- AM$`Account Name`[placei] %>% gsub("[^a-zA-Z]", "", .) %>% tolower()
    names_dm <- DM$Name[pos] %>% gsub("[^a-zA-Z]", "", .) %>% tolower()
    
    name_similarity <- stringdist::stringdist(name_am, names_dm, method = "jaccard", q=2)
    name_similarity[is.na(name_similarity)] <- 1
    
    #assume it is a perfect match i.e. there exists only one unique result [name similarity]
    if(sum(name_similarity<=0.75)==1){
      index <- which(name_similarity<=0.75)
      print(paste("Merging: ",DM$Name[pos[index]],"with", name_am))
      AM$fbid[placei] <- DM$`FB ID`[pos[index]]

      AM$reason[placei] <- paste("Merged [DM]: ",DM$Name[pos[index]],"with [AM]: ", name_am)
      AM$score[placei] <- paste(min(name_similarity))
      counter <- counter + 1
    } 
    
    # if there exists more than one unique result [name similarity] then find the closest location
    # based on min similarity
    else if(sum(name_similarity<=0.75)>1){
      #error()
      # print(paste("Distance:",dist_list[close_locations],"Name AM:", name_am,"Name DM:",names_dm, name_similarity))
      
      best_match_mask <- which(name_similarity==min(name_similarity))
      
      print(paste("Merging: ",DM$Name[pos[best_match_mask]],"with", name_am))
      AM$fbid[placei] <- DM$`FB ID`[pos[best_match_mask]]

      AM$reason[placei] <- paste("Merged [DM]: ",DM$Name[pos[best_match_mask]],"with [AM]: ", name_am)
      AM$score[placei] <- paste(min(name_similarity))
      counter <- counter + 1
    }
  } 
  
  else {
    print(paste("Match not found",AM$`Account Name`[placei]))
    AM$fbid[placei] <- "No Match"
    
    AM$reason[placei] <- "No establishments present in vicinity"
  }
  
}

AM$fbid[AM$fbid==""] <- "No Match"
AM$reason[AM$reason==""] <- "No establishments present in vicinity"
AM$score[AM$score==""] <- "1"

print("Saving files...")
# stopCluster(cl)
save(file="AM.RData",DM,AM)

# xlsx::write.xlsx(AM,file="matched_AM_DB.xlsx")
xlsx::write.xlsx(AM,file="matched_AM_DB_30k.xlsx")
