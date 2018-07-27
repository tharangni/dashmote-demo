rm(list = ls(all=TRUE)) #!caution

library(xml2)
library(httr)
library(dplyr)
library(readxl)
library(stringr)
library(jsonlite)
library(stringdist)

set.seed(123) #123, 111, 500

read_file <- read_excel(path = "matched_AM_DB_30k.xlsx", sheet = "Sheet1")
no_match <- subset(read_file, fbid == "No Match", select = colnames(read_file))

no_match$latitude <- str_split_fixed(no_match$Shipping_GeoLocation, ",", 2)[,1]
no_match$longitude <- str_split_fixed(no_match$Shipping_GeoLocation, ",", 2)[,2]

temp <- no_match[sample(1:nrow(no_match), 40, replace=FALSE),]

user_access_token = "EAAPNYkdHBmkBABdOWuRZCEq54JFWg6vwZBSDt0zY1HLP2BhXWxdmPX2SkOKWFBAtZAhZB2YkDJsyB9PLirsMqUF8mHIJy5xL0UTlZB2TKuZCYDAcuzcRZCprpbtEi4n16ZC2NGhyL26eUZBhaioCrSIeltTmTmXzsG6xoXdWdKZCbkvgZDZD"
c = 0
# facebook part
for (name in temp$`Account Name`) { 
  converted_name <- name %>% gsub("[^a-zA-Z0-9]", "&", .) %>% tolower()

  h1 <- handle('')
  graph_url <- paste("https://graph.facebook.com/v3.0/search?type=place&q=",converted_name,"&center=",temp$Shipping_GeoLocation[temp$`Account Name`==name],"&distance=200&fields=name,location&access_token=",user_access_token,sep="")

  result_html <- tryCatch(read_html(graph_url),error=function(x){return(0)})
  if(is.numeric(result_html)){next}
  
  result_html <- GET(url = graph_url, handle = h1, add_headers(c("user-agent"='Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/64.0.3282.186 Safari/537.36'))) 

  if(result_html$status_code==200){
    # print("ok")
    content_list <- content(result_html, "parsed")
    dummy_json <- toJSON(content_list, pretty = TRUE, auto_unbox = TRUE)
    clear_json <- gsub(pattern = '^\\{|\\}$|\"data\"\\:|\\[|\\]', replacement = "", x = dummy_json)
    
    e <- tryCatch(fromJSON(clear_json), error = function(x){return(0)})
    if(is.numeric(e)) { next }
    
    true_json <- fromJSON(clear_json) 
    }
  }
# print(paste("Results found for: ",nrow(temp)-c,"/",nrow(temp)))
