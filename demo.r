library(rvest)

url <- "http://www.imdb.com/search/title?count=100&release_date=2016,2016&title_type=feature"

html_view <- read_html(url)

rank_data_html <- html_nodes(html_view, xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "text-primary", " " ))]')

rank_data <- html_text(rank_data_html)

rank_data <- as.numeric(rank_data)

title_data_html <- html_nodes(html_view, xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "lister-item-header", " " ))]//a')

title_data <- html_text(title_data_html)
