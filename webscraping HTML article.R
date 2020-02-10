install.packages("rvest")
install.packages("dplyr")
install.packages("tidyr")
install.packages("stringr")


library(rvest)
library(dplyr)
library(tidyr)
library(stringr)

# copy and paste the url from the website with the table
# of data you want
url <- "https://plantfadb.org/fatty_acids"
# scrape the website
url_html <- read_html(url)
# extract the HTML table by identifying the 
# html node "table"
whole_table <- url_html %>% 
  html_nodes('table') %>%
  html_table(fill = TRUE) %>%
  .[[1]]

