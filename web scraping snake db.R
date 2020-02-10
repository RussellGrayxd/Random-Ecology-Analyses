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
url <- "http://snakedatabase.org/pages/ld50.php"

# scrape the website
url_html <- read_html(url)

# extract the HTML table by identifying the 
# html node "table"
whole_table <- url_html %>% 
  html_nodes('table') %>%
  html_table(fill = TRUE) %>%
  .[[1]]


# tidying the table
table_content <- whole_table %>%
  select(-X1) %>% # remove redundant column
  filter(!dplyr::row_number() %in% 1) # remove redundant rows (firt 2)


# only keep the first 12 columns and remove the unesccesary columns
# call the new dataframe "dat"
dat <- table_content[,c(1:12)]


# set the first row as column headers
colnames(dat) <- dat[1,]
# remove the first "column header row
dat <- dat[-1,]


#~~~~~~~~~~~~~~~~~~~~ Clean the species and common name data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Remove html tags and line breaks remaining in the data
dat$`Scientific NameCommon Name` <- str_replace_all(dat$`Scientific NameCommon Name`, "[\t\n]" , " ")

# replace spaces with _ to divide species and column name columns
dat$`Scientific NameCommon Name` <- gsub(dat$`Scientific NameCommon Name`, 
                                         pattern = " ", replacement = "_")
#Split columns 
df <- as.data.frame(str_split_fixed(dat$`Scientific NameCommon Name`, "_", 2))
df$V2 <- gsub(df$V2, 
              pattern = "_", replacement = " ")
#remove trailing whitespace
df$V2 <- trimws(df$V2, which = c("left"), whitespace = "[ \t\r\n]")
df$V1 <- trimws(df$V1, which = c("right"), whitespace = "[ \t\r\n]")
#remove empty containers
df[df==""] <- NA
df$V1 <- na.omit(df$V1)
df$V2 <- na.omit(df$V2)
# remove any duplicates that may have been created
df <- unique(df)
colnames(df) <- c("Species Name", "Common Name")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ re-add to the original Data frame ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#combine the new columns back with the data
dat.final <- cbind(df, dat)
# remove the 3rd column that we've now fixed
dat.final <- dat.final[,-3]

