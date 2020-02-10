install.packages("tabulizer")
install.packages("dplyr")

library(tabulizer)
library(dplyr)


# Location of pdf file, remember to change \ to /
location <- 'C:/Users/Russe/Desktop/Inat Data/R Ecology Training/Web scraping/Andriafidisonetal.20075bOtomops5d.pdf'

# Extract the table
out <- extract_tables(location)

#check the out object and find the pages with tables
# we just want the first table of values
out.tab.1 <- out[[3]]


# ~~~~~~~~~~~~~~~~~ Table 1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# create a dataframe and clean the data, remove rows and columns of no use
out.tab.df.1 <- as.data.frame(out.tab.1)
out.tab.df.1 <- out.tab.df.1[-c(1,15),-c(1,4,8)]
# change colnames to second row
colnames(out.tab.df.1) <- c("Month", "total bats", "YY", "XX", "%YY")
# remove the first row (column names)
out.tab.df.1 <- out.tab.df.1[-1,]



