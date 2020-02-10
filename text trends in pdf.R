library(tidytext)
library(pdftools)
library(janeaustenr)
library(dplyr)
library(stringr)
library(tidyverse)


pdf_file <- "C:/Users/Russell/Desktop/Wildlife Criminology Workshop/day4/RANGERINTERVIEWSNONAME.pdf"
txt <- pdf_text(pdf_file)
cat(txt[1])

opinions <- lapply(pdf_file, pdf_text)

length(opinions)

lapply(opinions, length) 


# install.packages("tm")
library(tm)
corp <- Corpus(URISource(pdf_file),
               readerControl = list(reader = readPDF))


opinions.tdm <- TermDocumentMatrix(corp, 
                                   control = 
                                     list(removePunctuation = TRUE,
                                          stopwords = TRUE,
                                          tolower = TRUE,
                                          stemming = TRUE,
                                          removeNumbers = TRUE,
                                          bounds = list(global = c(1, Inf)))) 


inspect(opinions.tdm)



corp <- tm_map(corp, removePunctuation, ucp = TRUE)

opinions.tdm <- TermDocumentMatrix(corp, 
                                   control = 
                                     list(stopwords = TRUE,
                                          tolower = TRUE,
                                          stemming = TRUE,
                                          removeNumbers = TRUE,
                                          bounds = list(global = c(1, Inf)))) 


findFreqTerms(opinions.tdm, lowfreq = 100, highfreq = Inf)


ft <- findFreqTerms(opinions.tdm, lowfreq = 100, highfreq = Inf)
as.matrix(opinions.tdm[ft,]) 


ft.tdm <- as.matrix(opinions.tdm[ft,])
sort(apply(ft.tdm, 1, sum), decreasing = TRUE)




