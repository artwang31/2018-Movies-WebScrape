library(tidyverse)
library(rvest)
library(ggplot2)

url <- 'https://www.imdb.com/search/title/?year=2018&title_type=feature&'
webpage <- read_html(url)

#Using CSS selectors to scrape the rankings section
rank_data_html <- html_nodes(webpage,'.text-primary')
#Converting the ranking data to text
rank_data <- html_text(rank_data_html)
rank_data <- as.numeric(rank_data)

#Using CSS selectors to scrape the title section
title_data_html <- html_nodes(webpage,'.lister-item-header a')
#Converting the title data to text
title_data <- html_text(title_data_html)

#Using CSS selectors to scrape the metascore section
metascore_data_html <- html_nodes(webpage,'.metascore')
#Converting the runtime data to text
metascore_data <- html_text(metascore_data_html)
for (i in c(46)){
  a<-metascore_data[1:(i-1)]
  b<-metascore_data[i:length(metascore_data)]
  metascore_data<-append(a,list("NA"))
  metascore_data<-append(metascore_data,b)
}
#Data-Preprocessing: removing extra space in metascore
metascore_data <- gsub(" ","",metascore_data)

#Using CSS selectors to scrape the gross revenue section, only gets Millions $$
gross_data_html <- html_nodes(webpage,'.ghost~ .text-muted+ span')
#Converting the gross revenue data to text
gross_data <- html_text(gross_data_html)
#Filling missing entries with NA
for (i in c(2,7,16,31,39,44,46,50)){
  c<-gross_data[1:(i-1)]
  d<-gross_data[i:length(gross_data)]
  gross_data<-append(c,list("NA"))
  gross_data<-append(gross_data,d)
}
gross_data <- gross_data[-c(51:52)]
#Data-Preprocessing: removing '$' and 'M' signs
gross_data<-gsub("M","",gross_data)
#gross_data<-substring(gross_data,2,6)
# Check the length of gross data
movies <- data.frame(Rank = rank_data, Title = title_data, Score = metascore_data, Gross = gross_data)
movies
ggplot(movies, aes(x = Gross, y = Score, color = Score)) +
  geom_point() +
  expand_limits(x = 0)
