library(rvest)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(brnn)

# college basketball stats: by year
# team stats
scrape_cbbteams <- function(given_year = 2019){
  year = paste(given_year)
  link <- paste0("https://www.sports-reference.com/cbb/seasons/",year,"-school-stats.html")
  page <- read_html(link)
  data.raw <- html_table(page, fill=TRUE)
  
  teams <- data.raw[[1]]
  teams <- teams[,-1]
  col.names <- c("Tm", "G", "W", "L", "W-L%", "SRS", "SOS", "Conf.W", "Conf.L", "Home.W", "Home.L", "Away.W", "Away.L", "Points.S", "Points.A",
                 "NA", "MP", "FG", "FGA", "FG.", "3P", "3PA", "3P.", "FT", "FTA", "FT.", "ORB", "TRB", "AST", "STL", "BLK", "TOV", "PF")
  teams <- teams[-1,]
  names(teams) <- col.names
  teams <- teams[,-16]
  
  for (i in 2:ncol(teams)){
    teams[,i] <- as.double(teams[,i])
  }
  
  teams <- na.omit(teams)
  return(teams)
}
stats2020 <- scrape_cbbteams(2020)
stats2019 <- scrape_cbbteams(2019)
stats2018 <- scrape_cbbteams(2018)
stats2017 <- scrape_cbbteams(2017)

split <- strsplit(stats2019$Tm[1], split = 
           "")[[1]]
ncaa.c <- merge(split[c(length(split)-3,length(split)-2,length(split)-1,length(split))])
