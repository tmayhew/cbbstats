library(rvest)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(brnn)

# college basketball stats: by year
# team stats
# Example: https://www.sports-reference.com/cbb/seasons/2020-school-stats.html"

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
  teams$Yr = given_year
  return(teams)
}
mar.mad <- function(df = stats2019){
  for(i in 1:nrow(df)){
    split <- strsplit(df$Tm[i], split = "")[[1]]
    ncaa.c <- paste(split[c(length(split)-3,length(split)-2,length(split)-1,length(split))], collapse = "")
    if(ncaa.c == "NCAA"){
      df$marmad[i] <- 1
    } else{
      df$marmad[i] <- 0
    }
  }
  return(df)
}

stats2020 <- scrape_cbbteams(2020)
stats2019 <- mar.mad(scrape_cbbteams(2019))
stats2018 <- mar.mad(scrape_cbbteams(2018))
stats2017 <- mar.mad(scrape_cbbteams(2017))
stats2016 <- mar.mad(scrape_cbbteams(2016))
stats2015 <- mar.mad(scrape_cbbteams(2015))
stats2014 <- mar.mad(scrape_cbbteams(2014))
stats2013 <- mar.mad(scrape_cbbteams(2013))
stats2012 <- mar.mad(scrape_cbbteams(2012))
stats2011 <- mar.mad(scrape_cbbteams(2011))
stats2010 <- mar.mad(scrape_cbbteams(2010))

df <- data.frame(rbind(stats2019, stats2018, stats2017, stats2016, stats2015, 
                   stats2014, stats2013, stats2012, stats2011, stats2010))
df$point.diff <- df$Points.S - df$Points.A

# W/L: R-sq = 0.344
ggplot(df, aes(x=marmad, y=W.L.), alpha = I(1/5)) + geom_point() + theme_bw()

# W: R-sq = 0.383
ggplot(df, aes(x=marmad, y=W), alpha = I(1/5)) + geom_point() + theme_bw()

# SRS: R-sq = 0.35
ggplot(df, aes(x=marmad, y=SRS), alpha = I(1/5)) + geom_point() + theme_bw()

summary(lm(marmad ~ W + SRS, data=df))

# Point Diff: R-sq = 0.33
summary.1 <- summary(lm(marmad ~ W + SRS + point.diff, data=df))
print(summary.1)


