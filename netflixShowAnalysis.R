rm(list=ls())
# web crawler
if(!require(rvest)) {
  install.packages("rvest")
  library(rvest)
} #load / install+load rvest

# filter
if(!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
} #load / install+load dplyr

if(!require(ggplot2)) {
  install.packages("ggplot2")
  library(ggplot2)
} #load / install+load ggplot2

csv<-read.csv("netflix_shows.csv", header=T, sep=",")
csv[["duration"]]<-as.numeric(gsub(' min','',csv[["duration"]]))
# get the movie which is release in 2019 and over 100 minutes
release2019<-filter(csv, release_year == 2019&type=='Movie'&duration>100&country!='')
imdbHTML = 'https://www.imdb.com'

storeRatingPoints <- function(rP){
  point<-10
  while (point>0){
    # the data of 10 points is in the index 1 of rP,
    # the data of 9 points is in the index 2 of rP, etc.
    ratingPoints[[as.character(point)]]<-c(ratingPoints[[as.character(point)]],rP[11-point])
    point<-point-1
  }
  return(ratingPoints)
}

userRatings <- c()
ratingPoints<-list()
i<-1
print(paste('Total number of movies: ',length(release2019[['title']])))
while ( i <= length(release2019[['title']])){
  key <- release2019$title[[i]]
  print(paste(i,': Get [',key,'] data'))
  # the result of searching
  key <-gsub("#","%23",gsub(" ","+",(gsub(":", "%3A", key))))
  tryCatch(
    {
      html <- read_html(paste(imdbHTML,"/find?q=",key,"&ref_=nv_sr_sm",sep='')) 
      header <- html %>% html_nodes(".findSectionHeader")%>% html_text()
      result <-html %>% html_nodes(".result_text a")
      cat('=====')
      if (length(result) == 0){
        # no data in IMDb
        stop()
      }else{
        hNum <-1
        while (hNum<=length(header)){
          if(header[hNum]=='Titles'){
            # get the movie data, not other data
            rHTML = paste(imdbHTML,xml_attrs(result[[hNum]])[["href"]],sep='')
            cat('=====')
            break
          }
          hNum<-hNum+1
        }
        resultHTML<- read_html(rHTML) 
        # get the rating on IMDb
        
        ratingData <- resultHTML %>% html_nodes(".ratingValue span")%>% html_text()
        userRatings<-c(userRatings,ratingData[1])
        cat('=====')
        
        # switch to each rate point page
        s<- unlist(strsplit(rHTML,'\\/\\?'))
        ratingHTML <- read_html(paste(s[1],'/ratings?',s[2],sep=''))
        
        # get each rate point
        eachRating <- ratingHTML %>% html_nodes(".title-ratings-sub-page table tr .leftAligned")%>% html_text()
        # gsub is used to delete ',' if the number over 1000.
        eachRating <- as.numeric(gsub(',','',eachRating[(2:11)]))
        
        ratingPoints<-storeRatingPoints(eachRating)
        cat('===== 100%\n')
      }
    }
    ,error = function(err)
    {
      # catch error
      userRatings<<-c(userRatings,NA)
      ratingPoints<<-storeRatingPoints(c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))
      cat('X\n')
    }
    ,finally = {
      i<-i+1
    }
  )
}

netflixDF <- data.frame(title=release2019[['title']],
                        country=release2019[['country']],
                        rating=release2019[['rating']],
                        userRating=userRatings,
                        point1 = ratingPoints$'1',
                        point2 = ratingPoints$'2',
                        point3 = ratingPoints$'3',
                        point4 = ratingPoints$'4',
                        point5 = ratingPoints$'5',
                        point6 = ratingPoints$'6',
                        point7 = ratingPoints$'7',
                        point8 = ratingPoints$'8',
                        point9 = ratingPoints$'9',
                        point10 = ratingPoints$'10')

# delete data which do not have user ratings or country data.
netflixDF<-filter(netflixDF, !is.na(userRatings)&country!='')
# some movie have multi-country, get the first country and remove country remaining.
netflixDF$country<-unlist(lapply(strsplit(as.character(netflixDF$country),','), '[[', 1))
#order by the user rating and select the first 5 rows of netflixDF.
highRating <- head(netflixDF[with(netflixDF, order(-as.numeric(userRating))),],5)

# count countries number
countCountriesPlot <- ggplot(netflixDF, aes(x=country)) + geom_histogram(stat='count', fill = "steelblue")
countCountriesPlot

# userRating
qplot(country, userRating, data=netflixDF)

#create data table
t<-'title points number'
pLevel <- c('point1','point2','point3','point4','point5','point6','point7','point8','point9','point10')
for( p in pLevel){
  i<-1
  while ( i<= 3){
    d<-paste(gsub(' ','_',highRating[['title']][i]), gsub('point','',p), highRating[[p]][i],sep=' ')
    t<- paste(t,d,sep='\n')
    i<-i+1
  }
}
dat <- read.table(text=t,header=TRUE)
# set the level of points
levels(dat$points) <- pLevel

# top 3 rating
ggplot(data=dat, aes(x=points, y=number, fill=title)) + 
  geom_bar(position = 'dodge', stat='identity') +
  geom_text(aes(label=number), position=position_dodge(width=1))

