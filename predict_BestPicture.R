## This script calculates predictions for the Academy Award for "Best Picture"
# Run this script to see the algorithm select the suprise winner in 2016, "Spotlight"

yearstopredict=2016
nruns=500

## get group lens data from
#       http://files.grouplens.org/datasets/movielens/ml-latest.zip
#       you can skipt this and download the data set manually
library(plyr)
if (!file.exists("ratings.Rdata")){
  install.packages("downloader")
  library(downloader)
  download("http://files.grouplens.org/datasets/movielens/ml-latest.zip", dest="ml-latest.zip", mode="wb")
  unzip("ml-latest.zip", exdir=".")
  
  ratings<-read.csv("ratings.csv")
  movies<-read.csv("movies.csv")
  awards <- read.csv("http://raw.githubusercontent.com/nigelhenry/oscarpredictor/master/academyawards.csv")

  trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  movies$title<-trim(movies$title)
  awards$awarddate<-as.POSIXct(awards$awarddate)
  ratings$timestamp<-as.POSIXct(ratings$timestamp,origin="1970-01-01",tz="UTC")
  
  save(ratings, movies, awards, file="oscarpredictors.Rdata")
} else {
  load("oscarpredictors.Rdata")
}

movies<-merge(movies, awards, by="title", all.x=T)
movies$won[is.na(movies$won)]<-F
movies$nominated[is.na(movies$nominated)]<-F
movies$year<-substr(movies$title, nchar(as.character(movies$title))-4, nchar(as.character(movies$title))-1)
movies<-movies[(movies$year>=1995 & movies$year<=2016 & !is.na(movies$year)) | movies$nominated,]
ratings <- ratings[ratings$movieId %in% movies$movieId,]
#ratings$won<-movies$won[match(ratings$movieId, movies$movieId)]
#ratings$won[is.na(ratings$won)]<-F
ratings$nominated<-movies$nominated[match(ratings$movieId, movies$movieId)]
ratings$nominated[is.na(ratings$nominated)]<-F

ratings$awarddate<-movies$awarddate[match(ratings$movieId,movies$movieId)]
ratings$qualified <- ratings$timestamp < ratings$awarddate
ratings$qualified[is.na(ratings$qualified)]<-T
ratings<-ratings[ratings$qualified,]
ratings$year<-as.numeric(format(ratings$awarddate,"%Y"))
ratings$year[is.na(ratings$year)]<-0

ratings$academy<- ifelse(ratings$nominated,5,0)
trainmaster<-ratings

prediction <- c()
probtable <- c()
pb<-txtProgressBar(min=min(yearstopredict), max=max(yearstopredict)+1, style=3)
for (testyear in yearstopredict){
  #forecast
  train <- trainmaster[trainmaster$year!=testyear, c("userId", "movieId", "rating", "academy","nominated")]
  test <- trainmaster[trainmaster$year==testyear, c("userId", "movieId", "rating", "academy","nominated")]
  test <-test[test$userId %in% train$userId[train$academy > 0],]
  train <-train[train$userId %in% test$userId,]
  mean_academy <- mean(train$academy)

  for (run in 1:nruns){
    setTxtProgressBar(pb, testyear+(run/nruns))
    # boostrapping
    traindata<-train[sample(1:nrow(train),replace=T),]
    testdata<-test[sample(1:nrow(test),replace=T),]
  
  #Step 1 in calculating Pearsons R - User correlation with the academy
  users<-ddply(traindata,"userId",summarize,
               r=(sum((rating-mean(rating))*(academy-mean_academy))/(sqrt(sum((rating-mean(rating))^2))*(sqrt(sum((academy-mean_academy)^2))))),
               useravg=mean(rating), nratings=length(rating))
  users$r <- ifelse(users$nratings>20, 1, users$nratings/20) * users$r

  #step2 - scaling and weighting ratings by user average and similarity to the academy
  testdata$useravg<-users$useravg[match(testdata$userId, users$userId)]
  testdata$r<-users$r[match(testdata$userId,users$userId)]
  testdata$p <-(testdata$r * (testdata$rating - testdata$useravg))

  moviedata<-ddply(testdata,"movieId",summarize,
                   avgrating=mean(rating), nratings=sum(!is.na(r)),
                   p=sum(p, na.rm=T), 
                   sum_r=sum(abs(r),na.rm=T)
                  )
  moviedata$prediction<-moviedata$p/(moviedata$sum_r)
  prediction[run]<-moviedata$movieId[which.max(moviedata$prediction)]
  
  }
  probtable <- c(probtable, table(prediction) / nruns)
}
close(pb)

preds <- movies[movies$movieId %in% names(probtable), c("title","year","movieId","genres","won")]
preds$prob <- probtable[match(preds$movieId, names(probtable))]
preds <- preds[order(preds$year, preds$prob),]
print(preds[,c("title","won","prob")])
