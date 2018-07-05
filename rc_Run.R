
channel <- 
  odbcConnect(
    "Biosense_Platform", 
    "BIOSENSE\\scrossenks", 
    "fuu9QQIOH"
    )

dtMonth <- "08"
dtYear <- "2017"

feedvisits <- 
  feedrc(
    channel=channel, 
    visit=TRUE, 
    arrived=TRUE, 
    month=dtMonth, 
    year=dtYear
    )

filename <- paste0( dtYear, dtMonth, "_Rec.csv")


write.csv(feedvisits, file=filename, row.names=FALSE)
