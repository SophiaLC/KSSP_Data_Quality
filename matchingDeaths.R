library(RODBC)
library(readxl)
library(dplyr)
library(stringr)
library(lubridate)
channel <- odbcConnect("Biosense_Platform", "BIOSENSE\\mwhiteks", "57Q75cUz")
dat <- sqlQuery(channel,
                "SELECT C_BioSense_ID, Birth_Date_Time, Patient_Zip, Administrative_Sex, Medical_Record_Number, 
                 Arrived_Date_Time, Chief_Complaint_Text, Admit_Reason_Description, Diagnosis_Code
                 FROM KS_PR_PROCESSED WHERE C_Visit_Date_Time >='2016-11-08' 
                 AND C_Visit_Date_Time <= '2017-01-08'")
odbcCloseAll()
vital <- read_excel("~/mhwii/matchingDeaths/VITAL_20170108b.xlsx")

# cleaning
dat$Birth_Date_Time <- dat$Birth_Date_Time %>% # take the variable
  as.character() %>% # turn to character
  str_replace_all("\\s.*$", "") # replace all the times

# structure of dates?
class(dat$Birth_Date_Time) # character
head(dat$Birth_Date_Time) # year, month, day
class(vital$DOB) # character
head(vital$DOB) # month, day, year

# more cleaning
vital$DOB <- vital$DOB %>% # take variable
  str_replace_all("/", "-") %>% # replace / with -
  mdy() # make it ymd

dat$Birth_Date_Time <- ymd(dat$Birth_Date_Time) # make it ymd

# structure of dates, again
class(dat$Birth_Date_Time) # Date
head(dat$Birth_Date_Time) # year, month, day
class(vital$DOB) # Date
head(vital$DOB) # year, month, day

# get one message per patient visit from back end data
dat <- dat %>% # take data
  group_by(C_BioSense_ID) %>% # group by C_BioSense_ID
  top_n(1, Arrived_Date_Time) %>% # take most recent message
  slice(1) # take just one of the messages, if there are many tied for most recent

# try to match on dob: how many of the DOBs are found in the vital file?
dat$DOBinVital <- sapply(dat$Birth_Date_Time, function(x) x %in% vital$DOB)
table(dat$DOBinVital)# /nrow(dat)*100 # 1705 matches (1.37% of the data); need to whittle this down

# reduce dat down to only matches
dat <- filter(dat, DOBinVital==TRUE)
nrow(dat) # successful

# next, move on to zips
# structure of zips--patient's residence zip code
class(dat$Patient_Zip) # factor
head(dat$Patient_Zip) # very messy
class(vital$ZIP) # character
head(vital$ZIP) # clean

# look at levels of zip in back end data
levels(dat$Patient_Zip)
dat$Patient_Zip <- dat$Patient_Zip %>% # take patient zip code
  as.character() %>% # make character vector
  trimws("both") %>% # remove both leading and trailing white space
  ifelse(grepl("^[0-9]", .), ., NA) %>% # if it doesn't start with numeric, give it NA
  str_sub(., 1, 5) # take just first five characters
levels(factor(dat$Patient_Zip)) # check out levels again

# clean up the administrative sex
levels(dat$Administrative_Sex)
levels(as.factor(vital$SEX))
vital$SEX <- as.factor(vital$SEX)

# try joining by both 
# clean vital
vital <- vital %>% # take data
  mutate(Death_Record_ID=DEATH_REC_ID, Patient_Zip=ZIP, Birth_Date_Time=DOB, Administrative_Sex=SEX) %>% 
  select(-c(DEATH_REC_ID, ZIP, DOB, SEX))

# inner join...
joined <- inner_join(dat, vital, by=c("Birth_Date_Time", "Patient_Zip", "Administrative_Sex"))

View(joined)

# any duplicated BioSense IDs or Death Record IDs?
which(table(droplevels(joined$C_BioSense_ID)) > 1) # no duplicated BioSense IDs
which(table(joined$Death_Record_ID) > 1) # multiple death record IDs are in there, though
arrange(filter(as.data.frame(table(joined$Death_Record_ID)), Freq>1), desc(Freq)) # list of IDS more than once and how many times

# save table of how many times a given death record id appeared
drtable <- table(joined$Death_Record_ID)

# save separate data frame of those that appeared just once
joinedOnce <- joined[joined$Death_Record_ID %in% names(drtable[drtable==1]),]

nrow(joinedOnce)

# do the duplicate death record IDs correspond to one medical record number, even though it is different biosense ids?
View(arrange(joined[joined$Death_Record_ID %in% names(drtable[drtable>1]),], desc(Death_Record_ID)))
# yes, it seems like many of these have the same medical record number

write.csv(joined, "joined.csv", row.names=FALSE)