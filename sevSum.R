# Author: Mark White
# Date: 25 May 2017
# Contact: markhwhiteii@gmail.com
# Purpose: To search admit reasons, chief complaints, diagnosis descriptions, and triage notes
# for terms related to sever summer weather, particularly storms and tornados.
# This function will generate a report returning all records that meet these search terms
# as well as a probability score of being a true positive. This score is based on a
# bag-of-words naive Bayes model using summer 2016 as training data.

# Preparing to use the function
# First, you must establish a connection to the database using your username and password:
channel <- odbcConnect("Biosense_Platform", "BIOSENSE\\username", "password")

# Next, you must load the model and training document term matrix
# USE THESE NAMES VERBATIM--only change to fit your directory
# The function will not work if they are not named model and dtrain, respectively
model <- readRDS(".../sevSumModel.rds") # loading model
dtrain <- readRDS(".../sevSumDFM.rds") # loading train dfm

# The function is included below. All you have to do is run the rest of the document,
# and the function will be loaded into your global environment.

# You need to specify three arguments:
# 1. The channel you are using to connect to the database
# 2. A beginning date with the format YYYY-MM-DD HH:MM:SS
# 3. An end date with that same format
# Make sure to put arguments 2 and 3 in quotation marks (i.e., as character strings)

sevSumReport <- function(channel, begin, end) {
  # loading packages
  suppressMessages(require(RODBC))
  suppressMessages(require(dplyr))
  suppressMessages(require(stringr))
  suppressMessages(require(quanteda))
  suppressMessages(require(maps))
  suppressMessages(require(ggplot2))
  
  # fetching data
  d <- sqlQuery(channel, paste0("SELECT C_BioSense_ID, C_Visit_Date_Time, Admit_Reason_Description, Chief_Complaint_Text,
                Diagnosis_Description, Triage_Notes, C_Patient_Age_Years, Administrative_Sex, C_Patient_County
                FROM KS_PR_Processed WHERE C_Visit_Date_Time >= '", begin, "' AND C_Visit_Date_Time <= '", end, "'"))
  
  # generating list of keywords
  keywords <- c("tornado", 
                "\\bwind\\b",
                "\\bwindy\\b",
                "\\bwinds\\b",
                "flood", 
                "\\bhail\\b",
                "\\bweather\\b", 
                "lightning",
                "\\blightening\\b",
                "\\btwister\\b",
                "\\bthunderclap\\b",
                "\\bthunderstorm\\b",
                "\\bthunderstorms\\b",
                "\\bthunder\\b",
                "\\brain\\b",
                "\\brained\\b",
                "\\brainy\\b",
                "\\brainstorm",
                "\\brains\\b",
                "\\bstorm\\b",
                "\\bstorms\\b",
                "\\bstormy\\b",
                "\\bstormed\\b")
  
  # cleaning the data
  d <- d %>% 
    tbl_df() %>% # making it a tbl
    mutate(Admit_Reason_Description=str_replace_na(Admit_Reason_Description, ""), # removing NAs
           Chief_Complaint_Text=str_replace_na(Chief_Complaint_Text, ""), # removing NAs
           Diagnosis_Description=str_replace_na(Diagnosis_Description, ""), # removing NAs
           Triage_Notes=str_replace_na(Triage_Notes, ""), # removing NAs
           Full_Text=str_replace_all(paste(Admit_Reason_Description, Chief_Complaint_Text, Diagnosis_Description, Triage_Notes), 
                                     "[^[a-zA-Z ]]", " ")) # combining all fields, removing everything but letter characters
  
  rows <- c() # creating empty vector I will fill with rows that 
  for (i in 1:length(keywords)) { # doing this for every keyword
    rows <- c(rows, grep(keywords[i], d$Full_Text)) # see if the keyword is found in the Full_Text field, if it is, return the row
  }
  
  # generating probability of true positive
  #model <- readRDS("~/mhwii/sevSum/sevSumModel.rds") # loading model # SHOULD ALREADY BE LOADED
  #dtrain <- readRDS("~/mhwii/sevSum/sevSumDFM.rds") # loading train dfm # SHOULD ALREADY BE LOADED
  
  # this gets only the record with the most written in the four fields
  d <- d[rows,] %>% # subsetting data to include only the data with the rows that match our search
    mutate(length=str_length(Full_Text)) %>% # getting length of the full_text
    subset(length>1) %>% # getting rid of any case that is only 1 character long (i.e., blank)
    group_by(C_BioSense_ID) %>% # group by biosense ID
    slice(which(length==max(length))) %>% # for every biosense ID, get the row that has the most in the Full_Text field
    slice(1) %>% # if there are ties, only take one of them
    ungroup() # ungroup by biposense ID
  
  # turns this text into a dfm
  dtest <- d$Full_Text %>% # take the text
    dfm(tolower=TRUE, stem=TRUE, remove=stopwords("english")) %>% # clean the text
    #dfm_trim(min_count=5) %>% # trim the next of words not used often. NOTE: may not want to do this for small data pulls like this
    dfm_select(dtrain) # giving dtest and dtrain the same features
  
  # getting predicted values for the new data
  predicted <- predict(model, dtest)
  d$severe_prob <- round(predicted$posterior.prob[,2]*100,4)
  
  d <- d[,colnames(d) != "Full_Text"] # delete the "full_text" column
  # what column numbers correspond to the four fields we want? save those in an object called fields
  fields <- which(colnames(d) %in% c("Chief_Complaint_Text", "Admit_Reason_Description", "Diagnosis_Description", "Triage_Notes"))
  d$Present_Keywords <- rep(NA, nrow(d)) # generate new variable, make them all NAs for now
  for (i in 1:nrow(d)) { # for every person
    for (j in min(fields):max(fields)) { # and all of the four fields of interest for that person
      for (k in 1:length(keywords)) { # and every keyword in the list
        if (grepl(keywords[k], d[i,j])==TRUE) { # if the keyword is found in that field for that person, then...
          # paste the name of the keyword in the new field that lists which keywords matched
          d$Present_Keywords[i] <- paste(d$Present_Keywords[i], str_replace_all(keywords[k], fixed("\\b"), ""), sep=", ")
        }
      }
    }
  }
  d$Present_Keywords <- str_sub(d$Present_Keywords, 5) # replace the NA stuff from concatenating with an NA string
  return(d) # return the report
}