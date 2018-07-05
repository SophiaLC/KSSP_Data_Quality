dayHeatCount <-
function(channel, date) {
  d <- sqlQuery(channel, paste0("SELECT C_BioSense_ID, C_Biosense_Facility_ID, Arrived_Date_Time, Admit_Reason_Description, Chief_Complaint_Text,
                                           Diagnosis_Description, Triage_Notes, Diagnosis_Code 
                                 FROM KS_PR_Processed
                                 WHERE C_Visit_Date_Time >= '", date, " 00:00:00' AND C_Visit_Date_Time <= '", date, " 23:59:59'")) # get data
  
  # first, cast a wide net and get everything with these keywords:
  keyw1 <- c("\\bheat\\b", "heatcramp", "heatex", "heatst", "heat-ex", "heat-related", "heat-stroke", "hypertherm", "overheat",
             "over heat", "sun stroke", "sunstr", "sun-str", "to hot", "too hot", "\\bheet\\b", "\\bhot\\b", 
             # SUNBURN WORDS
             "sunburn", "sun ex",
             "sun poison", "sun rash")
  
  # after you get those, only include ones that also include these:
  keyw2 <- c("excessive", "exhaust", "expos", "fatigue", "cramp", "stress", "in car", "outside", "out side", "prostration")
  
  # then whittle it down by eiminating any with these words:
  keyw3 <- c("\\ballerg", "feeling hot", "feels hot", "felt hot", "hot sensation", "heat sensation", "\\binflam", "ibuprofen",
             "ibuprophen", "\\balieve\\b", "\\bmotrin\\b", "tylenol", "\\binjur", "\\btrauma", "heat pack", "lumbago", 
             "relief", "resolve", "relieve", "releive", "dental", "hot dog", "hot grease", "hot peppers", "hot tea", "heat ache", 
             "heatache", "heat attack", "heat beat", "heatbeat", "heat burn", "heat flutter", "heat racing", "heat rate", "heatrate",
             "hitting heat", "palpitation", "\\baccident\\b", "\\balcohol\\b", "distress", "fever", "gets hot", "heat flash", 
             "hot flash", "hot tub", "no heat")
  
  # then some conditional whittling...
  # if burn is present, then these cannot be:
  keyw4 <- c("\\bmouth\\b")
  
  # if pain is present, then these cannot be:
  keyw5 <- c("\\blimb\\b", "\\barm\\b", "\\bshoulder\\b", "\\belbow\\b", "\\bwrist\\b", "\\bhand\\b", "\\bleg\\b", "\\bhip\\b", 
             "\\bgroin\\b", "\\bthigh\\b", "\\bknee\\b", "\\bankle\\b", "\\bfoot\\b", "\\bfeet\\b", "\\bred\\b", 
             "\\bradiat", "\\bredness\\b", "\\bswell\\b", "\\bswollen\\b", "\\bsurg", "\\bpost op\\b", "\\bback\\b", 
             "\\bneck\\b", "\\bflank\\b", "\\bjaw\\b", "\\bmouth\\b", "\\bteeth\\b", "\\btooth\\b")
  
  # if heat is present, then these cannot be:
  keyw6 <- c("\\bapplied\\b", "\\btried\\b", "\\bused\\b", "\\busing\\b", "\\bice\\b", "\\bcold\\b", "\\bsensitiv", "\\brash\\b")
  
  # if hot is present, then these cannot be:
  keyw7 <- c("\\bcold\\b", "\\bsensitiv", "\\bshower\\b", "\\boven\\b", "\\bsuicid")
  
  # look for these diagnosis codes
  dx1 <- c("^t67", "^x30", 
           # SUNBURN DX CODES
           "^l55", "^x32")
  
  # and exclude anything with these diagnosis codes
  dx2 <- c("^t50992a", "^w92")
  
  d <- d %>% 
    mutate(Admit_Reason_Description=str_replace_na(Admit_Reason_Description, ""), # removing NAs
           Chief_Complaint_Text=str_replace_na(Chief_Complaint_Text, ""), # removing NAs
           Diagnosis_Description=str_replace_na(Diagnosis_Description, ""), # removing NAs
           Triage_Notes=str_replace_na(Triage_Notes, ""), # removing NAs
           Full_Text=tolower(str_replace_all(paste(Admit_Reason_Description, Chief_Complaint_Text, Diagnosis_Description, Triage_Notes), 
                                             "[^[a-zA-Z ]]", " ")), # combining all fields, removing everything but letter characters, making lowercase
           Diagnosis_Code=tolower(Diagnosis_Code), # make lowercase
           Diagnosis_Code=str_replace_all(Diagnosis_Code, "[;]", " "), # replace semicolon with space
           Diagnosis_Code=str_replace_all(Diagnosis_Code, "[.]", "")) # remove period
  
  # DIAGNOSIS CODES: LOOK FOR ROWS WITH THESE
  rows <- NULL # creating empty vector to fill with rows that match search
  for (i in 1:length(dx1)) { # for every diagnisis code...
    rows <- c(rows, grep(dx1[i], d$Diagnosis_Code)) # if the diagnosis code is found in the Diagnosis_Code field, return the row number
  }
  
  if (length(rows)>0) { # if rows are longer than 0 (i.e., any rows were added)
    rows <- rows[!duplicated(rows)] # remove duplicates
    dxd <- d[rows,] # only include the rows we want 
  }
  
  # KEYWORDS 1: CAST WIDE NET
  rows <- NULL # creating empty vector to fill with rows that match search
  for (i in 1:length(keyw1)) { # doing this for every keyword
    rows <- c(rows, grep(keyw1[i], d$Full_Text)) # see if the keyword is found in the Full_Text field, if it is, return the row numbers
  }
  
  if (length(rows)>0) {
    rows <- rows[!duplicated(rows)] # remove duplicates
    d <- d[rows,] # only include the rows we want 
  }
  
  # KEYWORDS 2: ONLY INCLUDE ONES THAT ALSO INCLUDE THESE
  rows <- NULL # creating empty vector to fill with rows that match search
  for (i in 1:length(keyw2)) { # doing this for every keyword
    rows <- c(rows, grep(keyw2[i], d$Full_Text)) # see if the keyword is found in the Full_Text field, if it is, return the row numbers
  }
  
  if (length(rows)>0) {
    rows <- rows[!duplicated(rows)] # remove duplicates
    d <- d[rows,] # only include the rows we want 
  }
  
  # KEYWORDS 3: DELETE ANYTHING WITH THESE WORDS IN IT
  rows <- NULL # creating empty vector to fill with rows that match search
  for (i in 1:length(keyw3)) { # doing this for every keyword
    rows <- c(rows, grep(keyw3[i], d$Full_Text)) # see if the keyword is found in the Full_Text field, if it is, return the row numbers
  }
  
  if (length(rows)>0) {
    rows <- rows[!duplicated(rows)] # remove duplicates
    d <- d[-rows,] # only include the rows we want 
  }
  
  # KEYWORDS 4: CHECK, BUT ONLY IF BURN IS PRESENT
  rows <- NULL # creating empty vector to fill with rows that match search
  for (i in 1:nrow(d)) {
    if (grepl("\\bburn", d$Full_Text[i])==TRUE) {
      for (j in 1:length(keyw4)) {
        if (grepl(keyw4[j], d$Full_Text[i])==TRUE) {
          rows <- c(rows, i)
        }
      }
    }
  }
  
  if (length(rows)>0) {
    rows <- rows[!duplicated(rows)] # remove duplicates
    d <- d[-rows,] # only include the rows we want 
  }
  
  # KEYWORDS 5: CHECK, BUT ONLY IF PAIN IS PRESENT
  rows <- NULL # creating empty vector to fill with rows that match search
  for (i in 1:nrow(d)) {
    if (grepl("\\bpain\\b", d$Full_Text[i])==TRUE) {
      for (j in 1:length(keyw5)) {
        if (grepl(keyw5[j], d$Full_Text[i])==TRUE) {
          rows <- c(rows, i)
        }
      }
    }
  }
  
  if (length(rows)>0) {
    rows <- rows[!duplicated(rows)] # remove duplicates
    d <- d[-rows,] # only include the rows we want 
  }
  
  # KEYWORDS 6: CHECK, BUT ONLY IF HEAT IS PRESENT
  rows <- NULL # creating empty vector to fill with rows that match search
  for (i in 1:nrow(d)) {
    if (grepl("\\bheat\\b", d$Full_Text[i])==TRUE) {
      for (j in 1:length(keyw6)) {
        if (grepl(keyw6[j], d$Full_Text[i])==TRUE) {
          rows <- c(rows, i)
        }
      }
    }
  }
  
  if (length(rows)>0) {
    rows <- rows[!duplicated(rows)] # remove duplicates
    d <- d[-rows,] # only include the rows we want 
  }
  
  # KEYWORDS 7: CHECK, BUT ONLY IF HOT IS PRESENT
  rows <- NULL # creating empty vector to fill with rows that match search
  for (i in 1:nrow(d)) {
    if (grepl("\\bhot\\b", d$Full_Text[i])==TRUE) {
      for (j in 1:length(keyw7)) {
        if (grepl(keyw7[j], d$Full_Text[i])==TRUE) {
          rows <- c(rows, i)
        }
      }
    }
  }
  
  if (length(rows)>0) {
    rows <- rows[!duplicated(rows)] # remove duplicates
    d <- d[-rows,] # only include the rows we want 
  }
  
  # COMBINE DX RESULTS WITH TEXT RESULTS, DELETING DUPLICATE ROWS
  d <- d %>% 
    bind_rows(dxd) %>% 
    distinct()
  
  # REMOVING DX CODES WE DO NOT WANT
  rows <- NULL
  for (i in 1:length(dx2)) {
    rows <- c(rows, grep(dx2[i], d$Diagnosis_Code))
  }
  
  if (length(rows)>0) {
    rows <- rows[!duplicated(rows)] # remove duplicates
    d <- d[-rows,] # only include the rows we want 
  }
  
  d <- d %>% # take data
    group_by(C_BioSense_ID) %>% # group by patient visit
    top_n(1, Arrived_Date_Time) %>% # take most recent message
    slice(1) # take one, if there are multiple
  
  return(cat("There were", nrow(d), "heat related illness patient visits on", date))
}
