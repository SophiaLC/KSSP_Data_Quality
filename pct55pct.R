pct55pct <- 
  function(username, password, month, year) {
    suppressPackageStartupMessages(require(RODBC))
    suppressPackageStartupMessages(require(dplyr))
    suppressPackageStartupMessages(require(tidyr))
    
    # get data
    ch <- odbcConnect("BioSense_Platform", paste0("BIOSENSE\\", username), password)
    dat <- sqlQuery(
      ch, 
      paste0("SELECT C_BioSense_ID, Trigger_Event, Chief_Complaint_Combo, Diagnosis_Code, Diagnosis_Description, 
             Diagnosis_Type, Patient_Class_Code, Discharge_Disposition, Discharge_Date_Time, Medical_Record_Number,
             First_Patient_ID, Age_Reported, Birth_Date_Time, Race_Code, Ethnicity_Code, Patient_City,
             Patient_State, Patient_Zip, Patient_Country, C_Patient_County, Death_Date_Time, Patient_Death_Indicator
             FROM KS_PR_Processed WHERE MONTH(C_Visit_Date_Time) = ", month, " AND YEAR(C_Visit_Date_Time) = ", year)
      )
    odbcCloseAll()
    
    # make first part of output, with 16 fields that are required once per every visit
    out1 <- dat %>% # take data
      group_by(C_BioSense_ID) %>% # group by id
      select(-c(Discharge_Disposition, Discharge_Date_Time, Death_Date_Time, Patient_Death_Indicator)) %>% # get only vars we want
      summarise_all(funs(mean(is.na(.))!=1)) # if all are na it will equal 1, so true returns if it is reported at least once in a visit
    # will join this with the other four checks in a few steps  
    
    # make second part of output, for discharge disposition and discharge date time
    out2 <- dat %>% # take data
      filter(Trigger_Event=="A03") %>% # keep only A03 messages
      select(c(Discharge_Disposition, Discharge_Date_Time, C_BioSense_ID)) %>% # select vars only we need
      group_by(C_BioSense_ID) %>% # group by patient visit
      summarise_all(funs(mean(is.na(.))!=1)) # if all all are na, it will equal 1, so returns true if it reported at least once
    # will koin this with the other two checks in a few steps
    
    # make third part of output, for death date time and death indicator
    out3 <- dat %>% # take data
      filter(Discharge_Disposition %in% c(20, 40, 41, 42)) %>% # filter only rows with these discharge dispositions
      select(c(Death_Date_Time, Patient_Death_Indicator, C_BioSense_ID)) %>% # select only vars we need
      group_by(C_BioSense_ID) %>% # group by visit
      summarise_all(funs(mean(is.na(.))!=1)) # true if it is not all na; that is, if it is reported at least once
    
    return(
      out1 %>% # take first part
        full_join(., out2, by="C_BioSense_ID") %>% # join with the second part
        full_join(., out3, by="C_BioSense_ID") %>% # join with third part
        transmute(Percent_Complete=round(rowMeans(.[,2:ncol(.)], na.rm=TRUE),2)) %>% # get mean (proportion) of complete and round it
        mutate(
          `55 Pct`=ifelse(Percent_Complete>=.55, TRUE, FALSE),
          `65 Pct`=ifelse(Percent_Complete>=.65, TRUE, FALSE),
          `75 Pct`=ifelse(Percent_Complete>=.75, TRUE, FALSE),
          `85 Pct`=ifelse(Percent_Complete>=.85, TRUE, FALSE),
          `90 Pct`=ifelse(Percent_Complete>=.90, TRUE, FALSE),
          `95 Pct`=ifelse(Percent_Complete>=.95, TRUE, FALSE)
        ) %>% # mark if they met a certain threshold or not
        select(-Percent_Complete) %>% # group percent complete
        summarise_all(funs(mean)) %>% # get percent of people at each threshold
        gather(Treshold, Percent, 1:ncol(.)) %>% # gather data
        mutate(Percent=round(Percent*100,2)) # get to percent and round to two decimal points
    )
  }
