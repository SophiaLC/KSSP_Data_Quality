# AUTHOR: Mark White
# CONTACT: markhwhiteii@gmail.com
# DATE: 2017-05-04
# DOCUMENTATION: See valagGuide.pdf and valagExplainer.pdf

valag <- function(channel, begin, end, table, location="KS") {
  require(dplyr)
  query <- paste0("SELECT C_Biosense_Facility_ID, C_BioSense_ID, C_Visit_Date_Time, 
                  Arrived_Date_Time, Chief_Complaint_Text, Admit_Reason_Description
                  FROM ", location, "_", table, "_Processed WHERE C_Visit_Date_Time >= '", begin, "' AND C_Visit_Date_Time <= '", end, "'")
  dat <- sqlQuery(channel, query)
  names <- sqlQuery(channel, paste0("SELECT C_Biosense_Facility_ID, Facility_Name
                                     FROM ", location, "_MFT"))
  names$C_Biosense_Facility_ID <- as.factor(names$C_Biosense_Facility_ID)
  datfm <- slice(group_by(dat, C_BioSense_ID), which.min(Arrived_Date_Time))
  datfm$lag <- as.numeric(difftime(datfm$Arrived_Date_Time, datfm$C_Visit_Date_Time, units="hours"))
  outputfm <- data.frame(C_Biosense_Facility_ID=rownames(data.frame(Hours=with(datfm, tapply(lag, C_Biosense_Facility_ID, mean, na.rm=TRUE)))),
                         First_Message=as.numeric(round(with(datfm, tapply(lag, C_Biosense_Facility_ID, mean, na.rm=TRUE)),2)),
                         row.names=NULL)
  outputfm$C_Biosense_Facility_ID <- factor(outputfm$C_Biosense_Facility_ID, levels=levels(names$C_Biosense_Facility_ID))
  datccar <- dat[-which(is.na(dat$Chief_Complaint_Text)==TRUE & is.na(dat$Admit_Reason_Description)==TRUE),]
  datccar <- slice(group_by(datccar, C_BioSense_ID), which.min(Arrived_Date_Time))
  datccar$lag <- as.numeric(difftime(datccar$Arrived_Date_Time, datccar$C_Visit_Date_Time, units="hours"))  
  outputccar <- data.frame(C_Biosense_Facility_ID=rownames(data.frame(Hours=with(datccar, tapply(lag, C_Biosense_Facility_ID, mean, na.rm=TRUE)))),
                           CC_or_AR=as.numeric(round(with(datccar, tapply(lag, C_Biosense_Facility_ID, mean, na.rm=TRUE)),2)),
                           row.names=NULL)
  outputccar$C_Biosense_Facility_ID <- factor(outputccar$C_Biosense_Facility_ID, levels=levels(names$C_Biosense_Facility_ID))
  output <- left_join(outputfm, outputccar, by="C_Biosense_Facility_ID") 
  output <- right_join(names, output, by="C_Biosense_Facility_ID")
  cat("Average hours between visit and (a) arrival of first message (First_Message)
and (b) arrival of first message with chief complaint or admit reason (CC_or_AR), by facility.
Visits between:", begin, "and", end, "(inclusive).\n\n")
  print(output)
}

valag1 <- function(channel, begin, end, table, facility, location="KS") {
  require(dplyr)
  query <- paste0("SELECT C_BioSense_ID, C_Visit_Date_Time, Arrived_Date_Time, Chief_Complaint_Text, Admit_Reason_Description
                   FROM ", location, "_", table, "_Processed 
                   WHERE C_Visit_Date_Time >= '", begin, "' AND C_Visit_Date_Time <= '", end, "' AND C_Biosense_Facility_ID=", facility)
  dat <- sqlQuery(channel, query)
  name <- sqlQuery(channel, paste0("SELECT Facility_Name
                                    FROM ", location, "_MFT 
                                    WHERE C_Biosense_Facility_ID=", facility))
  datfm <- slice(group_by(dat, C_BioSense_ID), which.min(Arrived_Date_Time))
  datfm$lag <- as.numeric(difftime(datfm$Arrived_Date_Time, datfm$C_Visit_Date_Time, units="hours"))
  datccar <- dat[-which(is.na(dat$Chief_Complaint_Text)==TRUE & is.na(dat$Admit_Reason_Description)==TRUE),]
  datccar <- slice(group_by(dat, C_BioSense_ID), which.min(Arrived_Date_Time))
  datccar$lag <- as.numeric(difftime(datccar$Arrived_Date_Time, datccar$C_Visit_Date_Time, units="hours"))
  if (is.nan(mean(datfm$lag))==TRUE) {
    cat("There were no visits to this facility in this time frame.")
  } else {
    cat(as.character(name[1,1]),
"\nC_Biosense_Facility_ID:", facility,
"\nVisits between:", begin, "and", end, "(inclusive)
The average time between visit and first message arrival:", round(mean(datfm$lag),2), "hours
The average time between visit and first message with chief complaint or admit reason:", round(mean(datccar$lag),2), "hours") 
  }
}