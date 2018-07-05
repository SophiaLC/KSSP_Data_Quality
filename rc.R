# AUTHOR: Mark White
# CONTACT: markhwhiteii@gmail.com
# DATE: 2017-05-18
# DOCUMENTATION: See rcGuide.pdf and rcExplainer.pdf
# UPDATE INFORMATION:
# Wesley Medical Centers (3875) are going into production soon.
# The only problem is that two facilities have the same number:
# Both Wesley Medical Center and Wesley Medical Center -Pediatric ED.
# When we merge our data with facility names, it creates TWO rows
# for every one record in 3875--one labeled WMC, another WMC-PED.
# So all I did was add in a line specifying if it says WMC-PED,
# then we simply drop those duplicate rows.
# The user will just have to remember that Wesley Medical Center
# refers to both of the centers.
# The updated lines of code are marked below.

feedrc <- function(channel, visit, arrived, month, year) {
  
  suppressPackageStartupMessages(require(lubridate))
  suppressPackageStartupMessages(require(data.table))
  suppressPackageStartupMessages(require(dplyr))
  suppressPackageStartupMessages(require(Hmisc))
  
  if (visit==TRUE) {
    dv <- sqlQuery(channel, paste0("SELECT C_Visit_Date_Time, Feed_Name
                                   FROM KS_PR_PRocessed
                                   WHERE MONTH(C_Visit_Date_Time) = ", month, " AND YEAR(C_Visit_Date_Time) = ", year))
    dv$Visit_Date <- gsub(" UTC", "", floor_date(ymd_hms(dv$C_Visit_Date_Time), unit="day"))
    outv <- setDT(as.data.frame.matrix(table(dv$Visit_Date, dv$Feed_Name)), keep.rownames = TRUE)
  }
  
  if (arrived==TRUE) {
    da <- sqlQuery(channel, paste0("SELECT Arrived_Date_Time, Feed_Name
                                   FROM KS_PR_PRocessed
                                   WHERE MONTH(Arrived_Date_Time) = ", month, " AND YEAR(Arrived_Date_Time) = ", year))
    da$Arrived_Date <- gsub(" UTC", "", floor_date(ymd_hms(da$Arrived_Date_Time), unit="day"))
    outa <- setDT(as.data.frame.matrix(table(da$Arrived_Date, da$Feed_Name)), keep.rownames = TRUE)
  }
  
  if (visit==TRUE & arrived==FALSE) {
    setnames(outv, 1, "Visit_Date")
    return(outv)
    
  } else if (visit==FALSE & arrived==TRUE) {
    setnames(outa, 1, "Arrived_Date")
    return(outa) 
    
  } else if (visit==TRUE & arrived==TRUE) {
    setnames(outa, 1, "Date")
    setnames(outv, 1, "Date")
    outav <- full_join(outa, outv, by="Date", suffix=c("_A", "_V"))
    outav[is.na(outav)] <- 0
    return(outav[order(outav$Date),c(colnames(outav)[1], sort(colnames(outav)[2:length(colnames(outav))]))])
    
  } else {
    stop(cat("Please specify visit and/or arrived as TRUE."))
  }
}

facilityrc <- function(channel, visit, arrived, month, year) {
  
  suppressPackageStartupMessages(require(lubridate))
  suppressPackageStartupMessages(require(data.table))
  suppressPackageStartupMessages(require(dplyr))
  suppressPackageStartupMessages(require(Hmisc))
  
  if (visit==TRUE) {
    dv <- sqlQuery(channel, paste0("SELECT C_Visit_Date_Time, C_Biosense_Facility_ID
                                   FROM KS_PR_PRocessed
                                   WHERE MONTH(C_Visit_Date_Time) = ", month, " AND YEAR(C_Visit_Date_Time) = ", year))
    dv$Visit_Date <- gsub(" UTC", "", floor_date(ymd_hms(dv$C_Visit_Date_Time), unit="day"))
    
    
    dv <- left_join(dv, sqlQuery(channel, "SELECT C_Biosense_Facility_ID, Facility_Name FROM KS_MFT"), 
                    by="C_Biosense_Facility_ID")
    
    dv <- subset(dv, Facility_Name != "Wesley Medical Center -Pediatric ED") ## NEW LINE IN THIS UPDATE ##
    
    dv$Facility_Name <- droplevels(dv$Facility_Name)
    namekey <- unique(dv[,c("C_Biosense_Facility_ID","Facility_Name")])
    outv <- setDT(as.data.frame.matrix(table(dv$Visit_Date, dv$C_Biosense_Facility_ID)), keep.rownames = TRUE)
    for (i in 2:length(colnames(outv))) {
      id <- colnames(outv)[i]
      label(outv[[id]]) <- as.character(namekey[which(namekey$C_Biosense_Facility_ID==id),"Facility_Name"])
    }
  }
  
  if (arrived==TRUE) {
    da <- sqlQuery(channel, paste0("SELECT Arrived_Date_Time, C_Biosense_Facility_ID
                                   FROM KS_PR_PRocessed
                                   WHERE MONTH(Arrived_Date_Time) = ", month, " AND YEAR(Arrived_Date_Time) = ", year))
    da$Arrived_Date <- gsub(" UTC", "", floor_date(ymd_hms(da$Arrived_Date_Time), unit="day"))
    outa <- setDT(as.data.frame.matrix(table(da$Arrived_Date, da$C_Biosense_Facility_ID)), keep.rownames = TRUE)
    
    da <- left_join(da, sqlQuery(channel, "SELECT C_Biosense_Facility_ID, Facility_Name FROM KS_MFT"), 
                    by="C_Biosense_Facility_ID")
    
    da <- subset(da, Facility_Name != "Wesley Medical Center -Pediatric ED") ## NEW LINE IN THIS UPDATE ##
    
    da$Facility_Name <- droplevels(da$Facility_Name)
    namekey <- unique(da[,c("C_Biosense_Facility_ID","Facility_Name")])
    outa <- setDT(as.data.frame.matrix(table(da$Arrived_Date, da$C_Biosense_Facility_ID)), keep.rownames = TRUE)
    for (i in 2:length(colnames(outa))) {
      id <- colnames(outa)[i]
      label(outa[[id]]) <- as.character(namekey[which(namekey$C_Biosense_Facility_ID==id),"Facility_Name"])
    }
  }
  
  if (visit==TRUE & arrived==FALSE) {
    setnames(outv, 1, "Visit_Date")
    return(outv)
    
  } else if (visit==FALSE & arrived==TRUE) {
    setnames(outa, 1, "Arrived_Date")
    return(outa) 
    
  } else if (visit==TRUE & arrived==TRUE) {
    setnames(outa, 1, "Date")
    setnames(outv, 1, "Date")
    outav <- full_join(outa, outv, by="Date", suffix=c("_A", "_V"))
    outav[is.na(outav)] <- 0
    return(outav[order(outav$Date),c(colnames(outav)[1], sort(colnames(outav)[2:length(colnames(outav))]))])
    
  } else {
    stop(cat("Please specify visit and/or arrived as TRUE."))
  }  
}
