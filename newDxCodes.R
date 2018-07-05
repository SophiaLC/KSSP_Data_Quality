newDxCodes <- function(channel, month, year, oldcounts) {
  
  suppressMessages(require(RODBC))
  suppressMessages(require(dplyr))
  suppressMessages(require(stringr))
  suppressMessages(require(stringi))
  suppressMessages(require(tidytext))
  
  year_ch <- str_sub(year,start=-2)
  get_ch <- data.frame(month=c("01","02","03","04","05","06",
                               "07","08","09","10","11","12"),
                       ch=c("jan","feb","mar","apr","may",
                            "jun","jul","aug","spt","oct",
                            "nov","dec"))
  month_ch <- as.character(get_ch[which(get_ch$month %in% month),"ch"])
  
  newraw <- sqlQuery(channel, 
                     query=paste0("SELECT C_BioSense_ID, Diagnosis_Code
                                   FROM KS_PR_Processed 
                                   WHERE MONTH(C_Visit_Date_Time) = ", month, " AND YEAR(C_Visit_Date_Time) = ", year))
  
  newraw <- subset(newraw, subset=!is.na(Diagnosis_Code))

  newraw <- within(newraw, list(Diagnosis_Code <- str_replace_all(Diagnosis_Code, "[;]", " "),
                                Diagnosis_Code <- str_replace_all(Diagnosis_Code, "[.]", "")))
  
  newword <- unnest_tokens(newraw, code, Diagnosis_Code)
  newword <- filter(group_by(newword, C_BioSense_ID), !duplicated(code))
  newcounts <- count(ungroup(newword), code, sort=TRUE)
  colnames(newcounts) <- c("code", paste0("n_", month_ch, year_ch))
  
  oldcounts$new <- factor("not new")
  allcounts <- full_join(oldcounts, newcounts, by="code")
  allcounts$new <- ifelse(is.na(allcounts$new), "new", "not new")
  allcounts[is.na(allcounts)] <- 0
  
  newcodes <- c(allcounts[which(allcounts$new=="new"),"code"])
  
  report <- data_frame()
  for (i in 1:length(newcodes)) {
    report <- bind_rows(report, unique(newword[which(newword$code %in% newcodes[i]),c("C_BioSense_ID","code")]))
  }
  
  report <- report[-grep("^[[:digit:]]*$", report$code),]
  
  output <- list(report, subset(allcounts, select=-new))
  names(output) <- c("report", "allcounts")
  return(output)
}