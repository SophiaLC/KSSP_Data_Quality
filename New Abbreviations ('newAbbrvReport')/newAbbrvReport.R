newAbbrvReport <- function(channel, month, year) {
  
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
                     query=paste0("SELECT C_BioSense_ID, Admit_Reason_Description, Chief_Complaint_Text
                                   FROM KS_PR_Processed 
                                   WHERE MONTH(C_Visit_Date_Time) = ", month, " AND YEAR(C_Visit_Date_Time) = ", year))
  newraw <- within(newraw, list(Admit_Reason_Description <- str_replace_na(Admit_Reason_Description, ""),
                                Chief_Complaint_Text <- str_replace_na(Chief_Complaint_Text, ""),
                                textraw <- paste(Admit_Reason_Description, Chief_Complaint_Text),
                                text <- str_replace_all(textraw, "[^[a-zA-Z ]]", " ")))
  colnames(newraw) <- c("id","admit_reason","chief_complaint","text","textraw")
  
  newword <- unnest_tokens(newraw, word, text)
  newword <- filter(group_by(newword, id), !duplicated(word))
  data("stop_words")
  newword <- anti_join(newword, stop_words, by="word")
  
  newcounts <- count(ungroup(newword), word, sort=TRUE)
  
  oldcounts$new <- factor("not new")
  allcounts <- full_join(oldcounts, newcounts, by="word")
  colnames(allcounts)[which(names(allcounts)=="n")] <- paste0("n_", month_ch, year_ch)
  allcounts$new <- ifelse(is.na(allcounts$new), "new", "not new")
  allcounts[is.na(allcounts)] <- 0 
  
  allcounts$length <- stri_length(allcounts$word)
  newabrv <- c(allcounts[which(allcounts$new=="new" & allcounts$length<4),"word"]$word)
  
  report <- data_frame()
  for (i in 1:length(newabrv)) {
    report <- bind_rows(report, unique(newword[which(newword$word %in% newabrv[i]),c("id","word","textraw")]))
  }
  
  output <- list(report, subset(allcounts, select=-c(new,length)))
  names(output) <- c("report", "allcounts")
  return(output)
}