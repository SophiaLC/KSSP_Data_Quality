#### prep ####
setwd("~/mhwii/misc")
library(tabulizer)
library(dplyr)
library(tidyr)
library(magrittr)
library(stringr)

#### notes ####
# not including state totals, because that is at a different level of analysis than zip
# the different matrices on different pages get read in differently, so I had to change the code slightly for every page
# thus, this code is not as readily applicable to another .pdf or state, but the skeleton here can be adapted

#### pg. 7, amphetamine ####
pg7 <- extract_tables("report_yr_2016.pdf", pages=7) # read pdf into matrix
pg7 <- pg7[[1]][c(1:19),-c(1,3,6)] # getting just kansas
pg7 <- as.data.frame(pg7) # convert to data frame
colnames(pg7) <- c("zip", "q1", "q2", "q3", "q4", "total") # change column names
pg7$drug <- c("amphetamine") # enter drug type name
pg7[,c(2:6)] <- lapply(pg7[,c(2:6)], function(x) str_replace_all(x, ",", "")) # remove commas
pg7 <- gather(pg7, quarter, grams, q1:total) # convert to long format

#### pg. 23, dl-methamphetamine ####
pg23 <- extract_tables("report_yr_2016.pdf", pages=23) # read pdf into matrix
pg23 <- pg23[[2]][c(6,7),-c(1,3,5,7,9,11)] # getting just kansas
pg23 <- as.data.frame(pg23) # convert to data frame
colnames(pg23) <- c("zip", "q1", "q2", "q3", "q4", "total") # change column names
pg23$drug <- c("dl-methamphetamine") # enter drug type name
pg23 <- gather(pg23, quarter, grams, q1:total) # convert to long format

arcos_2016_summary_ks <- bind_rows(pg7, pg23) # combine data thus far

#### pg. 32, d-methamphetamine ####
pg32 <- extract_tables("report_yr_2016.pdf", pages=32) # read pdf into matrix
pg32 <- pg32[[2]][c(7:14),c(2,4,6,8,9,12)] # getting just kansas
pg32 <- as.data.frame(pg32) # convert to data frame
colnames(pg32) <- c("zip", "q1", "q2", "q3", "q4", "total") # change column names
pg32$drug <- c("d-methamphetamine") # enter drug type name
pg32 <- gather(pg32, quarter, grams, q1:total) # convert to long format

arcos_2016_summary_ks <- bind_rows(arcos_2016_summary_ks, pg32) # combine data thus far

#### pg. 46, lisdexamfetamine ####
pg46 <- extract_tables("report_yr_2016.pdf", pages=46) # read pdf into matrix
pg46 <- pg46[[1]][c(46:57),-c(1,3,6)] # getting just kansas
pg46 <- as.data.frame(pg46) # convert to data frame
colnames(pg46) <- c("zip", "q1", "q2", "q3", "q4", "total") # change column names
pg46$drug <- c("lisdexamfetamine") # enter drug type name
pg46[,c(2:6)] <- lapply(pg46[,c(2:6)], function(x) str_replace_all(x, ",", "")) # remove commas
pg46 <- gather(pg46, quarter, grams, q1:total) # convert to long format

arcos_2016_summary_ks <- bind_rows(arcos_2016_summary_ks, pg46) # combine data thus far

#### pg. 47, lisdexamfetamine ####
pg47 <- extract_tables("report_yr_2016.pdf", pages=47) # read pdf into matrix


pg47 <- pg47[[1]][c(1:7),-c(1)] # getting just kansas
pg47 <- as.data.frame(pg47) # convert to data frame
colnames(pg47) <- c("zip", "q1", "q2", "q3", "q4", "total") # change column names
pg47$drug <- c("lisdexamfetamine") # enter drug type name
pg47[,c(2:6)] <- lapply(pg47[,c(2:6)], function(x) str_replace_all(x, ",", "")) # remove commas
pg47 <- gather(pg47, quarter, grams, q1:total) # convert to long format

arcos_2016_summary_ks <- bind_rows(arcos_2016_summary_ks, pg47) # combine data thus far

#### pg. 66, methylphenidate ####
pg66 <- extract_tables("report_yr_2016.pdf", pages=66) # read pdf into matrix
pg66 <- pg66[[1]][c(29:45),-c(1,3,6)] # getting just kansas
pg66 <- as.data.frame(pg66) # convert to data frame
colnames(pg66) <- c("zip", "q1", "q2", "q3", "q4", "total") # change column names
pg66$drug <- c("methylphenidate") # enter drug type name
pg66[,c(2:6)] <- lapply(pg66[,c(2:6)], function(x) str_replace_all(x, ",", "")) # remove commas
pg66 <- gather(pg66, quarter, grams, q1:total) # convert to long format

arcos_2016_summary_ks <- bind_rows(arcos_2016_summary_ks, pg66) # combine data thus far

#### pg. 66b, methylphenidate ####
pg66 <- extract_tables("report_yr_2016.pdf", pages=66) # read pdf into matrix
pg66 <- pg66[[2]][c(1,2),-c(1,3,6)] # getting just kansas
pg66 <- as.data.frame(pg66) # convert to data frame
colnames(pg66) <- c("zip", "q1", "q2", "q3", "q4", "total") # change column names
pg66$drug <- c("methylphenidate") # enter drug type name
pg66[,c(2:6)] <- lapply(pg66[,c(2:6)], function(x) str_replace_all(x, ",", "")) # remove commas
pg66 <- gather(pg66, quarter, grams, q1:total) # convert to long format

arcos_2016_summary_ks <- bind_rows(arcos_2016_summary_ks, pg66) # combine data thus far

#### pg. 84, barbaturicacid ####
pg84 <- extract_tables("report_yr_2016.pdf", pages=84) # read pdf into matrix
pg84 <- pg84[[2]][c(32:38),c(2,4,6,8,10,12)] # getting just kansas
pg84 <- as.data.frame(pg84) # convert to data frame
colnames(pg84) <- c("zip", "q1", "q2", "q3", "q4", "total") # change column names
pg84$drug <- c("barbaturicacid") # enter drug type name
pg84 <- gather(pg84, quarter, grams, q1:total) # convert to long format

arcos_2016_summary_ks <- bind_rows(arcos_2016_summary_ks, pg84) # combine data thus far

#### pg. 92, amobarbital ####
pg92 <- extract_tables("report_yr_2016.pdf", pages=92) # read pdf into matrix
pg92 <- pg92[[2]][c(10,11),c(2,4,6,8,10,12)] # getting just kansas
pg92 <- as.data.frame(pg92) # convert to data frame
colnames(pg92) <- c("zip", "q1", "q2", "q3", "q4", "total") # change column names
pg92$drug <- c("amobarbital") # enter drug type name
pg92 <- gather(pg92, quarter, grams, q1:total) # convert to long format

arcos_2016_summary_ks <- bind_rows(arcos_2016_summary_ks, pg92) # combine data thus far

#### pg. 102, butalbital ####
pg102 <- extract_tables("report_yr_2016.pdf", pages=102) # read pdf into matrix
pg102 <- pg102[[1]][c(7:22),c(2,4,6,8,9,11)] # getting just kansas
pg102 <- as.data.frame(pg102) # convert to data frame
colnames(pg102) <- c("zip", "q1", "q2", "q3", "q4", "total") # change column names
pg102$drug <- c("butalbital") # enter drug type name
pg102[,c(2:6)] <- lapply(pg102[,c(2:6)], function(x) str_replace_all(x, ",", "")) # remove commas
pg102 <- gather(pg102, quarter, grams, q1:total) # convert to long format

arcos_2016_summary_ks <- bind_rows(arcos_2016_summary_ks, pg102) # combine data thus far

#### pg. 102b, butalbital ####
pg102 <- extract_tables("report_yr_2016.pdf", pages=102) # read pdf into matrix
pg102 <- pg102[[2]][c(1:3),c(2,4,6,8,9,11)] # getting just kansas
pg102 <- as.data.frame(pg102) # convert to data frame
colnames(pg102) <- c("zip", "q1", "q2", "q3", "q4", "total") # change column names
pg102$drug <- c("butalbital") # enter drug type name
pg102[,c(2:6)] <- lapply(pg102[,c(2:6)], function(x) str_replace_all(x, ",", "")) # remove commas
pg102 <- gather(pg102, quarter, grams, q1:total) # convert to long format

arcos_2016_summary_ks <- bind_rows(arcos_2016_summary_ks, pg102) # combine data thus far

#### pg. 121, pentobarbital ####
pg121 <- extract_tables("report_yr_2016.pdf", pages=121) # read pdf into matrix
pg121 <- pg121[[1]][c(23:41),-c(1,3,6)] # getting just kansas
pg121 <- as.data.frame(pg121) # convert to data frame
colnames(pg121) <- c("zip", "q1", "q2", "q3", "q4", "total") # change column names
pg121$drug <- c("pentobarbital") # enter drug type name
pg121[,c(2:6)] <- lapply(pg121[,c(2:6)], function(x) str_replace_all(x, ",", "")) # remove commas
pg121 <- gather(pg121, quarter, grams, q1:total) # convert to long format

arcos_2016_summary_ks <- bind_rows(arcos_2016_summary_ks, pg121) # combine data thus far

#### pg. 135, secobarbital ####
pg135 <- extract_tables("report_yr_2016.pdf", pages=135) # read pdf into matrix
pg135 <- pg135[[2]][c(13:15),c(2,4,6,8,10,12)] # getting just kansas
pg135 <- as.data.frame(pg135) # convert to data frame
colnames(pg135) <- c("zip", "q1", "q2", "q3", "q4", "total") # change column names
pg135$drug <- c("secobarbital") # enter drug type name
pg135 <- gather(pg135, quarter, grams, q1:total) # convert to long format

arcos_2016_summary_ks <- bind_rows(arcos_2016_summary_ks, pg135) # combine data thus far

#### pg. 147, cocaine ####
pg147 <- extract_tables("report_yr_2016.pdf", pages=147) # read pdf into matrix
pg147 <- pg147[[2]][c(25:41),-c(1,3,6,9)] # getting just kansas
pg147 <- as.data.frame(pg147) # convert to data frame
colnames(pg147) <- c("zip", "q1", "q2", "q3", "q4", "total") # change column names
pg147$drug <- c("cocaine") # enter drug type name
pg147[,c(2:6)] <- lapply(pg147[,c(2:6)], function(x) str_replace_all(x, ",", "")) # remove commas
pg147 <- gather(pg147, quarter, grams, q1:total) # convert to long format

arcos_2016_summary_ks <- bind_rows(arcos_2016_summary_ks, pg147) # combine data thus far

#### pg. 148, cocaine ####
pg148 <- extract_tables("report_yr_2016.pdf", pages=148) # read pdf into matrix
pg148 <- pg148[[1]][c(1:2),-c(1,3,6,9)] # getting just kansas
pg148 <- as.data.frame(pg148) # convert to data frame
colnames(pg148) <- c("zip", "q1", "q2", "q3", "q4", "total") # change column names
pg148 <- pg148[-2,]
pg148$drug <- c("cocaine") # enter drug type name
pg148 <- gather(pg148, quarter, grams, q1:total) # convert to long format

arcos_2016_summary_ks <- bind_rows(arcos_2016_summary_ks, pg148) # combine data thus far

#### pg. 166, codeine ####
pg166 <- extract_tables("report_yr_2016.pdf", pages=166) # read pdf into matrix
pg166 <- pg166[[1]][c(1:19),-c(1,3,6)] # getting just kansas
pg166 <- as.data.frame(pg166) # convert to data frame
colnames(pg166) <- c("zip", "q1", "q2", "q3", "q4", "total") # change column names
pg166$drug <- c("codeine") # enter drug type name
pg166[,c(2:6)] <- lapply(pg166[,c(2:6)], function(x) str_replace_all(x, ",", "")) # remove commas
pg166 <- gather(pg166, quarter, grams, q1:total) # convert to long format

arcos_2016_summary_ks <- bind_rows(arcos_2016_summary_ks, pg166) # combine data thus far

#### pg. 185, buprenorphine ####
pg185 <- extract_tables("report_yr_2016.pdf", pages=185) # read pdf into matrix
pg185 <- pg185[[2]][c(17:32),-c(1,3,6)] # getting just kansas
pg185 <- as.data.frame(pg185) # convert to data frame
colnames(pg185) <- c("zip", "q1", "q2", "q3", "q4", "total") # change column names
pg185$drug <- c("buprenorphine") # enter drug type name
pg185[,c(2:6)] <- lapply(pg185[,c(2:6)], function(x) str_replace_all(x, ",", "")) # remove commas
pg185 <- gather(pg185, quarter, grams, q1:total) # convert to long format

arcos_2016_summary_ks <- bind_rows(arcos_2016_summary_ks, pg185) # combine data thus far

#### pg. 202, dihydrocodeine ####
pg202 <- extract_tables("report_yr_2016.pdf", pages=202) # read pdf into matrix
pg202 <- pg202[[2]][c(9,10),c(2,4,6,8,10,12)] # getting just kansas
pg202 <- as.data.frame(pg202) # convert to data frame
colnames(pg202) <- c("zip", "q1", "q2", "q3", "q4", "total") # change column names
pg202$drug <- c("dihydrocodeine") # enter drug type name
pg202 <- gather(pg202, quarter, grams, q1:total) # convert to long format

arcos_2016_summary_ks <- bind_rows(arcos_2016_summary_ks, pg202) # combine data thus far

#### pg. 215 oxycodone ####
pg215 <- extract_tables("report_yr_2016.pdf", pages=215) # read pdf into matrix
pg215 <- pg215[[1]][c(6:12),-c(1,3,6)] # getting just kansas
pg215 <- as.data.frame(pg215) # convert to data frame
colnames(pg215) <- c("zip", "q1", "q2", "q3", "q4", "total") # change column names
pg215$drug <- c("oxycodone") # enter drug type name
pg215[,c(2:6)] <- lapply(pg215[,c(2:6)], function(x) str_replace_all(x, ",", "")) # remove commas
pg215 <- gather(pg215, quarter, grams, q1:total) # convert to long format

arcos_2016_summary_ks <- bind_rows(arcos_2016_summary_ks, pg215) # combine data thus far

#### pg. 215b oxycodone ####
pg215 <- extract_tables("report_yr_2016.pdf", pages=215) # read pdf into matrix
pg215 <- pg215[[2]][c(1:12),-c(1,3,6)] # getting just kansas
pg215 <- as.data.frame(pg215) # convert to data frame
colnames(pg215) <- c("zip", "q1", "q2", "q3", "q4", "total") # change column names
pg215$drug <- c("oxycodone") # enter drug type name
pg215[,c(2:6)] <- lapply(pg215[,c(2:6)], function(x) str_replace_all(x, ",", "")) # remove commas
pg215 <- gather(pg215, quarter, grams, q1:total) # convert to long format

arcos_2016_summary_ks <- bind_rows(arcos_2016_summary_ks, pg215) # combine data thus far

#### pg. 234 hydromorphone ####
pg234 <- extract_tables("report_yr_2016.pdf", pages=234) # read pdf into matrix
pg234 <- pg234[[1]][c(44:50),-c(1,3,6)] # getting just kansas
pg234 <- as.data.frame(pg234) # convert to data frame
colnames(pg234) <- c("zip", "q1", "q2", "q3", "q4", "total") # change column names
pg234$drug <- c("hydromorphone") # enter drug type name
pg234[,c(2:6)] <- lapply(pg234[,c(2:6)], function(x) str_replace_all(x, ",", "")) # remove commas
pg234 <- gather(pg234, quarter, grams, q1:total) # convert to long format

arcos_2016_summary_ks <- bind_rows(arcos_2016_summary_ks, pg234) # combine data thus far

#### pg. 234b hydromorphone ####
pg234 <- extract_tables("report_yr_2016.pdf", pages=234) # read pdf into matrix
pg234 <- as.data.frame(pg234[[2]]) # convert to data frame
colnames(pg234) <- c("zip", "q1", "q2", "q3", "q4", "total") # change column names
pg234$drug <- c("hydromorphone") # enter drug type name
pg234[,c(2:6)] <- lapply(pg234[,c(2:6)], function(x) str_replace_all(x, ",", "")) # remove commas
pg234 <- gather(pg234, quarter, grams, q1:total) # convert to long format

arcos_2016_summary_ks <- bind_rows(arcos_2016_summary_ks, pg234) # combine data thus far

#### pg. 235 hydromorphone ####
pg235 <- extract_tables("report_yr_2016.pdf", pages=235) # read pdf into matrix
pg235 <- pg235[[1]][c(1:8),-c(1,3,6)] # getting just kansas
pg235 <- as.data.frame(pg235) # convert to data frame
colnames(pg235) <- c("zip", "q1", "q2", "q3", "q4", "total") # change column names
pg235$drug <- c("hydromorphone") # enter drug type name
pg235[,c(2:6)] <- lapply(pg235[,c(2:6)], function(x) str_replace_all(x, ",", "")) # remove commas
pg235 <- gather(pg235, quarter, grams, q1:total) # convert to long format

arcos_2016_summary_ks <- bind_rows(arcos_2016_summary_ks, pg235) # combine data thus far

#### pg. 254 hydrocodone ####
pg254 <- extract_tables("report_yr_2016.pdf", pages=254) # read pdf into matrix
pg254 <- pg254[[1]][c(24:30),-c(1,3,6)] # getting just kansas
pg254 <- as.data.frame(pg254) # convert to data frame
colnames(pg254) <- c("zip", "q1", "q2", "q3", "q4", "total") # change column names
pg254$drug <- c("hydrocodone") # enter drug type name
pg254[,c(2:6)] <- lapply(pg254[,c(2:6)], function(x) str_replace_all(x, ",", "")) # remove commas
pg254 <- gather(pg254, quarter, grams, q1:total) # convert to long format

arcos_2016_summary_ks <- bind_rows(arcos_2016_summary_ks, pg254) # combine data thus far

#### pg. 254b hydrocodone ####
pg254 <- extract_tables("report_yr_2016.pdf", pages=254) # read pdf into matrix
pg254 <- pg254[[2]][c(1:12),-c(1,3,6)] # getting just kansas
pg254 <- as.data.frame(pg254) # convert to data frame
colnames(pg254) <- c("zip", "q1", "q2", "q3", "q4", "total") # change column names
pg254$drug <- c("hydrocodone") # enter drug type name
pg254[,c(2:6)] <- lapply(pg254[,c(2:6)], function(x) str_replace_all(x, ",", "")) # remove commas
pg254 <- gather(pg254, quarter, grams, q1:total) # convert to long format

arcos_2016_summary_ks <- bind_rows(arcos_2016_summary_ks, pg254) # combine data thus far

#### pg. 270 levorphanol ####
pg270 <- extract_tables("report_yr_2016.pdf", pages=270) # read pdf into matrix
pg270 <- pg270[[2]][c(19:23),c(2,4,6,8,9,12)] # getting just kansas
pg270 <- as.data.frame(pg270) # convert to data frame
colnames(pg270) <- c("zip", "q1", "q2", "q3", "q4", "total") # change column names
pg270$drug <- c("levorphanol") # enter drug type name
pg270 <- gather(pg270, quarter, grams, q1:total) # convert to long format

arcos_2016_summary_ks <- bind_rows(arcos_2016_summary_ks, pg270) # combine data thus far

#### pg. 282 meperidine ####
pg282 <- extract_tables("report_yr_2016.pdf", pages=282) # read pdf into matrix
pg282 <- pg282[[1]][c(22:38),-c(1,3,6)] # getting just kansas
pg282 <- as.data.frame(pg282) # convert to data frame
colnames(pg282) <- c("zip", "q1", "q2", "q3", "q4", "total") # change column names
pg282$drug <- c("meperidine") # enter drug type name
pg282[,c(2:6)] <- lapply(pg282[,c(2:6)], function(x) str_replace_all(x, ",", "")) # remove commas
pg282 <- gather(pg282, quarter, grams, q1:total) # convert to long format

arcos_2016_summary_ks <- bind_rows(arcos_2016_summary_ks, pg282) # combine data thus far

#### pg. 301 methadone ####
pg301 <- extract_tables("report_yr_2016.pdf", pages=301) # read pdf into matrix
pg301 <- pg301[[2]][c(22:34),-c(1,3,6)] # getting just kansas
pg301 <- as.data.frame(pg301) # convert to data frame
colnames(pg301) <- c("zip", "q1", "q2", "q3", "q4", "total") # change column names
pg301$drug <- c("methadone") # enter drug type name
pg301[,c(2:6)] <- lapply(pg301[,c(2:6)], function(x) str_replace_all(x, ",", "")) # remove commas
pg301 <- gather(pg301, quarter, grams, q1:total) # convert to long format

arcos_2016_summary_ks <- bind_rows(arcos_2016_summary_ks, pg301) # combine data thus far

#### pg. 302 methadone ####
pg302 <- extract_tables("report_yr_2016.pdf", pages=302) # read pdf into matrix
pg302 <- pg302[[1]][c(1:6),-c(1,3,6)] # getting just kansas
pg302 <- as.data.frame(pg302) # convert to data frame
colnames(pg302) <- c("zip", "q1", "q2", "q3", "q4", "total") # change column names
pg302$drug <- c("methadone") # enter drug type name
pg302[,c(2:6)] <- lapply(pg302[,c(2:6)], function(x) str_replace_all(x, ",", "")) # remove commas
pg302 <- gather(pg302, quarter, grams, q1:total) # convert to long format

arcos_2016_summary_ks <- bind_rows(arcos_2016_summary_ks, pg302) # combine data thus far

#### pg. 321 morphine ####
pg321 <- extract_tables("report_yr_2016.pdf", pages=321) # read pdf into matrix
pg321 <- pg321[[1]][c(26:44),-c(1,3,6)] # getting just kansas
pg321 <- as.data.frame(pg321) # convert to data frame
colnames(pg321) <- c("zip", "q1", "q2", "q3", "q4", "total") # change column names
pg321$drug <- c("morphine") # enter drug type name
pg321[,c(2:6)] <- lapply(pg321[,c(2:6)], function(x) str_replace_all(x, ",", "")) # remove commas
pg321 <- gather(pg321, quarter, grams, q1:total) # convert to long format

arcos_2016_summary_ks <- bind_rows(arcos_2016_summary_ks, pg321) # combine data thus far

#### pg. 336 opium tincture ####
pg336 <- extract_tables("report_yr_2016.pdf", pages=336) # read pdf into matrix
pg336 <- pg336[[1]][c(8,9),c(2,4,6,8,9,11)] # getting just kansas
pg336 <- as.data.frame(pg336) # convert to data frame
colnames(pg336) <- c("zip", "q1", "q2", "q3", "q4", "total") # change column names
pg336$drug <- c("opiumtincture") # enter drug type name
pg336 <- gather(pg336, quarter, grams, q1:total) # convert to long format

arcos_2016_summary_ks <- bind_rows(arcos_2016_summary_ks, pg336) # combine data thus far

#### pg. 344 opium powdered ####
pg344 <- extract_tables("report_yr_2016.pdf", pages=344) # read pdf into matrix
pg344 <- pg344[[1]][c(33:39),-c(1,3,5,7)] # getting just kansas
pg344 <- as.data.frame(pg344) # convert to data frame
colnames(pg344) <- c("zip", "q1", "q2", "q3", "q4", "total") # change column names
pg344$drug <- c("opiumpowdered") # enter drug type name
pg344[,c(2:6)] <- lapply(pg344[,c(2:6)], function(x) str_replace_all(x, ",", "")) # remove commas
pg344 <- gather(pg344, quarter, grams, q1:total) # convert to long format

arcos_2016_summary_ks <- bind_rows(arcos_2016_summary_ks, pg344) # combine data thus far

#### pg. 344b opium powdered ####
pg344 <- extract_tables("report_yr_2016.pdf", pages=344) # read pdf into matrix
pg344 <- pg344[[2]][-13,-1] # getting just kansas
pg344 <- as.data.frame(pg344) # convert to data frame
colnames(pg344) <- c("zip", "q1", "q2", "q3", "q4", "total") # change column names
pg344$drug <- c("opiumpowdered") # enter drug type name
pg344 <- gather(pg344, quarter, grams, q1:total) # convert to long format

arcos_2016_summary_ks <- bind_rows(arcos_2016_summary_ks, pg344) # combine data thus far

#### pg. 363 oxymorphone ####
pg363 <- extract_tables("report_yr_2016.pdf", pages=363) # read pdf into matrix
pg363 <- pg363[[2]][c(5:23),-c(1,3,6)] # getting just kansas
pg363 <- as.data.frame(pg363) # convert to data frame
colnames(pg363) <- c("zip", "q1", "q2", "q3", "q4", "total") # change column names
pg363$drug <- c("oxymorphone") # enter drug type name
pg363[,c(2:6)] <- lapply(pg363[,c(2:6)], function(x) str_replace_all(x, ",", "")) # remove commas
pg363 <- gather(pg363, quarter, grams, q1:total) # convert to long format

arcos_2016_summary_ks <- bind_rows(arcos_2016_summary_ks, pg363) # combine data thus far

#### pg. 378 noroxymorphone ####
pg378 <- extract_tables("report_yr_2016.pdf", pages=378) # read pdf into matrix
pg378 <- pg378[[1]][c(10,11),c(2,4,6,8,9,11)] # getting just kansas
pg378 <- as.data.frame(pg378) # convert to data frame
colnames(pg378) <- c("zip", "q1", "q2", "q3", "q4", "total") # change column names
pg378 <- pg378[-2,]
pg378$drug <- c("noroxymorphone") # enter drug type name
pg378 <- gather(pg378, quarter, grams, q1:total) # convert to long format

arcos_2016_summary_ks <- bind_rows(arcos_2016_summary_ks, pg378) # combine data thus far

#### pg. 384 alfentanil ####
pg384 <- extract_tables("report_yr_2016.pdf", pages=384) # read pdf into matrix
pg384 <- pg384[[1]][c(32,33),c(2,4,6,8,10,12)] # getting just kansas
pg384 <- as.data.frame(pg384) # convert to data frame
pg384 <- pg384[-1,]
colnames(pg384) <- c("zip", "q1", "q2", "q3", "q4", "total") # change column names
pg384$drug <- c("alfentanil") # enter drug type name
pg384 <- gather(pg384, quarter, grams, q1:total) # convert to long format

arcos_2016_summary_ks <- bind_rows(arcos_2016_summary_ks, pg384) # combine data thus far

#### pg. 384b alfentanil ####
pg384 <- extract_tables("report_yr_2016.pdf", pages=384) # read pdf into matrix
pg384 <- pg384[[2]][c(1:6),c(2,4,6,8,10,12)] # getting just kansas
pg384 <- as.data.frame(pg384) # convert to data frame
pg384 <- pg384[-1,]
colnames(pg384) <- c("zip", "q1", "q2", "q3", "q4", "total") # change column names
pg384$drug <- c("alfentanil") # enter drug type name
pg384 <- gather(pg384, quarter, grams, q1:total) # convert to long format

arcos_2016_summary_ks <- bind_rows(arcos_2016_summary_ks, pg384) # combine data thus far

#### pg. 396 remifentanil ####
pg396 <- extract_tables("report_yr_2016.pdf", pages=396) # read pdf into matrix
pg396 <- pg396[[2]][c(16:26),c(2,4,6,8,10,12)] # getting just kansas
pg396 <- as.data.frame(pg396) # convert to data frame
colnames(pg396) <- c("zip", "q1", "q2", "q3", "q4", "total") # change column names
pg396$drug <- c("remifentanil") # enter drug type name
pg396 <- gather(pg396, quarter, grams, q1:total) # convert to long format

arcos_2016_summary_ks <- bind_rows(arcos_2016_summary_ks, pg396) # combine data thus far

#### pg. 411 sufentanil base ####
pg411 <- extract_tables("report_yr_2016.pdf", pages=411) # read pdf into matrix
pg411 <- pg411[[1]][c(47:59),c(2,4,6,8,10,12)] # getting just kansas
pg411 <- as.data.frame(pg411) # convert to data frame
colnames(pg411) <- c("zip", "q1", "q2", "q3", "q4", "total") # change column names
pg411$drug <- c("sufentanilbase") # enter drug type name
pg411 <- gather(pg411, quarter, grams, q1:total) # convert to long format

arcos_2016_summary_ks <- bind_rows(arcos_2016_summary_ks, pg411) # combine data thus far

#### pg. 412 sufentanil base ####
pg412 <- extract_tables("report_yr_2016.pdf", pages=412) # read pdf into matrix
pg412 <- pg412[[1]][c(1:4),c(2,3,6,8,10,12)] # getting just kansas
pg412 <- as.data.frame(pg412) # convert to data frame
colnames(pg412) <- c("zip", "q1", "q2", "q3", "q4", "total") # change column names
pg412$drug <- c("sufentanilbase") # enter drug type name
pg412 <- gather(pg412, quarter, grams, q1:total) # convert to long format

arcos_2016_summary_ks <- bind_rows(arcos_2016_summary_ks, pg412) # combine data thus far

#### pg. 427 tapentadol ####
pg427 <- extract_tables("report_yr_2016.pdf", pages=427) # read pdf into matrix
pg427 <- pg427[[2]][c(38:51),-c(1,3,6)] # getting just kansas
pg427 <- as.data.frame(pg427) # convert to data frame
colnames(pg427) <- c("zip", "q1", "q2", "q3", "q4", "total") # change column names
pg427$drug <- c("tapentadol") # enter drug type name
pg427[,c(2:6)] <- lapply(pg427[,c(2:6)], function(x) str_replace_all(x, ",", "")) # remove commas
pg427 <- gather(pg427, quarter, grams, q1:total) # convert to long format

arcos_2016_summary_ks <- bind_rows(arcos_2016_summary_ks, pg427) # combine data thus far

#### pg. 428 tapentadol ####
pg428 <- extract_tables("report_yr_2016.pdf", pages=428) # read pdf into matrix
pg428 <- pg428[[1]][c(1:5),-c(1,3,6)] # getting just kansas
pg428 <- as.data.frame(pg428) # convert to data frame
colnames(pg428) <- c("zip", "q1", "q2", "q3", "q4", "total") # change column names
pg428$drug <- c("tapentadol") # enter drug type name
pg428 <- gather(pg428, quarter, grams, q1:total) # convert to long format

arcos_2016_summary_ks <- bind_rows(arcos_2016_summary_ks, pg428) # combine data thus far

#### pg. 447 fentanylbase ####
pg447 <- extract_tables("report_yr_2016.pdf", pages=447) # read pdf into matrix
pg447 <- pg447[[1]][c(25:43),-c(1,3,6)] # getting just kansas
pg447 <- as.data.frame(pg447) # convert to data frame
colnames(pg447) <- c("zip", "q1", "q2", "q3", "q4", "total") # change column names
pg447$drug <- c("fentanylbase") # enter drug type name
pg447 <- gather(pg447, quarter, grams, q1:total) # convert to long format

arcos_2016_summary_ks <- bind_rows(arcos_2016_summary_ks, pg447) # combine data thus far

#### all long format ####
write.csv(arcos_2016_summary_ks, "arcos_2016_summary_ks_v1.csv", row.names=FALSE)

d2 <- arcos_2016_summary_ks %>% 
  spread(quarter, grams)

write.csv(d2, "arcos_2016_summary_ks_v2.csv", row.names=FALSE)

d3 <- arcos_2016_summary_ks %>% 
  spread(drug, grams)

d3[is.na(d3)] <- 0

write.csv(d3, "arcos_2016_summary_ks_v3.csv", row.names=FALSE)

d4 <- arcos_2016_summary_ks %>% 
  unite(drug_quarter, c(drug, quarter)) %>% 
  spread(drug_quarter, grams)

d4[is.na(d4)] <- 0

write.csv(d4, "arcos_2016_summary_ks_v4.csv", row.names=FALSE)
