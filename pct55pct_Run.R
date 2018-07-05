
ch <- odbcConnect("BioSense_Platform", "BIOSENSE\\scrossenks", "fuu9QQIOH")
pct <- pct55pct(channel = ch, month = 07, year = 2017)

odbcCloseAll()
