
ch <- odbcConnect("BioSense_Platform", "BIOSENSE\\lskywalker01", "xwingred5")
pct <- pct55pct(channel = ch, month = 07, year = 2017)

odbcCloseAll()
