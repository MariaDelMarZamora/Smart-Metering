# ==== Initialize  ====== 
library(quanteda)
library(googlesheets)
library(tidyverse)


# ==== get GoogleDoc access and import sheet =====
gs_ls()
GS <- gs_title("Smart Meter Meta Review - Literature")
gs_ws_ls(GS)

ews <- gs_read(ss = GS, ws = "Econ Literature")
pws <- gs_read(ss = GS, ws = "Psych Literature")

combine
# df <- full_join(ews, pws)
# 
# df <- df %>% separate(col = `Author & Year`, into = c("Author", "Year"), sep = "\\(") %>%
#   mutate(Year = gsub("\\)", "", Year),
#          AUshort = sub("([A-Za-z]+).*", "\\1", Author))

#
pws <- pws %>% separate(col = `Author & Year`, into = c("Author", "Year"), sep = "\\(") %>%
  mutate(Year = gsub("\\)", "", Year),
         AUshort = sub("([A-Za-z]+).*", "\\1", Author))

# generate individual strings
  # WoS - pattern (TI=("title") AND AU=("Abdalla") AND PY=(2015))
  # Scopus - pattern (TITLE("") AND AUTH("Abdalla") AND PUBYEAR IS 2015 ))

pws <- pws %>% 
  mutate(WoS = paste('(TI = ("', Title, '") AND AU=("', AUshort, '") AND PY=(', Year, '))', sep = ""),
         Scopus = paste('(TITLE("', Title, '") AND AUTH("', AUshort,'") AND PUBYEAR IS ', Year, ')', sep = ""))

searchStrings <- data.frame(database = c("WoS", "Scopus"),
                            query = as.character(NA)) %>% 
  mutate(query = as.character(query))


#combine strings
searchStrings$query[searchStrings$database== "WoS"] <- paste0(pws$WoS, collapse = ' OR ')
searchStrings$query[searchStrings$database== "Scopus"] <- paste0(pws$Scopus, collapse = ' OR ')

write.csv(searchStrings, file = "../Queries/strings.csv")

