# ==== Initialize  ====== 
library(quanteda)
library(googlesheets)
library(tidyverse)
library(xlsx)

# Strings from manual search papers we found
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
ews <- ews %>% separate(col = `Author & Year`, into = c("Author", "Year"), sep = "\\(") %>%
  mutate(Year = gsub("\\)", "", Year),
         AUshort = sub("([A-Za-z]+).*", "\\1", Author)) %>% 
  filter(!is.na(Title))

# generate individual strings
  # WoS - pattern (TI=("title") AND AU=("Abdalla") AND PY=(2015))
  # Scopus - pattern (TITLE("") AND AUTH("Abdalla") AND PUBYEAR IS 2015 ))

ews <- ews %>% 
  mutate(WoS = paste('(TI = ("', Title, '") AND AU=("', AUshort, '") AND PY=(', Year, '))', sep = ""),
         Scopus = paste('(TITLE("', Title, '") AND AUTH("', AUshort,'") AND PUBYEAR IS ', Year, ')', sep = ""))

searchStrings <- data.frame(database = c("WoS", "Scopus"),
                            query = as.character(NA)) %>% 
  mutate(query = as.character(query))


#combine strings
searchStrings$query[searchStrings$database== "WoS"] <- paste0(ews$WoS, collapse = ' OR ')
searchStrings$query[searchStrings$database== "Scopus"] <- paste0(ews$Scopus, collapse = ' OR ')

write.csv(searchStrings, file = "../Queries/strings_ews.csv")


##### Generate SCOPUS and WoS queries for those found in reviews but not query to check
#####
revsN <- read.csv2("Data/Not_captured.csv")
Rws <- revsN %>%
  mutate(AUshort = sub("([A-Za-z]+).*", "\\1", AU)) 

Rws <- Rws %>% 
  mutate(WoS = paste('(TI = ("', TI, '") AND AU=("', AUshort, '") AND PY=(', PY, '))', sep = ""),
         Scopus = paste('(TITLE("', TI, '") AND AUTH("', AUshort,'") AND PUBYEAR IS ', PY, ')', sep = ""))

searchStrings <- data.frame(database = c("WoS", "Scopus"),
                            query = as.character(NA)) %>% 
  mutate(query = as.character(query))

#combine strings
searchStrings$query[searchStrings$database== "WoS"] <- paste0(Rws$WoS, collapse = ' OR ')
searchStrings$query[searchStrings$database== "Scopus"] <- paste0(Rws$Scopus, collapse = ' OR ')

write.csv2(searchStrings, file = "Queries/strings_Rws.csv")

# #all in reviews
# revs <- df_matched %>% 
#   mutate(AUshort = sub("([A-Za-z]+).*", "\\1", AU.x), 
#   PY = PY,
#   TI = TI.x,
#   WoS = paste('(TI = ("', TI, '") AND AU=("', AUshort, '") AND PY=(', PY, '))', sep = ""),
#   Scopus = paste('(TITLE("', TI, '") AND AUTH("', AUshort,'") AND PUBYEAR IS ', PY, ')', sep = "")
#   )
# 
# searchStrings <- data.frame(database = c("WoS", "Scopus"),
#                             query = as.character(NA)) %>% 
#   mutate(query = as.character(query))
# 
# #combine strings
# searchStrings$query[searchStrings$database== "WoS"] <- paste0(revs$WoS, collapse = ' OR ')
# searchStrings$query[searchStrings$database== "Scopus"] <- paste0(revs$Scopus, collapse = ' OR ')
# 
# WriteXLS::WriteXLS(searchStrings,  "Queries/strings_revs.xls")
# write.csv2(searchStrings, "Queries/strings_revs.csv", quote = FALSE)

#####
# From scoping excercise, Q2801, get relevant and generate targeted search strings

source("R/Docs processing.R")

table(docs$possible)

scoped <- docs %>% 
  filter(possible == T) %>% 
  mutate(AUshort = sub("([A-Za-z]+).*", "\\1", AU),
         WoS = paste('(TI = ("', TI, '") AND AU=("', AUshort, '") AND PY=(', PY, '))', sep = ""),
         Scopus = paste('(TITLE("', TI, '") AND AUTH("', AUshort,'") AND PUBYEAR IS ', PY, ')', sep = ""))

searchStrings <- data.frame(database = c("WoS", "Scopus"),
                            query = as.character(NA)) %>% 
  mutate(query = as.character(query))

#combine strings
searchStrings$query[searchStrings$database== "WoS"] <- paste0(scoped$WoS, collapse = ' OR ')
searchStrings$query[searchStrings$database== "Scopus"] <- paste0(scoped$Scopus, collapse = ' OR ')

write.csv2(searchStrings, file = "Queries/strings_scoped2801.csv", quote = FALSE)
