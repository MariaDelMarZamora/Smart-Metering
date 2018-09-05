library(tidyr)
library(dplyr)
library(ggplot2)
library(readxl)

docs <- read_excel("Data/SM Q2801 R1353.xlsx") 
colnames(docs) <- c("TI", "PY", "AU", "doc", "di", "tech", "netRel", "techRel", "khat", "java", "kocn", "delm")
docs <- docs %>% 
  mutate(
    DOI = gsub("http://dx.doi.org/", "", docs$di, fixed = T), 
    maybe = ifelse(khat == "Maybe" & netRel == 0, "maybe",
                   ifelse(java == "Maybe" & netRel == 0, "maybe",
                          ifelse(kocn == "Maybe" & netRel == 0, "maybe",
                                 ifelse(delm == "Maybe" & netRel == 0, "maybe",NA)))),
    relevant = ifelse(netRel > 0, TRUE, FALSE),
    query = 2801,
    this_study = ifelse(relevant == T, "cap_rel",
                        ifelse(relevant == F & maybe == "maybe", "cap_mayb",
                               ifelse((relevant == F & is.na(maybe)), "cap_NOT_rel",
                                      #                       "test")
                                      "wrong"))),
    possible = ifelse(relevant == TRUE | maybe == "maybe", TRUE, FALSE)
  ) %>% 
  select(TI, PY, AU, relevant, DOI, maybe,query, this_study, possible)

docs[is.na(docs$maybe) & docs$relevant==F,]$this_study <- "cap_NOT_rel"