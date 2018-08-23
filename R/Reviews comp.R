library(tidyr)
library(dplyr)
library(ggplot2)
library(readxl)
# Tomorrow: when to create the this_study variable perhaps from beginning with doc

#read in Review contents table
df <- read_excel("../Reviews Annotated.xlsx", sheet =3) 

cols <- variable.names(df)[5:12]
# df[cols] <- lapply(df[cols], factor)

#make into factor var
df <- df %>%
  mutate_each_(funs(factor(., levels = c("NA","x"), labels = c("No", "Yes"))), cols) %>% 
  mutate(
    reviews = "reviews"
  )


# read in query results 
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
                                      "wrong")))
  ) %>% 
  select(TI, PY, AU, relevant, DOI, maybe,query, this_study)

docs[is.na(docs$maybe) & docs$relevant==F,]$this_study <- "cap_NOT_rel"

table(docs$this_study, useNA = "always") #change below to include this study


#match review DOIs to  query results 
    #resulting df has only the 147 docs sourced from reviews
df_matched <- left_join(df, docs, by = "DOI") %>% 
  select(AU.x, DOI, TI.x, TI.y, PY.x, Karlin:Abrahamse_, -ACEEE, this_study, reviews, relevant, maybe, query) #adjust col name as reviews are added

table(df_matched$relevant)


#getting those captured by query but not reviews
poss_Rel <- docs %>% 
  filter(maybe == "maybe" | relevant == TRUE) %>%  #filter any yes or maybes
  group_by(TI) %>% 
  arrange(TI, maybe) %>% 
  mutate(
    indx = row_number()
  ) %>% 
  filter((relevant == T & indx == 1) | 
           (relevant == T & indx == 2) |
           (maybe == "maybe" & max(indx) == 1 )) %>%
  ungroup()



df_matched <- full_join(df_matched, poss_Rel, by = c("DOI" = "DOI", "relevant" = "relevant", "maybe" = "maybe", "query" = "query", "PY.x" = "PY", "this_study" = "this_study")) %>% 
  select(AU.x, AU, DOI, TI.x, TI.y, TI, PY.x, Karlin:Abrahamse_, this_study, reviews, relevant, maybe, query)

# Reconciliating TI and AU fields
df_matched <- df_matched %>%
  mutate(
    AU = gsub(",.*", " et al", AU)
  )

df_matched$AU.x[is.na(df_matched$AU.x)] <- df_matched$AU[is.na(df_matched$AU.x)]
df_matched$TI.x[is.na(df_matched$TI.x)] <- df_matched$TI[is.na(df_matched$TI.x)]
df_matched$TI.x[is.na(df_matched$TI.x)] <- df_matched$TI.y[is.na(df_matched$TI.x)]

df_matched <- df_matched %>% 
  select(AU.x, TI.x, PY.x, DOI, Karlin:query) %>% 
  arrange(PY.x)


# reshape data

#make paper row for plotting
df_matchedt <- df_matched %>% 
  mutate(
    paper_row = row_number()
  )

table(df_matchedt$this_study, useNA = "always") #check to see numbers match with Max

