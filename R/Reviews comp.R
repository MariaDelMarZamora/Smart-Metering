library(tidyr)
library(dplyr)
library(ggplot2)
library(gdata)


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
    relevant = ifelse(netRel > 0, TRUE, FALSE)
  ) %>% 
  select(TI, PY, relevant, DOI, maybe)

#match review DOIs to  query results 
    #resulting df has only the 147 docs sourced from reviews
df_matched <- left_join(df, docs, by = "DOI") %>% 
  select(AU, DOI, TI.x, TI.y, PY.x, Karlin:Abrahamse_, -ACEEE, reviews, relevant, maybe) #adjust col name as reviews are added

table(df_matched$relevant)


#getting those captured by query but not reviews
filter(maybe == "maybe" | relevant == TRUE)#filter any yes or maybes

