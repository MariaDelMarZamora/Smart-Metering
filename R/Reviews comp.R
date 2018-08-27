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
# df <- df %>%
#   mutate_each_(funs(factor(., levels = c("NA","x"), labels = c("No", "Yes"))), cols) %>% 
#   mutate(
#     reviews = "reviews"
#   )

#Maybe dont need to factor at all

df <- df %>%
  mutate_each_(funs(factor(.)), cols) %>% 
  mutate(
    reviews = "reviews",
    rev_rowN = row_number()
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


#####         Match review DOIs to  query results 

df.rev <- df %>% select(AU, TI, PY) %>% 
  mutate(
    TI.fuzz = ""
  )

df.query <- docs %>% select(TI, PY)

fuzzy_compare <- function(p,x,max.distance, value = T, ignore.case = T) {
  matches = agrep(p,x,max.distance, ignore.case = ignore.case, value = value)
  return(matches[1])
}

 
# l1 = data.frame(title=c("the quick brown fox","lorem ipsum dolor"))
# l2 = data.frame(title=c("the quicke brown fox","lorem ipsum dollar"))

#l1$fuzzy.title = sapply(l1$title,fuzzy_compare,l2$title,0.1)

df.rev$TI.fuzz <- sapply(df.rev$TI, 
                         fuzzy_compare,
                         df.query$TI,
                         max.distance = 0.1,
                         value = T,
                         ignore.case = T)

df.key <- df.rev %>%  select(TI, TI.fuzz)

df2 <- left_join(df, df.key, by = "TI")
    #resulting df has only the 147 docs sourced from reviews
df_matched <- left_join(df2, docs, by = "DOI") %>% 
  select(AU.x, DOI, TI.x, TI.y, PY.x, Karlin:Abrahamse_, -ACEEE, this_study, reviews, relevant, maybe, query,rev_rowN, TI.fuzz) %>%  # adjust col name as reviews are added
left_join(., docs, by = c("TI.fuzz" = "TI", "PY.x" = "PY")) %>% 
  select(AU.x, AU, DOI.x, DOI.y, TI.x, TI.y, TI.fuzz, PY.x, this_study.x, this_study.y, relevant.x, relevant.y, maybe.x, maybe.y, query.x, query.y, rev_rowN,  Karlin:Abrahamse_, reviews)

# consolidate fields
df_matched$TI.y[is.na(df_matched$TI.y)] <- df_matched$TI.fuzz[is.na(df_matched$TI.y)] #titles for checking - can drop later and keep TI.x
df_matched$DOI.x[grep("^NA", df_matched$DOI.x)] <- df_matched$DOI.y[grep("^NA", df_matched$DOI.x)]
df_matched$this_study.x[is.na(df_matched$this_study.x)] <- df_matched$this_study.y[is.na(df_matched$this_study.x)]
df_matched$relevant.x[is.na(df_matched$relevant.x)] <- df_matched$relevant.y[is.na(df_matched$relevant.x)]
df_matched$maybe.x[is.na(df_matched$maybe.x)] <- df_matched$maybe.y[is.na(df_matched$maybe.x)]
df_matched$query.x[is.na(df_matched$query.x)] <- df_matched$query.y[is.na(df_matched$query.x)]

# keep consolidated fields
df_matched <- df_matched %>% 
  select(AU.x, DOI.x, TI.x, PY.x, this_study.x, relevant.x, maybe.x, query.x,  Karlin:Abrahamse_, rev_rowN, reviews) 

names(df_matched) <- gsub("\\.x$", "", names(df_matched))



# variable.names(df_matched)

table(df_matched$relevant, useNA = "always")




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



df_matched <- full_join(df_matched, poss_Rel, by = c("DOI" = "DOI", "relevant" = "relevant", "maybe" = "maybe", "query" = "query", "PY" = "PY", "this_study" = "this_study")) %>% 
  select(AU.x, AU.y, DOI, TI.x, TI.y, PY, this_study:rev_rowN, indx, reviews)

# Reconciliating TI and AU fields
df_matched <- df_matched %>%
  mutate(
    AU = gsub(",.*", " et al", AU.y)
  )

df_matched$AU.x[is.na(df_matched$AU.x)] <- df_matched$AU[is.na(df_matched$AU.x)]
df_matched$TI.x[is.na(df_matched$TI.x)] <- df_matched$TI.y[is.na(df_matched$TI.x)]

df_matched <- df_matched %>% 
  select(AU.x, DOI, TI.x, PY:indx, reviews) %>% 
  arrange(PY)



#make paper row for plotting
df_matched <- df_matched %>% 
  mutate(
    paper_row = row_number()
  )

table(df_matched$this_study, useNA = "always") #check to see numbers match with Max
#Decide whether to keep NAs or switch to "not captured"

# reshape data
#first look at reviews

df_matched <- df_matched %>% 
  gather(key = Review, value = Included, Karlin:Abrahamse_, this_study) %>% 
  mutate(Included = factor(Included),
         text = "x")

df_matched[is.na(df_matched$Included),]$text <- ""


###### Plots
ggplot(df_matched, aes(x = paper_row, y = Review))+
  geom_tile(aes(fill = Included))+
  geom_text(aes(label = text))

ggplot(df_matched, aes(x = paper_row, y = Review)) +
  geom_point(aes(shape = Included, color = Included))+
  scale_shape_manual(values = c(0:2,4))+
  xlim(0,435)
  
ggplot(df_matched, aes(x = AU.x, y = Review)) +
  geom_point(aes(shape = Included, color = Included))+
  scale_shape_manual(values = c(0:2,4))+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))

# Just the ones captured by the reviews
df_revs <- df_matched %>% 
  filter(reviews == "reviews")

table(df_revs$Included, useNA = "always")


ggplot(df_revs, aes(x = paper_row, y = Review))+
  geom_tile(aes(fill = Included))+
  geom_text(aes(label = text))

ggplot(df_revs, aes(x = rev_rowN, y = Review))+
  geom_tile(aes(fill = Included))+
  geom_text(aes(label = text))

ggplot(df_revs, aes(x = AU.x, y = Review))+
  geom_tile(aes(fill = Included))+
  geom_text(aes(label = text))

ggplot(df_revs, aes(x = paper_row, y = Review)) +
  geom_point(aes(shape = Included, color = Included))+
  scale_shape_manual(values = c(0:2,4))

ggplot(df_revs, aes(x = rev_rowN, y = Review)) +
  geom_point(aes(shape = Included, color = Included))+
  scale_shape_manual(values = c(0:2,4))+
    xlim(0,147)


ggplot(df_revs, aes(x = AU.x, y = Review)) +
  geom_point(aes(shape = Included, color = Included))+
  scale_shape_manual(values = c(0:2,4))+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
