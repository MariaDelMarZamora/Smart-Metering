library(wordcloud2)
library(readxl)
library(tm)
library(SnowballC)
library(RCurl)
library(quanteda)
library(tidyr)
library(dplyr)
library(tidytext)



df <- read_excel("Queries/from reviews NC 2801.xlsx")

df <- df %>% 
  mutate(
    duplic = duplicated(wosarticle__ti)
  ) %>% 
  filter(duplic == FALSE)

#make df with only relevant columns, or paste all text from AB, TI and keywords into one "text" column (kwp, de, ti, ab)
#drop unnecessary columns (AU, PY, SO...) all except doc id
trimmed <- df %>% 
  mutate(
    
  ) %>% 
select()

#tidytext




#quanteda
# AbstractCorpus <- corpus(df$wosarticle__ab, docnames = df$wosarticle__ti, df) #
# 
# Abstractdfm <- dfm(AbstractCorpus, stem = T, remove = stopwords(kind = "english"))

