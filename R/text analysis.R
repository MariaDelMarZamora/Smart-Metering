library(wordcloud)
library(readxl)
# library(tm)
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
  group_by(wosarticle__ti) %>% 
  mutate(
    text = paste(wosarticle__ti, wosarticle__ab, wosarticle__kwp, wosarticle__de),
    keywords = paste(wosarticle__kwp, wosarticle__de),
    TI = wosarticle__ti,
    AB = wosarticle__ab,
    KWP = wosarticle__kwp,
    DE = wosarticle__de
  ) %>% 
  ungroup() %>% 
  select(TI, AB, KWP, DE, text, keywords)



#tidytext
tidy_papers <- trimmed %>% 
  unnest_tokens(word, text)

cleaned_papers <- tidy_papers %>%
  anti_join(get_stopwords()) %>% 
  filter(word != 'na') %>% 
  count(word, sort = TRUE) 

write.csv(cleaned_papers[1:100,], file = "Queries/freq_words NC set.csv")

cleaned_papers%>% 
  with(wordcloud(word, n, max.words = 150))
ggsave(file = "plots/all_tt.jpg")



#quanteda
AbstractCorpus <- corpus(df$wosarticle__ab, docnames = df$wosarticle__ti, df) #

Abstractdfm <- dfm(AbstractCorpus, stem = T, remove = stopwords("english"), remove_punct = TRUE)
textplot_wordcloud(Abstractdfm, random_order = T,
                   color = rev(RColorBrewer::brewer.pal(10, "RdBu")),
                   max_words = 200)
ggsave(file = 'plots/ab_q wordc.jpg')

TIcorpus <- corpus(df$wosarticle__ti, docnames = df$wosarticle__ti, df)
TIdfm <- dfm(TIcorpus, stem = T, remove = stopwords('english'), remove_punct = T)
textplot_wordcloud(TIdfm, random_order = T,
                   color = rev(RColorBrewer::brewer.pal(10, "RdBu")))
ggsave(file = 'plots/ti_q wordc.jpg')


df <- df %>% 
  mutate(
    keywords = paste(wosarticle__kwp, wosarticle__de)
  )

KWCorpus <- corpus(df$keywords, docnames = df$wosarticle__ti, df)
KWdfm <- dfm(KWCorpus, stem = T, remove = c('na',stopwords('english')), remove_punct = T)
textplot_wordcloud(KWdfm, random_order = T,
                   color = rev(RColorBrewer::brewer.pal(10, "RdBu")))
ggsave(file = 'plots/kw_q wordc.jpg')


