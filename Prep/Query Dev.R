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

# combine
df <- full_join(ews, pws)


#   west <- gs_read(ss=be, ws = "Westminster voting intentions", skip=1)
# 
# # convert to data.frame
# wdf <- as.data.frame(west)