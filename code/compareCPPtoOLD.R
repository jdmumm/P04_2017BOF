library(dplyr)
library (tidyr)
options(scipen = 999)

#Load
cpp <- read.csv("data/CPP.csv")
old <- read.csv("data/temp/2016QueryOutput/QUERY_CPUE_ANNUAL_SUMMARY_all_170127.csv")

str(old)

old %>% group_by(Year) %>% summarize(
  year = first(Year),
  all_kg = sum(Total_Spot_Wt_KG, na.rm = TRUE), 
  all_cnt = sum(Total_Spot_Count, na.rm = TRUE),
  lrg_cnt = sum(Est_Ct_LG, na.rm = TRUE),
  lrg_kg = sum(Est_Wt_Large, na.rm = TRUE)) -> o

str(cpp)

cpp %>% group_by(YEAR) %>% filter(SITE_ID != 11) %>% summarize(
  year = first(YEAR),
  all_kg = sum(all_Kg_cc, na.rm = TRUE), 
  all_cnt = sum(all_Cnt_cc, na.rm = TRUE), 
  lrg_cnt = sum(lrg_Cnt, na.rm = TRUE), 
  lrg_kg = sum(lrg_Kg, na.rm = TRUE)) -> n 

left_join(o,n, by = "year") -> both
both %>% select( year = year, 
                 all_cnt.x, 
                 all_cnt.y, 
                 all_kg.x, 
                 all_kg.y, 
                 lrg_kg.x, 
                 lrg_kg.y,
                 lrg_cnt.x, 
                 lrg_cnt.y) -> out 
out %>% transmute( 
  year = year,
  all_cnt_per = 100* (all_cnt.y-all_cnt.x)/all_cnt.x,
  all_kg_per = 100* (all_kg.y-all_kg.x)/all_kg.x,
  lrg_cnt_per = 100* (lrg_cnt.y-lrg_cnt.x)/lrg_cnt.x,
  lrg_kg_per = 100* (lrg_kg.y-lrg_kg.x)/lrg_kg.x)  -> perDif
