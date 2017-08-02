############
## CPUE   ##
############
# Calculates CATCH and CPUE for both Large and Alls from PWS spot shrimp pot survey. Used for 2017 BOF report.
# Josh Mumm 

# PREP ----
library(tidyverse)
cpp <- read.csv('data/CPP.csv') 


#rename vars 
cpp %>% transmute(year = YEAR, 
                 Event = EVENT_ID, 
                 Site = as.factor(SITE_ID), 
                 Station = STATION, 
                 Pot = as.factor(POT_ID),
                 Sample = SAMPLE_POT, 
                 all_cnt = all_Cnt_cc, 
                 all_kg = all_Kg_cc, 
                 lrg_cnt = lrg_Cnt,
                 lrg_kg = lrg_Kg) -> cpp

## ALLS ----
  #survey-wide 
  cpp %>% filter (Site != "11") %>% group_by(year) %>% 
    summarise ( 
      N = n(),
      tau_all_cnt = sum(all_cnt), 
      tau_all_kg = sum(all_kg),
      mu_all_cnt = tau_all_cnt/N,
      mu_all_kg = tau_all_kg/N,  
      var_all_cnt = sum((all_cnt - mu_all_cnt)^2)/N,
      var_all_kg = sum((all_kg - mu_all_kg)^2)/N)  -> all_byYear
  #bySite
  cpp %>% group_by(year, Site) %>% 
    summarise ( 
      N = n(),
      tau_all_cnt = sum(all_cnt), 
      tau_all_kg = sum(all_kg),
      mu_all_cnt = tau_all_cnt/N,
      mu_all_kg = tau_all_kg/N,  
      var_all_cnt = sum((all_cnt - mu_all_cnt)^2)/N,
      var_all_kg = sum((all_kg - mu_all_kg)^2)/N)  -> all_bySite

## LARGES ----
  # bySite 
    all_bySite %>% select(year, Site, N, mu_all_cnt, mu_all_kg) %>% right_join(cpp) %>% # join site-level stats to CPP
    filter (Sample == "Sample") %>% group_by(year, Site) %>% 
      summarise ( 
        n = n(),
        N = first(N),
        rh_cnt = sum(lrg_cnt, na.rm = T)/sum(all_cnt, na.rm = T), 
        rh_kg  = sum(lrg_kg, na.rm = T)/sum(all_kg, na.rm = T),
        mu_lrg_cnt = rh_cnt * first(mu_all_cnt), 
        mu_lrg_kg  = rh_kg * first(mu_all_kg),
        tau_lrg_cnt = mu_lrg_cnt * first(N),
        tau_lrg_kg =  mu_lrg_kg * first(N), 
        var_rh_cnt = sum((lrg_cnt - rh_cnt*all_cnt)^2, na.rm = T)/(n-1),
        var_rh_kg  = sum((lrg_kg - rh_cnt*all_kg)^2, na.rm = T)/(n-1), 
        var_mu_lrg_cnt = (N - n)/(N) * (var_rh_cnt/n), 
        var_mu_lrg_kg = (N - n)/(N) * (var_rh_kg/n), 
        var_tau_lrg_cnt = var_mu_lrg_cnt * first(N)^2,
        var_tau_lrg_kg = var_mu_lrg_kg * first(N)^2) -> large_bySite           
  
  #byYear 
    large_bySite %>% filter (Site != "11") %>% group_by (year) %>% 
      summarise (
        N = sum(N),
        tau_lrg_cnt = sum(tau_lrg_cnt),
        tau_lrg_kg = sum(tau_lrg_kg),
        mu_lrg_cnt = tau_lrg_cnt / N,
        mu_lrg_kg = tau_lrg_kg / N, 
        var_tau_lrg_cnt = sum(var_tau_lrg_cnt, na.rm = T), 
        var_tau_lrg_kg = sum(var_tau_lrg_kg, na.rm = T), 
        var_mu_lrg_cnt = var_tau_lrg_cnt/(N^2),
        var_mu_lrg_kg  = var_tau_lrg_kg/(N^2)) -> large_byYear
###############################################################################################
  
  
  
  
  
  
  
  
  
  
  
#errror checking compare to old output .  Not complete 
old <- read.csv('data/temp/2016QueryOutput/QUERY_CPUE_ANNUAL_SUMMARY_all_170127.csv')
str(old)
old %>% select (year = Year,
                N = Pot_Count,
                tau_all = Total_Spot_Count, 
                mu_all = CPUE_All_Count) -> o

left_join(byYear,o, by = "year" ) -> comp 


