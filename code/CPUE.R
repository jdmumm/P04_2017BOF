## CPUE ####
# Calculates CATCH and CPUE for both Large and Alls from PWS spot shrimp pot survey. Used for to calc variances for 2017 BOF report. Point ests from old queries.
# Josh Mumm 

## PREP ----
library(tidyverse)
cpp <- read.csv('data/CPP_lessSamps_d.csv') # _lessSamps is from temporaily correcting 
  #sample pot indicator on potperformance for those pots labeled sample but w/out any awls
  # and _b is from temporaily modifying cpp sqls to remove rounding in step 6 and remove pop blank weight from count in 2. 
  # and _c is from temporalily changing sample pot indicator on potPerformance again,  from null to Sample for those pots with awl. Offered little improvement
  # _d is same as b, after editing 2011 (removed cc recs wtih 0 count and weight - this shouldn't have made a dif).  More importantly i don't think _b fully removed
        #estimating blank weights from CPP_2.  Fixed here for _d.   
        # Plan to use _d.  ALLs - both tau and mu match old query for all years rounded to nearest .1 %.   Large Tau and Mu are < .7% dif for all years 
        
#rename vars 
cpp %>% transmute(year = Year, 
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
      var_all_cnt = sum((all_cnt - mu_all_cnt)^2)/(N-1), # 10/21 added -1 since treating as sample not pop now.  Leaving N as N not n for now. 
      var_all_kg = sum((all_kg - mu_all_kg)^2)/(N-1) ,
      sd_all_cnt = var_all_cnt^.5,
      sd_all_kg = var_all_kg^.5,
      se_all_cnt = sd_all_cnt/mu_all_cnt,
      se_all_kg = sd_all_kg/mu_all_kg,
      cv_all_cnt = 100* sd_all_cnt/mu_all_cnt,
      cv_all_kg = 100* sd_all_kg/mu_all_kg) -> all_byYear
  #bySite
  cpp %>% group_by(year, Site) %>% 
    summarise ( 
      N = n(),
      tau_all_cnt = sum(all_cnt), 
      tau_all_kg = sum(all_kg),
      mu_all_cnt = tau_all_cnt/N,
      mu_all_kg = tau_all_kg/N, 
      var_all_cnt = sum((all_cnt - mu_all_cnt)^2)/(N-1),
      var_all_kg = sum((all_kg - mu_all_kg)^2)/(N-1),
      sd_all_cnt = var_all_cnt^.5,
      sd_all_kg = var_all_kg^.5,
      se_all_cnt = sd_all_cnt/mu_all_cnt,
      se_all_kg = sd_all_kg/mu_all_kg,
      cv_all_cnt = 100* sd_all_cnt/mu_all_cnt,
      cv_all_kg = 100* sd_all_kg/mu_all_kg) -> all_bySite

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
        tau_lrg_cnt = mu_lrg_cnt * N,
        tau_lrg_kg =  mu_lrg_kg * N, 
        var_rh_cnt = sum(((lrg_cnt - rh_cnt*all_cnt)^2), na.rm = T)/(n-1),
        var_rh_kg  = sum(((lrg_kg - rh_kg*all_kg)^2), na.rm = T)/(n-1), 
        var_mu_lrg_cnt =(var_rh_cnt/n), # fpc goes here
        var_mu_lrg_kg = (var_rh_kg/n), # fpc goes here 
        var_tau_lrg_cnt = var_mu_lrg_cnt * N^2,
        var_tau_lrg_kg = var_mu_lrg_kg * N^2,
        cv_lrg_kg = 100* (var_rh_kg^.5)/mu_lrg_kg, 
        cv_lrg_cnt = 100 * (var_rh_cnt^.5)/mu_lrg_cnt) -> large_bySite           
  
  #byYear 
    large_bySite %>% filter (Site != "11") %>% group_by (year) %>% 
      summarise (
        n = sum(n),
        N = sum(N),
        tau_lrg_cnt = sum(tau_lrg_cnt),
        tau_lrg_kg = sum(tau_lrg_kg),
        mu_lrg_cnt = tau_lrg_cnt / N,
        mu_lrg_kg  = tau_lrg_kg / N, 
        var_rh_cnt = sum(var_rh_cnt),
        var_rh_kg = sum(var_rh_kg),
        var_tau_lrg_cnt = sum(var_tau_lrg_cnt), 
        var_tau_lrg_kg  = sum(var_tau_lrg_kg), 
        var_mu_lrg_cnt = var_tau_lrg_cnt/(N^2),
        var_mu_lrg_kg  = var_tau_lrg_kg/(N^2), 
        se_lrg_kg = var_rh_kg / mu_lrg_kg, 
        se_lrg_cnt = var_rh_cnt / mu_lrg_cnt, 
        cv_lrg_kg = 100* (var_rh_kg^.5)/mu_lrg_kg, 
        cv_lrg_cnt = 100* (var_rh_cnt^.5)/mu_lrg_cnt) -> large_byYear  # cv and se form might not be quite right
    
    
###############################################################################################
## errror checking.  compare to old output  Not Complete----
    options(scipen = 999)
    
    # ALLS      
    old <- read.csv('data/temp/2016QueryOutput/QUERY_CPUE_ANNUAL_SUMMARY_all_170127.csv')
    str(old)
    str(all_byYear)
    old %>% select (year = Year,
                    N = Pot_Count,
                    tau_all_cnt = Total_Spot_Count, 
                    tau_all_kg = Total_Spot_Wt_KG, 
                    mu_all_cnt = CPUE_All_Count,
                    mu_all_kg = CPUE_All_KG
                    ) -> o
    
    all_byYear[,1:6] -> n 
    
    o %>% left_join(n, by = "year" ) -> comp 
    comp
    str(comp)
    comp[ , order(names(comp))] -> comp# reorder columns
    
    #calc difs 
    o[,order(names(o))] -> o
    n[,order(names(n))] -> n 
    str(o)
    str(n)
    
    dif_year <- o[,1:5] - n[,1:5] 
    cbind(dif_year,year = o$year) -> dif_year
    
    per_dif_year <- 100* dif_year[,-6]/o[,-6]
    cbind(per_dif_year,year = o$year) -> per_dif_year
    per_dif_year %>% transmute(year = year,
                                 mu_all_cnt = round(mu_all_cnt,1),
                                 mu_all_kg = round(mu_all_kg,1),
                                 tau_all_cnt = round(tau_all_cnt,1),
                                 tau_all_kg = round(tau_all_kg,1)) -> per_dif_year_r                                                       
    
    # Larges ####  
    old %>% select (year = Year,
                    N = Pot_Count,
                    tau_lrg_cnt = Est_Ct_LG, 
                    tau_lrg_kg = Est_Wt_Large, 
                    mu_lrg_cnt = CPUE_Large_Count,
                    mu_lrg_kg = CPUE_Large_KG) -> o_l
    large_byYear[,1:6] -> n_l 
    o_l[,order(names(o_l))] -> o_l
    n_l[,order(names(n_l))] -> n_l 
    
    dif_year_l <- o_l[,1:5] - n_l[,1:5] 
    cbind(dif_year_l,year = n_l$year) -> dif_year_l
    
    per_dif_year_l <- 100* dif_year_l[,-6]/n_l[,-6]
    cbind(per_dif_year_l,year = n_l$year) -> per_dif_year_l
    per_dif_year_l %>% transmute(year = year,
                    mu_lrg_cnt = round(mu_lrg_cnt,1),
                    mu_lrg_kg = round(mu_lrg_kg,1),
                    tau_lrg_cnt = round(tau_lrg_cnt,1),
                    tau_lrg_kg = round(tau_lrg_kg,1)) -> per_dif_year_l_r
   
    
    
    summary(per_dif_year_l)
    cpp %>% filter (Sample == "Sample", is.na(lrg_cnt)) %>% group_by (year) %>% summarize ( n())

