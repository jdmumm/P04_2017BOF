## CPUE ####
# Calculates CATCH and CPUE for both Large and Alls from PWS spot shrimp pot survey. Used for to calc variances for 2017 BOF report. 
#Point ests used in report are from static SS output from old queries.
## VARs in report were output from this scripts 171103
# Josh Mumm 
## modified to follow Xinxians recomendatoins 171129.  From CPUE_revisedRat.R

## PREP ----
library(tidyverse)
cpp <- read.csv('data/CPP_lessSamps_d_180119.csv') # changed to cpp w 2017 in 2017 branch on 180119
  # _lessSamps is from temporaily correcting sample pot indicator on potperformance for those pots labeled sample but w/out any awls
  # lessSamps_d is is from modifying cpp sqls to remove rounding in step 6 and remove pop blank weight from count in 2. 
  # Also after editing 2011 DB (removed cc recs wtih 0 count and weight - this shouldn't have made a dif).  
  # Will use _d.  ALLs - both tau and mu match old query for all years rounded to nearest .1 %.   Large Tau and Mu are < .7% dif for all years 
read.csv('data/SiteStatArea_LUT.csv')  %>% transmute (Site = as.factor(SiteNum), Area = ShrimpArea) -> area

#rename vars and calc r 
  cpp %>% transmute(year = YEAR, 
                 Event = EVENT_ID, 
                 Site = as.factor(SITE_ID), 
                 Station = STATION, 
                 Pot = as.factor(POT_ID),
                 Sample = SAMPLE_POT, 
                 all_cnt = all_Cnt_cc, 
                 all_kg = all_Kg_cc, 
                 lrg_cnt = lrg_Cnt,
                 lrg_kg = lrg_Kg,
                 r_kg = lrg_kg/all_kg, 
                 r_cnt = lrg_cnt/all_cnt) -> cpp

left_join(cpp, area) -> cpp
## ALLS ----
  #survey-wide 
  cpp %>% filter (Site %in% c("1","2","3","4","5","7","8") & Station %in% c("A","B","C","D")) %>% group_by(year) %>% # core filter
    summarise ( 
      N = n(),
      tau_all_cnt = sum(all_cnt), 
      tau_all_kg = sum(all_kg),
      mu_all_cnt = tau_all_cnt/N,
      mu_all_kg = tau_all_kg/N,  
      var_all_cnt = sum((all_cnt - mu_all_cnt)^2)/(N-1), # n-1 since treating as sample not pop now(10/21).  Leaving N as N, rather than  n for now. 
      var_all_kg = sum((all_kg - mu_all_kg)^2)/(N-1) , # xz has N insteasd of N-1
      se_all_cnt = (var_all_cnt^.5)/(N^.5),
      se_all_kg = (var_all_kg^.5)/(N^.5),
      cv_all_cnt = 100* (var_all_cnt^.5)/mu_all_cnt,
      cv_all_kg = 100* (var_all_kg^.5)/mu_all_kg) -> all_byYear
  #bySite
  cpp %>% filter (Station %in% c("A","B","C","D")) %>% group_by(year, Site) %>% # core filter
    summarise ( 
      Area = first(Area), 
      N = n(),
      tau_all_cnt = sum(all_cnt), 
      tau_all_kg = sum(all_kg),
      mu_all_cnt = tau_all_cnt/N,
      mu_all_kg = tau_all_kg/N, 
      var_all_cnt = sum((all_cnt - mu_all_cnt)^2)/(N-1),
      var_all_kg = sum((all_kg - mu_all_kg)^2)/(N-1),
      se_all_cnt = (var_all_cnt^.5)/(N^.5),
      se_all_kg = (var_all_kg^.5)/(N^.5),
      cv_all_cnt = 100* (var_all_cnt^.5)/mu_all_cnt,
      cv_all_kg = 100* (var_all_kg^.5)/mu_all_kg) -> all_bySite
  #byArea
    cpp %>% filter (Site %in% c("1","2","3","4","5","7","8") & Station %in% c("A","B","C","D")) %>% group_by(year, Area) %>%  # core filter
    summarise ( 
      N = n(),
      tau_all_cnt = sum(all_cnt), 
      tau_all_kg = sum(all_kg),
      mu_all_cnt = tau_all_cnt/N,
      mu_all_kg = tau_all_kg/N, 
      var_all_cnt = sum((all_cnt - mu_all_cnt)^2)/(N-1),
      var_all_kg = sum((all_kg - mu_all_kg)^2)/(N-1),
      se_all_cnt = (var_all_cnt^.5)/(N^.5),
      se_all_kg = (var_all_kg^.5)/(N^.5),
      cv_all_cnt = 100*  (var_all_cnt^.5)/mu_all_cnt,
      cv_all_kg = 100* (var_all_kg^.5)/mu_all_kg) -> all_byArea
  
## LARGES ----
  # bySite 
    all_bySite %>% select(year, Site, Area, N, mu_all_cnt, mu_all_kg, tau_all_kg, var_all_kg) %>% right_join(cpp) %>% # join site-level stats to CPP
      filter (Sample == "Sample") %>% group_by(year, Site) %>% 
      summarise ( 
        Area = first(Area),
        n = n(),
        N = first(N),
        mu_hat_all_kg = sum(all_kg, na.rm = T)/n,
        var_all_kg = first(var_all_kg), # xz uses N instead od N-1 for num of var_all_kg
        tau_all_kg = first(tau_all_kg), 
        r_bar = sum(lrg_kg, na.rm = T)/sum(all_kg, na.rm = T),
        s2_h = sum((lrg_kg - r_bar*all_kg)^2, na.rm = T)/ (n-1),
        r_var = (s2_h/n) * (1/mu_hat_all_kg)^2 * (N-n)/N, 
        mu_lrg_kg  = r_bar * first(mu_all_kg),
        tau_lrg_kg =  mu_lrg_kg * N, 
        var_tau_lrg_kg = (tau_all_kg^2)*r_var + (r_bar^2)*var_all_kg - r_var*var_all_kg, # xz uses N instead od N-1 for num of var_all_kg
        var_mu_lrg_kg = var_tau_lrg_kg/(N^2) ) -> large_bySite      
    
    #byYear 
    large_bySite %>% filter (Site != "11") %>% group_by (year) %>% 
      summarise (
        n = sum(n),
        N = sum(N),
        tau_lrg_kg = sum(tau_lrg_kg),
        mu_lrg_kg  = tau_lrg_kg / N, 
        var_tau_lrg_kg  = sum(var_tau_lrg_kg, na.rm = T), # Na.rm added as bandaid for 2011 site 5 no shrimp in cpp.  Edit data eventually. 
        var_mu_lrg_kg = var_tau_lrg_kg/(N^2) ,
        se_lrg_kg = (var_mu_lrg_kg^.5)) -> large_byYear  
   
      #byArea
    large_bySite %>% filter (Site != "11") %>% group_by (year,Area) %>% 
      summarise (
        n = sum(n),
        N = sum(N),
        tau_lrg_kg = sum(tau_lrg_kg),
        mu_lrg_kg  = tau_lrg_kg / N, 
        var_tau_lrg_kg  = sum(var_tau_lrg_kg, na.rm = T), # Na.rm added as bandaid for 2011 site 5 no shrimp in cpp.  Edit data eventually. 
        var_mu_lrg_kg = var_tau_lrg_kg/(N^2) ,
        se_lrg_kg = (var_mu_lrg_kg^.5)) -> large_byArea

#select, join and Write ----
  all_byYear %>% left_join (large_byYear) %>% select(year, N, n, var_all_kg, se_all_kg, var_tau_lrg_kg, se_lrg_kg)-> var_byYear
  all_byArea %>% left_join (large_byArea) %>% select(year, Area, N, n, var_all_kg, se_all_kg, var_tau_lrg_kg, se_lrg_kg)-> var_byArea
  
  write.csv(var_byYear, "./output/var_byYear_xz_w17_core.csv", row.names = F)  
  write.csv(var_byArea, "./output/var_byArea_xz_w17_core.csv", row.names = F)    
    
    
    
    
    
    
    
        
# ###############################################################################################
# ## errror checking.  compare to old output  (this copied from CPUE.r and probably won't run exactly as is since  above modified. ----
#     options(scipen = 999)
#     
#     # ALLS      
#     old <- read.csv('data/temp/2016QueryOutput/QUERY_CPUE_ANNUAL_SUMMARY_all_170127.csv')
#     str(old)
#     str(all_byYear)
#     old %>% select (year = Year,
#                     N = Pot_Count,
#                     tau_all_cnt = Total_Spot_Count, 
#                     tau_all_kg = Total_Spot_Wt_KG, 
#                     mu_all_cnt = CPUE_All_Count,
#                     mu_all_kg = CPUE_All_KG
#                     ) -> o
#     
#     all_byYear[,1:6] -> n 
#     
#     o %>% left_join(n, by = "year" ) -> comp 
#     comp
#     str(comp)
#     comp[ , order(names(comp))] -> comp# reorder columns
#     
#     #calc difs 
#     o[,order(names(o))] -> o
#     n[,order(names(n))] -> n 
#     str(o)
#     str(n)
#     
#     dif_year <- o[,1:5] - n[,1:5] 
#     cbind(dif_year,year = o$year) -> dif_year
#     
#     per_dif_year <- 100* dif_year[,-6]/o[,-6]
#     cbind(per_dif_year,year = o$year) -> per_dif_year
#     per_dif_year %>% transmute(year = year,
#                                  mu_all_cnt = round(mu_all_cnt,1),
#                                  mu_all_kg = round(mu_all_kg,1),
#                                  tau_all_cnt = round(tau_all_cnt,1),
#                                  tau_all_kg = round(tau_all_kg,1)) -> per_dif_year_r                                                       
#     
#     # Larges ####  
#     old %>% select (year = Year,
#                     N = Pot_Count,
#                     tau_lrg_cnt = Est_Ct_LG, 
#                     tau_lrg_kg = Est_Wt_Large, 
#                     mu_lrg_cnt = CPUE_Large_Count,
#                     mu_lrg_kg = CPUE_Large_KG) -> o_l
#     large_byYear[,c(1,3:7)] -> n_l 
#     o_l[,order(names(o_l))] -> o_l
#     n_l[,order(names(n_l))] -> n_l 
#     
#     dif_year_l <- o_l[,1:5] - n_l[,1:5] 
#     cbind(dif_year_l,year = n_l$year) -> dif_year_l
#     
#     per_dif_year_l <- 100* dif_year_l[,-6]/n_l[,-6]
#     cbind(per_dif_year_l,year = n_l$year) -> per_dif_year_l
#     per_dif_year_l %>% transmute(year = year,
#                     mu_lrg_cnt = round(mu_lrg_cnt,1),
#                     mu_lrg_kg = round(mu_lrg_kg,1),
#                     tau_lrg_cnt = round(tau_lrg_cnt,1),
#                     tau_lrg_kg = round(tau_lrg_kg,1)) -> per_dif_year_l_r
#    
#     
#     
#     summary(per_dif_year_l)
#     cpp %>% filter (Sample == "Sample", is.na(lrg_cnt)) %>% group_by (year) %>% summarize ( n())

