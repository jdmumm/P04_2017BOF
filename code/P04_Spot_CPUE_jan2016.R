# 151229
# New shimp pot survey script.  Intended to replace existing CPUE sql in DB. 
# motivated primarily as part of shirmp catch up report. 
# variance of size specific CPUEs is imposible to calulate from existing sqls. 

setwd("C:/temp/newP04_SurveySummaryScript")
cpp <- read.csv("CPP.csv")
str(cpp)
names(cpp) <- c("year","event","site","station","pot","sample","all_cnt","all_kg","lrg_cnt","lrg_kg")


#Notes in PWS spot shrimp survey doc ADFG_PWS Spot Shrimp Survey_Metadata.doc and field description in DB state that all
# pots were sampled prior to 2006;  In 2005 all females were measured and half of males; and a single pot from each string 
# was selected from 2006 onward.  From the data,  apparently more than one pot was sometimes chosen in in 2012. 

#check sample pot field
  early <- subset(cpp, year < 2006) 
  late <- subset(cpp, year >= 2006) 

  summary (early$sample)
  summary (late$sample)

  

#####################################################################
# BY SITE  
####################################################################

##ALLS##
#simply need to aggregate all_cnt and all_kg using mean, by site and year
  
  #mean
  alls_avgBySite <- aggregate (cpp[,c("all_cnt","all_kg")], by = list(site = cpp$site, year = cpp$year), FUN = "mean")
  names(alls_avgBySite) <- c("site", "year", "avg_all_cnt","avg_all_kg")
  head(alls_avgBySite)
  str(alls_avgBySite)
  
  #var 
  alls_varBySite <- aggregate (cpp[,c("all_cnt","all_kg")], by = list(site = cpp$site, year = cpp$year), FUN = "var")
  names(alls_varBySite) <- c("site", "year", "var_all_cnt","var_all_kg")
  head(alls_varBySite)
  
  # join vars to all
  bySite <- merge(x = alls_avgBySite, y = alls_varBySite, by = c("year","site"), all.x = TRUE)
  head(bySite)
  str(bySite)
#####################

##LRGS##
#multiply ratio of large to alls within sampled pots at given site,  by total of alls at that site. 

# Calc prop large at each site. Subseting to only sample pots,  then dividing sum of larges by sum of alls by site.  
  samplePots <- subset(cpp, sample == "Sample")
  head(samplePots)
  tail(samplePots)
  str(samplePots)
  
  #sum larges and alls in sample pots by site 
  sampled <- aggregate(samplePots[, c("lrg_cnt","lrg_kg", "all_cnt", "all_kg")],
                       by = list(site = samplePots$site, year = samplePots$year), FUN = "sum")
  str(sampled)

  #calc ratio of lrg to alls at each site, year.  
  sampled$propLrg_cnt <- sampled$lrg_cnt/sampled$all_cnt
  sampled$propLrg_kg <- sampled$lrg_kg/sampled$all_kg  
  str(sampled)
  
   
  #add total sample pots(n_h) and and total pots (N_h) to by site
  #n_h
    n_h <- aggregate(samplePots[,"all_cnt"], by = list(site = samplePots$site, year = samplePots$year), FUN = "NROW")
    str(n_h)
    names(n_h) <- c("site", "year", "n_h")
    tail(n_h)

  #N_h 
    N_h <- aggregate(cpp[,"all_cnt"], by = list(site = cpp$site, year = cpp$year), FUN = "NROW")  
    str(N_h)
    names(N_h) <- c("site", "year", "N_h")
    tail(N_h)
  
  #merge to bySite  
    bySite <- merge(x = bySite, y = N_h, by = c("year","site"), all.x = TRUE)
    str(bySite)
    
    bySite <- merge(x = bySite, y = n_h, by = c("year","site"), all.x = TRUE)
    str(bySite)
    tail(bySite)

  #add propLrg to bySite from sampled
    bySite <- merge(x = bySite, y = sampled[,c(1,2,7,8)], by = c("year","site"), all.x = TRUE)
    str(bySite)
     
  #calc estimated catch of larges by site as product of sample ratio and catch alls
    bySite$estAvg_lrg_cnt <- bySite$avg_all_cnt * bySite$propLrg_cnt
    bySite$estAvg_lrg_kg <- bySite$avg_all_kg * bySite$propLrg_kg

    head(bySite)
    tail(bySite)

##VARIANCE associated with these estimated avg catch of larges for each site## 
  # compile all necessary values into one df
  # rows will be individual sampled pots
      
  varRat <- merge(x = samplePots, y = bySite, by = c("year","site"), all.x = TRUE)
  str(varRat)
  tail(varRat)
  #remove exra cols
  varRat <- varRat[,-c(3,4,6,11:14,19,20)]
  str(varRat)
  
  #calc residual squeares for each pot. T
  #this is squar of diffence bettween actual catch of larges in given pot from that that estimated as product of alls and samp rat at that site. 
  attach(varRat)
  #byCount
  varRat$rs_cnt <- (lrg_cnt - propLrg_cnt * all_cnt)^2
  head(varRat)
  #byKg
  varRat$rs_kg <- (lrg_kg - propLrg_kg * all_kg)^2
  head(varRat)
  detach(varRat)
  
  #sum residu squares by site
  ss <- aggregate(varRat[,c("rs_cnt","rs_kg")], by = list(site = samplePots$site, year = samplePots$year), FUN = "sum")
  str(ss)
  names(ss) <- c("site","year","ss_cnt","ss_kg")
  str(ss)
  
  # merge sum squares to bySite
  bySite <- merge(x = bySite, y = ss, by = c("year","site"), all.x = TRUE)
  str(bySite)

  #finally, calc var of sample rat by site from ss 
  bySite$varRat_cnt <- bySite$ss_cnt / (bySite$n_h - 1)
  bySite$varRat_kg <- bySite$ss_kg / (bySite$n_h - 1)
  str(bySite)

  #FINALLY, est VAR OF ESTIMATED AVG CATCH OF LARGES by site,   using var of sample rat
  attach(bySite)
  bySite$var_avg_lrg_cnt <- ((bySite$N_h - bySite$n_h)/bySite$N_h) * (bySite$varRat_cnt/bySite$n_h) 
  bySite$var_avg_lrg_kg <- ((bySite$N_h - bySite$n_h)/bySite$N_h) * (bySite$varRat_kg/bySite$n_h) 
  str(bySite)
  tail(bySite)
  head(bySite)  # hmm is this form right for early years?   When n_h = N_h est var form outputs 0.   
                # This does make sense when we realize the population here is all pots fished.   
                # And the variance here is the variance associated with the estimated mean of the of the sample pots
                # SO,  if we sample all pots, then there will be no variance of the estimated mean.  It will always 
                # equal only one value.   I think confusion arises when we confuse this variance of the mean with variance
                # of the large per pot.   Of course there is variance amongst the individual pot totals, but if we sample 
                # all the pots,  there will be no variance of the mean per pot,  ...given the population of those pots fished. 

  #above is traditional estimator.  Now,  lets use use cochran's less biased versios, by multiplying traditional, by 
  # square of  (population mean of alls at site h / sample mean of alls at site h ) .
  # lets add these values to bySite.   already have pop mean of alls by site (avg_all_cnt and avg_all_kg). 
  # sample mean of alls is probably in sampled df.  Nope, only sample sample totals.   Lets add sample means.  

  sampled_means <- aggregate(samplePots[, c("lrg_cnt","lrg_kg", "all_cnt", "all_kg")],
                     by = list(site = samplePots$site, year = samplePots$year), FUN = "mean")
  
  str(sampled_means)
  names(sampled_means) <- c("site", "year", "avg_lrg_cnt_samp", "avg_lrg_kg_samp", "avg_all_cnt_samp", "avg_all_kg_samp")
  str(sampled_means)
  
  bySite <- merge(x = bySite, y = sampled_means[,c(-3,-4)], by = c("year","site"), all.x=TRUE)
  str(bySite)

  #add cochrans correction factor 
  bySite$cochCF_cnt <- (bySite$avg_all_cnt/bySite$avg_all_cnt_samp) ^ 2
  bySite$cochCF_kg <- (bySite$avg_all_kg/bySite$avg_all_kg_samp) ^ 2
  
  summary(bySite[,c(21,22)])  # hmm,  does have effect,  CF ranges from .08 to 33.   w avg 1.3
  tail(bySite)
  head(bySite)

  # calc cochrans corrected estimated variance of estimated mean catch of larges as prod of trad and CF
  bySite$var_estAvg_lrg_cnt_coch <- bySite$var_avg_lrg_cnt * bySite$cochCF_cnt
  bySite$var_estAvg_lrg_kg_coch <- bySite$var_avg_lrg_kg * bySite$cochCF_kg
  
  tail(bySite)
  head(bySite)
  str(bySite)

  #remove extra cols
  bySite <- bySite[,-c(13:22)]
  str(bySite)
##########################################################################################################################
## Totals  ##     
# add total catch of alls and est total catch of lrgs. 

#tot alls .   sum from CPP
  alls_totBySite <- aggregate (cpp[,c("all_cnt","all_kg")], by = list(site = cpp$site, year = cpp$year), FUN = "sum")
  str(alls_totBySite)
  names(alls_totBySite) <- c("site", "year", "tot_all_cnt","tot_all_kg")
  
  bySite <- merge(x = bySite, y = alls_totBySite, by = c("year","site"), all.x = TRUE)
  str(bySite)

#est tot larges 
  bySite$estTot_lrg_cnt <- bySite$tot_all_cnt * bySite$propLrg_cnt 
  bySite$estTot_lrg_kg <- bySite$tot_all_kg * bySite$propLrg_kg
  str(bySite)
  
#est var of est tot larges
  bySite$var_estTot_lrg_cnt <- bySite$var_estAvg_lrg_cnt_coch * (bySite$N_h ^ 2)
  bySite$var_estTot_lrg_kg <- bySite$var_estAvg_lrg_kg_coch * (bySite$N_h ^ 2) 

str(bySite)
tail(bySite)

write.csv(bySite, "bySite.csv") 

##########################################################################################################################
##########################################################################################################################
##BY YEAR ##
############
# remove site 11 for now
bySite <- bySite[bySite$site != 11,]

byYear_sites <- aggregate(bySite[,"site"], by = list(year = bySite$year), FUN = "NROW")
names(byYear_sites) <- c("year", "sites") 

##LARGES##
  byYear <- aggregate (bySite[,c("N_h","n_h","estTot_lrg_cnt", "estTot_lrg_kg","var_estTot_lrg_cnt", "var_estTot_lrg_kg")]
                       , by = list(year= bySite$year), FUN = "sum")
  
  byYear_avgProps <- aggregate(bySite[,c("propLrg_cnt","propLrg_kg")], by = list(year = bySite$year), FUN = "mean")
  
  byYear <- merge(x = byYear, y = byYear_avgProps, by = c("year"), all.x = TRUE)
  
  #est avg catch large by year
  byYear$estAvg_lrg_cnt <- byYear$estTot_lrg_cnt/byYear$N_h  
  byYear$estAvg_lrg_kg <- byYear$estTot_lrg_kg/byYear$N_h  
  
  #est var of above est catch large by year
  byYear$var_estAvg_lrg_cnt <- byYear$var_estTot_lrg_cnt / (byYear$N_h^2)
  byYear$var_estAvg_lrg_kg <- byYear$var_estTot_lrg_kg / (byYear$N_h^2)
  
  byYear <- merge(x = byYear, y = byYear_sites, by = "year", all.x = TRUE)
  
  str(byYear)

##ALLS, tot and var##
  cpp <- cpp[cpp$site != 11,]
  
  alls_byYear <- aggregate(cpp[c("all_cnt","all_kg")], by = list(year = cpp$year),  FUN = "sum")
  names(alls_byYear) <- c("year", "tot_all_cnt","tot_all_kg")
  alls_byYear
  
  alls_byYear_var <- aggregate(cpp[c("all_cnt","all_kg")], by = list(year = cpp$year),  FUN = "var")
  names(alls_byYear_var) <- c("year", "var_all_cnt","var_all_kg")
  alls_byYear_var
  
  alls_byYear <- merge(x= alls_byYear, y = alls_byYear_var, by = "year", all.x = TRUE)
  str(alls_byYear) 

byYear  <- merge(x = byYear, y = alls_byYear, by = "year", all.x = "true")
str(byYear) 

#avg all
byYear$avg_all_cnt <- byYear$tot_all_cnt / byYear$N_h
byYear$avg_all_kg <- byYear$tot_all_kg / byYear$N_h

str(byYear)
#reaarange cols
byYear <- byYear[,c(1:3,14:16,8,9,19,20,17,18,4:7,10:13)]
str(byYear)
write.csv(byYear, "byYear.csv")


