# pull all data for id's in divestment cohort
library(data.table)
library(tableone)

# load datasets
# full cohort
#dat <- fread('[path]') 

# subsets
hhdiv_divestors <- fread('./Datasets/hhdiv_divesters2.csv')
hhdiv_matchedcomparators <- fread('./Datasets/hhdiv_matchedcomparators2.csv')

# just keep ids and time0 (divestment date)
hhdiv_divestors <- hhdiv_divestors[,c("id","fhhdobs_date","dedup_hhid_size")]
names(hhdiv_divestors)[names(hhdiv_divestors)=='dedup_hhid_size'] <- 'hhd_hhid' # the hh id associated with the divestment
hhdiv_divestors$fhhdobs_date <- as.Date(hhdiv_divestors$fhhdobs_date) 
hhdiv_matchedcomparators <- hhdiv_matchedcomparators[,c("id","fhhdobs_date","dedup_hhid_size","hhd_id")]
names(hhdiv_matchedcomparators)[names(hhdiv_matchedcomparators)=='dedup_hhid_size'] <- 'hhd_hhid' # the hh id associated with the divestment
hhdiv_matchedcomparators$fhhdobs_date <- as.Date(hhdiv_matchedcomparators$fhhdobs_date) 

# for both divs and matches
# flag hh change: 1 = change, 0 = stable
dat[, hhchange := dedup_hhid_size != shift(dedup_hhid_size, fill = NA), by = id] 
dat$hhchange <- ifelse(dat$hhchange==0|is.na(dat$hhchange), 0, dat$hhchange) # fill NA = 0
dat[, hhgroup := cumsum(hhchange), by = id] # a change is a new "group", but multiple rows could be under a given hh
dat[, durdays := as.numeric(date1 - date0), by = .(id, hhgroup)] # duration within a pers-period
dat[, cumdurdays := cumsum(durdays), by = .(id, hhgroup)] # additive, for folks that have stable hh

# divesters : do they purchase or household purchase again? 
datdiv <- merge(dat, hhdiv_divestors, by="id") # subset to divestors
datdiv <- datdiv[date0>=fhhdobs_date] # filter to records on or after time0
datdiv[, hhdid_flag := ifelse(trimws(dedup_hhid_size) == trimws(hhd_hhid), 1, 0)] # this row's hh is the same hh from divestment record
datdiv[, hh_reacq := ifelse(exposed_hhld_HG == 1, 1, 0)] # household reacquire hgun(s)
datdiv[, time2_hhreacq := ifelse(hh_reacq == 1, as.numeric(date0 - fhhdobs_date), NA)] 
datdiv[, personal_acq := ifelse(hgun >= 1, 1, 0)] # personally acquires hgun(s)
datdiv[, time2_persacq := ifelse(personal_acq == 1, as.numeric(date0 - fhhdobs_date), NA)] 

#datdiv <- (datdiv[,c(1,15,11:12,34:44)])

fwrite(datdiv, "../Datasets/datdiv.csv")

# matches : do they purchase or household ultimately divest? 
datmatch <- merge(dat, hhdiv_matchedcomparators, by="id") # subset to divestors
length(unique(datmatch$id))

matcherrors <- hhdiv_matchedcomparators[!dat, on = "id"]
matcherrors
length(unique(matcherrors$id))

# match date makes this a little bit more complicated than above
datmatch <- datmatch[date1>=fhhdobs_date] # filter to records including or after time0
# secondary flag to look at time0 instead of date0 for first person-period
datmatch[, durdays2 := ifelse(fhhdobs_date>date0, as.numeric(date1-fhhdobs_date),durdays)] 
           
datmatch[, hhdid_flag := ifelse(trimws(dedup_hhid_size) == trimws(hhd_hhid), 1, 0)] # this row's hh is the same hh from divestment record
datmatch[, hh_div := ifelse(exposed_hhld_HG == 0, 1, 0)] # household divests of hgun(s)
datmatch[, time2_hh_div := ifelse(hh_div == 1, as.numeric(date0 - fhhdobs_date), NA)] 
datmatch[, personal_acq := ifelse(hgun >= 1, 1, 0)] # personally acquires hgun(s)
datmatch[, time2_persacq := ifelse(personal_acq == 1, as.numeric(date0 - fhhdobs_date), NA)] 

#datmatch <- (datmatch[,c(1,15,11:12,34:46)])
fwrite(datmatch, "../Datasets/datmatch.csv")


## aggregate data ----
#datdiv <- fread("../Datasets/datdiv.csv")
setkey(datdiv,id,date0)
ddivsum <- datdiv[, .(
  timeinhh_pretime0 = as.numeric(first(cumdurdays[hhdid_flag == 1]) - first(durdays[hhdid_flag == 1]) ) , 
  maxtime_divhh = max(cumdurdays[hhdid_flag == 1], na.rm = TRUE),
  first_reacq = min(date0[hh_reacq == 1], na.rm = TRUE),
  time2_first_reacq = min(as.numeric(date0[hh_reacq == 1] - min(fhhdobs_date)), na.rm = TRUE) ,
  first_persacq = min(date0[personal_acq == 1], na.rm = TRUE),
  time2_persacq = min(as.numeric(date0[personal_acq == 1] - min(fhhdobs_date)), na.rm = TRUE),
  time2_hhchange = min(as.numeric(date0[hhdid_flag == 0] - min(fhhdobs_date)), na.rm = TRUE)
), by = id]
ddivsum$time2_first_reacq <- ifelse(is.infinite(ddivsum$time2_first_reacq), NA, ddivsum$time2_first_reacq)
ddivsum$time2_persacq <- ifelse(is.infinite(ddivsum$time2_persacq), NA, ddivsum$time2_persacq)
ddivsum$time2_hhchange <- ifelse(is.infinite(ddivsum$time2_hhchange), NA, ddivsum$time2_hhchange)
fwrite(ddivsum, "../Datasets/datdivsum.csv")

#datmatch <- fread("../Datasets/datmatch.csv")
setkey(datmatch,id,date0)
dmatchsum <- datmatch[, .(
  timeinhh_pretime0 = min(as.numeric(cumdurdays[hhdid_flag == 1] - min(durdays2[hhdid_flag == 1], na.rm = TRUE)), na.rm = TRUE), 
  maxtime_divhh = max(cumdurdays[hhdid_flag == 1], na.rm = TRUE),
  first_hhdiv = min(date0[hh_div == 1], na.rm = TRUE),
  time2_first_hhdiv = min(as.numeric(date0[hh_div == 1] - min(fhhdobs_date)), na.rm = TRUE) ,
  first_persacq = min(date0[personal_acq == 1], na.rm = TRUE),
  time2_persacq = min(as.numeric(date0[personal_acq == 1] - min(fhhdobs_date)), na.rm = TRUE) ,
  time2_hhchange = min(as.numeric(date0[hhdid_flag == 0] - min(fhhdobs_date)), na.rm = TRUE)
), by = .(id,hhd_id)]
dmatchsum$time2_first_hhdiv <- ifelse(is.infinite(dmatchsum$time2_first_hhdiv), NA, dmatchsum$time2_first_hhdiv)
dmatchsum$time2_persacq <- ifelse(is.infinite(dmatchsum$time2_persacq), NA, dmatchsum$time2_persacq)
dmatchsum$time2_hhchange <- ifelse(is.infinite(dmatchsum$time2_hhchange), NA, dmatchsum$time2_hhchange)

fwrite(dmatchsum, "../Datasets/dmatchsum.csv")



## for paper
dmatchsum$flaghhdiv <- ifelse(!is.na(dmatchsum$time2_first_hhdiv), 1, 0)
dmatchsum$flagpersaq <- ifelse(!is.na(dmatchsum$time2_persacq), 1, 0)
dmatchsum$flaghhchange <- ifelse(!is.na(dmatchsum$time2_hhchange), 1, 0)

CreateCatTable(vars = c("flaghhchange", "flaghhdiv", "flagpersaq"), 
               dat = dmatchsum)

ddivsum$flaghhrea <- ifelse(!is.na(ddivsum$time2_first_reacq), 1, 0)
ddivsum$flagpersaq <- ifelse(!is.na(ddivsum$time2_persacq), 1, 0)
ddivsum$flaghhchange <- ifelse(!is.na(ddivsum$time2_hhchange), 1, 0)

CreateCatTable(vars = c("flaghhchange", "flaghhrea", "flagpersaq"), 
               dat = ddivsum)

x <- (merge(hhdiv_matchedcomparators, hhdiv_divesters, by.x = "hhd_id", by.y = "id")[,c("county_census.x","county_census.y", "tract.x", "tract.y")])
x <- x %>% mutate(tractflag = ifelse(tract.x == tract.y, 1, 0), countyflag = ifelse(county_census.x == county_census.y, 1, 0))
x %>% summarize(tract=sum(tractflag,na.rm=T), a = sum(tractflag,na.rm=T)/n(), county = sum(countyflag,na.rm=T)-sum(tractflag,na.rm=T), B = (sum(countyflag,na.rm=T)-sum(tractflag,na.rm=T))/n())
