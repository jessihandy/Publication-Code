#***************************************************************
#' HH Divestment
#' Author: Jessica Handy
#' Created: 27 MAR 2026
#' Last modified: 20 AUG 2026
#' 
#' Purpose: Clean matched cohort; base tables
#' 
#***************************************************************

# Admin ----
## load packages
pacman::p_load(
  data.table, 
  lubridate,
  dplyr, 
  tableone
)


## load data 
hhdiv_divesters <- read.csv("./Datasets/hhdiv_divesters2.csv")
hhdiv_matchedcomparators <- read.csv("./Datasets/hhdiv_matchedcomparators2.csv")

## fix formatting for merge
hhdiv_matchedcomparators$fhhdobs_date <- as.Date(hhdiv_matchedcomparators$fhhdobs_date)
hhdiv_matchedcomparators$deathdate <- as.Date(hhdiv_matchedcomparators$deathdate)
hhdiv_matchedcomparators$endobs_date <- as.Date(hhdiv_matchedcomparators$endobs_date)
hhdiv_matchedcomparators$end_fu <- as.Date(ifelse(!is.na(hhdiv_matchedcomparators$deathdate) & hhdiv_matchedcomparators$deathdate < hhdiv_matchedcomparators$endobs_date, hhdiv_matchedcomparators$deathdate, hhdiv_matchedcomparators$endobs_date))
hhdiv_matchedcomparators$tract <- as.integer(hhdiv_matchedcomparators$tract)
hhdiv_matchedcomparators$county_census <- as.integer(hhdiv_matchedcomparators$county_census)
hhdiv_matchedcomparators$divestment <- 0
hhdiv_matchedcomparators$w <- 0.5

hhdiv_divesters$fhhdobs_date <- as.Date(hhdiv_divesters$fhhdobs_date)
hhdiv_divesters$deathdate <- as.Date(hhdiv_divesters$deathdate)
hhdiv_divesters$endobs_date <- as.Date(hhdiv_divesters$endobs_date)
hhdiv_divesters$end_fu <- as.Date(ifelse(!is.na(hhdiv_divesters$deathdate) & hhdiv_divesters$deathdate < hhdiv_divesters$endobs_date, hhdiv_divesters$deathdate, hhdiv_divesters$endobs_date))
hhdiv_divesters$tract <- as.integer(hhdiv_divesters$tract)
hhdiv_divesters$county_census <- as.integer(hhdiv_divesters$county_census)
hhdiv_divesters$divestment <- 1
hhdiv_divesters$w <- 1


dat <- bind_rows(hhdiv_divesters, hhdiv_matchedcomparators)
dat$ltfu <- ifelse(dat$ever_die != 1 & dat$endobs_date < "2021-12-31", 1, 0)
dat$ever_dieNoSuic <- ifelse(dat$ever_die == 1 & dat$ever_suic != 1, 1, 0 )
dat$obstime <- as.numeric(dat$end_fu - dat$fhhdobs_date)
dat$obstime_mo <- (((year(dat$end_fu) - year(dat$fhhdobs_date) )* 12) + (month(dat$end_fu) - month(dat$fhhdobs_date)))
# note: fhhdobs_date  is time0

#dat$w <- 1
dat$cluster <- coalesce(dat$hhd_id, dat$id) # fill for divesters to match

# link RUCA -----
# match tract formatting for RUCA join
dat$tract = as.integer(dat$tract) - as.integer(dat$county)*1000000
# note this automatically drops "na" text tracts/counties

##add in urbanicity variables (all dummy variables)
# note: original cohort uses 2010 census tracts and aligned RUCA codes
datruca <- fread('../Data/ruca_code_category_v2.csv', 
                 colClasses = c('countycode'='character')) # specify character type to allow join
datruca <- datruca[, c(2:7)] # drop extra vars
names(datruca)[names(datruca)=='countycode'] <- 'county' # note this is name in current cohort file, verify any changes
names(datruca)[names(datruca)=='ctcode'] <- 'tract'

dat$county <- sprintf("%03d", as.integer(dat$county)) # force field type to merge correct error
dat$tract <- as.integer(dat$tract) # force field type to merge correct error
nrow(dat)


#temp1 <- merge(temp1,datruca,by=c('county_census','tract'),all.x=TRUE)
dat <- merge(dat,datruca,by=c('county','tract'),all.x=TRUE)
summary(dat$urban_core) # QC - did it link? 

# for records starting 2018-02-16 onward, census tracts are geocoded using 2020 census
# need to subset and relink data with date0 >= 2018-02-16 with newer RUCA

# note: some records after 2018-02-16 still map to 2010 census tracts (e.g., 600 in county 019 
# instead of 601 or 602) -- modified code to overwrite with 2020 records if data, available
# otherwise default to 2010 RUCA

datruca <- fread('../Data/ruca_2020.csv', 
                 colClasses = c('countycode'='character')) # specify character type to allow join
datruca <- datruca[, c(2:3,7:10)] # drop extra vars
names(datruca)[names(datruca)=='countycode'] <- 'county'
names(datruca)[names(datruca)=='ctcode'] <- 'tract'

temp2 <- subset(dat, endobs_date>="2018-02-16") # records after xtn seam, onward
temp2 <- merge(temp2,datruca,by=c('county','tract'),all.x=TRUE)
temp2$urban_core <- ifelse(!is.na(temp2$urban_core.y),temp2$urban_core.y,temp2$urban_core.x) 
temp2$sub_urban <- ifelse(!is.na(temp2$sub_urban.y),temp2$sub_urban.y,temp2$sub_urban.x) 
temp2$large_rural_town <- ifelse(!is.na(temp2$large_rural_town.y),temp2$large_rural_town.y,temp2$large_rural_town.x) 
temp2$small_town_isolated_rural <- ifelse(!is.na(temp2$small_town_isolated_rural.y),temp2$small_town_isolated_rural.y,temp2$small_town_isolated_rural.x) 
nrow(temp2)
summary(temp2$urban_core.y) # QC - did it link

#temp2 <- temp2[, -c(59:66)] # check before run that these are still appr to remove .x/.y
temp2 <- temp2[, -c(60:67)]

# we still need to separate temp1 to rbind
temp1 <- subset(dat, endobs_date<"2018-02-16") # records 02/2018 and prior
nrow(temp1)

dat <- rbind(temp1,temp2)
nrow(dat)
summary(dat)


# Tables ----
# table 1
dat$agegrp <- case_when(dat$ageyr_t0 <35 ~ "21-34", 
                        dat$ageyr_t0 <50 ~ "35-49", 
                        dat$ageyr_t0 <65 ~ "50-64", 
                        TRUE ~ "65+")

print(CreateTableOne(
  vars = c("agegrp", "sex", "race", 
           "urban_core", "sub_urban", "large_rural_town", 
           "small_town_isolated_rural"),
  strata = "divestment",
  data = dat,
  factorVars = c("agegrp", "sex", "race", "urban_core", 
                 "sub_urban", "large_rural_town", "small_town_isolated_rural")
), showAllLevels = TRUE)



## QC ----
# why are matches not 1:1 
setDT(dat)
setkey(dat,cluster,id)
# flag the lag
dat[,agechange := if_else(shift(agegrp, 1, type = "lag", fill = NA)==agegrp, 
                                  0,1 ), by = cluster]
dat[,sexchange := if_else(shift(sex, 1, type = "lag", fill = NA)==sex, 
                          0,1 ), by = cluster]
dat[,racechange := if_else(shift(race, 1, type = "lag", fill = NA)==race, 
                          0,1 ), by = cluster]
dat[,countychange := if_else(shift(county_census, 1, type = "lag", fill = NA)==county_census, 
                          0,1 ), by = cluster]
dat[,tractchange := if_else(shift(tract, 1, type = "lag", fill = NA)==tract, 
                             0,1 ), by = cluster]
# fill in the NAs
dat[,agechange := if_else(!is.na(agechange), agechange, if_else(
  shift(agegrp, 1, type = "lead", fill = NA)==agegrp, 0, 1)), by = cluster]

dat[,sexchange := if_else(!is.na(sexchange), sexchange, if_else(
  shift(sex, 1, type = "lead", fill = NA)==sex, 0, 1)), by = cluster]
dat[,racechange := if_else(!is.na(racechange), racechange, if_else(
  shift(race, 1, type = "lead", fill = NA)==race, 0, 1)), by = cluster]
dat[,countychange := if_else(!is.na(countychange), countychange, if_else(
  shift(county_census, 1, type = "lead", fill = NA)==county_census, 0, 1)), by = cluster]
dat[,tractchange := if_else(!is.na(tractchange), tractchange, if_else(
  shift(tract, 1, type = "lead", fill = NA)==tract, 0, 1)), by = cluster]
dat[,sexracetractC := ifelse(sexchange == 1 & racechange == 1 & tractchange == 1, 1, 0)]
dat$year <- year(dat$fhhdobs_date) # year of t0

# annual "changes" from div to match
# seems to occur across years
CreateTableOne(
  vars = c("agechange", "sexchange", "racechange", "countychange", "tractchange"),
  strata = "year",
  data = dat,
  factorVars = c("agechange", "sexchange", "racechange", "countychange", "tractchange")
)

CreateTableOne(
  vars = c("countychange", "tractchange"),
  strata = "divestment",
  data = dat,
  factorVars = c("countychange", "tractchange")
)


# table 2
#' Report by divestment status the N lost to follow-up (no death and endobs < 12/31/2021),
#'  N die of non-suicide, 
#'  N suicide as total, 
#'  firearm, and non-firearm
CreateTableOne(
  vars = c("ltfu", "ever_die","ever_dieNoSuic","ever_suic", "ever_fmsuic", "ever_nfmsuic", "obstime"),
  strata = "divestment",
  data = dat,
  factorVar = c("ltfu","ever_die","ever_dieNoSuic", "ever_suic", "ever_fmsuic", "ever_nfmsuic")
)

fwrite(dat, "./Datasets/full_match1_2.csv")
