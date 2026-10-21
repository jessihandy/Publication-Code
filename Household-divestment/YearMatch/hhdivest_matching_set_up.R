#dat <- fread('/projects/studdert/longshot-pitt/Data/LongSHOT2021_hhld_021026.csv') ##as of 2/2026, extended cohort
## Household divestment program for setting up matching
## Last updated: March 25, 2026
## Primary functions of program:
##    - prepares data for matching
##    - creates matching functions to be called in separate files
## 
#################

##R packages
options(repos = c(CRAN = "http://cran.us.r-project.org"))
if (!require("pacman")) install.packages("pacman") 
pacman::p_load(
    reshape, 
    lubridate, 
    data.table, 
    runner,
    dplyr,
    tableone, 
    survival)

##reading in data
#dat <- fread('[path]') 

##print summary of latest dataset
#summary(dat)
colnames(dat)

##renaming minimal variables to ease coding
names(dat)[names(dat)=='fakeid.2023'] <- 'id'
length(unique(dat$id))
names(dat)[names(dat)=='death_date'] <- 'deathdate'
names(dat)[names(dat)=='hgun'] <- 'hguns'
names(dat)[names(dat)=='lgun'] <- 'lguns'
names(dat)[names(dat)=='cum_hhld_HG'] <- 'hh_hguns'
names(dat)[names(dat)=='cum_hhld_LG'] <- 'hh_lguns'
names(dat)[names(dat)=='stobs'] <- 'stobs_date'
names(dat)[names(dat)=='endobs'] <- 'endobs_date'
names(dat)[names(dat)=='census_tract'] <- 'tract'
names(dat)[names(dat)=='county'] <- 'county_census'

##drop prior purchasers
dat <- setDT(dat)[GO_at_entry=='no']
length(unique(dat$id))

##number of cohabitants (known) based on 2, 3, or 4-person hh
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
dat[,hhsize:=as.numeric(substrRight(dedup_hhid_size,1))] # old way, pull N from end of id
#dat[,hhsize:=as.numeric(hhld_size)] # new variable in feb16 cohort codes directly 
dat[,hhsize234:=ifelse(hhsize>=2 & hhsize<=4,1,0)]
dat[,hhsize234:=ifelse(is.na(hhsize)==TRUE,0,hhsize234)]

##non-missing date of birth
dat <- setDT(dat)[is.na(DATE_OF_BIRTH)==FALSE]
length(unique(dat$id))
##relabel NA race as 'unknown'
#dat$race <- ifelse(is.na(dat$race)==TRUE,'unknown',dat$race)
##relabel NA sex as 'unknown'
#dat$sex <- ifelse(is.na(dat$sex)==TRUE,'unknown',dat$sex)
##drop those with unknown sex or race
dat <- setDT(dat)[is.na(race)==FALSE]
length(unique(dat$id))
dat <- setDT(dat)[race!='unknown']
length(unique(dat$id))
dat <- setDT(dat)[is.na(sex)==FALSE]
length(unique(dat$id))

##sort by id and time period
dat <- dat[with(dat,order(id,as.Date(date0,'%d%b%Y'))),]

##merging in death information into all rows of person-periods (death date, ever die)
dat$ever_causedeath <- dat$causedeath
dat[,ever_die:=ifelse(is.na(causedeath)==TRUE,0,1)]
##drop 'zombie' person-periods
##i.e., drop periods of time completely after a death date for that individual
nrow(dat)
length(unique(dat$id))
dat <- setDT(dat)[as.Date(date0,'%d%b%Y') <= as.Date(deathdate,'%d%b%Y') | is.na(deathdate)==TRUE]
nrow(dat)
length(unique(dat$id))

##new variable: birth cohort (5-year bins)
##0 = <=1900; 1 = 1901-1905; ... 20 = 1996-2000
dat$yrbirth <- year(as.Date(dat$DATE_OF_BIRTH))
dat$yrbirth_ind5 <- ceiling((dat$yrbirth-1900)/5)

##creating variables of exposed indicator and 'ever exposed'
dat$hguns_ind <- as.numeric(dat$hguns>0)
dat[,hguns_hx:=cummax(hguns_ind),by=id]

##creating variable for first hh exposure/purchase (inclusive of own purchase)
dat_fhhp <- setDT(dat)[hh_hguns>=1] 
dat_fhhp <- dat_fhhp[dat_fhhp[,.I[which.min(as.Date(date0,'%d%b%Y'))],by=id]$V1]
dat_fhhp <- dat_fhhp[,.(id,date0)]
colnames(dat_fhhp) <- c('id','fhhpobs_date')
dat <- merge(dat,dat_fhhp,by='id',all=TRUE)
rm(dat_fhhp)
##removing person-periods prior to first hh exposure
dat <- dat[as.Date(date0,'%d%b%Y') >= as.Date(fhhpobs_date,'%d%b%Y')]

##extract dates
g04.date = as.Date("2004-10-18")
s05.date = as.Date("2005-10-24")
g06.date = as.Date("2006-10-23")
s08.date = as.Date("2008-01-22")
p08.date = as.Date("2008-05-19")
g08.date = as.Date("2008-10-20")
p10.date = as.Date("2010-05-24")
g10.date = as.Date("2010-10-18")
p12.date = as.Date("2012-05-21")
g12.date = as.Date("2012-10-22")
p14.date = as.Date("2014-05-19")
g14.date = as.Date("2014-10-20")
p16.date = as.Date("2016-05-23")
#cohort extension
g16.date = as.Date("2018-02-16")
p18.date = as.Date("2019-05-30")
g18.date = as.Date("2019-12-05")
p20.date = as.Date("2020-12-05")
g20.date = as.Date("2021-05-02")
 
vfdates <- data.frame(matrix(nrow=18,ncol=2))
colnames(vfdates) <- c('date','year')
vfdates$date <- c(g04.date,
                  s05.date,g06.date,s08.date,
                  p08.date,g08.date,p10.date,g10.date,
                  p12.date,g12.date,p14.date,g14.date,
                  p16.date,g16.date,p18.date,g18.date, 
                  p20.date,g20.date)
vfdates$year <- year(as.Date(vfdates$date,format='%d%b%Y'))

##flags if there was a household composition change from the last person-period
##useful for saying if there was a change between date0 and the day before date0
dat[, hhcompchange := if_else(shift(dedup_hhid_size, 1, type = "lag", fill = NA)==dedup_hhid_size, 
                                  0,1 ), by = id]

##flags if there was a household handgun change from the last person-period
##useful for saying if there was a change between date0 and the day before date0
dat[, hh_hguns_change := if_else(shift(hh_hguns, 1, type = "lag", fill = NA)==hh_hguns, 
                                  0,1 ), by = id]

#################
## creating a dataset of all individuals experiencing a hh 'divestment' (hh_hguns from >=1 to 0)
## defining age at time of hh divestment (time zero)
#################

##look for first hh 'divestment'
##among those with:
##-no history of personal ownership and 
##-hhsize of 2, 3, or 4 and 
##-no hh comp change
dat_fhhd <- setDT(dat)[hh_hguns==0 & hguns_hx==0 & hhsize234==1 & hhcompchange==0 & hh_hguns_change==1 & hhsize==2]
dat_fhhd <- dat_fhhd[dat_fhhd[,.I[which.min(as.Date(date0,'%d%b%Y'))],by=id]$V1]
nrow(dat_fhhd) ##quality check on number of unique hh divesters observed

##age at time zero
dat_fhhd$ageyr_t0 <- as.numeric(round(difftime(as.Date(dat_fhhd$date0,'%d%b%Y'),
                         as.Date(dat_fhhd$DATE_OF_BIRTH),
                         units='days')/365.25,0))
summary(dat_fhhd$ageyr_t0)

##remove those who are ages < 21 at time of hh divestment
dat_fhhd <- subset(dat_fhhd,ageyr_t0>=21)
##remove those who are ages >= 100 at time of hh divestment
dat_fhhd <- subset(dat_fhhd,ageyr_t0<100)

##number of individuals 18-99 in 2-4 person households with no history of personal ownership and a cohabitant that divested
nrow(dat_fhhd)

##remove those on the voter file seams
dat_fhhd <- subset(dat_fhhd,!(as.Date(date0) %in% vfdates$date))
nrow(dat_fhhd)

##remove those who aren't geocoded with a tract at time of divestment
dat_fhhd <- dat_fhhd[!(tract=='n_VF_no_C')]
nrow(dat_fhhd)
dat_fhhd$fhhdobs_date <- dat_fhhd$date0

##defining first hh 'divestment' date to go into the full dataset from these
dat_fhhd2 <- dat_fhhd[,.(id,fhhdobs_date)]
dat <- merge(dat,dat_fhhd2,by='id',all=TRUE)

#################
## creating function to match by year, sex, and race 
## input to ultimate function is simply calendar year of the divestment date
#################

match_by_year_sex_race <- function(cyear=2004,csex='M',crace='white') {
  
##restrict first hh divestments to cyear, csex, crace
dat_fhhd2 <- dat_fhhd[year(as.Date(fhhdobs_date,'%d%b%Y'))==cyear
                     & sex==csex
                     & race==crace]

##returning to original person-period data to find unexposed in this time (of same sex and race)
##must be no hx of personal ownership, hh size of 2-4, have hh exposure (i.e., not divested)
dat_matchable2 <- dat[hh_hguns>0
                      & hguns_hx==0
                      & hhsize234==1 & hhsize==2
                      & year(as.Date(date0,'%d%b%Y'))<=cyear
                      & year(as.Date(date1,'%d%b%Y'))>=cyear
                      & race==crace
                      & sex==csex]
##loop within the relevant subset to find matches
##this matches on birth cohort, census tract, sex, and race
for(i in 1:nrow(dat_fhhd2)) {
  temprace <- dat_fhhd2[i,]$race  
  tempsex <- dat_fhhd2[i,]$sex
  tempyrbirth_ind5 <- dat_fhhd2[i,]$yrbirth_ind5
  tempcounty <- dat_fhhd2[i,]$county_census
  temptract <- dat_fhhd2[i,]$tract
  tempid <- dat_fhhd2[i,]$id
  tempfhhdobs_date <- dat_fhhd2[i,]$fhhdobs_date
  temp <- subset(dat_matchable2,yrbirth_ind5==tempyrbirth_ind5 & tract==temptract)
  temp <- subset(temp,as.Date(temp$date0,'%d%b%Y')<=as.Date(tempfhhdobs_date,'%d%b%Y'))
  temp <- subset(temp,as.Date(temp$date1,'%d%b%Y')>as.Date(tempfhhdobs_date,'%d%b%Y'))
  ##drop those already dead by fhhdobs_date ('zombie' voterfile time post-death)
  temp <- subset(temp,is.na(temp$deathdate)==TRUE | difftime(as.Date(temp$deathdate,'%d%b%Y'),as.Date(tempfhhdobs_date,'%d%b%Y'),units='days')>=0)
##drop those ages<21
  temp <- subset(temp,as.numeric(round(difftime(as.Date(tempfhhdobs_date,'%d%b%Y'),
                                         as.Date(temp$DATE_OF_BIRTH),
                                        units='days')/365.25,0))>=21)
  ##drop those ages>=100
  temp <- subset(temp,as.numeric(round(difftime(as.Date(tempfhhdobs_date,'%d%b%Y'),
                                         as.Date(temp$DATE_OF_BIRTH),
                                        units='days')/365.25,0))<100)    
    
  newmat <- matrix(ncol=ncol(dat_matches),nrow=nrow(temp))
  colnames(newmat) <- colnames(dat_matches)
  newmat[,1] <- temp$id
  newmat[,2] <- rep(tempid,nrow(temp))
  newmat[,3] <- rep(tempfhhdobs_date,nrow(temp))
  newmat[,4] <- temp$deathdate
  newmat[,5] <- temp$ever_die
  newmat[,6] <- temp$endobs_date
  newmat[,7] <- as.numeric(round(difftime(as.Date(tempfhhdobs_date,'%d%b%Y'),
                                         as.Date(temp$DATE_OF_BIRTH),
                                        units='days')/365.25,0))
  newmat[,8] <- temp$sex
  newmat[,9] <- temp$race
  newmat[,10] <- temp$ever_causedeath
  newmat[,11] <- temp$dedup_hhid_size
  newmat[,12] <- temp$tract
  newmat[,13] <- temp$county_census
  newmat[,14] <- temp$hguns
  newmat[,15] <- temp$lguns
  newmat[,16] <- temp$hh_hguns
  newmat[,17] <- temp$hh_lguns
  
  if(nrow(temp)>=1) {dat_matches <<- rbind(dat_matches,newmat[sample(nrow(newmat),1),])}
  #if(nrow(temp)==1) {dat_matches <<- rbind(dat_matches,newmat)}
  vect <- c(tempid,nrow(temp))
  dat_matchn <<- rbind(dat_matchn,vect)
  rm(newmat,temprace,tempsex,tempyrbirth_ind5,tempcounty,temptract,tempid,tempfhhdobs_date,temp)
}
}

##calling the function above into a function for each year
match_by_year <- function(ccyear=2004) {
    match_by_year_sex_race(cyear=ccyear,csex='F',crace='asian')
        print(nrow(dat_matchn))
    match_by_year_sex_race(cyear=ccyear,csex='F',crace='black')
        print(nrow(dat_matchn))
    match_by_year_sex_race(cyear=ccyear,csex='F',crace='hispanic')
        print(nrow(dat_matchn))
    match_by_year_sex_race(cyear=ccyear,csex='F',crace='other')
         print(nrow(dat_matchn))
    #match_by_year_sex_race(cyear=ccyear,csex='F',crace='unknown')
     #   print(nrow(dat_matchn))
    match_by_year_sex_race(cyear=ccyear,csex='F',crace='white')
        print(nrow(dat_matchn))
    match_by_year_sex_race(cyear=ccyear,csex='M',crace='asian')
        print(nrow(dat_matchn))
    match_by_year_sex_race(cyear=ccyear,csex='M',crace='black')
        print(nrow(dat_matchn))
    match_by_year_sex_race(cyear=ccyear,csex='M',crace='hispanic')
        print(nrow(dat_matchn))
    match_by_year_sex_race(cyear=ccyear,csex='M',crace='other')
        print(nrow(dat_matchn))
    #match_by_year_sex_race(cyear=ccyear,csex='M',crace='unknown')
     #   print(nrow(dat_matchn))
    match_by_year_sex_race(cyear=ccyear,csex='M',crace='white')
        print(nrow(dat_matchn))
   # match_by_year_sex_race(cyear=ccyear,csex='unknown',crace='asian')
    #    print(nrow(dat_matchn))
    #match_by_year_sex_race(cyear=ccyear,csex='unknown',crace='black')
    #    print(nrow(dat_matchn))
    #match_by_year_sex_race(cyear=ccyear,csex='unknown',crace='hispanic')
    #    print(nrow(dat_matchn))
    #match_by_year_sex_race(cyear=ccyear,csex='unknown',crace='other')
    #    print(nrow(dat_matchn))
    #match_by_year_sex_race(cyear=ccyear,csex='unknown',crace='unknown')
    #    print(nrow(dat_matchn))
    #match_by_year_sex_race(cyear=ccyear,csex='unknown',crace='white')
    #    print(nrow(dat_matchn))
}

#################
## match by year but saving datasets along the way by year
#################

match_save_year <- function(theyear) {

##empty 'bank' of possible matches
dat_matches <<- matrix(ncol=17,nrow=1)
dat_matches <<- as.data.frame(dat_matches)
colnames(dat_matches) <<- c('id','hhd_id','fhhdobs_date','deathdate','ever_die','endobs_date',
                           'ageyr_t0','sex','race','ever_causedeath','dedup_hhid_size',
                           'tract','county_census',
                           'hguns','lguns','hh_hguns','hh_lguns'
                           )

##empty dataset for number of matches
dat_matchn <<- matrix(ncol=2,nrow=1)
dat_matchn <<- as.data.frame(dat_matchn)
colnames(dat_matchn) <<- c('hhd_id','nmatch')
    
##do the matching
match_by_year(ccyear=theyear)

##drop empty first row of each dataset
dat_matches <<- subset(dat_matches,is.na(id)==FALSE)
dat_matchn <<- subset(dat_matchn,is.na(hhd_id)==FALSE)

##print number of matches, number without 1 matches
print(nrow(subset(dat_matchn,nmatch<1)))
print(nrow(dat_matchn))

##save temporary dataset
write.csv(dat_matches,paste('./Datasets/match_interim_step_',theyear,'.csv',sep=''))
write.csv(dat_matchn,paste('./Datasets/match_interim_step2_',theyear,'.csv',sep=''))
}