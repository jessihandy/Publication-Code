
##this is now 2-1 matching



##source the original code

source('./Programs/MatchedCohort2/hhdivest_matching_set_up.R',echo=TRUE)

##bringing altogether the saved interim datasets so that next steps can proceed
dat_matches <- rbindlist(lapply(c(
    './Datasets/match_interim_step_2004.csv','./Datasets/match_interim_step_2005.csv',
    './Datasets/match_interim_step_2006.csv','./Datasets/match_interim_step_2007.csv',
    './Datasets/match_interim_step_2008.csv','./Datasets/match_interim_step_2009.csv',
    './Datasets/match_interim_step_2010.csv','./Datasets/match_interim_step_2011.csv',
    './Datasets/match_interim_step_2012.csv','./Datasets/match_interim_step_2013.csv',
    './Datasets/match_interim_step_2014.csv','./Datasets/match_interim_step_2015.csv',
    './Datasets/match_interim_step_2016.csv','./Datasets/match_interim_step_2017.csv',
    './Datasets/match_interim_step_2018.csv','./Datasets/match_interim_step_2019.csv',
    './Datasets/match_interim_step_2020.csv','./Datasets/match_interim_step_2021.csv'
    ), fread),use.names=TRUE)

##bringing altogether the saved interim datasets so that next steps can proceed
dat_matchn <- rbindlist(lapply(c(
    './Datasets/match_interim_step2_2004.csv','./Datasets/match_interim_step2_2005.csv',
    './Datasets/match_interim_step2_2006.csv','./Datasets/match_interim_step2_2007.csv',
    './Datasets/match_interim_step2_2008.csv','./Datasets/match_interim_step2_2009.csv',
    './Datasets/match_interim_step2_2010.csv','./Datasets/match_interim_step2_2011.csv',
    './Datasets/match_interim_step2_2012.csv','./Datasets/match_interim_step2_2013.csv',
    './Datasets/match_interim_step2_2014.csv','./Datasets/match_interim_step2_2015.csv',
    './Datasets/match_interim_step2_2016.csv','./Datasets/match_interim_step2_2017.csv',
    './Datasets/match_interim_step2_2018.csv','./Datasets/match_interim_step2_2019.csv',
    './Datasets/match_interim_step2_2020.csv','./Datasets/match_interim_step2_2021.csv'
    ), fread),use.names=TRUE)

##drop any phantom/placeholder rows carried over into dat_matches
dat_matches <- subset(dat_matches, is.na(id)==FALSE)
dat_matches <- subset(dat_matches, select=-V1)

##tag round-1 matches as tract-level (round 1's original, tightest caliper)
dat_matches$match_level <- 'tract'

##account for every divester
nmatch_gt <- as.data.frame(table(dat_matches$hhd_id), stringsAsFactors=FALSE)
colnames(nmatch_gt) <- c('hhd_id','nmatch_gt')
nmatch_gt$hhd_id <- as.numeric(as.character(nmatch_gt$hhd_id))
dat_matchn <- data.frame(hhd_id = dat_fhhd$id)
dat_matchn <- merge(dat_matchn, nmatch_gt, by='hhd_id', all.x=TRUE)
dat_matchn$nmatch_gt[is.na(dat_matchn$nmatch_gt)] <- 0
dat_matchn$nmatch <- dat_matchn$nmatch_gt
dat_matchn$nmatch_gt <- NULL

##QC: are these the same?
cat('number of exposed households in dat_fhhd:', nrow(dat_fhhd), '\n')
cat('number of unique exposed households in rebuilt dat_matchn:', nrow(dat_matchn), '\n')

##check duplicate ids
cat('dat_matchn has one row per hhd_id:', !any(duplicated(dat_matchn$hhd_id)), '\n')
cat('dat_fhhd has one row per id:', !any(duplicated(dat_fhhd$id)), '\n')
if(any(duplicated(dat_fhhd$id))) {
  print(dat_fhhd[dat_fhhd$id %in% dat_fhhd$id[duplicated(dat_fhhd$id)],])
}

##how many need more matches?
nrow(subset(dat_matchn,nmatch<2 & hhd_id!=0))
nrow(dat_matches)
nrow(dat_fhhd)
nrow(dat_fhhd)-nrow(dat_matches)
dat_needmatches <- subset(dat_matchn,nmatch<2 & hhd_id!=0)
nrow(dat_needmatches)


##set seed for reproducibility 
set.seed(124421)

##empty 'bank' of possible matches, round 1b (tract top-up)
dat_matches1b <- matrix(ncol=17,nrow=1)
dat_matches1b <- as.data.frame(dat_matches1b)
colnames(dat_matches1b) <- c('id','hhd_id','fhhdobs_date','deathdate','ever_die','endobs_date',
                           'ageyr_t0','sex','race','ever_causedeath','dedup_hhid_size',
                           'tract','county_census',
                           'hguns','lguns','hh_hguns','hh_lguns'
                           )

for(k in 0:21) {

dat_needmatches <- subset(dat_matchn,nmatch<2)
dat_fhhd2 <- subset(dat_fhhd, id %in% dat_needmatches$hhd_id)
dat_fhhd2 <- merge(dat_fhhd2, dat_needmatches[,c('hhd_id','nmatch')],
                    by.x='id', by.y='hhd_id', all.x=TRUE)

if(nrow(dat_fhhd2)!=length(unique(dat_fhhd2$id))) {
  stop('dat_fhhd2 has duplicate id rows after merge in round 1b, k=',k,
       ' - check dat_matchn/dat_needmatches for duplicate hhd_id values before proceeding')
}

dat_fhhd2 <- subset(dat_fhhd2,yrbirth_ind5==k)
if(nrow(dat_fhhd2)>0) {
dat_matchable2 <- dat[hh_hguns>0
                      & hguns_hx==0
                      & hhsize234==1 & hhsize==2 & yrbirth_ind5==k]

for(i in 1:nrow(dat_fhhd2)) {
  tempsex <- dat_fhhd2[i,]$sex
  tempyrbirth_ind5 <- dat_fhhd2[i,]$yrbirth_ind5
  temptract <- dat_fhhd2[i,]$tract
  tempid <- dat_fhhd2[i,]$id
  temprace <- dat_fhhd2[i,]$race
  tempfhhdobs_date <- dat_fhhd2[i,]$fhhdobs_date
  tempnmatch <- dat_fhhd2[i,]$nmatch

  ##SAME caliper as round 1: exact tract match (not county-widened)
  temp <- subset(dat_matchable2,yrbirth_ind5==tempyrbirth_ind5 & tract==temptract & race==temprace & sex==tempsex)
  temp <- subset(temp,as.Date(temp$date0,'%d%b%Y')<=as.Date(tempfhhdobs_date,'%d%b%Y'))
  temp <- subset(temp,as.Date(temp$date1,'%d%b%Y')>as.Date(tempfhhdobs_date,'%d%b%Y'))
  temp <- subset(temp,is.na(temp$deathdate)==TRUE | difftime(as.Date(temp$deathdate,'%d%b%Y'),as.Date(tempfhhdobs_date,'%d%b%Y'),units='days')>=0)
  temp <- subset(temp,as.numeric(round(difftime(as.Date(tempfhhdobs_date,'%d%b%Y'),
                                         as.Date(temp$DATE_OF_BIRTH),
                                        units='days')/365.25,0))>=21)
  temp <- subset(temp,as.numeric(round(difftime(as.Date(tempfhhdobs_date,'%d%b%Y'),
                                         as.Date(temp$DATE_OF_BIRTH),
                                        units='days')/365.25,0))<100)

  ##exclude anyone already matched to THIS household in round 1
  already_matched_ids <- dat_matches$id[dat_matches$hhd_id==tempid]
  temp <- subset(temp, !(id %in% already_matched_ids))
  temp <- temp[!duplicated(temp$id),]

  newmat <- matrix(ncol=ncol(dat_matches1b),nrow=nrow(temp))
  colnames(newmat) <- colnames(dat_matches1b)
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

  n_needed <- 2 - tempnmatch
  n_assigned <- min(nrow(temp),n_needed)
  if(n_assigned>0) {dat_matches1b <- rbind(dat_matches1b,newmat[sample(nrow(newmat),n_assigned),])}
  rm(newmat,tempsex,tempyrbirth_ind5,temptract,tempid,tempfhhdobs_date,tempnmatch,temp,n_needed,n_assigned)
}
}
print(k)
}

dat_matches1b <- subset(dat_matches1b,is.na(id)==FALSE)
nrow(dat_matches1b)

##fold tract-level top-up matches into dat_matches, tagged 'tract'
dat_matches1b$match_level <- 'tract'
dat_matches <- rbind(dat_matches,dat_matches1b,use.names=TRUE)

##recompute ground truth before moving on to the county round
nmatch_gt <- as.data.frame(table(dat_matches$hhd_id), stringsAsFactors=FALSE)
colnames(nmatch_gt) <- c('hhd_id','nmatch_gt')
nmatch_gt$hhd_id <- as.numeric(as.character(nmatch_gt$hhd_id))
dat_matchn <- data.frame(hhd_id = dat_fhhd$id)
dat_matchn <- merge(dat_matchn, nmatch_gt, by='hhd_id', all.x=TRUE)
dat_matchn$nmatch_gt[is.na(dat_matchn$nmatch_gt)] <- 0
dat_matchn$nmatch <- dat_matchn$nmatch_gt
dat_matchn$nmatch_gt <- NULL

cat('after tract top-up, number still needing more matches:', nrow(subset(dat_matchn,nmatch<2)), '\n')

## find matches for those STILL without 2 matches after the tract top-up
## widens the caliper: first replaces census tract with county, then all geo

##empty 'bank' of possible matches, round 2
dat_matches2 <- matrix(ncol=17,nrow=1)
dat_matches2 <- as.data.frame(dat_matches2)
colnames(dat_matches2) <- c('id','hhd_id','fhhdobs_date','deathdate','ever_die','endobs_date',
                           'ageyr_t0','sex','race','ever_causedeath','dedup_hhid_size',
                           'tract','county_census',
                           'hguns','lguns','hh_hguns','hh_lguns'
                           )

##empty dataset for number of matches, round 2

dat_matchn2 <- matrix(ncol=3,nrow=1)
dat_matchn2 <- as.data.frame(dat_matchn2)
colnames(dat_matchn2) <- c('hhd_id','nmatch2','nmatch_needed')

##loop over birth cohorts just to make matching more efficient for this iteration
for(k in 0:21) {

##restrict to those with insufficient matches
dat_needmatches <- subset(dat_matchn,nmatch<2)

##merge this somehow, restricting to those needing matches only
dat_fhhd2 <- subset(dat_fhhd, id %in% dat_needmatches$hhd_id)

##how many more matches needed?
dat_fhhd2 <- merge(dat_fhhd2, dat_needmatches[,c('hhd_id','nmatch')],
                    by.x='id', by.y='hhd_id', all.x=TRUE)


if(nrow(dat_fhhd2)!=length(unique(dat_fhhd2$id))) {
  stop('dat_fhhd2 has duplicate id rows after merge in round 2, k=',k,
       ' - check dat_matchn/dat_needmatches for duplicate hhd_id values before proceeding')
}

dat_fhhd2 <- subset(dat_fhhd2,yrbirth_ind5==k)
if(nrow(dat_fhhd2)>0) {
##returning to original person-period data to find who is eligible to match
dat_matchable2 <- dat[hh_hguns>0
                      & hguns_hx==0
                      & hhsize234==1 & hhsize==2 & yrbirth_ind5==k]

##loop within the relevant subset to find matches
##widened caliper relative to prior loop - county, not tract
for(i in 1:nrow(dat_fhhd2)) {
  tempsex <- dat_fhhd2[i,]$sex
  tempyrbirth_ind5 <- dat_fhhd2[i,]$yrbirth_ind5
  tempcounty <- dat_fhhd2[i,]$county_census
  tempid <- dat_fhhd2[i,]$id
  temprace <- dat_fhhd2[i,]$race
  tempfhhdobs_date <- dat_fhhd2[i,]$fhhdobs_date
  tempnmatch <- dat_fhhd2[i,]$nmatch
#  temp <- subset(dat_matchable2,race==temprace)
#  temp <- subset(dat_matchable2,sex==tempsex)
  temp <- subset(dat_matchable2,yrbirth_ind5==tempyrbirth_ind5 & county_census==tempcounty & race==temprace & sex==tempsex)
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
  ##drop those in the same tract (otherwise they would have matched earlier...)
  ##temptract <- dat_fhhd2[i,]$tract
  ##temp <- subset(temp,tract!=temptract)

  ##exclude any unexposed person already matched to THIS SPECIFIC hhld
  already_matched_ids <- dat_matches$id[dat_matches$hhd_id==tempid]
  temp <- subset(temp, !(id %in% already_matched_ids))

  temp <- temp[!duplicated(temp$id),]

  newmat <- matrix(ncol=ncol(dat_matches2),nrow=nrow(temp))
  colnames(newmat) <- colnames(dat_matches2)
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

n_needed <- 2 - tempnmatch

n_assigned <- min(nrow(temp),n_needed)

##save new matches info
if(n_assigned>0) {dat_matches2 <- rbind(dat_matches2,newmat[sample(nrow(newmat),n_assigned),])}
vect <- c(tempid,n_assigned,n_needed)
dat_matchn2 <- rbind(dat_matchn2,vect)
rm(newmat,tempsex,tempyrbirth_ind5,tempcounty,tempid,tempfhhdobs_date,tempnmatch,temp,n_needed,n_assigned)
}
}
print(k)
}

##inspect new matches
##drop empty first row of each dataset
dat_matches2 <- subset(dat_matches2,is.na(id)==FALSE)
dat_matchn2 <- subset(dat_matchn2,is.na(hhd_id)==FALSE)
nrow(dat_matches2)

##bring new matches into initial matches' dataset
dat_matches2$match_level <- 'county'
dat_matches <- rbind(dat_matches,dat_matches2,use.names=TRUE)

##more checks, should be matches per divester when all matching done
nrow(dat_matches)
nrow(dat_fhhd)

nmatch2_gt <- as.data.frame(table(dat_matches$hhd_id), stringsAsFactors=FALSE)
colnames(nmatch2_gt) <- c('hhd_id','nmatch2_total')
nmatch2_gt$hhd_id <- as.numeric(as.character(nmatch2_gt$hhd_id))
dat_matchn2 <- data.frame(hhd_id = dat_fhhd$id)
dat_matchn2 <- merge(dat_matchn2, nmatch2_gt, by='hhd_id', all.x=TRUE)
dat_matchn2$nmatch2_total[is.na(dat_matchn2$nmatch2_total)] <- 0

cat('dat_matchn2 (ground truth) has one row per hhd_id:', !any(duplicated(dat_matchn2$hhd_id)), '\n')
nrow(subset(dat_matchn2,nmatch2_total<2))
nrow(subset(dat_matchn2,nmatch2_total<2 & hhd_id!=0))

##empty 'bank' of possible matches, round 3
dat_matches3 <- matrix(ncol=17,nrow=1)
dat_matches3 <- as.data.frame(dat_matches3)
colnames(dat_matches3) <- c('id','hhd_id','fhhdobs_date','deathdate','ever_die','endobs_date',
                           'ageyr_t0','sex','race','ever_causedeath','dedup_hhid_size',
                           'tract','county_census',
                           'hguns','lguns','hh_hguns','hh_lguns'
                           )

dat_matchn3 <- matrix(ncol=3,nrow=1)
dat_matchn3 <- as.data.frame(dat_matchn3)
colnames(dat_matchn3) <- c('hhd_id','nmatch3','nmatch_needed')

##restrict to those with insufficient matches, using the corrected cumulative total
dat_needmatches <- subset(dat_matchn2,nmatch2_total<2)
##merge this somehow into fp_dat, restricting to those needing matches only
dat_fhhd2 <- subset(dat_fhhd, id %in% dat_needmatches$hhd_id)

dat_fhhd2 <- merge(dat_fhhd2, dat_needmatches[,c('hhd_id','nmatch2_total')],
                    by.x='id', by.y='hhd_id', all.x=TRUE)

if(nrow(dat_fhhd2)!=length(unique(dat_fhhd2$id))) {
  stop('dat_fhhd2 has duplicate id rows after merge in round 3',
       ' - check dat_matchn2/dat_needmatches for duplicate hhd_id values before proceeding')
}

##returning to original person-period data to find unexposed
dat_matchable2 <- dat[hh_hguns>0
                      & hguns_hx==0
                      & hhsize234==1 & hhsize==2]

##loop within the relevant subset to find matches
##widened caliper relative to prior loop - no reference to geography
for(i in 1:nrow(dat_fhhd2)) {
  tempsex <- dat_fhhd2[i,]$sex
  tempyrbirth_ind5 <- dat_fhhd2[i,]$yrbirth_ind5
  tempid <- dat_fhhd2[i,]$id
  temprace <- dat_fhhd2[i,]$race
  tempcounty <- dat_fhhd2[i,]$county_census
  tempfhhdobs_date <- dat_fhhd2[i,]$fhhdobs_date
  tempnmatch2 <- dat_fhhd2[i,]$nmatch2_total
#  temp <- subset(dat_matchable2,race==temprace)
 # temp <- subset(dat_matchable2,sex==tempsex)
  temp <- subset(dat_matchable2,yrbirth_ind5==tempyrbirth_ind5 & race==temprace & sex==tempsex)
  temp <- subset(temp,as.Date(temp$date0,'%d%b%Y')<=as.Date(tempfhhdobs_date,'%d%b%Y'))
  temp <- subset(temp,as.Date(temp$date1,'%d%b%Y')>=as.Date(tempfhhdobs_date,'%d%b%Y'))
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

  already_matched_ids <- dat_matches$id[dat_matches$hhd_id==tempid]
  temp <- subset(temp, !(id %in% already_matched_ids))

  temp <- temp[!duplicated(temp$id),]

  newmat <- matrix(ncol=ncol(dat_matches3),nrow=nrow(temp))
  colnames(newmat) <- colnames(dat_matches3)
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

n_needed <- 2 - tempnmatch2

n_assigned <- min(nrow(temp),n_needed)

##save new matches info
if(n_assigned>0) {dat_matches3 <- rbind(dat_matches3,newmat[sample(nrow(newmat),n_assigned),])}
vect <- c(tempid,n_assigned,n_needed)
dat_matchn3 <- rbind(dat_matchn3,vect)
rm(newmat,tempsex,tempyrbirth_ind5,tempid,tempfhhdobs_date,tempnmatch2,temp,n_needed,n_assigned)
}

##drop empty first row
dat_matchn3 <- subset(dat_matchn3,is.na(hhd_id)==FALSE)

dat_matches3$match_level <- 'none'

dat_matches3 <- subset(dat_matches3, is.na(id)==FALSE)

##bring new matches into initial matches' dataset
dat_matches <- rbind(dat_matches,dat_matches3,use.names=TRUE)
dat_matches <- subset(dat_matches,is.na(hhd_id)==FALSE)
dat_matches <- subset(dat_matches,is.na(id)==FALSE)

##more checks, should be matches per purchaser when all matching done
nrow(dat_matches)
nrow(dat_fhhd)

nmatch3_gt <- as.data.frame(table(dat_matches$hhd_id), stringsAsFactors=FALSE)
colnames(nmatch3_gt) <- c('hhd_id','nmatch3_total')
nmatch3_gt$hhd_id <- as.numeric(as.character(nmatch3_gt$hhd_id))
dat_matchn3_gt <- data.frame(hhd_id = dat_fhhd$id)
dat_matchn3_gt <- merge(dat_matchn3_gt, nmatch3_gt, by='hhd_id', all.x=TRUE)
dat_matchn3_gt$nmatch3_total[is.na(dat_matchn3_gt$nmatch3_total)] <- 0

##restrict first purchasers to those with insufficient matches
dat_needmatches <- subset(dat_matchn3_gt,nmatch3_total<2 & hhd_id!=0)
##how many need more matches?
print('##how many need more matches?')
nrow(subset(dat_needmatches,nmatch3_total<2))

table(table(dat_matches$hhd_id))
nrow(dat_matches) == 2*nrow(dat_fhhd)

cat('any household with >2 matches:', any(table(dat_matches$hhd_id)>2), '\n')
dup_control_pairs <- dat_matches[duplicated(dat_matches[,c('id','hhd_id')]),]
cat('number of duplicate (control, household) pairs:', nrow(dup_control_pairs), '\n')

strict_hhd_counts <- table(dat_matches$hhd_id)
hhd_with_2 <- as.numeric(names(strict_hhd_counts)[strict_hhd_counts==2])

n_dropped_hhd <- nrow(dat_fhhd) - length(hhd_with_2)
cat('number of exposed households dropped for having fewer than 2 matches:', n_dropped_hhd, '\n')
cat('percent of exposed cohort dropped:', round(100*n_dropped_hhd/nrow(dat_fhhd),2), '\n')

dat_matches <- subset(dat_matches, hhd_id %in% hhd_with_2)
dat_fhhd <- subset(dat_fhhd, id %in% hhd_with_2)

##confirm the strict 1:2 ratio now holds exactly
table(table(dat_matches$hhd_id))
nrow(dat_matches) == 2*nrow(dat_fhhd)

dat_matches$match_level <- factor(dat_matches$match_level, levels=c('tract','county','none'))
match_level_n <- table(dat_matches$match_level)
match_level_pct <- round(100*prop.table(match_level_n),2)
match_level_summary <- data.frame(
  match_level = names(match_level_n),
  n = as.integer(match_level_n),
  pct = as.numeric(match_level_pct)
)
print(match_level_summary)

##same breakdown, but per exposed household rather than per match - i.e., of each
##household's (up to) 2 matches, how many were tract- vs. county- vs. no-geography-level.
##Useful for flagging households where NEITHER match met the tightest criterion.
hhd_level_summary <- aggregate(match_level ~ hhd_id, data=dat_matches,
                                FUN=function(x) paste(sort(as.character(x)),collapse=','))
table(hhd_level_summary$match_level)

##adding suicide outcomes
suicide_icdcodes <- c('X60','X61','X62','X63','X64','X65',
                      'X66','X67','X68','X69','X70','X71',
                      'X72','X73','X74','X75','X76','X77',
                      'X78','X79','X80','X81','X82','X83',
                      'X84')
fmsuicide_icdcodes <- c('X72','X73','X74')
nfmsuicide_icdcodes <- c('X60','X61','X62','X63','X64','X65',
                      'X66','X67','X68','X69','X70','X71',
                      'X75','X76','X77',
                      'X78','X79','X80','X81','X82','X83',
                      'X84')
dat_matches$ever_suic <- ifelse(is.element(substr(dat_matches$ever_causedeath,1,3),suicide_icdcodes),1,0)
dat_matches$ever_fmsuic <- ifelse(is.element(substr(dat_matches$ever_causedeath,1,3),fmsuicide_icdcodes),1,0)
dat_matches$ever_nfmsuic <- ifelse(is.element(substr(dat_matches$ever_causedeath,1,3),nfmsuicide_icdcodes),1,0)
dat_fhhd$ever_suic <- ifelse(is.element(substr(dat_fhhd$ever_causedeath,1,3),suicide_icdcodes),1,0)
dat_fhhd$ever_fmsuic <- ifelse(is.element(substr(dat_fhhd$ever_causedeath,1,3),fmsuicide_icdcodes),1,0)
dat_fhhd$ever_nfmsuic <- ifelse(is.element(substr(dat_fhhd$ever_causedeath,1,3),nfmsuicide_icdcodes),1,0)

summary(factor(dat_matches$ever_suic))
summary(factor(dat_matches$ever_fmsuic))
summary(factor(dat_matches$ever_nfmsuic))
summary(factor(dat_fhhd$ever_suic))
summary(factor(dat_fhhd$ever_fmsuic))
summary(factor(dat_fhhd$ever_nfmsuic))

##check naming of datasets
colnames(dat_matches)
colnames(dat_fhhd)
nrow(dat_matches)
nrow(dat_fhhd)

##saving datasets
fwrite(dat_matches,'./Datasets/hhdiv_matchedcomparators2.csv' )
fwrite(dat_fhhd,'./Datasets/hhdiv_divesters2.csv')





##@SS: need to debug above something that isn't finding a need for matches, but for now dropping the unmatched
temp <- dat_fhhd$id
temp2 <- dat_matches$hhd_id
thematchedones <- temp[temp %in% temp2]
dat_matches <- dat_matches[hhd_id %in% thematchedones]
dat_fhhd <- dat_fhhd[id %in% thematchedones]
nrow(dat_matches)
length(unique(dat_matches$hhd_id))
length(unique(dat_matches$id))
nrow(dat_fhhd)
length(unique(dat_fhhd$id))


temp <- dat_fhhd$id
temp2 <- dat_matches$hhd_id
uhoh <- subset(dat_fhhd,!(id %in% temp2))


