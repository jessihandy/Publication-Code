#################
## 
## Program to find matches and temporarily save a matched file for all years
## Primary functions of program:
##   - First runs hhdivest_matching_set_up.R, which has the matching functions and initial data cleaning
##   - Then runs hhd_match_YYYY.R where YYYY are years 2004-2021
##   - When all years are run, can run subsequent code to combine information and save a final cohort
##
#################

##run data set up code (also has the matching functions)
source('./Programs/MatchedCohort2/hhdivest_matching_set_up.R',echo=TRUE)

##runs each year, gc() between
source('./Programs/MatchedCohort2/YearMatch/hhd_match_2004.R',echo=TRUE)
gc()
source('./Programs/MatchedCohort2/YearMatch/hhd_match_2005.R',echo=TRUE)
gc()
source('./Programs/MatchedCohort2/YearMatch/hhd_match_2006.R',echo=TRUE)
gc()
source('./Programs/MatchedCohort2/YearMatch/hhd_match_2007.R',echo=TRUE)
gc()
source('./Programs/MatchedCohort2/YearMatch/hhd_match_2008.R',echo=TRUE)
gc()
source('./Programs/MatchedCohort2/YearMatch/hhd_match_2009.R',echo=TRUE)
gc()
