#***************************************************************
#' HH Divestment
#' Author: Jessica Handy
#' Created: 30 MAR 2026
#' Last modified: 20 AUG 2026
#' 
#' Purpose: Boot straps 5-yr point estimates from matched survival analysis
#' 
#***************************************************************

# Admin ------
## Load packages
options(repos = c(CRAN = "http://cran.us.r-project.org"))
pacman::p_load(
  data.table,
  survival, 
  scales,
  ggplot2,
  flexsurv,
  boot
)


## Read in data
dat <- fread("./Datasets/full_match1_2.csv")
dat$divestment <- factor(dat$divestment)

# Bootstrapped CIs -----
### Using bootstrap functions from MatchIt library for robust SEs
## see: https://kosukeimai.github.io/MatchIt/articles/estimating-effects.html#using-bootstrapping-to-estimate-confidence-intervals
## "The cluster bootstrap"

## Note for bootstrapping, only need 5-year point estimates
# i.e., not constructing full curves

#Unique pair IDs
pair_ids <- levels(factor(dat$cluster))
#Unit IDs, split by pair membership
split_inds <- split(seq_len(nrow(dat)), dat$cluster)

ncpus <- parallel::detectCores() - 1 # for multicore runs

## All suicide ----
cluster_boot_suic <- function(pairs, i) {

  #Extract units corresponding to selected pairs
  ids <- unlist(split_inds[pairs[i]])

  #Subset dat with block bootstrapped indices
  boot_dat <- dat[ids,]

  sui <- flexsurvreg(Surv(obstime, ever_suic) ~ divestment, weights = w, 
                     data = boot_dat, dist = "lnorm")
  p.dat <- summary(sui, type = "survival", tidy = TRUE, t = c(1826, 1827), ci = FALSE)
  p.dat$est2 <- 1-p.dat$est # CI
  p.dat$ci10k <- p.dat$est2*10000 # CI per 10k pop
  p.dat$time.mo <- p.dat$time/30.4 # CI per 10k pop

  div.inc <- mean(p.dat[p.dat$divestment==1&p.dat$time %in% 1826:1827,]$ci10k)
  nodiv.inc <- mean(p.dat[p.dat$divestment==0&p.dat$time %in% 1826:1827,]$ci10k)
  rd <- div.inc-nodiv.inc
  rr <- div.inc/nodiv.inc

  return(c(div.inc, nodiv.inc, rd, rr))

}

set.seed(54321)
cluster_boot_out <- boot(pair_ids, cluster_boot_suic, R=500, parallel = "multicore", ncpus = ncpus)

cluster_boot_out
# Check how many iterations failed
failed_rows <- rowSums(is.na(cluster_boot_out$t))
num_failed <- sum(failed_rows > 0)
num_successful <- 500 - num_failed

print(paste("Total iterations:", 500))
print(paste("Successful iterations:", num_successful))
print(paste("Failed iterations:", num_failed))

# Remove failed rows before computing CIs
cluster_boot_out$t <- na.omit(cluster_boot_out$t)
cluster_boot_out$R <- nrow(cluster_boot_out$t)

print("All suicide")
#boot.ci(cluster_boot_out, type = "perc")
lapply(1:4, function(i) boot.ci(cluster_boot_out, index = i, type = "perc"))

## Firearm suicide ----
cluster_boot_suic <- function(pairs, i) {
tryCatch({
  #Extract units corresponding to selected pairs
  ids <- unlist(split_inds[pairs[i]])
  
  #Subset dat with block bootstrapped indices
  boot_dat <- dat[ids,]
  
  sui <- flexsurvreg(Surv(obstime, ever_fmsuic) ~ divestment, weights = w,
                     data = boot_dat, dist = "lnorm")
  p.dat <- summary(sui, type = "survival", tidy = TRUE, t = c(1826, 1827), ci = FALSE)
  p.dat$est2 <- 1-p.dat$est # CI
  p.dat$ci10k <- p.dat$est2*10000 # CI per 10k pop
  p.dat$time.mo <- p.dat$time/30.4 # CI per 10k pop
  
  div.inc <- mean(p.dat[p.dat$divestment==1&p.dat$time %in% 1826:1827,]$ci10k)
  nodiv.inc <- mean(p.dat[p.dat$divestment==0&p.dat$time %in% 1826:1827,]$ci10k)
  rd <- div.inc-nodiv.inc
  rr <- div.inc/nodiv.inc
  
  return(c(div.inc, nodiv.inc, rd, rr))
} , error = function(e) {
  return(c(NA, NA, NA, NA)) } ) }

set.seed(56309)
cluster_boot_out <- boot(pair_ids, cluster_boot_suic, R=500, parallel = "multicore", ncpus = ncpus)

cluster_boot_out
# Check how many iterations failed
failed_rows <- rowSums(is.na(cluster_boot_out$t))
num_failed <- sum(failed_rows > 0)
num_successful <- 500 - num_failed

print(paste("Total iterations:", 500))
print(paste("Successful iterations:", num_successful))
print(paste("Failed iterations:", num_failed))

# Remove failed rows before computing CIs
cluster_boot_out$t <- na.omit(cluster_boot_out$t)
cluster_boot_out$R <- nrow(cluster_boot_out$t)

rowSums(is.na(cluster_boot_out$t))
sum(failed_rows > 0)
500 - num_failed

print("Firearm suicide")
#boot.ci(cluster_boot_out, type = "perc")
lapply(1:4, function(i) boot.ci(cluster_boot_out, index = i, type = "perc"))
gc()

## Nonfirearm suicide ----
cluster_boot_suic <- function(pairs, i) {
tryCatch({  
  #Extract units corresponding to selected pairs
  ids <- unlist(split_inds[pairs[i]])
  
  #Subset dat with block bootstrapped indices
  boot_dat <- dat[ids,]
  
  sui <- flexsurvreg(Surv(obstime, ever_nfmsuic) ~ divestment, weights = w,
                     data = boot_dat, dist = "lnorm")
  p.dat <- summary(sui, type = "survival", tidy = TRUE, t = c(1826, 1827), ci = FALSE)
  p.dat$est2 <- 1-p.dat$est # CI
  p.dat$ci10k <- p.dat$est2*10000 # CI per 10k pop
  p.dat$time.mo <- p.dat$time/30.4 # CI per 10k pop
  
  div.inc <- mean(p.dat[p.dat$divestment==1&p.dat$time %in% 1826:1827,]$ci10k)
  nodiv.inc <- mean(p.dat[p.dat$divestment==0&p.dat$time %in% 1826:1827,]$ci10k)
  rd <- div.inc-nodiv.inc
  rr <- div.inc/nodiv.inc
  
  return(c(div.inc, nodiv.inc, rd, rr))
} , error = function(e) {
  return(c(NA, NA, NA, NA)) } ) }

set.seed(67890) # set.seed(11111)
cluster_boot_out <- boot(pair_ids, cluster_boot_suic, R=500, parallel = "multicore", ncpus = ncpus)

cluster_boot_out
# Check how many iterations failed
failed_rows <- rowSums(is.na(cluster_boot_out$t))
num_failed <- sum(failed_rows > 0)
num_successful <- 500 - num_failed

print(paste("Total iterations:", 500))
print(paste("Successful iterations:", num_successful))
print(paste("Failed iterations:", num_failed))

# Remove failed rows before computing CIs
cluster_boot_out$t <- na.omit(cluster_boot_out$t)
cluster_boot_out$R <- nrow(cluster_boot_out$t)

print("Non-firearm suicide")
#boot.ci(cluster_boot_out, type = "perc")
lapply(1:4, function(i) boot.ci(cluster_boot_out, index = i, type = "perc"))
gc()

# Negative Control Outcomes ------
# relevant COD codes
accident_icdcodes <- paste0("V", sprintf("%02d", 1:99))
liver_icdcodes <- c('K70')
lung_icdcodes <- c('C34')
mi_icdcodes <- c('I21','I22') # note 'I23' = complications arising from MI
od_icdcodes <- paste0("X", sprintf("%02d", 40:44))

dat$ever_accident <- ifelse(is.element(substr(dat$ever_causedeath,1,3),accident_icdcodes),1,0)
dat$ever_liver <- ifelse(is.element(substr(dat$ever_causedeath,1,3),liver_icdcodes),1,0)
dat$ever_lung <- ifelse(is.element(substr(dat$ever_causedeath,1,3),lung_icdcodes),1,0)
dat$ever_mi <- ifelse(is.element(substr(dat$ever_causedeath,1,3),mi_icdcodes),1,0)
dat$ever_od <- ifelse(is.element(substr(dat$ever_causedeath,1,3),od_icdcodes),1,0)

## All-cause mortality -----
cluster_boot_nc <- function(pairs, i) {
tryCatch({  
  #Extract units corresponding to selected pairs
  ids <- unlist(split_inds[pairs[i]])
  
  #Subset dat with block bootstrapped indices
  boot_dat <- dat[ids,]
  
  cod <- flexsurvreg(Surv(obstime, ever_die) ~ divestment, weights = w,
                     data = boot_dat, dist = "lnorm") 
  p.dat <- summary(cod, type = "survival", tidy = TRUE, t = c(1826, 1827), ci = FALSE)
  p.dat$est2 <- 1-p.dat$est # CI
  p.dat$ci10k <- p.dat$est2*10000 # CI per 10k pop
  p.dat$time.mo <- p.dat$time/30.4 # CI per 10k pop
  
  div.inc <- mean(p.dat[p.dat$divestment==1&p.dat$time %in% 1826:1827,]$ci10k)
  nodiv.inc <- mean(p.dat[p.dat$divestment==0&p.dat$time %in% 1826:1827,]$ci10k)
  rd <- div.inc-nodiv.inc
  rr <- div.inc/nodiv.inc
  
  return(c(div.inc, nodiv.inc, rd, rr))
} , error = function(e) {
  return(c(NA, NA, NA, NA)) } ) }

set.seed(130426)
cluster_boot_out <- boot(pair_ids, cluster_boot_nc, R=500, parallel = "multicore", ncpus = ncpus)

cluster_boot_out
# Check how many iterations failed
failed_rows <- rowSums(is.na(cluster_boot_out$t))
num_failed <- sum(failed_rows > 0)
num_successful <- 500 - num_failed

print(paste("Total iterations:", 500))
print(paste("Successful iterations:", num_successful))
print(paste("Failed iterations:", num_failed))

# Remove failed rows before computing CIs
cluster_boot_out$t <- na.omit(cluster_boot_out$t)
cluster_boot_out$R <- nrow(cluster_boot_out$t)

print("All cause mort")
#boot.ci(cluster_boot_out, type = "perc")
lapply(1:4, function(i) boot.ci(cluster_boot_out, index = i, type = "perc"))
gc()


## Liver -----
cluster_boot_nc <- function(pairs, i) {
  tryCatch({    
  #Extract units corresponding to selected pairs
  ids <- unlist(split_inds[pairs[i]])
  
  #Subset dat with block bootstrapped indices
  boot_dat <- dat[ids,]
  
  cod <- flexsurvreg(Surv(obstime, ever_liver) ~ divestment, weights = w,
                     data = boot_dat, dist = "lnorm")
  p.dat <- summary(cod, type = "survival", tidy = TRUE, t = c(1826, 1827), ci = FALSE)
  p.dat$est2 <- 1-p.dat$est # CI
  p.dat$ci10k <- p.dat$est2*10000 # CI per 10k pop
  p.dat$time.mo <- p.dat$time/30.4 # CI per 10k pop
  
  div.inc <- mean(p.dat[p.dat$divestment==1&p.dat$time %in% 1826:1827,]$ci10k)
  nodiv.inc <- mean(p.dat[p.dat$divestment==0&p.dat$time %in% 1826:1827,]$ci10k)
  rd <- div.inc-nodiv.inc
  rr <- div.inc/nodiv.inc
  
  return(c(div.inc, nodiv.inc, rd, rr))
  } , error = function(e) {
    return(c(NA, NA, NA, NA)) } ) }

set.seed(21356)
cluster_boot_out <- boot(pair_ids, cluster_boot_nc, R=500, parallel = "multicore", ncpus = ncpus)

cluster_boot_out
# Check how many iterations failed
failed_rows <- rowSums(is.na(cluster_boot_out$t))
num_failed <- sum(failed_rows > 0)
num_successful <- 500 - num_failed

print(paste("Total iterations:", 500))
print(paste("Successful iterations:", num_successful))
print(paste("Failed iterations:", num_failed))

# Remove failed rows before computing CIs
cluster_boot_out$t <- na.omit(cluster_boot_out$t)
cluster_boot_out$R <- nrow(cluster_boot_out$t)

print("Liver disease")
#boot.ci(cluster_boot_out, type = "perc")
lapply(1:4, function(i) boot.ci(cluster_boot_out, index = i, type = "perc"))
gc()

## Lung -----
cluster_boot_nc <- function(pairs, i) {
  tryCatch({    
  #Extract units corresponding to selected pairs
  ids <- unlist(split_inds[pairs[i]])
  
  #Subset dat with block bootstrapped indices
  boot_dat <- dat[ids,]
  
  cod <- flexsurvreg(Surv(obstime, ever_lung) ~ divestment, weights = w,
                     data = boot_dat, dist = "lnorm")
  p.dat <- summary(cod, type = "survival", tidy = TRUE, t = c(1826, 1827), ci = FALSE)
  p.dat$est2 <- 1-p.dat$est # CI
  p.dat$ci10k <- p.dat$est2*10000 # CI per 10k pop
  p.dat$time.mo <- p.dat$time/30.4 # CI per 10k pop
  
  div.inc <- mean(p.dat[p.dat$divestment==1&p.dat$time %in% 1826:1827,]$ci10k)
  nodiv.inc <- mean(p.dat[p.dat$divestment==0&p.dat$time %in% 1826:1827,]$ci10k)
  rd <- div.inc-nodiv.inc
  rr <- div.inc/nodiv.inc
  
  return(c(div.inc, nodiv.inc, rd, rr))
  } , error = function(e) {
    return(c(NA, NA, NA, NA)) } ) }

set.seed(12406)
cluster_boot_out <- boot(pair_ids, cluster_boot_nc, R=500, parallel = "multicore", ncpus = ncpus)

cluster_boot_out
# Check how many iterations failed
failed_rows <- rowSums(is.na(cluster_boot_out$t))
num_failed <- sum(failed_rows > 0)
num_successful <- 500 - num_failed

print(paste("Total iterations:", 500))
print(paste("Successful iterations:", num_successful))
print(paste("Failed iterations:", num_failed))

# Remove failed rows before computing CIs
cluster_boot_out$t <- na.omit(cluster_boot_out$t)
cluster_boot_out$R <- nrow(cluster_boot_out$t)

print("Lung")
#boot.ci(cluster_boot_out, type = "perc")
lapply(1:4, function(i) boot.ci(cluster_boot_out, index = i, type = "perc"))
gc()

## MI -----
cluster_boot_nc <- function(pairs, i) {
  tryCatch({    
  #Extract units corresponding to selected pairs
  ids <- unlist(split_inds[pairs[i]])
  
  #Subset dat with block bootstrapped indices
  boot_dat <- dat[ids,]
  
  cod <- flexsurvreg(Surv(obstime, ever_mi) ~ divestment, weights = w,
                     data = boot_dat, dist = "lnorm")
  p.dat <- summary(cod, type = "survival", tidy = TRUE, t = c(1826, 1827), ci = FALSE)
  p.dat$est2 <- 1-p.dat$est # CI
  p.dat$ci10k <- p.dat$est2*10000 # CI per 10k pop
  p.dat$time.mo <- p.dat$time/30.4 # CI per 10k pop
  
  div.inc <- mean(p.dat[p.dat$divestment==1&p.dat$time %in% 1826:1827,]$ci10k)
  nodiv.inc <- mean(p.dat[p.dat$divestment==0&p.dat$time %in% 1826:1827,]$ci10k)
  rd <- div.inc-nodiv.inc
  rr <- div.inc/nodiv.inc
  
  return(c(div.inc, nodiv.inc, rd, rr))
  
  } , error = function(e) {
    return(c(NA, NA, NA, NA)) } ) }

set.seed(12406)
cluster_boot_out <- boot(pair_ids, cluster_boot_nc, R=500, parallel = "multicore", ncpus = ncpus)

cluster_boot_out
# Check how many iterations failed
failed_rows <- rowSums(is.na(cluster_boot_out$t))
num_failed <- sum(failed_rows > 0)
num_successful <- 500 - num_failed

print(paste("Total iterations:", 500))
print(paste("Successful iterations:", num_successful))
print(paste("Failed iterations:", num_failed))

# Remove failed rows before computing CIs
cluster_boot_out$t <- na.omit(cluster_boot_out$t)
cluster_boot_out$R <- nrow(cluster_boot_out$t)

print("MI")
#boot.ci(cluster_boot_out, type = "perc")
lapply(1:4, function(i) boot.ci(cluster_boot_out, index = i, type = "perc"))
gc()

## OD -----
cluster_boot_nc <- function(pairs, i) {
  tryCatch({  
  #Extract units corresponding to selected pairs
  ids <- unlist(split_inds[pairs[i]])
  
  #Subset dat with block bootstrapped indices
  boot_dat <- dat[ids,]
  
  cod <- flexsurvreg(Surv(obstime, ever_od) ~ divestment, weights = w,
                     data = boot_dat, dist = "lnorm")
  p.dat <- summary(cod, type = "survival", tidy=T, t = c(1826,1827))
  p.dat$est2 <- 1-p.dat$est # CI
  p.dat$ci10k <- p.dat$est2*10000 # CI per 10k pop
  p.dat$time.mo <- p.dat$time/30.4 # CI per 10k pop
  
  div.inc <- mean(p.dat[p.dat$divestment==1&p.dat$time %in% 1826:1827,]$ci10k)
  nodiv.inc <- mean(p.dat[p.dat$divestment==0&p.dat$time %in% 1826:1827,]$ci10k)
  rd <- div.inc-nodiv.inc
  rr <- div.inc/nodiv.inc
  
  return(c(div.inc, nodiv.inc, rd, rr))
  } , error = function(e) {
    return(c(NA, NA, NA, NA)) } ) }

set.seed(35969)
cluster_boot_out <- boot(pair_ids, cluster_boot_nc, R=500, parallel = "multicore", ncpus = ncpus)

cluster_boot_out
# Check how many iterations failed
failed_rows <- rowSums(is.na(cluster_boot_out$t))
num_failed <- sum(failed_rows > 0)
num_successful <- 500 - num_failed

print(paste("Total iterations:", 500))
print(paste("Successful iterations:", num_successful))
print(paste("Failed iterations:", num_failed))

# Remove failed rows before computing CIs
cluster_boot_out$t <- na.omit(cluster_boot_out$t)
cluster_boot_out$R <- nrow(cluster_boot_out$t)

cluster_boot_out
print("OD")
#boot.ci(cluster_boot_out, type = "perc")
lapply(1:4, function(i) boot.ci(cluster_boot_out, index = i, type = "perc"))
gc()


