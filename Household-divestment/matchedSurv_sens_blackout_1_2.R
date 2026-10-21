#***************************************************************
#' HH Divestment
#' Author: Jessica Handy
#' Created: 1 SEP 2026
#' Last modified: 1 SEP 2026
#' 
#' Purpose: Repeat analysis on sample excluding blackouts
#' 
#***************************************************************

# Admin ------
## Load packages
pacman::p_load(
  dplyr,
  data.table,
  survival, 
  flexsurv,
  ggplot2, patchwork, cowplot, # plotting
  boot
)


## Read in data ----
dat <- fread("./Datasets/full_match1_2.csv")
x.ids <- read.csv("../Data/id_blackout_address.csv")
x.ids <- rename(x.ids, id = x)

### Flag blackout addresses  ----
# merge, keep only flagged divester cohabs
y <- merge(dat, x.ids) 
table(y$divestment) # flags in both groups
y <- y[y$divestment == 1,] # filter
y <- y[, "cluster"] # need cluster id to flag div+match in original data
z <- merge(dat, y)
length(unique(z$id)) # QC - should be approx equal to divesters x3 (1:2 match)
# but, some matches repeat; full length (non-unique ==)
length(unique(z$cluster)) # QC - should be equal to flagged divesters
z <- z[, "cluster"]

dat <- anti_join(dat, z)
length(unique(dat$id))
length(unique(dat[divestment == 1,]$id))

# Standardized survival curves ----
## All suicide -----
dat$divestment <- factor(dat$divestment)
sui <- flexsurvreg(Surv(obstime, ever_suic) ~ divestment, weights = w, 
                   data = dat, dist = "lnorm")
p.sui <- summary(sui, type = "survival", tidy=T)
p.sui$est2 <- 1-p.sui$est # CI
p.sui$ci10k <- p.sui$est2*10000 # CI per 10k pop
p.sui$time.mo <- p.sui$time/30.4 # CI per 10k pop


sui.plot <- ggplot() +
  geom_line(aes(x = time.mo, y = ci10k, 
                color = divestment, linetype = factor(divestment)), data=p.sui, linewidth = 0.5) +
  #geom_step(aes(x = time.mo, y = ci, color=divestment, linetype=factor(divestment)), data=cox.plot.data) +
  scale_linetype_manual(
    values = c("0" = "dotdash", "1" = "solid"),
    labels = c("0" = "No Divestment", "1" = "Divestment")
  ) +
  scale_color_manual(values = c("0" = "black","1" = "black"), 
                     labels = c("0" = "No Divestment", "1" = "Divestment")) +
  scale_x_continuous(expand = c(0, 0), limits = c(0,60)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0,8), 
                     labels = scales::label_number(accuracy = 1)) +
  labs(title = "Any Suicide Death", 
       x = "Months", 
       y = "Deaths per 10,000",
       color = "",
       linetype = "") +
  theme(panel.background = element_rect(fill = "white"), 
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.25), 
        plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 8),
        axis.text = element_text(size = 8), 
        legend.text = element_text(size = 8),
        legend.position='inside', 
        legend.position.inside = c(0.01, 0.985),
        legend.justification = c(0, 1),
        legend.margin = margin(-5, 5, 3, 3),
        legend.background = element_rect(color = "black", linewidth = 0.2, linetype = "solid"),
        axis.text.y = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
        text = element_text(family = "Arial"))


div.inc <- mean(p.sui[p.sui$divestment==1&p.sui$time %in% 1826:1827,]$ci10k)
nodiv.inc <- mean(p.sui[p.sui$divestment==0&p.sui$time %in% 1826:1827,]$ci10k)
rd <- div.inc-nodiv.inc
rr <- div.inc/nodiv.inc

df <- data.frame(d = "Suicide", div.inc, nodiv.inc, rd, rr)

## FM suicide -----
fmsui <- flexsurvreg(Surv(obstime, ever_fmsuic) ~ divestment, weights = w,
                     data = dat, dist = "lnorm")

p.fmsui <- summary(fmsui, type = "survival", tidy=T)
p.fmsui$est2 <- 1-p.fmsui$est # CI
p.fmsui$ci10k <- p.fmsui$est2*10000 # CI per 10k pop
p.fmsui$time.mo <- p.fmsui$time/30.4 # CI per 10k pop

div.inc <- mean(p.fmsui[p.fmsui$divestment==1&p.fmsui$time %in% 1826:1827,]$ci10k)
nodiv.inc <- mean(p.fmsui[p.fmsui$divestment==0&p.fmsui$time %in% 1826:1827,]$ci10k)
rd <- div.inc-nodiv.inc
rr <- div.inc/nodiv.inc

df2 <- data.frame(d = "Firearm Suicide", div.inc, nodiv.inc, rd, rr)
df <- rbind(df,df2)

fmsui.plot <- ggplot() +
  geom_line(aes(x = time.mo, y = ci10k, 
                color = divestment, linetype = divestment), data=p.fmsui, linewidth = 0.5) +
  scale_linetype_manual(
    values = c("0" = "dotdash", "1" = "solid"),
    labels = c("0" = "No Divestment", "1" = "Divestment")
  ) +
  scale_color_manual(values = c("0" = "black","1" = "black"), 
                     labels = c("0" = "No Divestment", "1" = "Divestment")) +
  scale_x_continuous(expand = c(0, 0), limits = c(0,60)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0,8), 
                     labels = scales::label_number(accuracy = 1)) +
  labs(title = "Fireram Suicide Death", 
       x = "Months", 
       y = "Deaths per 10,000",
       color = "",
       linetype = "") +
  theme(panel.background = element_rect(fill = "white"), 
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.25), 
        plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 8),
        axis.text = element_text(size = 8), 
        legend.text = element_text(size = 8),
        legend.position='inside', 
        legend.position.inside = c(0.01, 0.985),
        legend.justification = c(0, 1),
        legend.margin = margin(-5, 5, 3, 3),
        legend.background = element_rect(color = "black", linewidth = 0.2, linetype = "solid"),
        axis.text.y = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
        text = element_text(family = "Arial"))

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

