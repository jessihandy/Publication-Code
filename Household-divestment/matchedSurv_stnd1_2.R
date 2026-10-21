#***************************************************************
#' HH Divestment
#' Author: Jessica Handy
#' Created: 30 MAR 2026
#' Last modified: 20 AUG 2026
#' 
#' Purpose: Survival analysis 
#'    Standardized survival curves
#'    Extracts 5-yr risks
#'    Plots
#' 
#***************************************************************

# Admin ------
## Load packages
pacman::p_load(
  data.table,
  survival, 
  sandwich,
  cmprsk,
  flexsurv,
  ggplot2, patchwork, cowplot, # plotting
  boot
)


## Read in data
dat <- fread("./Datasets/full_match1_2.csv")

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


## Non FM suicide -----
nfmsui <- flexsurvreg(Surv(obstime, ever_nfmsuic) ~ divestment, weights = w,
                   data = dat, dist = "lnorm")
p.nfmsui <- summary(nfmsui, type = "survival", tidy=T)
p.nfmsui$est2 <- 1-p.nfmsui$est # CI
p.nfmsui$ci10k <- p.nfmsui$est2*10000 # CI per 10k pop
p.nfmsui$time.mo <- p.nfmsui$time/30.4 # CI per 10k pop
wplot.1 <- ggplot() + geom_line(aes(x=time.mo, y=ci10k, color = divestment), data=p.nfmsui)

div.inc <- mean(p.nfmsui[p.nfmsui$divestment==1&p.nfmsui$time %in% 1826:1827,]$ci10k)
nodiv.inc <- mean(p.nfmsui[p.nfmsui$divestment==0&p.nfmsui$time %in% 1826:1827,]$ci10k)
rd <- div.inc-nodiv.inc
rr <- div.inc/nodiv.inc

df2 <- data.frame(d = "Non-firearm Suicide", div.inc, nodiv.inc, rd, rr)
df <- rbind(df,df2)

nfmsui.plot <- ggplot() +
  geom_line(aes(x = time.mo, y = ci10k, 
                color = divestment, linetype = divestment), data=p.nfmsui, linewidth = 0.5) +
  scale_linetype_manual(
    values = c("0" = "dotdash", "1" = "solid"),
    labels = c("0" = "No Divestment", "1" = "Divestment")
  ) +
  scale_color_manual(values = c("0" = "black","1" = "black"), 
                     labels = c("0" = "No Divestment", "1" = "Divestment")) +
  scale_x_continuous(expand = c(0, 0), limits = c(0,60)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0,8), 
                     labels = scales::label_number(accuracy = 1)) +
  labs(title = "Nonfireram Suicide Death", 
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

#combo <- sui.plot + fmsui.plot + nfmsui.plot
#combo <- sui.plot + (fmsui.plot / nfmsui.plot) + plot_layout(ncol = 2)
combo <- sui.plot / (fmsui.plot + nfmsui.plot) + plot_layout(nrow = 2)
combo <- sui.plot + fmsui.plot + plot_layout(ncol = 2)

ggsave(
  "suifmsui_plots1_2.png",
  plot = combo,
  width = 17,
  height = 8.5,
  units = "cm",
  dpi = 300 # Recommended for high resolution
)

# Extract the legend from one plot
legend <- get_legend(sui.plot + theme(legend.justification = c(0, 2)))
sui.plot_nolegend <- sui.plot + theme(legend.position = "none")
fmsui.plot_nolegend <- fmsui.plot + theme(legend.position = "none")
nfmsui.plot_nolegend <- nfmsui.plot + theme(legend.position = "none")
combo <- (sui.plot_nolegend + legend + plot_layout(widths = c(4, 1))) / 
  (fmsui.plot_nolegend + nfmsui.plot_nolegend) +
  plot_layout(heights = c(1, 1))

ggsave(
  "suicide_plots21_2.png",
  plot = combo,
  width = 17,
  height = 17,
  units = "cm",
  dpi = 300 # Recommended for high resolution
)

ggsave(
  "suicide_plot_solo21_2.png",
  plot = sui.plot,
  width = 8.4,
  height = 8.4,
  units = "cm",
  dpi = 300 # Recommended for high resolution
)

ggsave(
  "fmsuicide_plot_solo1_2.png",
  plot = fmsui.plot,
  width = 8.4,
  height = 8.4,
  units = "cm",
  dpi = 300 # Recommended for high resolution
)

ggsave(
  "nfmsuicide_plot_solo1_2.png",
  plot = nfmsui.plot,
  width = 8.4,
  height = 8.4,
  units = "cm",
  dpi = 300 # Recommended for high resolution
)


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
die <- flexsurvreg(Surv(obstime, ever_die) ~ divestment, weights = w,
                   data = dat, dist = "lnorm")

p.die <- summary(die, type = "survival", tidy=T)
p.die$est2 <- 1-p.die$est # CI
p.die$ci10k <- p.die$est2*10000 # CI per 10k pop
p.die$time.mo <- p.die$time/30.4 # CI per 10k pop
wplot.die <- ggplot() + geom_line(aes(x=time.mo, y=ci10k, color = divestment), data=p.die)

div.inc <- mean(p.die[p.die$divestment==1&p.die$time %in% 1826:1827,]$ci10k)
nodiv.inc <- mean(p.die[p.die$divestment==0&p.die$time %in% 1826:1827,]$ci10k)
rd <- div.inc-nodiv.inc
rr <- div.inc/nodiv.inc

df2 <- data.frame(d = "All-cause Mortality", div.inc, nodiv.inc, rd, rr)
df <- rbind(df,df2)

## Liver -----
liver <- flexsurvreg(Surv(obstime, ever_liver) ~ divestment, weights = w,
                        data = dat, dist = "lnorm")

p.liver <- summary(liver, type = "survival", tidy=T)
p.liver$est2 <- 1-p.liver$est # CI
p.liver$ci10k <- p.liver$est2*10000 # CI per 10k pop
p.liver$time.mo <- p.liver$time/30.4 # CI per 10k pop
wplot.liver <- ggplot() + geom_line(aes(x=time.mo, y=ci10k, color = divestment), data=p.liver)

div.inc <- mean(p.liver[p.liver$divestment==1&p.liver$time %in% 1826:1827,]$ci10k)
nodiv.inc <- mean(p.liver[p.liver$divestment==0&p.liver$time %in% 1826:1827,]$ci10k)
rd <- div.inc-nodiv.inc
rr <- div.inc/nodiv.inc

df2 <- data.frame(d = "Liver disease", div.inc, nodiv.inc, rd, rr)
df <- rbind(df,df2)

## Lung -----
lung <- flexsurvreg(Surv(obstime, ever_lung) ~ divestment, weights = w,
                        data = dat, dist = "lnorm")
p.lung <- summary(lung, type = "survival", tidy=T)
p.lung$est2 <- 1-p.lung$est # CI
p.lung$ci10k <- p.lung$est2*10000 # CI per 10k pop
p.lung$time.mo <- p.lung$time/30.4 # CI per 10k pop
wplot.lung <- ggplot() + geom_line(aes(x=time.mo, y=ci10k, color = divestment), data=p.lung)

div.inc <- mean(p.lung[p.lung$divestment==1&p.lung$time %in% 1826:1827,]$ci10k)
nodiv.inc <- mean(p.lung[p.lung$divestment==0&p.lung$time %in% 1826:1827,]$ci10k)
rd <- div.inc-nodiv.inc
rr <- div.inc/nodiv.inc

df2 <- data.frame(d = "Lung cancer", div.inc, nodiv.inc, rd, rr)
df <- rbind(df,df2)

## MI -----
mi <- flexsurvreg(Surv(obstime, ever_mi) ~ divestment, weights = w,
                        data = dat, dist = "lnorm")

p.mi <- summary(mi, type = "survival", tidy=T)
p.mi$est2 <- 1-p.mi$est # CI
p.mi$ci10k <- p.mi$est2*10000 # CI per 10k pop
p.mi$time.mo <- p.mi$time/30.4 # CI per 10k pop
wplot.mi <- ggplot() + geom_line(aes(x=time.mo, y=ci10k, color = divestment), data=p.mi)

div.inc <- mean(p.mi[p.mi$divestment==1&p.mi$time %in% 1826:1827,]$ci10k)
nodiv.inc <- mean(p.mi[p.mi$divestment==0&p.mi$time %in% 1826:1827,]$ci10k)
rd <- div.inc-nodiv.inc
rr <- div.inc/nodiv.inc

df2 <- data.frame(d = "MI", div.inc, nodiv.inc, rd, rr)
df <- rbind(df,df2)

## OD -----
od <- flexsurvreg(Surv(obstime, ever_od) ~ divestment, weights = w,
                        data = dat, dist = "lnorm")

p.od <- summary(od, type = "survival", tidy=T)
p.od$est2 <- 1-p.od$est # CI
p.od$ci10k <- p.od$est2*10000 # CI per 10k pop
p.od$time.mo <- p.od$time/30.4 # CI per 10k pop
wplot.od <- ggplot() + geom_line(aes(x=time.mo, y=ci10k, color = divestment), data=p.od)

div.inc <- mean(p.od[p.od$divestment==1&p.od$time %in% 1826:1827,]$ci10k)
nodiv.inc <- mean(p.od[p.od$divestment==0&p.od$time %in% 1826:1827,]$ci10k)
rd <- div.inc-nodiv.inc
rr <- div.inc/nodiv.inc

df2 <- data.frame(d = "Unintentional Overdose", div.inc, nodiv.inc, rd, rr)
df <- rbind(df,df2)


write.csv(df, "./Output/est_5yr_summary_noCIs_1_2.csv")
