# This script was written by Tomás Lejarraga to analyze the data from the Investment Game (Exp. 1), with Jan Woike and Ralph Hertwig

# Clear all
rm(list = ls(all = TRUE))

# Load packages needed
library(reshape)
library(ggplot2)
library(grid)
library(gridExtra)
library(lme4)
library(pastecs)
library(Rmisc)

# Load data
setwd("~/Downloads")
perc_stock <- read.table("mmc6.txt", header = TRUE)
perc_stock$condition <- ifelse(perc_stock$condition == "Shock experience", "le",
                               ifelse(perc_stock$condition == "No-shock experience", "se", 
                                      ifelse(perc_stock$condition == "Shock description", "ld", "sd")))
perc_stock$condition_l <- ifelse(perc_stock$condition == "le", "Long experience",
                                 ifelse(perc_stock$condition == "se", "Short experience", 
                                        ifelse(perc_stock$condition == "ld", "Long description", "Short description")))
value_stock <- read.table("mmc7.txt", header = TRUE)
value_stock$value_stock <- value_stock$value_stock/1000

#plot
perc_stock$condition_l <- factor(perc_stock$condition_l)
perc_stock$condition_l <- factor(perc_stock$condition_l, levels = c("Long experience", "Short experience", "Long description", "Short description"))
behavior <- ggplot(data = perc_stock, aes(x = period, y = perc_stock, colour = condition_l)) +
  stat_summary(fun.y = mean, geom = "line", alpha = 0.75) +
  geom_point(alpha = .05) +
  stat_smooth(method = "loess", se = FALSE, size = 1.5) +
  ylab("Percentage invested in stocks") +
  xlab("Period") +
  theme(panel.background = element_blank()) +
  theme(text = element_text(size = 15)) +
  theme(legend.justification = c(0, 0), legend.position = c(.7, .7)) +  
  theme(legend.title = element_blank()) +
  theme(legend.key = element_blank()) +
  theme(legend.background = element_blank()) +
  theme(panel.background = element_rect(colour = "grey")) +
  ylim(0, 1) +
  scale_colour_brewer(palette = "RdBu") +
  theme(legend.position="none")+
  theme(plot.margin = unit(c(0,0,0,0), "cm"))

stocks <- ggplot(data = value_stock, aes(x = period, y = value_stock)) +
  geom_line() +
  theme(text = element_text(size = 15)) +
  theme(panel.background = element_blank()) +
  theme(panel.background = element_rect(colour = "grey")) +
  theme(legend.title = element_blank()) +
  theme(legend.key = element_blank()) +
  theme(legend.background = element_blank()) +
  ylab("Price of stocks (thousands)") +
  xlab("") +
  annotate("rect", xmin = 0, xmax = 40, ymin = 0, ymax = max(value_stock$value_stock), alpha = .05) +
  annotate("rect", xmin = 40, xmax = 100, ymin = 0, ymax = max(value_stock$value_stock), alpha = .1) +
  annotate("rect", xmin = 100, xmax = max(value_stock$period), ymin = 0, ymax = max(value_stock$value_stock), alpha = .15) +
  annotate("text", x = 100 + (172 - 100)/2, y = 15, label = "Evaluation window") +
  annotate("text", x = 0, y = 4.1, label = "Shock experience", hjust = 0, colour = "#AD2224", size = 4) +
  annotate("text", x = 40, y = 3.1, label = "No-shock experience", hjust = 0, colour = "#E0A482", size = 4) +
  annotate("text", x = 100, y = 2.1, label = "Shock description", hjust = 0, colour = "#4070AD", size = 4) +
  annotate("text", x = 100, y = 1.1, label = "No-shock description", hjust = 0, colour = "#A1C3DC", size = 4) +
  annotate("segment", x = 0, xend = 172, y = 3.6, yend = 3.6, arrow = arrow(length = unit(0.2,"cm"), type = "closed"), colour = "#AD2224", size = 1) +
  annotate("segment", x = 40, xend = 172, y = 2.6, yend = 2.6, arrow = arrow(length = unit(0.2,"cm"), type = "closed"), colour = "#E0A482", size = 1) +
  annotate("segment", x = 100, xend = 172, y = 1.6, yend = 1.6, arrow = arrow(length = unit(0.2,"cm"), type = "closed"), colour = "#4070AD", size = 1) +
  annotate("segment", x = 100, xend = 172, y = 0.6, yend = 0.6, arrow = arrow(length = unit(0.2,"cm"), type = "closed"), colour = "#A1C3DC", size = 1) +
  annotate("segment", x = 0, xend = 100, y = 1.6, yend = 1.6, linetype = "dotted", colour = "#4070AD", size = 1) +
  annotate("segment", x = 40, xend = 100, y = 0.6, yend = 0.6, linetype = "dotted", colour = "#A1C3DC", size = 1) +
  theme(plot.margin = unit(c(0,0,0,0), "cm"))

gA <- ggplot_gtable(ggplot_build(stocks))
gB <- ggplot_gtable(ggplot_build(behavior))
maxWidth = unit.pmax(gA$widths[2:3], gB$widths[2:3])
gA$widths[2:3] <- maxWidth
gB$widths[2:3] <- maxWidth

png("Fig1.png", width = 20, height = 20, units = "cm", res = 300)
grid.arrange(gA, gB, nrow=2)
dev.off()


#Compare conditions in the spirit of the "new statistics" by Geoff Cumming (2012)
mean_perc_stock <- aggregate(perc_stock$perc_stock, by = list(perc_stock$period, perc_stock$condition), FUN = mean)
colnames(mean_perc_stock) <- c("period", "condition", "mean_perc_stock")
wide_mean_perc_stock <- reshape(mean_perc_stock, timevar = "condition", idvar = "period", direction = "wide")
colnames(wide_mean_perc_stock) <- c("period", "long_description", "long_experience", "short_description", "short_experience")
wide_mean_perc_stock$short_higher_long_exp <- ifelse(wide_mean_perc_stock$period < 100, NA, ifelse(wide_mean_perc_stock$long_experience > wide_mean_perc_stock$short_experience, 0, 1))
wide_mean_perc_stock$short_higher_long_desc <- ifelse(wide_mean_perc_stock$period < 100, NA, ifelse(wide_mean_perc_stock$long_description > wide_mean_perc_stock$short_description, 0, 1))
mean(wide_mean_perc_stock$short_higher_long_exp, na.rm = TRUE)
mean(wide_mean_perc_stock$short_higher_long_desc, na.rm = TRUE)

long_exp_agg_stock <- mean(wide_mean_perc_stock$long_experience[wide_mean_perc_stock$period >= 100])
short_exp_agg_stock <- mean(wide_mean_perc_stock$short_experience[wide_mean_perc_stock$period >= 100])
(short_exp_agg_stock - long_exp_agg_stock)/long_exp_agg_stock
prop.test(sum(wide_mean_perc_stock$short_higher_long_exp, na.rm = TRUE), length(na.omit(wide_mean_perc_stock$short_higher_long_exp)))

long_desc_agg_stock <- mean(wide_mean_perc_stock$long_description[wide_mean_perc_stock$period >= 100])
short_desc_agg_stock <- mean(wide_mean_perc_stock$short_description[wide_mean_perc_stock$period >= 100])
(short_desc_agg_stock - long_desc_agg_stock)/long_desc_agg_stock
prop.test(sum(wide_mean_perc_stock$short_higher_long_desc, na.rm = TRUE), length(na.omit(wide_mean_perc_stock$short_higher_long_desc)))

wide_mean_perc_stock$diff_exp <- ifelse(wide_mean_perc_stock$period < 100, NA, wide_mean_perc_stock$short_experience - wide_mean_perc_stock$long_experience)
wide_mean_perc_stock$diff_desc <- ifelse(wide_mean_perc_stock$period < 100, NA, wide_mean_perc_stock$short_description - wide_mean_perc_stock$long_description)

diff_exp <- ggplot(data = wide_mean_perc_stock, aes(x = period, y = diff_exp)) +
  geom_area() +
  theme(legend.title = element_blank()) +
  theme(legend.key = element_blank()) +
  theme(legend.background = element_blank()) +
  theme(panel.background = element_blank()) +
  theme_bw() +
  theme(text = element_text(size = 15)) +
  theme(panel.grid.major = element_line(colour = "grey")) +
  ylab("Difference in mean investment in stocks") +
  xlab("Period") +
  xlim(100, 172) +
  ylim(-0.2, 0.2) +
  ggtitle("Experience: No-shock vs. shock") +
  theme(plot.title=element_text(size = 15)) +
  theme(plot.margin = unit(c(0,0,0,0), "cm"))

diff_desc <- ggplot(data = wide_mean_perc_stock, aes(x = period, y = diff_desc)) +
  geom_area() +
  theme(legend.title = element_blank()) +
  theme(legend.key = element_blank()) +
  theme(legend.background = element_blank()) +
  theme(panel.background = element_blank()) +
  theme_bw() +
  theme(text = element_text(size = 15)) +
  theme(panel.grid.major = element_line(colour = "grey")) +
  ylab(NULL) +
  xlab("Period") +
  xlim(100, 172) +
  ylim(-0.2, 0.2) +
  ggtitle("Description: No-shock vs. shock") +
  theme(plot.title=element_text(size = 15)) +
  theme(plot.margin = unit(c(0,0,0,0), "cm"))

gA <- ggplot_gtable(ggplot_build(diff_exp))
gB <- ggplot_gtable(ggplot_build(diff_desc))
maxWidth = unit.pmax(gA$widths[2:3], gB$widths[2:3])
gA$widths[2:3] <- maxWidth
gB$widths[2:3] <- maxWidth

png("Fig3.png", width = 20, height = 12, units = "cm", res = 300)
grid.arrange(gA, gB, ncol=2)
dev.off()

#Compare means and CIs
perc_stock_eval <- subset(perc_stock, period >= 100)
mean_perc_stock_eval <- aggregate(perc_stock_eval$perc_stock, by = list(perc_stock_eval$subject, perc_stock_eval$condition), FUN = mean)
colnames(mean_perc_stock_eval) <- c("subject", "condition", "mean_perc_stock")

le <- subset(mean_perc_stock_eval, condition == "le")
se <- subset(mean_perc_stock_eval, condition == "se")
ld <- subset(mean_perc_stock_eval, condition == "ld")
sd <- subset(mean_perc_stock_eval, condition == "sd")

le_ci_mean <- mean(le$mean_perc_stock)
le_ci_upper <- mean(le$mean_perc_stock) + qt(.975, length(le$mean_perc_stock) - 1)*sd(le$mean_perc_stock)/(sqrt(length(le$mean_perc_stock)))
le_ci_lower <- mean(le$mean_perc_stock) - qt(.975, length(le$mean_perc_stock) - 1)*sd(le$mean_perc_stock)/(sqrt(length(le$mean_perc_stock)))
se_ci_mean <- mean(se$mean_perc_stock)
se_ci_upper <- mean(se$mean_perc_stock) + qt(.975, length(se$mean_perc_stock) - 1)*sd(se$mean_perc_stock)/(sqrt(length(se$mean_perc_stock)))
se_ci_lower <- mean(se$mean_perc_stock) - qt(.975, length(se$mean_perc_stock) - 1)*sd(se$mean_perc_stock)/(sqrt(length(se$mean_perc_stock)))
ld_ci_mean <- mean(ld$mean_perc_stock)
ld_ci_upper <- mean(ld$mean_perc_stock) + qt(.975, length(ld$mean_perc_stock) - 1)*sd(ld$mean_perc_stock)/(sqrt(length(ld$mean_perc_stock)))
ld_ci_lower <- mean(ld$mean_perc_stock) - qt(.975, length(ld$mean_perc_stock) - 1)*sd(ld$mean_perc_stock)/(sqrt(length(ld$mean_perc_stock)))
sd_ci_mean <- mean(sd$mean_perc_stock)
sd_ci_upper <- mean(sd$mean_perc_stock) + qt(.975, length(sd$mean_perc_stock) - 1)*sd(sd$mean_perc_stock)/(sqrt(length(sd$mean_perc_stock)))
sd_ci_lower <- mean(sd$mean_perc_stock) - qt(.975, length(sd$mean_perc_stock) - 1)*sd(sd$mean_perc_stock)/(sqrt(length(sd$mean_perc_stock)))

le_ci <- data.frame(cbind(le_ci_mean, le_ci_upper, le_ci_lower))
se_ci <- data.frame(cbind(se_ci_mean, se_ci_upper, se_ci_lower))
ld_ci <- data.frame(cbind(ld_ci_mean, ld_ci_upper, ld_ci_lower))
sd_ci <- data.frame(cbind(sd_ci_mean, sd_ci_upper, sd_ci_lower))
colnames(le_ci) <- c("mean", "upper", "lower")
colnames(se_ci) <- c("mean", "upper", "lower")
colnames(ld_ci) <- c("mean", "upper", "lower")
colnames(sd_ci) <- c("mean", "upper", "lower")

differences <- rbind(le_ci, se_ci, ld_ci, sd_ci)
differences$condition <- c("le", "se", "ld", "sd")
pal <- c("#AD2224", "#E0A482", "#4070AD", "#A1C3DC")

differences <- ggplot(data = differences, aes(x = condition, y = mean)) +
  geom_point(size = 4, col = pal) +
  geom_errorbar(aes(ymax = upper, ymin = lower), width = 0.2, col = pal) + 
  scale_x_discrete(limits = c("le", "se", "ld", "sd"), labels = c("Shock \n exp.", "No-shock \n exp.", "Shock \n desc.", "No-shock \n desc.")) +
  theme(legend.title = element_blank()) +
  theme(legend.key = element_blank()) +
  theme(legend.background = element_blank()) +
  theme_bw() +
  theme(text = element_text(size = 15)) +
  theme(panel.grid.major = element_line(colour = "grey")) +
  ylab(NULL) +
  xlab("") +
  ylim(0, .5)  +
  theme(plot.margin = unit(c(0,0,0,0), "cm"))

mean_perc_stock_eval$condition_length <- ifelse(mean_perc_stock_eval$condition == "le", "l",
                                           ifelse(mean_perc_stock_eval$condition == "ld", "l", "s"))
mean_perc_stock_eval$condition_source <- ifelse(mean_perc_stock_eval$condition == "le", "e",
                                                ifelse(mean_perc_stock_eval$condition == "se", "e", "d"))
l <- subset(mean_perc_stock_eval, condition_length == "l")
s <- subset(mean_perc_stock_eval, condition_length == "s")
e <- subset(mean_perc_stock_eval, condition_source == "e")
d <- subset(mean_perc_stock_eval, condition_source == "d")

l_ci_mean <- mean(l$mean_perc_stock)
l_ci_upper <- mean(l$mean_perc_stock) + qt(.975, length(l$mean_perc_stock) - 1)*sd(l$mean_perc_stock)/(sqrt(length(l$mean_perc_stock)))
l_ci_lower <- mean(l$mean_perc_stock) - qt(.975, length(l$mean_perc_stock) - 1)*sd(l$mean_perc_stock)/(sqrt(length(l$mean_perc_stock)))
s_ci_mean <- mean(s$mean_perc_stock)
s_ci_upper <- mean(s$mean_perc_stock) + qt(.975, length(s$mean_perc_stock) - 1)*sd(s$mean_perc_stock)/(sqrt(length(s$mean_perc_stock)))
s_ci_lower <- mean(s$mean_perc_stock) - qt(.975, length(s$mean_perc_stock) - 1)*sd(s$mean_perc_stock)/(sqrt(length(s$mean_perc_stock)))
e_ci_mean <- mean(e$mean_perc_stock)
e_ci_upper <- mean(e$mean_perc_stock) + qt(.975, length(e$mean_perc_stock) - 1)*sd(e$mean_perc_stock)/(sqrt(length(e$mean_perc_stock)))
e_ci_lower <- mean(e$mean_perc_stock) - qt(.975, length(e$mean_perc_stock) - 1)*sd(e$mean_perc_stock)/(sqrt(length(e$mean_perc_stock)))
d_ci_mean <- mean(d$mean_perc_stock)
d_ci_upper <- mean(d$mean_perc_stock) + qt(.975, length(d$mean_perc_stock) - 1)*sd(d$mean_perc_stock)/(sqrt(length(d$mean_perc_stock)))
d_ci_lower <- mean(d$mean_perc_stock) - qt(.975, length(d$mean_perc_stock) - 1)*sd(d$mean_perc_stock)/(sqrt(length(d$mean_perc_stock)))

l_ci <- data.frame(cbind(l_ci_mean, l_ci_upper, l_ci_lower))
s_ci <- data.frame(cbind(s_ci_mean, s_ci_upper, s_ci_lower))
e_ci <- data.frame(cbind(e_ci_mean, e_ci_upper, e_ci_lower))
d_ci <- data.frame(cbind(d_ci_mean, d_ci_upper, d_ci_lower))
colnames(l_ci) <- c("mean", "upper", "lower")
colnames(s_ci) <- c("mean", "upper", "lower")
colnames(e_ci) <- c("mean", "upper", "lower")
colnames(d_ci) <- c("mean", "upper", "lower")

differences_cond <- rbind(l_ci, s_ci, e_ci, d_ci)
differences_cond$condition <- c("l", "s", "e", "d")

differences_cond <- ggplot(data = differences_cond, aes(x = condition, y = mean)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = upper, ymin = lower), width = 0.2) + 
  scale_x_discrete(limits = c("l", "s", "e", "d"), labels = c("Shock", "No-shock", "Exp.", "Desc.")) +
  theme(legend.title = element_blank()) +
  theme(legend.key = element_blank()) +
  theme(legend.background = element_blank()) +
  theme_bw() +
  theme(panel.grid.major = element_line(colour = "grey")) +
  theme(text = element_text(size = 15)) +
  ylab("Mean investment in stocks") +
  xlab("") +
  ylim(0, .5)  +
  theme(plot.margin = unit(c(0,0,0,0), "cm"))

gA <- ggplot_gtable(ggplot_build(differences_cond))
gB <- ggplot_gtable(ggplot_build(differences))
maxWidth = unit.pmax(gA$widths[2:3], gB$widths[2:3])
gA$widths[2:3] <- maxWidth
gB$widths[2:3] <- maxWidth

maxHeight = unit.pmax(gA$heights[2:5], gB$heights[2:5])
gA$heights[2:5] <- maxHeight
gB$heights[2:5] <- maxHeight

png("Fig2.png", width = 20, height = 12, units = "cm", res = 300)
grid.arrange(gA, gB, nrow=1)
dev.off()

#CI of the differences (t distribution)
l_s_mean <- mean(l$mean_perc_stock) - mean(s$mean_perc_stock)
l_s_pooled_sd <- sqrt((((length(l$mean_perc_stock) - 1)*sd(l$mean_perc_stock)^2 + length(s$mean_perc_stock) - 1)*sd(s$mean_perc_stock)^2)/(length(l$mean_perc_stock) + length(s$mean_perc_stock) - 2))
l_s_upper <- l_s_mean + qt(.975, length(l$mean_perc_stock) + length(s$mean_perc_stock) - 2) * l_s_pooled_sd * sqrt(1/length(l$mean_perc_stock) + 1/length(s$mean_perc_stock))
l_s_lower <- l_s_mean - qt(.975, length(l$mean_perc_stock) + length(s$mean_perc_stock) - 2) * l_s_pooled_sd * sqrt(1/length(l$mean_perc_stock) + 1/length(s$mean_perc_stock))

e_d_mean <- mean(e$mean_perc_stock) - mean(d$mean_perc_stock)
e_d_pooled_sd <- sqrt((((length(e$mean_perc_stock) - 1)*sd(e$mean_perc_stock)^2 + length(d$mean_perc_stock) - 1)*sd(d$mean_perc_stock)^2)/(length(e$mean_perc_stock) + length(d$mean_perc_stock) - 2))
e_d_upper <- e_d_mean + qt(.975, length(e$mean_perc_stock) + length(d$mean_perc_stock) - 2) * e_d_pooled_sd * sqrt(1/length(e$mean_perc_stock) + 1/length(d$mean_perc_stock))
e_d_lower <- e_d_mean - qt(.975, length(e$mean_perc_stock) + length(d$mean_perc_stock) - 2) * e_d_pooled_sd * sqrt(1/length(e$mean_perc_stock) + 1/length(d$mean_perc_stock))

le_se_mean <- mean(le$mean_perc_stock) - mean(se$mean_perc_stock)
le_se_pooled_sd <- sqrt((((length(le$mean_perc_stock) - 1)*sd(le$mean_perc_stock)^2 + length(se$mean_perc_stock) - 1)*sd(se$mean_perc_stock)^2)/(length(le$mean_perc_stock) + length(se$mean_perc_stock) - 2))
le_se_upper <- le_se_mean + qt(.975, length(le$mean_perc_stock) + length(se$mean_perc_stock) - 2) * le_se_pooled_sd * sqrt(1/length(le$mean_perc_stock) + 1/length(se$mean_perc_stock))
le_se_lower <- le_se_mean - qt(.975, length(le$mean_perc_stock) + length(se$mean_perc_stock) - 2) * le_se_pooled_sd * sqrt(1/length(le$mean_perc_stock) + 1/length(se$mean_perc_stock))

ld_sd_mean <- mean(ld$mean_perc_stock) - mean(sd$mean_perc_stock)
ld_sd_pooled_sd <- sqrt((((length(ld$mean_perc_stock) - 1)*sd(ld$mean_perc_stock)^2 + length(sd$mean_perc_stock) - 1)*sd(sd$mean_perc_stock)^2)/(length(ld$mean_perc_stock) + length(sd$mean_perc_stock) - 2))
ld_sd_upper <- ld_sd_mean + qt(.975, length(ld$mean_perc_stock) + length(sd$mean_perc_stock) - 2) * ld_sd_pooled_sd * sqrt(1/length(ld$mean_perc_stock) + 1/length(sd$mean_perc_stock))
ld_sd_lower <- ld_sd_mean - qt(.975, length(ld$mean_perc_stock) + length(sd$mean_perc_stock) - 2) * ld_sd_pooled_sd * sqrt(1/length(ld$mean_perc_stock) + 1/length(sd$mean_perc_stock))

le_ld_mean <- mean(le$mean_perc_stock) - mean(ld$mean_perc_stock)
le_ld_pooled_sd <- sqrt((((length(le$mean_perc_stock) - 1)*sd(le$mean_perc_stock)^2 + length(ld$mean_perc_stock) - 1)*sd(ld$mean_perc_stock)^2)/(length(le$mean_perc_stock) + length(ld$mean_perc_stock) - 2))
le_ld_upper <- le_ld_mean + qt(.975, length(le$mean_perc_stock) + length(ld$mean_perc_stock) - 2) * le_ld_pooled_sd * sqrt(1/length(le$mean_perc_stock) + 1/length(ld$mean_perc_stock))
le_ld_lower <- le_ld_mean - qt(.975, length(le$mean_perc_stock) + length(ld$mean_perc_stock) - 2) * le_ld_pooled_sd * sqrt(1/length(le$mean_perc_stock) + 1/length(ld$mean_perc_stock))


# Correlation between stock price change and investment change
value_stock$stock_change_rat <- 0
for(i in 1:length(value_stock$stock_change_rat)) {
  value_stock$stock_change_rat[i] <- ifelse(value_stock$period[i] == 1, NA, value_stock$value_stock[i]/value_stock$value_stock[i - 1])
}

perc_stock$change_rat <- 0
for(i in 1:length(perc_stock$change_rat)) {
  perc_stock$change_rat[i] <- ifelse(perc_stock$period[i] == 172, NA, perc_stock$perc_stock[i + 1]/perc_stock$perc_stock[i])
}

perc_stock$change_vol <- 0
for(i in 1:length(perc_stock$change_vol)) {
  perc_stock$change_vol[i] <- ifelse(perc_stock$period[i] == 172, NA, perc_stock$perc_stock[i + 1] - perc_stock$perc_stock[i])
}

perc_stock$change_coded <- ifelse(perc_stock$change_vol == 0, 0, ifelse(perc_stock$change_vol > 0, 1, -1))

reactivity <- merge(perc_stock, value_stock)
reactivity$length <- ifelse(reactivity$condition == "le" | reactivity$condition == "ld", "Long", "Short")
reactivity$type <- ifelse(reactivity$condition == "le" | reactivity$condition == "se", "Experience", "Description")

le_cor <- cor.test(reactivity$stock_change_rat[reactivity$condition == "le"], reactivity$change_vol[reactivity$condition == "le"], use = "pairwise.complete.obs")
se_cor <- cor.test(reactivity$stock_change_rat[reactivity$condition == "se"], reactivity$change_vol[reactivity$condition == "se"], use = "pairwise.complete.obs")
ld_cor <- cor.test(reactivity$stock_change_rat[reactivity$condition == "ld"], reactivity$change_vol[reactivity$condition == "ld"], use = "pairwise.complete.obs")
sd_cor <- cor.test(reactivity$stock_change_rat[reactivity$condition == "sd"], reactivity$change_vol[reactivity$condition == "sd"], use = "pairwise.complete.obs")

long_exp_reactivity <- subset(reactivity, condition == "le")
short_exp_reactivity <- subset(reactivity, condition == "se")
long_desc_reactivity <- subset(reactivity, condition == "ld")
short_desc_reactivity <- subset(reactivity, condition == "sd")

#Calculate correlation of each individual and then average across individuals
reactivity <- reactivity[order(reactivity$subject, reactivity$period),]
correlations <- ddply(reactivity, .(subject), summarize, cor.v2.v3 = cor(stock_change_rat, change_vol, use = "pairwise.complete.obs")) 
colnames(correlations) <- c("subject", "correlation")
correlations$condition <- substr(correlations$subject, 1, 2)
correlations$length <- substr(correlations$subject, 1, 1)
correlations$type <- substr(correlations$subject, 2, 2)
corr <- by(correlations$correlation, factor(correlations$condition), mean, na.rm = TRUE)
length(correlations$correlation[correlations$correlation > 0])/length(correlations$correlation)
       
long_exp_gr <- ggplot(data = long_exp_reactivity, aes(x = stock_change_rat, y = change_vol)) +
  geom_point(alpha = .1, colour = "#A20015", size = 1) +
  stat_smooth(method = "lm", se = TRUE, size = 1, colour = "#A20015") +
  stat_smooth(data = long_exp_reactivity, aes(x = stock_change_rat, y = change_vol, colours = subject), method = "lm", se = FALSE, size = .1, colour = "#A20015") +
  ylab("Change in stock investment") +
  xlab("") +
  theme(panel.background = element_blank()) +
  theme(text = element_text(size = 15)) +
  #theme(legend.justification = c(0, 0), legend.position = c(.7, .5)) +  
  theme(legend.title = element_blank()) +
  theme(legend.key = element_blank()) +
  theme(legend.background = element_blank()) +
  theme(panel.background = element_rect(colour = "grey")) +
  ggtitle("Shock experience") +
  annotate("text", x = 0.85, y = 0.9, label = paste("Mean r =",round(corr[2], 2)), hjust = 0, size = 5) +
  theme(plot.title=element_text(size = 15)) +
  theme(plot.margin = unit(c(0,0,0,0), "cm"))

short_exp_gr <- ggplot(data = short_exp_reactivity, aes(x = stock_change_rat, y = change_vol)) +
  geom_point(alpha = .1, colour = "#DA926D", size = 1) +
  stat_smooth(method = "lm", se = TRUE, size = 1, colour = "#DA926D") +
  stat_smooth(data = short_exp_reactivity, aes(x = stock_change_rat, y = change_vol, colours = subject), method = "lm", se = FALSE, size = .1, colour = "#DA926D") +
  ylab("") +
  xlab("") +
  theme(panel.background = element_blank()) +
  theme(text = element_text(size = 15)) +
  #theme(legend.justification = c(0, 0), legend.position = c(.7, .5)) +  
  theme(legend.title = element_blank()) +
  theme(legend.key = element_blank()) +
  theme(legend.background = element_blank()) +
  theme(panel.background = element_rect(colour = "grey")) +
  ggtitle("No-shock experience") +
  annotate("text", x = 0.85, y = 0.9, label = paste("Mean r =",round(corr[4], 2)), hjust = 0, size = 5) +
  theme(plot.title=element_text(size = 15)) +
  theme(plot.margin = unit(c(0,0,0,0), "cm"))

long_desc_gr <- ggplot(data = long_desc_reactivity, aes(x = stock_change_rat, y = change_vol)) +
  geom_point(alpha = .1, colour = "#345B9F", size = 1) +
  stat_smooth(method = "lm", se = TRUE, size = 1, colour = "#345B9F") +
  stat_smooth(data = long_desc_reactivity, aes(x = stock_change_rat, y = change_vol, colours = subject), method = "lm", se = FALSE, size = .1, colour = "#345B9F") +
  ylab("Change in stock investment") +
  xlab("Change in stock price") +
  theme(panel.background = element_blank()) +
  theme(text = element_text(size = 15)) +
  #theme(legend.justification = c(0, 0), legend.position = c(.7, .5)) +  
  theme(legend.title = element_blank()) +
  theme(legend.key = element_blank()) +
  theme(legend.background = element_blank()) +
  theme(panel.background = element_rect(colour = "grey")) +
  ggtitle("Shock description") +
  annotate("text", x = 0.85, y = 0.9, label = paste("Mean r =", round(corr[1], 2)), hjust = 0, size = 5) +
  theme(plot.title=element_text(size = 15)) +
  theme(plot.margin = unit(c(0,0,0,0), "cm"))

short_desc_gr <- ggplot(data = short_desc_reactivity, aes(x = stock_change_rat, y = change_vol)) +
  geom_point(alpha = .1, colour = "#91B6D5", size = 1) +
  stat_smooth(method = "lm", se = FALSE, size = 1, colour = "#91B6D5") +
  stat_smooth(data = short_desc_reactivity, aes(x = stock_change_rat, y = change_vol, colours = subject), method = "lm", se = FALSE, size = .1, colour = "#91B6D5") +
  ylab("") +
  xlab("Change in stock price") +
  theme(panel.background = element_blank()) +
  theme(text = element_text(size = 15)) +
  #theme(legend.justification = c(0, 0), legend.position = c(.7, .5)) +  
  theme(legend.title = element_blank()) +
  theme(legend.key = element_blank()) +
  theme(legend.background = element_blank()) +
  theme(panel.background = element_rect(colour = "grey")) +
  ggtitle("No-shock description") +
  annotate("text", x = 0.85, y = 0.9, label = paste("Mean r =",round(corr[3], 2)), hjust = 0, size = 5) +
  theme(plot.title=element_text(size = 15)) +
  theme(plot.margin = unit(c(0,0,0,0), "cm"))

gA <- ggplot_gtable(ggplot_build(long_exp_gr))
gB <- ggplot_gtable(ggplot_build(short_exp_gr))
gC <- ggplot_gtable(ggplot_build(long_desc_gr))
gD <- ggplot_gtable(ggplot_build(short_desc_gr))
maxWidth = unit.pmax(gA$widths[2:3], gB$widths[2:3], gC$widths[2:3], gD$widths[2:3])
gA$widths[2:3] <- maxWidth
gB$widths[2:3] <- maxWidth
gC$widths[2:3] <- maxWidth
gD$widths[2:3] <- maxWidth

png("Fig4.png", width = 20, height = 20, units = "cm", res = 300)
grid.arrange(gA, gB, gC, gD, nrow=2)
dev.off()


