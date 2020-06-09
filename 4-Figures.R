library(tidyverse)
library(feather)
library(lubridate)

setwd("~/Projects/Colombia-drug-IUU//")

dat <- read_feather('data/Colombia_KSDaily_2018-03-10_2018-04-22.feather')

# dat1 <- filter(dat, t >= "2017-09-01 01:00:00 PDT")

# Subset +- 15-days
dat1 <- filter(dat, t >= "2018-03-23 01:00:00")

# [1] "t"      "lag"    "ks"     "pvalue" "js"     "kl"     "hl"  

ggplot(dat1, aes(t, lag, fill=kl)) + 
  geom_tile() +
  geom_vline(data = NULL, aes(xintercept = as.POSIXct("2018-04-07 12:00:00")), color='red', size=1) +
  scale_fill_continuous(type = "viridis") +
  scale_y_continuous(expand = c(0,0), breaks = seq(1, length(unique(dat1$lag)), 24), labels = seq(1, 10, 1), limits = c(0, length(unique(dat1$lag)))) +
  scale_x_datetime(expand = c(0, 0), breaks = "5 days") +
  labs(x=NULL, y="Lagged Days") + 
  theme_bw() +
  NULL


ggsave("figures/Figure2_lagged_ks.pdf", width=10, height=5)


pdat <- dat1 %>% 
  select(-pvalue) %>% 
  filter(lag <= 24*2) %>%
  gather(key = stat, value = value, -t, -lag) %>% 
  group_by(t, stat) %>% 
  summarise(mean = mean(value),
            kurt = moments::kurtosis(value)) %>% 
  ungroup()

ggplot(pdat, aes(t, mean, color=stat)) + geom_line() +
  geom_vline(data = NULL, aes(xintercept = as.POSIXct("2018-04-07 02:00:00")), color='red') +
  theme_bw() +
  labs(x=NULL, y="Mean Anomaly Index (2-day lag)") +
  scale_x_datetime( breaks = "5 days") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~stat, scales = "free") +
  theme(legend.position = "none") +
  NULL

ggsave("figures/Figure3_Mean_Anomaly_Index.pdf", width=8, height=6)

ggplot(pdat, aes(t, kurt, color=stat)) + geom_line() +
  geom_vline(data = NULL, aes(xintercept = as.POSIXct("2018-04-07 02:00:00")), color='red') +
  theme_bw() +
  labs(x=NULL, y="Kurtosis Anomaly Index (2-day lag)") +
  scale_x_datetime( breaks = "5 days") +
  facet_wrap(~stat, scales = "free")  + 
  theme(legend.position = "none") +
  NULL

ggsave("figures/Figure4_Kurtosis_Anomaly_Index.pdf", width=8, height=6)
