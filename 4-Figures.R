library(tidyverse)
library(feather)

setwd("~/Projects/San-Diego-IUU/")

dat <- read_feather('data/SanDiego_KSDaily_2017-08-20_2017-10-01.feather')

dat1 <- filter(dat, t >= "2017-09-01 01:00:00 PDT")

# [1] "t"      "lag"    "ks"     "pvalue" "js"     "kl"     "hl"  

ggplot(dat1, aes(t, lag, fill=ks)) + 
  geom_tile() +
  geom_vline(data = NULL, aes(xintercept = as.POSIXct("2017-09-17 02:00:00")), color='red') +
  scale_fill_continuous(type = "viridis") +
  scale_y_continuous(expand = c(0,0), breaks = seq(1, length(unique(dat1$lag)), 24), labels = seq(1, 11, 1), limits = c(0, length(unique(dat1$lag)))) +
  scale_x_datetime(expand = c(0, 0)) +
  labs(x=NULL, y="Lag Days") + 
  theme_bw() +
  NULL


pdat <- dat1 %>% 
  filter(lag <= 24*2) %>%
  gather(key = stat, value = value, -t, -lag) %>% 
  group_by(t, stat) %>% 
  summarise(mean = mean(value),
            kurt = moments::kurtosis(value)) %>% 
  ungroup()

ggplot(pdat, aes(t, mean, color=stat)) + geom_line() +
  geom_vline(data = NULL, aes(xintercept = as.POSIXct("2017-09-18 02:00:00")), color='red') +
  theme_bw() +
  facet_wrap(~stat, scales = "free") +
  theme(legend.position = "none") +
  NULL

ggplot(pdat, aes(t, kurt, color=stat)) + geom_line() +
  geom_vline(data = NULL, aes(xintercept = as.POSIXct("2017-09-18 02:00:00")), color='red') +
  theme_bw() +
  facet_wrap(~stat, scales = "free")  + 
  theme(legend.position = "none") +
  NULL

