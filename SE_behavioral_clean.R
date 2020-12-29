rm(list = ls())
options(scipen=999)
library(lme4)
library(lmerTest) # add t test p values
library(tidyverse)
library(ggplot2)

### data 
df <- read_csv('~/Box/Project_SE/DataAnalysis_04032020/1_processData/RT_indiv_EEGpower_45hz_C34z.csv') %>%
  select(Response, TimeError, subid, block, trial, speed, direction) %>% # n=23400
  filter(Response==1) %>% # n = 22992
  filter(abs(TimeError) < 200) # n = 22955

range(df$TimeError) # [1] -198  195, for selecting bins 
histogram <- df %>%
  ggplot(aes(x=TimeError, fill=direction)) + 
  geom_histogram(bins = 40, alpha=0.55) +
  geom_vline(xintercept = 0, size=0.15, color='black') +
  scale_x_continuous(limits = c(-200, 200)) + 
  facet_grid(subid~speed)


RT_2SD <- df %>% # filter out RTs > 2SDs in any block
  group_by(subid, direction, speed) %>% 
  filter(abs(TimeError-mean(TimeError)) < 2*sd(TimeError)) %>%
  # mutate(TimeError=scale(TimeError)) %>%
  ungroup()

1-nrow(RT_2SD)/nrow(df) # 4.674363% filtered
range(RT_2SD$TimeError) # [1] -174  138

histogram2 <- RT_2SD %>%
  ggplot(aes(x=TimeError, fill=direction)) + 
  geom_histogram(bins=40, alpha=0.55) +
  geom_vline(xintercept = 0, size=0.2, color='black') +
  xlim(-200,200) +
  facet_grid(subid~speed)

write_csv(RT_2SD, '~/Desktop/RT_ls200_within2SD.csv')
