# library("viridis")
# library(RColorBrewer)
library(ggplot2)
# library(dplyr)
# library(stringr)
# library(ggpubr)
# library(grid)
library(scales)

df <- read.csv('/Users/jort/coding/h5-clades/20241108-analyses-for-revisions/speed-testing-new-seqs/speed_test_data_fc.tsv', sep='\t')

# factored x axis
ggplot(df, aes(x = factor(n), y = foldchange, group = n)) + 
  geom_point(size = 6,
             position = position_jitter(width = 0.05)) +
  stat_summary(fun = mean, 
               fun.min = function(x) mean(x) - sd(x), 
               fun.max = function(x) mean(x) + sd(x),
               geom = 'errorbar',  width = 0.15, linewidth=1) +
  stat_summary(fun = mean, fun.min = mean, fun.max = mean,
               geom = 'errorbar',  width = .3, linewidth=1) +
  labs(x = '', y = '') +
  theme_classic() +
  theme(legend.position = 'none',
        axis.text.y=element_text(size=25, color="black"),
        axis.text.x=element_text(size=25, color="black"),
        axis.title=element_text(size=30, color="black"),
        axis.ticks = element_line(size=1.5, color="black"),
        axis.ticks.length = unit(0.3, "cm"),
        panel.border = element_rect(fill=NA, linewidth=1)
  ) +
  scale_y_continuous(limits=c(60,110)) +
  scale_x_discrete(labels=c('100', '1,000', '10,000')) +
  labs(
    x = 'Number of sequences analyzed',
    y = 'Fold difference in runtime
(LABEL / Nextclade)'
  )




df_abs <- read.csv('/Users/jort/coding/h5-clades/20241108-analyses-for-revisions/speed-testing-new-seqs/speed_test_data_absolute.tsv', sep='\t')

ggplot(df_abs, aes(x = factor(n), y = runtime, color = program)) +
  geom_point(size = 6,
             position = position_jitter(width = 0.05)) +
  stat_summary(fun = mean, 
               fun.min = function(x) mean(x) - sd(x), 
               fun.max = function(x) mean(x) + sd(x),
               geom = 'errorbar',  width = 0.15, linewidth=1) +
  stat_summary(fun = mean, fun.min = mean, fun.max = mean,
               geom = 'errorbar',  width = .3, linewidth=1) +
  scale_color_manual(labels = c('LABEL', 'Nextclade'), values=c('#000000', '#4C72B0')) +
  labs(x = '', y = '') +
  theme_classic() +
  theme(legend.title=element_blank(),
        legend.text=element_text(size=25, color="black"),
        axis.text.y=element_text(size=25, color="black"),
        axis.text.x=element_text(size=25, color="black"),
        axis.title=element_text(size=30, color="black"),
        axis.ticks = element_line(size=1.5, color="black"),
        axis.ticks.length = unit(0.3, "cm"),
        panel.border = element_rect(fill=NA, linewidth=1)
  ) +
  scale_y_log10(limits=c(0.1,2000),
                breaks = c(0.1, 1, 10, 100, 1000, 10000),
                labels = c('0.1', '1', '10', '100', '1,000', '10,000')) +
  scale_x_discrete(labels=c('100', '1,000', '10,000')) +
  labs(
    x = 'Number of sequences analyzed',
    y = 'Runtime (s)'
  )

# no legend
ggplot(df_abs, aes(x = factor(n), y = runtime, color = program)) +
  geom_point(size = 6,
             position = position_jitter(width = 0.05)) +
  stat_summary(fun = mean, 
               fun.min = function(x) mean(x) - sd(x), 
               fun.max = function(x) mean(x) + sd(x),
               geom = 'errorbar',  width = 0.15, linewidth=1) +
  stat_summary(fun = mean, fun.min = mean, fun.max = mean,
               geom = 'errorbar',  width = .3, linewidth=1) +
  scale_color_manual(labels = c('LABEL', 'Nextclade'), values=c('#000000', '#4C72B0')) +
  labs(x = '', y = '') +
  theme_classic() +
  theme(legend.position = 'none',
        axis.text.y=element_text(size=25, color="black"),
        axis.text.x=element_text(size=25, color="black"),
        axis.title=element_text(size=30, color="black"),
        axis.ticks = element_line(size=1.5, color="black"),
        axis.ticks.length = unit(0.3, "cm"),
        panel.border = element_rect(fill=NA, linewidth=1)
  ) +
  scale_y_log10(limits=c(0.1,2000),
                breaks = c(0.1, 1, 10, 100, 1000, 10000),
                labels = c('0.1', '1', '10', '100', '1,000', '10,000')) +
  scale_x_discrete(labels=c('100', '1,000', '10,000')) +
  labs(
    x = 'Number of sequences analyzed',
    y = 'Runtime (s)'
  )
