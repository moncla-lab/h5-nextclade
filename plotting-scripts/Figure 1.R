library("viridis")
library(RColorBrewer)
library(ggplot2)
library(dplyr)
library(stringr)
library(ggpubr)
library(grid)



seaborn_palette = c('#4C72B0',
                    '#DD8452',
                    '#55A868',
                    '#C44E52',
                    '#8172B3',
                    '#937860',
                    '#DA8BC3',
                    '#8C8C8C',
                    '#CCB974',
                    '#64B5CD')

clades_2344 = c('2.3.4.4',
                '2.3.4.4-like',
                '2.3.4.4a',
                '2.3.4.4b',
                '2.3.4.4c',
                '2.3.4.4d',
                '2.3.4.4e',
                '2.3.4.4f',
                '2.3.4.4g',
                '2.3.4.4h')

clades_2321 = c('2.3.2.1',
                '2.3.2.1a',
                '2.3.2.1b',
                '2.3.2.1c',
                '2.3.2.1c-like',
                '2.3.2.1d',
                '2.3.2.1e',
                '2.3.2.1f')



results_2344 = '/Users/jort/coding/h5-clades/20240712-LABEL-benchmarking/2344/output/nextclade_analysis.tsv'
results_2321 = '/Users/jort/coding/h5-clades/20240712-LABEL-benchmarking/2321/output/nextclade_analysis.tsv'
results_h5nx = '/Users/jort/coding/h5-clades/20240712-LABEL-benchmarking/h5nx/output/nextclade_analysis_no_2321_2344.tsv'



df_2321 = read.csv(results_2321, sep='\t')

df_2321_label_clade_counts = df_2321 %>%
  count(label_clade, name="totaln")

df_2321_counts = df_2321 %>%
  count(label_clade, clade, name="n")

df_2321_counts = merge(df_2321_counts, df_2321_label_clade_counts, by='label_clade')

df_2321_counts$percent = df_2321_counts$n / df_2321_counts$totaln

df_2321_match = df_2321_counts[df_2321_counts$label_clade == df_2321_counts$clade,]
df_2321_match$clade = 'Match'

df_2321_mismatch = df_2321_counts[df_2321_counts$label_clade != df_2321_counts$clade,]

df_2321_combined = rbind(df_2321_match, df_2321_mismatch)

test_palette = seaborn_palette[length(unique(df_2321_combined$clade)):1]

p1 <- ggplot(data=df_2321_combined, aes(x=label_clade, fill=clade)) +
  geom_bar(stat='identity', aes(y=percent*100)) +
  scale_fill_manual(values=test_palette) +
  theme_classic() +
  theme(legend.title = element_text(size = 35, color='black'),
        legend.text = element_text(size = 25, color='black'),
        axis.text.y=element_text(size=35, color='black'),
        axis.text.x=element_text(angle=-45, hjust=0, size=35, color='black'),
        axis.title=element_text(size=35, color='black'),
        axis.ticks = element_line(size=2,color="black"),
        axis.ticks.length = unit(0.35, "cm")) +
  labs(y='Percent of\nassignments', x='LABEL clade', fill='Nextclade\nassignment') +
  geom_text(aes(x=label_clade, y=-5, label=paste0(totaln)), size=8, color='black')



df_2344 = read.csv(results_2344, sep='\t')

df_2344_label_clade_counts = df_2344 %>%
  count(label_clade, name="totaln")

df_2344_counts = df_2344 %>%
  count(label_clade, clade, name="n")

df_2344_counts = merge(df_2344_counts, df_2344_label_clade_counts, by='label_clade')

df_2344_counts$percent = df_2344_counts$n / df_2344_counts$totaln

df_2344_match = df_2344_counts[df_2344_counts$label_clade == df_2344_counts$clade,]
df_2344_match$clade = 'Match'

df_2344_mismatch = df_2344_counts[df_2344_counts$label_clade != df_2344_counts$clade,]

df_2344_combined = rbind(df_2344_match, df_2344_mismatch)

test_palette = seaborn_palette[length(unique(df_2344_combined$clade)):1]

p2 <- ggplot(data=df_2344_combined, aes(x=label_clade, fill=clade)) +
  geom_bar(stat='identity', aes(y=percent*100)) +
  scale_fill_manual(values=test_palette) +
  theme_classic() +
  theme(legend.title = element_text(size = 35, color='black'),
        legend.text = element_text(size = 25, color='black'),
        axis.text.y=element_text(size=35, color='black'),
        axis.text.x=element_text(angle=-45, hjust=0, size=35, color='black'),
        axis.title=element_text(size=35, color='black'),
        axis.ticks = element_line(size=2,color="black"),
        axis.ticks.length = unit(0.35, "cm")) +
  labs(y='Percent of\nassignments', x='LABEL clade', fill='Nextclade\nassignment') +
  geom_text(aes(x=label_clade, y=-5, label=paste0(totaln)), size=8, color='black')



df_h5nx = read.csv(results_h5nx, sep='\t')

df_h5nx_label_clade_counts = df_h5nx %>%
  count(label_clade, name="totaln")

df_h5nx_counts = df_h5nx %>%
  count(label_clade, clade, name="n")

df_h5nx_counts = merge(df_h5nx_counts, df_h5nx_label_clade_counts, by='label_clade')

df_h5nx_counts$percent = df_h5nx_counts$n / df_h5nx_counts$totaln

df_h5nx_match = df_h5nx_counts[df_h5nx_counts$label_clade == df_h5nx_counts$clade,]
df_h5nx_match$clade = 'Match'

df_h5nx_mismatch = df_h5nx_counts[df_h5nx_counts$label_clade != df_h5nx_counts$clade,]

df_h5nx_combined = rbind(df_h5nx_match, df_h5nx_mismatch)

df_h5nx_combined$match = df_h5nx_combined$clade == 'Match'

filtered_df_h5nx_combined = df_h5nx_combined %>%
  filter(!(str_detect(label_clade, 'like'))) %>%
  filter(label_clade!='?')

bool_palette = seaborn_palette[2:1]

p3 <- ggplot(data=filtered_df_h5nx_combined, aes(x=label_clade, fill=match)) +
  geom_bar(stat='identity', aes(y=percent*100)) +
  scale_fill_manual(values=bool_palette, labels=c('Not a match','Match')) +
  theme_classic() +
  theme(legend.title = element_text(size = 35, color='black'),
        legend.text = element_text(size = 25, color='black'),
        axis.text.y=element_text(size=35, color='black'),
        axis.text.x=element_text(angle=-45, hjust=0, size=35, color='black'),
        axis.title=element_text(size=35, color='black'),
        axis.ticks = element_line(size=2,color="black"),
        axis.ticks.length = unit(0.35, "cm"),
        axis.title.x = element_text(vjust=10)) +
  labs(y='Percent of\nassignments', x='LABEL clade', fill='Nextclade\nassignment') +
  geom_text(aes(x=label_clade, y=-5, label=paste0("*"), size=totaln), color='black') +
  scale_size_continuous(range=c(5,30), breaks=c(100,500,1000))#, guide='none')



g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

# draw 2.3.2.1 plot and legend
p1 + theme(legend.position="none")
grid.newpage()
grid.draw(g_legend(p1))

# draw 2.3.4.4 plot and legend
p2 + theme(legend.position="none")
grid.newpage()
grid.draw(g_legend(p2))

# draw all-clades plot and legend
p3 + theme(legend.position="none")
grid.newpage()
grid.draw(g_legend(p3))