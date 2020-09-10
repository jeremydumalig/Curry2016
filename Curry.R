library(tidyverse)
library(ggplot2)
library(ggrepel)
library(hexbin)
library(ggExtra)
library(gridExtra)
library(ggpubr)
rm(list=ls())

curry <- read_csv(file="https://raw.githubusercontent.com/jeremydumalig/DataBank/master/nba_savant201939.csv")
top10 <- read_csv(file="https://raw.githubusercontent.com/jeremydumalig/DataBank/master/nba_savant%20(1).csv")
pergame <- read_csv(file='https://raw.githubusercontent.com/jeremydumalig/DataBank/master/nba_2016.csv')
totals <- read_csv(file='https://raw.githubusercontent.com/jeremydumalig/DataBank/master/nbatotals2016.csv')
hollinger <- read_csv(file='https://raw.githubusercontent.com/jeremydumalig/DataBank/master/hollinger_raw.csv')

#####################################################################

#merged totals + per game
merged <- 
  pergame %>%
  merge(totals, by='Player') %>%
  mutate(
    `3P%.x` = 100 * `3P%.x`,
    `FG%.x` = 100 * `FG%.x`,
    `FT%.x` = 100 * `FT%.x`
  )

#merged for top 10 3PM
merged10 <- filter(merged, 
                   Player == 'Stephen Curry' |
                   Player == 'CJ McCollum' |
                   Player == 'Damian Lillard' | 
                   Player == 'James Harden' |
                   Player == 'J.J. Redick' |
                   Player == 'J.R. Smith' |
                   Player == 'Klay Thompson' |
                   Player == 'Kyle Lowry' |
                   Player == 'Paul George' |
                   Player == 'Wesley Matthews')

#positions
positions <- filter(merged, `Pos.x` != 'SG-SF' & `Pos.x` != 'PF-C')
positions <- 
  positions %>%
  filter(`Pos.x` == 'PG' | `Pos.x` == 'SG') %>%
  mutate(`Pos.x` = 'Guard')

all_pos <- mutate(merged, `Pos.x` = 'All Positions')

positions <- rbind(positions, all_pos)

boxplot_labels <- data.frame(
  "Pos.x" = c('Guard', 'Guard', 'All Positions'), 
  "3PA.y" = c(886, 657, 72),
  "Player" = c('Stephen Curry', 'James Harden', 'Median NBA Player')
)

#qualified
q <- filter(merged, `FG.y` >= 300 & `3P.y` >= 82 & `FT.y` >= 125)
dq <- filter(merged, `FG.y` < 300 | `3P.y` < 82 | `FT.y` < 125)

#50-40-90 club
club <- filter(q, `FG%.x` >= 50 & `3P%.x` >= 40 & `FT%.x` >= 90)

#50-40-90 scatterplot axis limits
summary_fg <- summary(merged$`FG%.x`)
summary_3p <- summary(merged$`3P%.x`)
iqr_fg <- IQR(merged$`FG%.x`, na.rm=TRUE)
iqr_3p <- IQR(merged$`3P%.x`, na.rm=TRUE)

#hollinger wrangling
hollinger <- 
  hollinger %>%
  filter(PLAYER != 'PLAYER') %>%
  mutate(
    `TS%` = 100 * as.numeric(`TS%`),
    USG = as.numeric(USG),
    PER = as.numeric(PER)
  ) %>%
  select(PLAYER, `TS%`, USG, PER)

#linear regression: true shooting x per 
per_ts <- lm(data=hollinger, PER ~ `TS%`)
per_ts_intercept <- coef(per_ts)['(Intercept)']
per_ts_slope <- coef(per_ts)['`TS%`']

#mutate for predictions and residuals
hollinger <- mutate(hollinger,
                    pred = `TS%`*per_ts_slope + per_ts_intercept, 
                    res = PER - (`TS%`*per_ts_slope + per_ts_intercept))

#top 5 in residuals
head_res <- head(arrange(hollinger, desc(res)), 5)

#3-point shooters for relative frequency
all_top10 <- 
  curry %>%
  rbind(mutate(top10, name='Rest of Top 10 \n3-Point Shooters \nCombined')) %>%
  filter(shot_type=='3PT Field Goal') %>%
  mutate(
    'Shot Distance' = case_when(
      shot_distance < 25 ~ '<25 ft',
      shot_distance >= 25 & shot_distance < 30 ~ '25-30 ft',
      shot_distance >= 30 ~ '30+ ft'
    )
  ) %>%
  group_by(name, `Shot Distance`) %>%
  summarize(
    sd_count = n(),
    `3PM` = sum(shot_made_flag),
    `3P%` = 100 * `3PM` / sd_count
  ) %>%
  ungroup() %>%
  mutate(
    total = case_when(
      name == 'Stephen Curry' ~ 876, #400/876
      name != 'Stephen Curry' ~ 4803 #1890/4803
    ), 
    rel = 100 * sd_count / total
  ) %>%
  group_by(name) %>%
  mutate(
    cumsum = cumsum(sd_count),
    cumrel = cumsum / total
  ) %>%
  ungroup()

#####################################################################

#theme borders
theme_borders <- 
  theme_linedraw() +
  theme(
    plot.margin = margin(1, 0.5, 0.5, 0.5, "cm"),
    plot.background = element_rect(
      fill = "grey90",
      color = "black"
    ),
    legend.box.background = element_rect(size=0.75),
    title = element_text(size=20),
    legend.title = element_text(size=10),
    axis.title.x = element_text(size=15),
    axis.title.y = element_text(size=15)
  )

#3PM Boxplots by Position
ggplot(data=positions) + 
  geom_boxplot(aes(
    x=`Pos.x`, 
    y=`3PA.y`,
    fill=`Pos.x`)
  ) + 
  ylim(c(0, 887)) +
  scale_fill_manual(
    values=c('#B6D8E9', '#0050A2')
  ) +
  labs(
    #title=expression(paste(bold("Boxplot:"), " Total 3-Pointers Attempted by Position")),
    x='Position',
    y='3-Pointers Attempted (3PA)',
    fill='Position'
  ) + 
  geom_label_repel(
    data=filter(boxplot_labels), 
    aes(x=`Pos.x`, y=`X3PA.y`, label=Player),
    size=3,
    label.r=0.35,
    point.padding=0.5,
    segment.alpha=0.5
  ) +
  theme_borders

#3PM x 3P%
ggplot(data=merged) + 
  geom_vline(xintercept=82, linetype='dashed', color='red') +
  geom_point(aes(
    x=`3P.y`, 
    y=`3P%.x`, 
    fill=`3PA.y`
  ), size=5, pch=21) + 
  geom_label_repel(
    data=filter(merged10, `3P%.y` > 0.45), 
    aes(x=`3P.y`, y=`3P%.x`, label=Player), 
    size=3,
    label.r=0.35,
    point.padding=0.5,
    segment.alpha=0.5
  ) +
  #scale_fill_gradient(low = "#3863ff", high = "#ab3bff") +
  labs(
    #title=expression(paste(bold("Scatterplot:"), " 3-Pointers Made vs 3-Point Percentage")),
    x='3-Pointers Made (3PM)',
    y='3-Point Percentage (3P%)',
    fill='3-Pointers \nAttempted (3PA)'
  ) +
  lims(x=c(0, 405), y=c(25, 50)) +
  theme_borders +
  scale_fill_steps(low = "#3863ff", high = "#ab3bff")

#50-40-90 Scatterplot
ggplot() + 
  #geom_vline(xintercept=50, linetype='dashed', color='red') +
  #geom_hline(yintercept=40, linetype='dashed', color='red') +
  geom_point(data=dq, aes(x=`FG%.x`, y=`3P%.x`), fill='gray', size=6, pch=21, alpha=0.1) +
  geom_point(data=q, aes(x=`FG%.x`, y=`3P%.x`, fill=`FT%.x`), size=6, pch=21, alpha=0.9) +
  geom_label_repel(
    data=club, 
    aes(x=`FG%.x`, y=`3P%.x`, label=Player), 
    size=4,
    label.r=0.35,
    point.padding=0.5,
    segment.alpha=0.5
  ) +
  #scale_fill_gradient(low='blue', high='light blue') +
  lims(
    x=c(35, 55),
    y=c(20, 50)
    #x=c(summary_fg['1st Qu.'] - iqr_fg, summary_fg['3rd Qu.'] + iqr_fg), 
    #y=c(summary_3p['1st Qu.'] - iqr_3p, summary_3p['3rd Qu.'] + iqr_3p)
  ) +
  labs(
    #title=expression(paste(bold("Scatterplot:"), " Field Goal Percentage vs 3-Point Percentage")),
    x='Field Goal Percentage (FG%)',
    y='3-Point Percentage (3P%)',
    fill='Free Throw \nPercentage (FT%)'
  ) +
  theme_borders +
  scale_fill_steps(low='blue', high='light blue')

#TS% x PER
ggplot(data=hollinger, aes(`TS%`, PER)) +
  #geom_abline(intercept=per_ts_intercept, slope=per_ts_slope, linetype='dashed', color='red') +
  #geom_segment(aes(x=`TS%`, xend=`TS%`, y=pred, yend=PER), size=0.25, alpha=0.8) +
  geom_point(aes(fill=USG), size=5, pch=21, alpha=0.9) +
  geom_label_repel(
    data=filter(hollinger, PER==31.56), 
    aes(x=`TS%`, y=PER, label=PLAYER), 
    size=3.5,
    label.r=0.35,
    point.padding=0.5,
    segment.alpha=0.5
  ) + 
  #scale_fill_gradient(low='light blue', high='blue') +
  labs(
    #title=expression(paste(bold("Scatterplot:"), " True Shooting Percentage vs Player Efficiency Rating")),
    x='True Shooting Percentage (TS%)',
    y='Player Efficiency Rating (PER)',
    fill='Usage Rate (USG%)'
  ) +
  theme_borders +
  scale_fill_steps(low='light blue', high='blue')

#TS% x PER residual plot
ggplot(data=hollinger, aes(`TS%`, res)) +
  geom_hline(yintercept=0, linetype='dashed', color='red') +
  geom_segment(data=head_res, aes(x=`TS%`, xend=`TS%`, y=0, yend=res), size=0.25, alpha=0.8) +
  geom_point(aes(fill=USG), size=5, pch=21, alpha=0.9) +
  geom_label_repel(
    data=head_res, 
    aes(x=`TS%`, y=res, label=PLAYER), 
    size=3.5,
    label.r=0.35,
    point.padding=0.5,
    segment.alpha=0.5
  ) +
  scale_fill_gradient(low='blue', high='light blue') +
  labs(
    title=expression(paste(
      bold("Residual Plot:"), 
      " True Shooting Percentage vs Player Efficiency Rating")
    ),
    x='True Shooting Percentage (TS%)',
    y='Residuals',
    fill='Usage Rate (USG%)'
  ) +
  ylim(c(-12.2, 12.2)) +
  theme_borders

#3PT Shot Distance relative frequency histogram, 3-point percentage
rel_freq <- ggplot(data=all_top10, aes(x=`Shot Distance`, y=rel, fill=name)) +
  geom_bar(
    stat="identity", 
    position=position_dodge(), 
    color='black',
    alpha=1
  ) +
  labs(
    title=expression(#paste(
      bold("Histogram A")#, 
      #" Relative Frequency of 3-Point Shot Distance")
    ),
    x='3PT Shot Distance',
    y='Relative Frequency (%)',
    fill='Player'
  ) +
  ylim(c(0, 63.5)) +
  scale_fill_manual(values=c('#b5d6ff', '#004196')) +
  theme_borders
sd_3pp <- ggplot(data=all_top10, aes(x=`Shot Distance`, y=`3P%`, fill=name)) +
  geom_bar(
    stat="identity", 
    position=position_dodge(), 
    color='black',
    alpha=1
  ) +
  labs(
    title=expression(#paste(
      bold("Histogram B")#, 
      #" 3-Point Percentage by Shot Distance")
    ),
    x='3PT Shot Distance',
    y='3-Point Percentage (3P%)',
    fill='Player'
  ) +
  ylim(c(0, 63.5)) +
  scale_fill_manual(values=c('#b5d6ff', '#004196')) +
  theme_borders
  
grid.arrange(rel_freq, sd_3pp, nrow=2)
