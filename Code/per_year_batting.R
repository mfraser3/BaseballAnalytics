

library(tidyverse)
library(Lahman)

wOBA.Weights <- read_csv(paste0(working_dir, "/wOBA_Weights.csv"))

# Per year summary data over all seasons for all position players
Batting.Summary.By.Season <- function() {
  Batting %>%
  group_by(playerID, yearID) %>%
  summarise(across(.cols = 4:20, sum, na.rm = FALSE)) %>%
  replace(is.na(.), 0) %>%
  mutate(BA = ifelse(AB > 0, round(H/AB, 3), NA),
         PA = AB + BB + HBP + SH + SF,
         TB = H + X2B + 2 * X3B + 3 * HR,
         SLG = ifelse(AB > 0, round(TB/AB, 3), NA),
         OBP = ifelse(PA > 0,
                      round((H + BB + HBP)/(PA - SH), 3), NA),
         OPS = round(OBP + SLG, 3),
         BABIP = ifelse(AB > 0, round((H - HR)/(AB - SO - HR + SF), 3), NA)) %>%
  left_join(People %>% dplyr::select(playerID, nameFirst, nameLast), by = "playerID") %>%
  relocate(nameFirst, .after = playerID) %>%
  relocate(nameLast, .after = nameFirst) %>%
  left_join(wOBA.Weights, by = "yearID") %>%
  mutate(wOBA = ((wBB * BB) + (wHBP * HBP) + (w1B * (H - X2B - X3B - HR)) + (w2B * X2B) + (w3B * X3B) + (wHR * HR))/(AB + BB - IBB + SF + HBP),
         wRAA = (((wOBA - league.wOBA)/wOBAScale)) * PA,
         wRC = (((wOBA - league.wOBA)/wOBAScale) + (RperPA)) * PA,
         `OPS+` = 100 * ((OBP/mean(OBP, na.rm = TRUE)) + (SLG/mean(SLG, na.rm = TRUE)) - 1))
}

x11(type = "cairo", antialias = "subpixel", width = 10, height = 10)

Season.Z.wOBA.Plot <- season.Z.wOBA %>%
  filter(pid != "Other") %>%
  ggplot(aes(x = pid,
             y = wRC, group = pid,
             color = pid)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1) +
  geom_text_repel(aes(label = yearID)) +
  stat_boxplot(geom = "errorbar") +
  scale_color_manual(name = "Player",
                     values = fraser_colours_diverging) +
  scale_y_continuous(name = "wRC") +
  scale_x_discrete(name = "Player") +
  labs(title = "This is a title",
       subtitle = "This is a subtitle",
       caption = "This is a caption") +
  fraser_theme

Season.Z.wOBA.Plot

main.title <- "This is a title"
main.subtitle <- "This is a subtitle"
main.caption <- "This is a caption"
x.axis.title <- "Player"
y.axis.title <- "wOBA Z-Score"
y.axis.breaks <- c(-2.5, 0, 2.5, 5, 7)
fraser.boxplot <- function(data, x, y, group) {
  ggplot(data = {{data}},
         aes(x = {{x}},
             y = {{y}},
             group = {{group}})) +
    geom_boxplot(outlier.shape = NA) +
    geom_jitter(width = 0.1) +
    stat_boxplot(geom = "errorbar") +
    labs(title = main.title,
         subtitle = main.subtitle,
         caption = main.caption) +
    scale_x_discrete(name = x.axis.title) +
    scale_y_continuous(name = y.axis.title,
                       breaks = y.axis.breaks) +
    fraser_theme
}
test <- fraser.boxplot(season.Z.wOBA, x = pid, y = Z.wOBA, group = pid)
test


opsplus.plot <- fraser.boxplot()