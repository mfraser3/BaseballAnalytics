library(tidyverse)
library(ggpubr)
library(Fraser.Tools.Plotting)
library(gt)

runs.expectancy.matrix <- function(season, path_to_files){
 
  ## Path should contain the necessary files. For the 2022 season, for example,
  ## the path should contain all2022.csv. fields.csv is downloaded on demand
  ## from GitHub.
  
  ## Get play by play data for selected season and add appropriate headers ####
  
  data <- readr::read_csv(paste0("all", season, ".csv"), col_names = FALSE,
                          show_col_types = FALSE)
  fields <- readr::read_csv("https://raw.githubusercontent.com/beanumber/baseball_R/master/data/fields.csv")
  colnames(data) <- fields$Header
  
  data$RUNS <- with(data, AWAY_SCORE_CT + HOME_SCORE_CT)
  data$HALF.INNING <- with(data, paste(GAME_ID, INN_CT, BAT_HOME_ID))
  data$RUNS.SCORED <- with(data, (BAT_DEST_ID > 3) + (RUN1_DEST_ID > 3) + (RUN2_DEST_ID > 3) + (RUN3_DEST_ID > 3))
  
  RUNS.SCORED.INNING <- aggregate(data$RUNS.SCORED, list(HALF.INNING = data$HALF.INNING), sum)
  RUNS.SCORED.START <- aggregate(data$RUNS, list(HALF.INNING = data$HALF.INNING), "[", 1)
  
  MAX <- data.frame(HALF.INNING = RUNS.SCORED.START$HALF.INNING)
  MAX$x <- RUNS.SCORED.INNING$x + RUNS.SCORED.START$x
  data <- merge(data, MAX)
  N <- ncol(data)
  names(data)[N] <- "MAX.RUNS"
  
  data$RUNS.ROI <- with(data, MAX.RUNS - RUNS)
  
  RUNNER1 <- ifelse(is.na(as.character(data[, "BASE1_RUN_ID"])), 0, 1)
  RUNNER2 <- ifelse(is.na(as.character(data[, "BASE2_RUN_ID"])), 0, 1)
  RUNNER3 <- ifelse(is.na(as.character(data[, "BASE3_RUN_ID"])), 0, 1)
  
  get.state <- function(runner1, runner2, runner3, outs) {
    runners <- paste(runner1, runner2, runner3, sep = "")
    paste(runners, outs)
  }
  
  data$STATE <- get.state(RUNNER1, RUNNER2, RUNNER3, data$OUTS_CT)
  
  NRUNNER1 <- with(data, as.numeric(RUN1_DEST_ID == 1 | BAT_DEST_ID == 1))
  NRUNNER2 <- with(data, as.numeric(RUN1_DEST_ID == 2 | RUN2_DEST_ID == 2 | BAT_DEST_ID == 2))
  NRUNNER3 <- with(data, as.numeric(RUN1_DEST_ID == 3 | RUN2_DEST_ID == 3 | RUN3_DEST_ID == 3 | BAT_DEST_ID == 1))
  NOUTS <- with(data, OUTS_CT + EVENT_OUTS_CT)
  data$NEW.STATE <- get.state(NRUNNER1, NRUNNER2, NRUNNER3, NOUTS)
  
  data <- subset(data, (STATE != NEW.STATE) | (RUNS.SCORED > 0))
  
  data.outs <- data %>%
    dplyr::group_by(HALF.INNING) %>% 
    dplyr::summarize(Outs.Inning = sum(EVENT_OUTS_CT))
  data <- merge(data, data.outs)
  dataC <- subset(data, Outs.Inning == 3)
  
  RUNS <- with(dataC, aggregate(RUNS.ROI, list(STATE), mean))
  
  RUNS$Outs <- substr(RUNS$Group, 5, 5)
  RUNS <- RUNS[order(RUNS$Outs), ]
  
  RUNS.out <- matrix(round(RUNS$x, 2), 8, 3)
  dimnames(RUNS.out)[[2]] <- c(0, 1, 2)
  dimnames(RUNS.out)[[1]] <- c("Bases Empty", "Third", "Second", "Second and Third", "First", "First and Third",
                               "First and Second", "Bases Loaded")
  
  RUNS.out
  
}


df <- runs.expectancy.matrix(2022, getwd())
season <- 2022

runs.expectancy.matrix.table <- function(season, df) {
  
  df <- as.data.frame(df)
  df <- rownames_to_column(df, var = "Runners")
  tab <- gt(df)
  tab <- tab %>%
    tab_header(
      title = "Runs Expectancy Matrix",
      subtitle = ("Expected number of runs scored based on number of runners on base and number of outs in the inning.\nData from 2022 Major League Baseball season.")) %>%
    tab_spanner(label = "Runs Expected With Indicated Number of Outs",
                columns = c(2:4)) %>%
    cols_align(align = "center",
               columns = c(2:4)) %>%
    cols_label(Runners = "Position of Runners on Base",
               `0` = "0 Outs",
               `1` = "1 Out",
               `2` = "2 Outs")
  
}
tab <- runs.expectancy.matrix.table(2022, df)
tab

runs.expectancy.matrix.plot <- function(season, df) {
  df <- as.data.frame(df)
  df <- rownames_to_column(df, var = "Situation")
  df2 <- as.data.frame(df) %>%
    pivot_longer(cols = c(2:4),
                      names_to = "Outs",
                      values_to = "Runs_Expected")
  plot <- ggplot(data = df2,
                 aes(x = Situation,
                     y = Runs_Expected,
                     fill = Outs)) +
    geom_col(position = "dodge") +
    scale_x_discrete(name = "Position of Runners on Base") +
    scale_y_continuous(name = "Runs Expected") +
    scale_fill_brewer(palette = "Dark2") +
    labs(caption = "Created with data from Baseball Savant.\nCode modified from 'Analyzing Baseball Data with R' by Max Marchi and Jim Albert.") +
    fraser_theme +
    theme(
      axis.text.x = element_text(angle = 45,
                                 hjust = 1,
                                 vjust = 1),
      plot.caption.position = "plot"
    )
  
  plot
}

plot <- runs.expectancy.matrix.plot(2022, df)
plot
