library(data.table)
library(tidyverse)

source('R/readInRawFunctions.R')

# Raw Data Directory
dataDir <- file.path('data', '199807')

# Source in other R files
sourceDir <- function(path, trace = TRUE, ...) {
  for (nm in list.files(path, pattern = "[.][RrSsQq]$")) {
    if (grepl('DATS6450', nm)) next
    if (trace) cat(nm,":")
    source(file.path(path, nm), ...)
    if (trace) cat("\n")
  }
}
sourceDir('R')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Read in Files ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
allDat <- readInRawFiles(dataDir)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get player actions per hand ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# In this section, will get thes sequence of player actions per hand between 
# whom bet, checked, folded, etc...

# Create a vector to substitute actions
actionNames <- c('-', 'B', 'f', 'k', 'b', 'c', 'r', 'A', 'Q', 'K')
actions <- c('no_action', 'blind', 'fold', 'check', 'bet', 'call',
                 'raise', 'all_in', 'quit', 'kicked')
names(actions) <- actionNames

rounds <- c('preflop', 'flop', 'turn', 'river')
rndNums <- length(rounds)

# Get list of actions per betting round, combine into 1 table, update
# order variable to account for differences due to per round and sort
allActs <- 
  lapply(rounds, createActionPerRound) %>% 
  bind_rows %>%
  mutate(ord = case_when(
            round == 'flop' ~ ord + 100,
            round == 'turn' ~ ord + 200,
            round == 'river' ~ ord + 300,
            TRUE             ~ ord
            )) %>% 
  arrange(timestamp, ord) %>% 
  as.tbl

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create dataset that can be used for MCMC ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Small and Big blind columns
blinds <- 
  allActs %>% 
  filter(grepl('blind', action)) %>% 
  mutate('small_blind' = ifelse(action == 'small_blind', 1, 0),
         'big_blind' = ifelse(action == 'big_blind', 1, 0)) %>% 
  select(-round, -action)

# Whom bet first during the round ----
initBet <- 
  allActs %>% 
  filter(action == 'bet') %>% 
  mutate('flopBet' = ifelse(round == 'flop', 1, 0),
         'turn' = ifelse(round == 'turn', 1, 0),
         'river' = ifelse(round == 'river', 1, 0)) %>% 
  select(-action, -round)

# Num time voluntarily puts chips into the pot ----
betsRaises <- 
  allActs %>% 
  filter(action %in% c('bet', 'raise')) %>% 
  group_by(timestamp, player) %>% 
  summarize(num_raises = n())

