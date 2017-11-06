library(data.table)
library(tidyverse)

dataDir <- file.path('data', '199807')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Read in Files ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Hand Database ----

hdbFile <- file.path(dataDir, 'hdb')
hdbColNames <- c('timestamp', 'dealer', 'hand_num', 'num_players',
              'flop', 'turn', 'river', 'showdown', 'card1', 
              'card2', 'card3' ,'card4', 'card5')

# Read in HDB table
hdbRaw <- data.table::fread(hdbFile, header = FALSE, fill = TRUE, 
                         col.names = hdbColNames)

# Splits up flop, turn, river, and showdown 
cleanHDB <- function(hdbRaw) {
  
  hdbTmp <- copy(hdbRaw)
  newColNames <- c('num_players_flop', 'potsize_flop', 'num_players_turn',
                   'potsize_turn', 'num_players_river', 'potsize_river', 
                   'num_players_showdown', 'potsize_showdown')
  
  vars <- c('flop', 'turn', 'river', 'showdown')

  for (var in vars) {
    newCols <- grep(var, newColNames, ignore.case = TRUE, value = TRUE)
    hdbTmp <- tidyr::separate_(hdbTmp, var, newCols, sep = '/')
  }
  
  return(hdbTmp)
}

hdb <- cleanHDB(hdbRaw)


# Roster Database ----

rosterFile <- file.path(dataDir, 'hroster')
roster <- data.table::fread(rosterFile, header = FALSE, fill = TRUE)


# Player Database ----

# Gets a list of all player files since each player's data is in a separate file
pdbDir <- file.path(dataDir, 'pdb')
playerFiles <- list.files(pdbDir)

# Read in player data
readPlayerDatabase <- function(player) {
  infile <- file.path(pdbDir, player)
  scan(infile, what = as.list(rep("", 13)), sep = '', fill = TRUE, 
       na.strings = "") %>% as.data.table
}

player_data <- 
  lapply(playerFiles, readPlayerDatabase) %>% 
  data.table::rbindlist(use.names = TRUE)

# Update column names
playerColNames <- c('player', 'timestamp', 'num_players', 'position', 'preflop',
                    'flop', 'turn', 'river', 'bankroll', 'action', 
                    'winnings', 'player_hand1', 'player_hand2')

names(player_data) <- playerColNames

## make sure numeric vars are numeric
numFunc <- function(x) ifelse(all(is.na(suppressWarnings(as.numeric(x)))), 
                              'character', 'numeric')

finalPlayer <- 
  player_data %>% 
  select(-num_players) %>% # Drop number of players (exists in hdb)
  mutate_if(function(x) numFunc(x) == 'numeric', as.numeric)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Merge Files ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

allDat <- merge(hdb, finalPlayer, by = 'timestamp', all.x = TRUE)
setorder(allDat, timestamp, position)

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

# Create steps in the game. To do that will cycle through each 
# betting timeframe (preflop, flop, etc..) and split out actions into multiple
# columns. After deleting out no-action and stacking columns underneath each
# other, it should be in order.
createActionPerRound <- function(round) {
  
  # split up actions during the round
  splitActs <- 
    stringr::str_split_fixed(allDat[[round]], pattern = "", n = Inf)
  colnames(splitActs) <- paste0('round', seq(1, ncol(splitActs)))
  playActDt <- data.frame('timestamp' = allDat$timestamp,
                          'num_players' = allDat$num_players,
                          'player' = allDat$player, 
                          'position' = allDat$position,
                          splitActs, 
                          stringsAsFactors = FALSE)
  # order the actions
  byAct <- 
    playActDt %>% 
    melt(id.vars = c('timestamp', 'num_players', 'player', 'position'), 
         value.name = 'action') %>% 
    filter(!(action %in% c("", "-"))) %>% 
    mutate(ord = (as.numeric(substr(variable, 6, 6)) * num_players) 
                            + position) %>% 
    select(-variable) %>% 
    mutate('round' = round,
           'action' = actions[action]) %>%
    mutate('action' = case_when(
                position == 1 & action == 'blind' ~ 'small_blind',
                position == 2 & action == 'blind' ~ 'big_blind',
                TRUE                              ~ action
                )) %>% 
    select(-position) %>% 
    unique
  
  return(byAct)
}
  
# Create a dataset of actions per each betting round
allActs <- 
  lapply(rounds, createActionPerRound) %>% bind_rows %>% as.tbl

# Update order variable to account for differences in round and sort dataset
allActs <- 
  allActs %>% 
  mutate(ord = case_when(
            round == 'flop' ~ ord + 100,
            round == 'turn' ~ ord + 200,
            round == 'river' ~ ord + 300,
            TRUE             ~ ord
            )) %>% 
  arrange(timestamp, ord)
