library(data.table)
library(tidyverse)

dataDir <- file.path('data', '199807')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Read in Files 
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
                    'winnings', 'card1', 'card2')

names(player_data) <- playerColNames
