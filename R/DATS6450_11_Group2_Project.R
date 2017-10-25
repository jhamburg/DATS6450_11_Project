library(data.table)
library(tidyverse)

dataDir <- file.path('data', '199807')

#~~~~~~~~~~~~~~~~~~~~~~~~~
# Read in Files ---- 
#~~~~~~~~~~~~~~~~~~~~~~~~~

# Hand Database
hdbFile <- file.path(dataDir, 'hdb')
hdbColNames <- c('timestamp', 'dealer', 'hand_num', 'num_players',
              'flop', 'turn', 'river', 'showdown', 'card1', 
              'card2', 'card3' ,'card4', 'card5')


readr::read_fwf(hdbFile, readr::fwf_empty(hdbFile), na = c(NA, ' '))

hdb <- data.table::fread(hdbFile, header = FALSE, fill = TRUE, 
                         col.names = hdbColNames)

# Roster Database
rosterFile <- file.path(dataDir, 'hroster')
roster <- data.table::fread(rosterFile, header = FALSE, fill = TRUE)


# Player Database
pdbDir <- file.path(dataDir, 'pdb')
playerFiles <- list.files(pdbDir)

playerColNames <- c('player', 'timestamp', 'num_players', 'position', 'preflop',
                    'flop', 'turn', 'river', 'bankroll', 'action', 
                    'winnings', 'card1', 'card2')

readPlayerDatabase <- function(player) {
  infile <- file.path(pdbDir, player)
  # numLines <- R.utils::countLines(infile)
  # # 
  # i <- numLines
  # numCols <- 0
  # while (numCols != 13 && i > 0) {
  # # while (numCols != 13 && i < numLines) {
  #   colPositions <- readr::fwf_empty(infile, n = i)
  #   numCols <- length(colPositions$begin)
  #   i <- i - 1
  #   if (i == 1) stop('Could not find just 1 column')
  # }

  # # data.table::fread(file.path(pdbDir, player), header = FALSE, fill = TRUE, 
  # #                   col.names = playerColNames)
  # 
  # readr::read_fwf(infile, colPositions, na = c(NA, ' '))
  # readr::read_table2(infile, col_names = FALSE, na = " ", guess_max = numLines)
  scan(infile, what = as.list(rep("", 13)), sep = '', fill = TRUE, 
       na.strings = "") %>% as.data.table
             # stringsAsFactors = TRUE, strip.white = FALSE)
}

player_data <- 
  lapply(playerFiles, readPlayerDatabase) %>% 
  data.table::rbindlist(use.names = TRUE)

names(player_data) <- playerColNames


c('num_players_flop', 'potsize_flop', 'num_players_turn',
'potsize_turn', 'num_players_river', 'potsize_river', 
'num_players_showdown', 'potsize_showdown')

names(hdb) <- colNames
