readInRawFiles <- function(dataDir) {
  # Hand Database ----
  
  hdbFile <- file.path(dataDir, 'hdb')
  hdbColNames <- c('timestamp', 'gameset', 'game', 'players',
                   'flop', 'turn', 'river', 'showdown', 'flop1', 
                   'flop2', 'flop3' ,'turn', 'river')
  
  # Read in HDB table
  hdbRaw <- data.table::fread(hdbFile, header = FALSE, fill = TRUE, 
                              col.names = hdbColNames)
  
  # Splits up flop, turn, river, and showdown 
  cleanHDB <- function(hdbRaw) {
    
    hdbTmp <- copy(hdbRaw)
    newColNames <- c('playersflop', 'potflop', 'playersturn',
                     'potturn', 'playersriver', 'potriver', 
                     'playersshowdown', 'potshowdown')
    
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
  playerColNames <- c('playername', 'timestamp', 'players', 'position', 
                      'preflopaction', 'flopaction', 'turnaction', 
                      'riveraction', 'bankroll', 'totalaction', 'won', 
                      'player_hand1', 'player_hand2')
  
  names(player_data) <- playerColNames
  
  ## make sure numeric vars are numeric
  numFunc <- function(x) ifelse(all(is.na(suppressWarnings(as.numeric(x)))), 
                                'character', 'numeric')
  
  finalPlayer <- 
    player_data %>% 
    select(-players) %>% # Drop number of players (exists in hdb)
    mutate_if(function(x) numFunc(x) == 'numeric', as.numeric)
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Merge Files ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  allDat <- merge(hdb, finalPlayer, by = 'timestamp', all.x = TRUE)
  setorder(allDat, timestamp, position)
  return(allDat)
}
