readInRawFiles <- function(dataDir) {
  months <- paste(dataDir, list.files(dataDir), sep = '/')
  dat <- lapply(months, readInMonthRawFiles)
  return(dat %>% bind_rows)
}

readInMonthRawFiles <- function(monthDir) {
  # helper function to make sure numeric vars are numeric
  numFunc <- function(x) ifelse(all(is.na(suppressWarnings(as.numeric(x)))), 
                                'character', 'numeric')
  
  # Hand Database ----
  hdbFile <- file.path(monthDir, 'hdb')
  hdbColNames <- c('timestamp', 'gameset', 'game', 'players',
                   'flopAct', 'turnAct', 'riverAct', 'showdownAct', 'flop1', 
                   'flop2', 'flop3' ,'turn', 'river')
  
  # Read in HDB table
  # hdbRaw <- data.table::fread(hdbFile, header = FALSE, fill = TRUE, quote = "",
                              # col.names = hdbColNames, na.strings = "")
  hdbRaw <- scan(hdbFile, what = as.list(rep("", 13)), sep = '', fill = TRUE, 
                 na.strings = "", quiet = TRUE) %>% as.data.table
  names(hdbRaw) <- hdbColNames
  
  # Splits up flop, turn, river, and showdown 
  cleanHDB <- function(hdbRaw) {
    
    hdbTmp <- data.table::copy(hdbRaw)
    newColNames <- c('playersflop', 'potflop', 'playersturn',
                     'potturn', 'playersriver', 'potriver', 
                     'playersshowdown', 'potshowdown')
    
    vars <- c('flop', 'turn', 'river', 'showdown')

    for (var in vars) {
      newCols <- grep(var, newColNames, ignore.case = TRUE, value = TRUE)
      hdbTmp <- tidyr::separate_(hdbTmp, paste0(var, 'Act'), newCols, sep = '/')
    }
    
    return(hdbTmp)
  }
  
  hdb <- 
    cleanHDB(hdbRaw) %>% 
    mutate_if(function(x) numFunc(x) == 'numeric', as.numeric)
  
  
  # Roster Database ----
  
  # rosterFile <- file.path(monthDir, 'hroster')
  # roster <- data.table::fread(rosterFile, header = FALSE, fill = TRUE)

  
  # Player Database ----
  
  # Gets a list of all player files since each player's data is in a separate file
  pdbDir <- file.path(monthDir, 'pdb')
  playerFiles <- list.files(pdbDir)
  
  # Read in player data
  readPlayerDatabase <- function(player) {
    infile <- file.path(pdbDir, player)
    scan(infile, what = as.list(rep("", 13)), sep = '', fill = TRUE, 
         na.strings = "", quiet = TRUE) %>% as.data.table
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
  
  finalPlayer <- 
    player_data %>% 
    select(-players) # Drop number of players (exists in hdb)

  # Fix numeric columns and make sure player name is character
  playerNumCols <- 
    purrr::map_lgl(finalPlayer, function(x) numFunc(x) == 'numeric')
  playerNumCols['playername'] <- FALSE 
  
  finalPlayer <-
    finalPlayer %>% 
    mutate_at(names(finalPlayer)[playerNumCols], as.numeric)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Merge Files ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  allDat <- merge(hdb, finalPlayer, by = 'timestamp', all.x = TRUE)
  setorder(allDat, timestamp, position)
  return(allDat)
}
