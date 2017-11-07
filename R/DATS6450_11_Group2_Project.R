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
# allDat <- readInRawFiles(dataDir)
allDat <- data.table::fread(file.path('data', 'pokerData.csv'))

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
# Create extra columns that can be used for analysis ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Small and Big blind columns
blinds <- 
  allActs %>% 
  filter(grepl('blind', action)) %>% 
  mutate('small_blind' = ifelse(action == 'small_blind', 1, 0),
         'big_blind' = ifelse(action == 'big_blind', 1, 0)) %>% 
  select(-round, -action, -players, -ord)

# Whom bet first during the round ----
initBet <- 
  allActs %>% 
  filter(action == 'bet') %>% 
  mutate('flopInitBet' = ifelse(round == 'flop', 1, 0),
         'turnInitBet' = ifelse(round == 'turn', 1, 0),
         'riverInit' = ifelse(round == 'river', 1, 0)) %>% 
  select(-action, -round, -players, -ord)

# Num time raises the pot ----
betsRaises <- 
  allActs %>% 
  filter(action %in% c('bet', 'raise')) %>% 
  group_by(timestamp, playername) %>% 
  summarize(num_raises = n())

# Num time voluntarily puts chips in the pot ----
numVpip <- 
  allActs %>% 
  filter(action %in% c('bet', 'raise', 'call')) %>% 
  group_by(timestamp, playername) %>% 
  summarize(num_vpip = n())

# Num time check raises ----

### Create obj for later processing
checkRaiseDat <- 
  allActs %>% 
  filter(action %in% c('check', 'raise'))

### Find players with two actions in a round
twoActs <- 
  checkRaiseDat %>% 
  group_by(timestamp, round, playername) %>% 
  summarize(n = n()) %>% 
  filter(n > 1) %>% 
  select(-n)

# Run function on each two action scenario 
getCheckRaises <- function(vect) {
  # Filter data to current scenario
  actions <- 
    checkRaiseDat %>% 
    filter(timestamp == vect[['timestamp']],
           round == vect[['round']],
           playername == vect[['playername']]) %>% 
    arrange(ord) %>% 
    pull(action)
  
  # Make sure both check and raise exist, if not exit
  if (!('check' %in% actions & 'raise' %in% actions)) {
    return(NULL)
  }
  
  # Find what actions were checks and see if any of the actions right after
  # it was a raise, indicating a "check then raise"
  chks <- grep('check', actions)
  checkRaise <- any(actions[chks + 1] == 'raise')
  
  if (checkRaise) {
    return(
      tibble(timestamp = vect[['timestamp']],
           round = vect[['round']],
           playername = vect[['playername']],
           checkRaise = 1)
    )
  } else {
    return(NULL)
  }
}

# This step takes a while --- need to think of better, more efficient way
# of doing this....
checkRaises <- apply(twoActs, 1, getCheckRaises) %>% bind_rows
finalCheckRaises <- 
  checkRaises %>% 
  mutate('timestamp' = as.numeric(timestamp)) %>% # fix class for merging
  group_by(timestamp, playername) %>% 
  summarize(checkRaises = sum(checkRaise))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create final dataset for analysis ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

finalDat <- 
  allDat %>% 
  select(-flop1, -flop2, -flop3, -turn, -river, -preflopaction, -flopaction,
         -turnaction, -riveraction, -card1, -card2, -`_merge`, -gameset) %>% 
  select(timestamp, game, playername, everything()) %>% # reorder columns
  left_join(blinds, by = c('timestamp', 'playername')) %>% 
  left_join(initBet, by = c('timestamp', 'playername')) %>% 
  left_join(betsRaises, by = c('timestamp', 'playername')) %>% 
  left_join(numVpip, by = c('timestamp', 'playername')) %>% 
  left_join(finalCheckRaises, by = c('timestamp', 'playername')) %>% 
  as.tbl

data.table::fwrite(finalDat, file.path('data', 'exampleOutput.csv'))
