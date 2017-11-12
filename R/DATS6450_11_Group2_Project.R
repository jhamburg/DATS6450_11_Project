library(data.table)
library(tidyverse)
library(corrplot)
library(Boruta)

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

# Whom bet first during the round ----
initBet <- 
  allActs %>% 
  filter(action == 'bet') %>% 
  mutate('flopInitBet' = ifelse(round == 'flop', 1, 0),
         'turnInitBet' = ifelse(round == 'turn', 1, 0),
         'riverInitBet' = ifelse(round == 'river', 1, 0)) %>% 
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
  summarize(num_checkRaises = sum(checkRaise))

# Given that the amount of money in each round is highly correlated with 
# one another, will drop those variables in lieu of an average amount added
# per round 
avgBet <-
  allDat %>%
  select(timestamp, potflop, potturn, potriver, potshowdown) %>% 
  mutate(betTurn = potturn - potflop,
         betRiver = potriver - potturn, 
         betShowdown = potshowdown - potriver) %>% 
  group_by(timestamp) %>% 
  summarise(avgBet = mean(c(potflop, betTurn, betRiver, betShowdown)))

# Will remove hands where everyone folds and the big blind wins
allFoldHands <-
  allActs %>% 
  group_by(timestamp, players) %>% 
  summarise(n = n()) %>% 
  filter(players + 1 == n) %>% 
  pull(timestamp)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create final dataset for analysis ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Will join over calculated columns, convert mising data to 0, create 
# dependent variable winner, and drop the following variables:
#   - the cards since looking for betting strategy independent of cards
#     * flop1, flop2, flop3, turn, river, card1, card2
#   - action variables since accounted for in calculated variables
#     * preflopaction, flopaction, turnaction, riveraction
#   - extra player count variables --- all of the variables will be highly 
#     correlated so should only keep 1. Keeping flop count to indicate # of 
#     players that sees cards
#     * players, playersturn, playersriver, playersshowdown
#   - most money variables, since strategy should be independent of money
#     * potflop, potturn, potriver, potshowdown, bankroll, totalaction
#   - extra variables 
#     * _merge, gameset, won (already accounted for in winning)

cleanedDat <-
  allDat %>%
  filter(!(timestamp %in% allFoldHands)) %>% 
  mutate('winner' = ifelse(won > 0, 1, 0)) %>% 
  select(-flop1, -flop2, -flop3, -turn, -river, -preflopaction, -flopaction,
         -turnaction, -riveraction, -card1, -card2, -`_merge`, -gameset,
         -players, -playersturn, -playersriver, -playersshowdown, -potflop,
         -potturn, -potriver, -potshowdown, -totalaction, -bankroll, -won) %>%
  select(timestamp, game, playername, everything()) %>% # reorder columns
  left_join(initBet, by = c('timestamp', 'playername')) %>% 
  left_join(betsRaises, by = c('timestamp', 'playername')) %>% 
  left_join(numVpip, by = c('timestamp', 'playername')) %>% 
  left_join(finalCheckRaises, by = c('timestamp', 'playername')) %>%
  left_join(avgBet, by = 'timestamp') %>% 
  mutate_all(function(x) ifelse(is.na(x), 0, x)) %>%
  select(-timestamp, -game) %>% # remove time variables
  as.tbl

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Exploratory Analysis ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# check to see the distribution of hands player by player and games won by player
playerStats <- 
  cleanedDat %>% 
  group_by(playername) %>% 
  summarize(handsPlayed = n(),
            gamesWon = sum(winner))

otherPlayerStats <- 
  cleanedDat %>%
  select(-winner) %>% 
  group_by(playername) %>% 
  summarize_all(c("mean", "median"))

ggplot(playerStats, aes(handsPlayed)) +
  geom_density()

ggplot(playerStats, aes(gamesWon)) +
  geom_density()


# The majority of players won less than 100 hands. Will keep only
# those with over 100 wins to make sure there is a good sample of 
# wins amongst the remaining players
playersWith100Wins <- 
  playerStats %>% 
  filter(gamesWon > 100) %>% 
  pull(playername)


# Create factor var object for later
factorVars <- c('flopInitBet', 'turnInitBet', 'riverInitBet', 'winner')
charVars <- c('playername')

filteredDat <- 
  cleanedDat %>% 
  filter(playername %in% playersWith100Wins) %>% 
  select(-one_of(charVars))

# Look at correlation plot for numeric variables. Will leave binary variables in
filteredDat %>% 
  as.matrix %>% 
  cor %>% 
  corrplot(method = 'ellipse')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Feature Selection ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Check normal logistic regression
checkLogRegForVars <- function(dat) {
  # Original model with all variables
  logRegMod <- glm(winner ~ ., family = 'binomial',
                   data = dat)
  # select sig. variables
  sumLogReg <- summary(logRegMod)
  print(sumLogReg)
  
  toselect.x <- sumLogReg$coeff[-1,4] < 0.01
  relevant.x <- names(toselect.x)[toselect.x == TRUE] 
  
  # formula with only sig variables
  sig.formula <- as.formula(paste("winner ~",paste(relevant.x, collapse= "+")))
  sig.model <- glm(formula = sig.formula, family = 'binomial',
                   data = dat)
  
  print(summary(sig.model))
  return(relevant.x)
}

fixedFactorDat <- 
  filteredDat %>% 
  mutate_at(vars(one_of(factorVars)), as.factor)

sigVars1 <- checkLogRegForVars(filteredDat)

# Feature selection through Boruta
borutaOut <- Boruta(winner ~ .,
                    data = fixedFactorDat,
                    doTrace = 2)

plot(borutaOut)

# collect Confirmed and Tentative variables
borutaSignif <- 
  borutaOut$finalDecision[borutaOut$finalDecision %in% 
                                c("Confirmed", "Tentative")] %>% 
  names

print(borutaSignif) 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create final dataset ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
finalDat <-
  cleanedDat %>% 
  filter(playername %in% playersWith100Wins)

data.table::fwrite(finalDat, file.path('data', 'exampleOutput.csv'))

