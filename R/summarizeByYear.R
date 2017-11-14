
playerStatsByYear <- function(dataDir) {
  months <- getListofMonths(dataDir)
  yearmon <- substring(months, 13)
  years <- substr(yearmon, 1, 4) %>% unique
  sumDat <- lapply(years, sumDataByYear) %>% bind_rows

  sumDat  
}


sumDataByYear <- function(yr) {
  monthsInYear <- grep(yr, months, value = TRUE)
  yearDat <- lapply(monthsInYear, readInMonthRawFiles) %>% bind_rows
  
  yearDat %>% 
    mutate('winner' = ifelse(won > 0, 1, 0),
           'year' = yr) %>% 
    group_by(playername, year) %>% 
    summarize(n = n(),
              wins = sum(winner)) %>% 
    arrange(-wins)
}
