# q:  which school should a good high school prospect go to?
    # base this on:  total earnings, average earnings, length of career,
    # but also likelihood information:  probability of making more than X number
    # do all of this by position, so we can be specific

# q:  how should we visualize this info?

# q:  when did guards become the dominant earners in the NBA?  

# q:  using the rsci index, how much money would player ranked x make in his nba career
#     (via survival analysis)

library(tidyverse)
# load data

path <- "C:\\Users\\josep\\OneDrive\\Desktop\\NBA Data\\kaggle\\NBA Salaries(1990-2023).csv"
df <- read.csv(path, header = T, sep = ",")

str(df)

# need to validate data:  check against some names to verify accuracy
# then we will need to remove first column and change player names to lower case

df <- df[-1]
colnames(df) <- tolower(colnames(df))

# also the names aren't great and too long:  shorten them

newcols <- c("player", "season", "salary", "adj_salary")
colnames(df) <- newcols

# remove special characters, etc from

library(stringi)

scrub <- function(x){
  x <- tolower(x)
  x <- stri_trans_general(x, "Latin-ASCII") # lots of int'l players have weird accents in names
  x <- gsub("[[:punct:]]", "", x) # getting rid of special chars
  x <- trimws(x, "both") # removing special chars created a whitespace, this removes
  #x <- gsub("(?<=\\b\\w)\\s(?=\\w\\b)", "", x, perl = TRUE) # for the AC greens etc
  return(x)
}

#
df[1:4] <- lapply(df[1:4], scrub)

#convert col 2 -4 to numeric
df <- df |>
  mutate(across(c(2:4), as.numeric)) |>
  mutate(season = as.integer(season + 1)) # added 1 so the year matches draft & nba finals

# before we reshape the data, get number of unique values in df to compare after

print(n_distinct(df$player))

# reshape data from long to wide

master <- reshape(df, timevar = "season", idvar = "player", 
              v.names = c("salary", "adj_salary"), direction = "wide")

#now that we know we're still in good shape, let's move fwd
# now we need to pull in other datasets, including one with names and positions
# can use nbastatR package

library(nbastatR)
# first we should get a dictionary of players, then their bios
Sys.setenv(VROOM_CONNECTION_SIZE = 500000)
player_dict <- nba_players() |>
  arrange(namePlayer)

colnames(player_dict) <- tolower(colnames(player_dict))
player_dict <- player_dict |>
  rename(player = nameplayer, from = yearseasonfirst, to = yearseasonlast) |>
  mutate(from = from + 1) |>
  mutate(to = to +1) # need year in and yearout to reflect 2nd half of yr

player_dict$player <- sapply(player_dict$player, scrub)
  
# now we need to scrape the rsci index and draft data from bbref
# then we get the draft combine info and we merge all of that with the dict
library(rvest)

jump <- seq(1998, 2022, by = 001)
site <- paste('https://www.basketball-reference.com/awards/recruit_rankings_',
              jump, '.html', sep="")

rsciList <- lapply(site, function(i) {
  webpage <- read_html(i)
  Sys.sleep(20)
  draft_table <- html_nodes(webpage, 'table')
  draft <- html_table(draft_table)[[1]]
})

# since there are differing numbers of columns in the list 
# the early years (1998 etc) have four extra columns in them
# need first 10 columns of a list of dataframes
rscinames <- tolower(rsciList[[1]][1, 1:10])
#turn contents of list to data frames--easier to use than tibbles
risky <- lapply(rsciList, as.data.frame)
# change column names
risky <- lapply(risky, setNames, rscinames)
# extract first ten columns, and all rows of each dataframe
risky <- lapply(risky, "[", , 1:10)
# add variable denoting the hs graduation year of each player
risky <- mapply(cbind, risky, "hs_class" = jump, SIMPLIFY = F)
# drop first row and empty rows
risky <- lapply(risky, function(x) {x <- x[x$player != "" & x$player != "Player",]})
# turn list to one big, beautiful dataframe
risky <- do.call(rbind, risky)
# rename "tm" to "team"
risky <- risky |> rename("team" = "tm")
# apply scrub function to name, college and team
risky[c("player", "team", "college")] <- lapply(risky[c("player", "team", "college")], scrub)
# convert all other columns to numeric
risky <- risky |>
  mutate_at(c("rd", "pk", "from", "to", "ws", "hs_class"), as.numeric)
# need to remove "T" from rsci column--it's for ties, but we don't need it
risky <- risky |>
  mutate(rsci = gsub("T", "", rsci)) |>
  mutate(rsci = as.numeric(rsci)) |>
  mutate(rsci = case_when(rsci == 1 ~ rsci,
                          rsci == lag(rsci, 1) ~ (rsci + 1),
                          .default = rsci))
  #mutate(rsci = ifelse(rsci == lag(rsci, 1), rsci + 1, rsci))

# remove 'college' from player column
risky <- risky |>
  mutate(player = gsub("college", "", player))

###### now work on the nba draft combine data #####


















