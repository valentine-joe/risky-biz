# q:  which school should a good high school prospect go to?
    # base this on:  total earnings, average earnings, length of career,
    # but also likelihood information:  probability of making more than X number
    # do all of this by position, so we can be specific

# q:  how should we visualize this info?

# q:  using the rsci index, how much money would player ranked x make in his nba career

library(tidyverse)
# load data

path <- "C:\\Users\\josep\\OneDrive\\Desktop\\NBA Data\\kaggle\\NBA Salaries(1990-2023).csv"
money <- read.csv(path, header = T, sep = ",")

str(money)

# need to validate data:  check against some names to verify accuracy
# then we will need to remove first column and change player names to lower case

money <- money[-1]
colnames(money) <- tolower(colnames(money))

# also the names aren't great and too long:  shorten them

newcols <- c("player", "season", "salary", "adj_salary")
colnames(money) <- newcols
rm(newcols)
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
money[1:4] <- lapply(money[1:4], scrub)

#convert col 2 -4 to numeric
money <- money |>
  mutate(across(c(2:4), as.numeric)) |>
  mutate(season = as.integer(season + 1)) # added 1 so the year matches draft & nba finals

# before we reshape the data, get number of unique values in df to compare after

print(n_distinct(money$player))

# reshape data from long to wide

money <- reshape(money, timevar = "season", idvar = "player", 
              v.names = c("salary", "adj_salary"), direction = "wide")

# since the data has missing values for most entries and is sparse and 
# hard to see, sum each players adjusted salary to get the total career earnings

money <- money |>
  rowwise() |>
  mutate(adj_car_earn = sum(across(starts_with("adj")), na.rm = T),
         car_earn = sum(across(starts_with("salary")), na.rm = T)) |>
  mutate(player = trimws(player, "both")) |>
  select("player", "car_earn", "adj_car_earn")

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

start_time <- Sys.time()
jump <- seq(1998, 2022, by = 001)
site <- paste('https://www.basketball-reference.com/awards/recruit_rankings_',
              jump, '.html', sep="")

rsciList <- lapply(site, function(i) {
  webpage <- read_html(i)
  Sys.sleep(20)
  draft_table <- html_nodes(webpage, 'table')
  draft <- html_table(draft_table)[[1]]
})

end_time <- Sys.time()
print(end_time - start_time)

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

# remove 'college' from player column
risky <- risky |>
  mutate(player = gsub("college", "", player),
         player = trimws(player, "both"),
         drafted = ifelse(draft != "", 1, 0))


# there's duplicates in the risky df due to players reclassifying to later class
# likely same for draftxpr--certain players went back to college or TO college
# build function to serve both purposes

reclassify <- function(df, x, y=NULL){
  df <- df |>
    group_by(df[,c(x,y)]) |>
    arrange(df[,x], .by_group= TRUE) |>
    mutate(reclassified = ifelse(n() > 1, 1, 0)) |>
    slice_tail()
    return(df)
}

risky <- reclassify(risky, "player", "college")

###### now load nba combine data from draft express #####

draftxpr <- draft_combines(years = jump)

# on first glance, this data is fine but the combine year is off
# for example, Gilbert Arenas was drafted in 2001--I know this because
# i checked but also because i just know Arizona Guards

#colnames(draftxpr) <- tolower(colnames(draftxpr))

draftxpr <- draftxpr|>
  rename_with(tolower) |>
  rename(player = nameplayer) |>
  dplyr::select(-matches('"')) |>
  dplyr::select(1:23) |>
  dplyr::select(-c("namefirst", "namelast", "heightwoshoes", "heightwshoes",
                   "wingspan", "reachstandingo")) |>
  mutate(position1 = substr(slugposition, 1, 2),
         position2 = str_sub(slugposition, -2, -1),
         yearcombine = yearcombine - 1,
         player = sapply(player, scrub)) |>
  mutate(position1 = tolower(gsub("-", "", position1)),
         position2 = tolower(gsub("-", "", position2))) |>
  mutate(position2 = trimws(position2, "both")) |>
  select(-slugposition) |>
  relocate(where(is.character), .before = where(is.numeric)) |>
  mutate(player = trimws(player, "both")) 

# there are duplicates in this df, remove the original
# i.e. final impression matters most w.r.t. being drafted
# if it didn't, players wouldn't be back at the combine

draftxpr <- reclassify(draftxpr, "player")



# draftdupe <- draftxpr |>
#   filter(duplicated(player))
# 
# # now take the rows of draftxpr that aren't in draftdupe, and stack
# 
# draftxpr <- draftxpr |>
#   filter(!player %in% draftdupe$player) |>
#   rbind(draftdupe)
# 
# rm(draftdupe)

#join draftxpr and player_dict
draftxpr <- draftxpr |> left_join(player_dict, by = "idplayer") |>
  rename(player = player.x) |>
  select(-c(player.y, reclassify))

# join risky dataset with money dataset
risky <- risky |> 
  left_join(money, by = "player", multiple = "last") |>
  ungroup()

risky[is.na(risky)] <- ""
# now join risky and draftxpr
  
jj <- left_join(risky, draftxpr, by = c("player", "from", "to"))






















