#q:  do heirs of nba players fare better in the draft?  more likely to be drafted?
#q:  how does this effect productivity?  
#q:  how does this effect lifetime earnings?  do they make more? less?

library(RKaggle)
library(tidyverse)
library(rvest)
library(stringi)
library(stringr)

### first things first, let's pull all info on secondgen nba players we can find

secondgen <- lapply('https://en.wikipedia.org/wiki/List_of_second-generation_NBA_players', function(i) {
  webpage <- read_html(i)
  tbl <- html_nodes(webpage, 'table')
  tbl_first <- html_table(tbl)[[1]]
})

# workhorse clean up function for names, etc

scrub <- function(x){
  x <- tolower(x)
  x <- stri_trans_general(x, "Latin-ASCII") # lots of int'l players have weird accents in names
  x <- gsub("[[:punct:]]", "", x) # getting rid of special chars
  x <- trimws(x, "both") # removing special chars created a whitespace, this removes
  #x <- gsub("(?<=\\b\\w)\\s(?=\\w\\b)", "", x, perl = TRUE) # for the AC greens etc
  return(x)
}


secondgen <- do.call(rbind.data.frame, secondgen)
str(secondgen)
secondgen <- secondgen |> select(1:2) # drop last two columns
colnames(secondgen) <- lapply(colnames(secondgen), scrub)

secondgen <- lapply(secondgen, scrub)
secondgen <- as.data.frame(secondgen)

# based on what i can tell there's only a few problem rows: 2, 15, 16, 29, 34, 60, 63, 87--row 2 needs two extra rows after it
prob_rows <- c(2, 15:16, 29, 34, 60, 63, 87)
print(secondgen[prob_rows,])

secondgen <- secondgen |>
  filter(!row_number() %in% prob_rows)

# this part should be in f'n but we'll do it manually for now--we'll clean it up later
barry <- data.frame(father = rep("Rick Barry", 3), sons = c("jon barry", "brent barry", "drew barry"))
curry <- data.frame(father = rep("Dell Curry", 2), sons = c("stephen curry", "seth curry"))
davis <- data.frame(father = rep("Dale Davis", 1), sons = "trayce jackson davis")
grant <- data.frame(father = rep("Harvey Grant", 2), sons = c("jerami grant", "jerian grant"))
harper <- data.frame(father = rep("Ron Harper", 2), sons = c("ron harper jr", "dylan harper"))
nance <- data.frame(father = rep("Larry Nance", 2), sons = c("larry nance jr", "pete nance"))
paxson <- data.frame(father = rep("Jim Paxson Sr", 2), sons = c("jim paxon jr", "john paxson"))
thompson <- data.frame(father = rep("Mychal Thompson", 2), sons = c("mychel thompson", "klay thompson"))

addendum <- rbind(barry, curry, davis, grant, harper, nance, paxson, thompson)
rm(barry, curry, davis, grant, harper, nance, paxson, thompson)
secondgen <- rbind(secondgen, addendum)


write.csv(secondgen, file = "secondgen_nba_2025.csv")

# now we need to scrape the rsci index and draft data from bbref
# then we get the draft combine info and we merge all of that together

start_time <- Sys.time()
jump <- seq(1998, 2025, by = 001)
site <- paste('https://www.basketball-reference.com/awards/recruit_rankings_',
              jump, '.html', sep="")

rsciList <- lapply(site, function(i) {
  webpage <- read_html(i)
  Sys.sleep(5)
  rsci_table <- html_nodes(webpage, 'table')
  draft <- html_table(rsci_table)[[1]]
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

# note players in risky index who weren't drafted but played in NBA

risky <- risky |>
  ungroup() |>
  mutate(across(everything(), as.character)) |>
  mutate(from = replace_na(from, ""),
         to = replace_na(to, ""),
         rd = replace_na(rd, ""),
         pk = replace_na(pk, ""),
         ws = replace_na(ws, ""),
         draft = replace_na(draft, ""),
         undrafted_played = ifelse(draft == "" & from != "", 1, 0))

risky <- risky |>
  mutate(across(-c(player, college, team), as.numeric))

# write a copy to a csv
write.csv(risky, file = "rsci_1998_2025.csv")

# make df of players who reclassified per risky list
# these likely reclassiied down.  later players reclassified up
# there may be a useful distinction between the two
reclassified_down <- risky |> filter(reclassified == 1)

########### pre draft combine and draft scraping ############

###### now load predraft measurement data and draft info #####

library(RKaggle)
draftcomb <- RKaggle::get_dataset("marcusfern/nba-draft-combine") |>
  rename_with(tolower) |>
  mutate(position1 = substr(pos, 1, 2),
         position2 = ifelse(nchar(pos) > 2, substr(pos, 4,5), pos)) |>
  relocate(where(is.character), .before = where(is.numeric)) |>
  select(-pos) |>
  mutate(player = gsub("^(.+)\\s(.+)$", "\\2 \\1", player))

draftcomb$player <- sapply(draftcomb$player, scrub)
draftcomb <- reclassify(draftcomb, "player")

#write to csv for easier loading later
write.csv(draftcomb, file = "draft_combine_2000_2025.csv")

# now scrape all the nba draft info:  stats stuff

library(tidyverse)
library(rvest)

jump <- seq(1976, 2024, by = 001)
site <- paste('https://www.basketball-reference.com/draft/NBA_', jump, '.html', sep="")

dfList <- lapply(site, function(i) {
  webpage <- read_html(i)
  Sys.sleep(5)
  draft_table <- html_nodes(webpage, 'table')
  draft <- html_table(draft_table)[[1]]
})

draftdf <- do.call(rbind, dfList)

names(draftdf) <- c("rank", "pick", "team", "player", "college", "years", "gp.tot",
                    "mp.tot", "pts.tot", "trb.tot", "ast.tot", "fgp", "threepp",
                    "ftp", "mpg", "ppg", "rpg", "apg", "ws", "ws48", "bpm", "vorp")

draftdf <- draftdf[-1, ]
draftdf <- draftdf |> filter(player != "" & player != "Player")
draftdf[c("team", "player", "college")] <- lapply(draftdf[c("team", "player", "college")], scrub)
draftdf[, c(1,2,6:ncol(draftdf))] <- lapply(draftdf[,c(1,2,6:ncol(draftdf))], as.numeric)

# now pull in the salary dataset

money <- RKaggle::get_dataset("loganlauton/nba-players-and-team-data")[[4]] |>
  rename_with(tolower)
str(money)

# need to validate data:  check against some names to verify accuracy
# then we will need to remove first column and change player names to lower case

money <- money[-1]

# also the names aren't great and too long:  shorten them

newcols <- c("player", "season", "salary", "adj_salary")
colnames(money) <- newcols
rm(newcols)
# remove special characters, etc via function



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

# now that we know we're still in good shape, let's move fwd
# now we need to pull in other datasets, 
  




#risky <- risky |> filter(hs_class <= 2022)





# add in df with info on reclassified players





# join risky dataset with money dataset
risky <- risky |> 
  left_join(money, by = "player", multiple = "last") |>
  ungroup()

risky[is.na(risky)] <- ""
# now join risky and draftxpr
  
jj <- left_join(risky, draftcomb, by = c("player"))






















