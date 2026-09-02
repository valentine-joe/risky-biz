############################################################
#################### NBA LINEAGE PROJECT ###################
############################################################

# Research Questions
#
# 1. Are heirs of NBA players more likely to be drafted?
# 2. Are heirs of NBA players more likely to reach the NBA?
# 3. Conditional on reaching the NBA, are they better?
# 4. Do they earn more money over their careers?
#
# Analytical Universe:
#
# ONE ROW = ONE RSCI PROSPECT
#
# All other datasets are attached as outcomes.
#
############################################################


############################################################
######################## SETUP #############################
############################################################

library(RKaggle)
library(tidyverse)
library(rvest)
library(stringi)
library(stringr)


############################################################
#################### HELPER FUNCTIONS ######################
############################################################

scrub <- function(x){
  
  x <- tolower(x)
  x <- stri_trans_general(x, "Latin-ASCII")
  x <- gsub("[^[:alnum:] -]", "", x)
  x <- trimws(x)
  
  return(x)
  
}


make_key <- function(x){
  
  x <- scrub(x)
  
  x <- gsub("-", " ", x)
  x <- gsub("\\s+", " ", x)
  x <- trimws(x)
  
  return(x)
  
}


reclassify <- function(df, ...) {
  
  df %>%
    group_by(...) %>%
    mutate(
      reclassified = as.integer(n() > 1)
    ) %>%
    slice_tail(n = 1) %>%
    ungroup()
  
}


############################################################
############### SECTION 1: SECOND-GEN DATA #################
############################################################

secondgen <- lapply(
  "https://en.wikipedia.org/wiki/List_of_second-generation_NBA_players",
  function(i){
    
    webpage <- read_html(i)
    
    tbl <- html_nodes(webpage, "table")
    
    html_table(tbl)[[1]]
    
  }
)

secondgen <- do.call(rbind.data.frame, secondgen)

secondgen <- secondgen %>%
  select(1:2)

colnames(secondgen) <- lapply(colnames(secondgen), scrub)

secondgen <- lapply(secondgen, scrub)
secondgen <- as.data.frame(secondgen)

prob_rows <- c(2, 15:16, 29, 34, 60, 63, 87)

secondgen <- secondgen %>%
  filter(!row_number() %in% prob_rows)

barry <- data.frame(
  father = rep("Rick Barry",3),
  sons = c(
    "jon barry",
    "brent barry",
    "drew barry"
  )
)

curry <- data.frame(
  father = rep("Dell Curry",2),
  sons = c(
    "stephen curry",
    "seth curry"
  )
)

davis <- data.frame(
  father = "Dale Davis",
  sons = "trayce jackson davis"
)

grant <- data.frame(
  father = rep("Harvey Grant",2),
  sons = c(
    "jerami grant",
    "jerian grant"
  )
)

harper <- data.frame(
  father = rep("Ron Harper",2),
  sons = c(
    "ron harper jr",
    "dylan harper"
  )
)

nance <- data.frame(
  father = rep("Larry Nance",2),
  sons = c(
    "larry nance jr",
    "pete nance"
  )
)

paxson <- data.frame(
  father = rep("Jim Paxson Sr",2),
  sons = c(
    "jim paxson jr",
    "john paxson"
  )
)

thompson <- data.frame(
  father = rep("Mychal Thompson",2),
  sons = c(
    "mychel thompson",
    "klay thompson"
  )
)

addendum <- rbind(
  barry,
  curry,
  davis,
  grant,
  harper,
  nance,
  paxson,
  thompson
)

secondgen <- rbind(
  secondgen,
  addendum
)

secondgen <- secondgen %>%
  mutate(
    father = make_key(father),
    sons = make_key(sons)
  )

sg_lookup <- secondgen %>%
  rename(player = sons) %>%
  mutate(secondgen = 1) %>%
  distinct(player,.keep_all = TRUE)


############################################################
############### SECTION 2: RSCI UNIVERSE ###################
############################################################

jump <- seq(1998,2025,1)

site <- paste0(
  "https://www.basketball-reference.com/awards/recruit_rankings_",
  jump,
  ".html"
)

rsciList <- lapply(
  site,
  function(i){
    
    webpage <- read_html(i)
    
    Sys.sleep(5)
    
    rsci_table <- html_nodes(webpage,"table")
    
    html_table(rsci_table)[[1]]
    
  }
)

rscinames <- tolower(
  rsciList[[1]][1,1:10]
)

risky <- lapply(rsciList, as.data.frame)

risky <- lapply(
  risky,
  setNames,
  rscinames
)

risky <- lapply(
  risky,
  "[",
  ,
  1:10
)

risky <- mapply(
  cbind,
  risky,
  hs_class = jump,
  SIMPLIFY = FALSE
)

risky <- lapply(
  risky,
  function(x){
    x[x$player != "" &
        x$player != "Player",]
  }
)

risky <- do.call(
  rbind,
  risky
)

risky <- risky %>%
  rename(team = tm)

risky[c(
  "player",
  "team",
  "college"
)] <- lapply(
  risky[c(
    "player",
    "team",
    "college"
  )],
  make_key
)

risky <- risky %>%
  mutate(
    across(
      c(
        rd,
        pk,
        from,
        to,
        ws,
        hs_class
      ),
      as.numeric
    )
  )

risky <- risky %>%
  mutate(
    rsci = gsub("T","",rsci)
  ) %>%
  mutate(
    rsci = as.numeric(rsci)
  )

risky <- risky %>%
  mutate(
    player = gsub(
      "college",
      "",
      player
    ),
    player = trimws(player)
  )

risky <- risky %>%
  reclassify(player,college)

risky <- risky %>%
  mutate(
    player_college =
      paste(
        player,
        college, sep = "_")
  )
        
rsci_universe <- risky


############################################################
############ SECTION 3: OUTCOME DATASETS ###################
############################################################

################ Draft Combine ################

draftcomb <- RKaggle::get_dataset(
  "marcusfern/nba-draft-combine"
) %>%
  rename_with(tolower)

draftcomb$player <- gsub(
  "^(.*?),\\s*(.*)$",
  "\\2 \\1",
  draftcomb$player
)

draftcomb <- draftcomb %>%
  mutate(
    position1 = substr(pos,1,2),
    position2 = ifelse(
      nchar(pos) > 2,
      substr(pos,4,5),
      pos
    )
  ) %>%
  select(-pos)

draftcomb$player <- make_key(
  draftcomb$player
)

draftcomb <- draftcomb %>%
  reclassify(player)

combine_features <- draftcomb %>%
  distinct(
    player,
    .keep_all = TRUE
  )





################ Draft Outcomes ################

jump <- seq(1998,2024,1)

site <- paste0(
  "https://www.basketball-reference.com/draft/NBA_",
  jump,
  ".html"
)

dfList <- lapply(
  site,
  function(i){
    
    webpage <- read_html(i)
    
    Sys.sleep(5)
    
    draft_table <- html_nodes(
      webpage,
      "table"
    )
    
    html_table(draft_table)[[1]]
    
  }
)

draftdf <- do.call(
  rbind,
  dfList
)

names(draftdf) <- c(
  "rank","pick","team",
  "player","college",
  "years","gp.tot",
  "mp.tot","pts.tot",
  "trb.tot","ast.tot",
  "fgp","threepp",
  "ftp","mpg","ppg",
  "rpg","apg",
  "ws","ws48",
  "bpm","vorp"
)

draftdf <- draftdf[-1,]

draftdf <- draftdf %>%
  filter(
    player != "",
    player != "Player",
    player != "round 2"
  )

draftdf$player <- make_key(
  draftdf$player
)

draftdf <- draftdf %>%
  mutate(
    across(
      c(
        pick,
        years,
        gp.tot,
        mp.tot,
        pts.tot,
        trb.tot,
        ast.tot,
        ws,
        ws48,
        bpm,
        vorp
      ),
      as.numeric
    )
  )




draft_features <- draftdf %>%
  select(
    player,
    pick,
    years,
    gp.tot,
    mp.tot,
    pts.tot,
    trb.tot,
    ast.tot,
    ws,
    ws48,
    bpm,
    vorp
  ) %>%
  rename(
    nba_ws = ws,
    nba_ws48 = ws48,
    nba_bpm = bpm,
    nba_vorp = vorp
  ) %>%
  distinct(
    player,
    .keep_all = TRUE
  )

draft_features <- draftdf %>%
  mutate(
    college = make_key(college),
    player_college =
      paste(
        player,
        college,
        sep = "_"
      )
  )


draft_features <- draft_features %>%
  select(
    -player,
    -college
  )
############################################################
################### SALARY DATA ############################
############################################################

money <- RKaggle::get_dataset(
  "loganlauton/nba-players-and-team-data"
)[[4]]

money <- money %>%
  rename_with(tolower)

# drop index column
money <- money[-1]

colnames(money) <- c(
  "player",
  "season",
  "salary",
  "adj_salary"
)

# remove dollar signs, commas, etc.
money[1:4] <- lapply(
  money[1:4],
  scrub
)

# convert to appropriate types
money <- money %>%
  mutate(
    across(
      c(
        season,
        salary,
        adj_salary
      ),
      as.numeric
    )
  ) %>%
  mutate(
    season = season + 1
  )

############################################################
################### DIAGNOSTICS ############################
############################################################

cat("\nUnique players:",
    n_distinct(money$player))

cat("\nRows:",
    nrow(money))

cat("\n")

# identify duplicate player-season records

dup_check <- money %>%
  count(
    player,
    season
  ) %>%
  filter(
    n > 1
  ) %>%
  arrange(
    desc(n)
  )

if(nrow(dup_check) > 0){
  
  cat(
    "\nWARNING: duplicate player-season records detected\n"
  )
  
  print(
    head(
      dup_check,
      20
    )
  )
  
} else {
  
  cat(
    "\nNo duplicate player-season records detected\n"
  )
  
}

############################################################
############### BUILD CAREER EARNINGS ######################
############################################################

# OPTIONAL:
# Remove perfect duplicate rows if they exist

money <- money %>%
  distinct(
    player,
    season,
    salary,
    adj_salary,
    .keep_all = TRUE
  )

money <- money %>%
  
  group_by(player) %>%
  
  summarise(
    
    car_earn =
      sum(
        salary,
        na.rm = TRUE
      ),
    
    adj_car_earn =
      sum(
        adj_salary,
        na.rm = TRUE
      ),
    
    seasons_paid =
      n_distinct(season),
    
    .groups = "drop"
    
  )

############################################################
################ QUICK SANITY CHECK ########################
############################################################

money %>%
  arrange(
    desc(adj_car_earn)
  ) %>%
  select(
    player,
    car_earn,
    adj_car_earn
  ) %>%
  head(20)

############################################################
################ FINAL MERGE DATASET #######################
############################################################

salary_features <- money

############################################################
############### SECTION 4: MASTER DATASET ##################
############################################################

master <- rsci_universe %>%
  
  left_join(
    sg_lookup,
    by = "player"
  ) %>%
  
  left_join(
    draft_features,
    by = "player_college"
  ) %>%
  
  left_join(
    combine_features,
    by = "player"
  ) %>%
  
  left_join(
    salary_features,
    by = "player"
  )

master <- master %>%
  mutate(
    
    secondgen =
      replace_na(
        secondgen,
        0
      ),
    
    drafted_flag =
      ifelse(
        !is.na(pick),
        1,
        0
      ),
    
    nba_flag =
      ifelse(
        !is.na(gp.tot) &
          gp.tot > 0,
        1,
        0
      ),
    
    earnings_flag =
      ifelse(
        !is.na(adj_car_earn) &
          adj_car_earn > 0,
        1,
        0
      ),
    
    log_earnings =
      log1p(adj_car_earn),
    
    top10 =
      ifelse(rsci <= 10,1,0),
    
    top25 =
      ifelse(rsci <= 25,1,0),
    
    top50 =
      ifelse(rsci <= 50,1,0),
    
    top100 =
      ifelse(rsci <= 100,1,0)
    
  )

master <- master %>%
  mutate(
    rsci_tier = case_when(
      rsci <= 10 ~ "Top 10",
      rsci <= 25 ~ "11-25",
      rsci <= 50 ~ "26-50",
      TRUE ~ "51-100"
    )
  )


write.csv(
  master,
  "master_nba_lineage_analysis.csv",
  row.names = FALSE
)


master <- master %>%
  rename(
    
    rsci_ws = ws.x,
    
    nba_ws = ws.y,
    
    nba_bpm = bpm,
    
    nba_vorp = vorp,
    
    nba_gp = gp.tot,
    
    nba_years = years
    
  )

master_old <- master |> filter(hs_class <= 2020)

############################################################
#################### SECTION 5 #############################
###################### ANALYSIS ############################
############################################################

################ Descriptives ################

master %>%
  summarise(
    players = n(),
    secondgen_players =
      sum(secondgen)
  )


master_old |>
  summarise(
    players = n(),
    secondgen_players = sum(secondgen)
  )
################ Draft Rates ################

draft_results <- master %>%
  group_by(secondgen) %>%
  summarise(
    n = n(),
    draft_rate =
      mean(drafted_flag)
  )

print(draft_results)

draft_results_b <- master_old |>
  group_by(secondgen) %>%
  summarise(
    n = n(),
    draft_rate =
      mean(drafted_flag)
  )

print(draft_results_b)
################ NBA Rates ################

nba_results <- master %>%
  group_by(secondgen) %>%
  summarise(
    nba_rate =
      mean(nba_flag)
  )

print(nba_results)

nba_results_b <- master_old %>%
  group_by(secondgen) %>%
  summarise(
    nba_rate =
      mean(nba_flag)
  )

print(nba_results_b)
################ Productivity ################

productivity <- master %>%
  filter(nba_flag == 1) %>%
  group_by(secondgen) %>%
  summarise(
    avg_ws = mean(nba_ws, na.rm = TRUE),
    median_ws = median(nba_ws, na.rm = TRUE),
    avg_vorp = mean(nba_vorp, na.rm = TRUE),
    avg_bpm = mean(nba_bpm, na.rm = TRUE)
  )

print(productivity)



############## rsci tier #################

master %>%
  group_by(secondgen) %>%
  summarise(
    avg_rsci = mean(rsci, na.rm = TRUE),
    median_rsci = median(rsci, na.rm = TRUE)
  )


master %>%
  group_by(rsci_tier, secondgen) %>%
  summarise(
    n = n(),
    drafted = sum(drafted_flag),
    draft_rate = mean(drafted_flag),
    .groups = "drop"
    )


master <- master %>%
  mutate(
    rsci_tier = case_when(
      rsci <= 10 ~ "Top 10",
      rsci <= 25 ~ "11-25",
      rsci <= 50 ~ "26-50",
      TRUE ~ "51-100"
    )
  )

  
tier_results <- master %>%
  group_by(rsci_tier, secondgen) %>%
  summarise(
    n = n(),
    nba_rate = mean(nba_flag),
    .groups = "drop"
  )

print(tier_results)

ggplot(
  tier_results,
  aes(
    rsci_tier,
    nba_rate,
    fill = factor(secondgen)
  )
) +
  geom_col(
    position = "dodge"
  ) +
  scale_y_continuous(
    labels = scales::percent
  )




######## use being drafted rather than simply playing in nba

master %>%
  group_by(rsci_tier, secondgen) %>%
  summarise(
    draft_rate = mean(drafted_flag),
    .groups = "drop"
  )

master %>%
  group_by(rsci_tier, secondgen) %>%
  summarise(
    n = n(),
    drafted = sum(drafted_flag),
    draft_rate = mean(drafted_flag),
    .groups = "drop"
  )




################ Earnings ################

earnings <- master %>%
  filter(nba_flag == 1) %>%
  group_by(secondgen) %>%
  summarise(
    
    mean_earnings =
      mean(
        adj_car_earn,
        na.rm = TRUE
      ),
    
    median_earnings =
      median(
        adj_car_earn,
        na.rm = TRUE
      )
    
  )

print(earnings)


################ Regressions ################

draft_model <- glm(
  drafted_flag ~ secondgen + rsci,
  family = binomial,
  data = master
)

summary(draft_model)


nba_model <- glm(
  nba_flag ~ secondgen + rsci,
  family = binomial,
  data = master
)

summary(nba_model)


ws_model <- lm(
  nba_ws ~ secondgen + rsci,
  data =
    master %>%
    filter(
      nba_flag == 1
    )
)

summary(ws_model)

master <- master %>%
  mutate(
    top25 = ifelse(rsci <= 25, 1, 0)
  )

nba_model2 <- glm(
  nba_flag ~ secondgen + top25,
  family = binomial,
  data = master
)

summary(nba_model2)

nba_model3 <- glm(
  nba_flag ~ secondgen + factor(rsci_tier),
  family = binomial,
  data = master
)

summary(nba_model3)

nba_model4 <- glm(
  nba_flag ~ secondgen*factor(rsci_tier),
  family = binomial,
  data = master
)

summary(nba_model4)

#among rsci ranked recruits from 1998-2025, second generation players reached
#the nba at substantially higher rates than comparable recruits
#this relationship remained statistically significant after controlling
#for recruiting rank and recruiting tier classification

########### using combine variables ##########

master %>%
  group_by(secondgen) %>%
  summarise(
    n_players = n(),
    height_obs = sum(!is.na(hgt)),
    wingspan_obs = sum(!is.na(wngspn)),
    vert_obs = sum(!is.na(lpvert)),
    sprint_obs = sum(!is.na(sprint))
  )

master %>%
  group_by(secondgen) %>%
  summarise(
    height = mean(hgt, na.rm = TRUE),
    wingspan = mean(wngspn, na.rm = TRUE),
    vert = mean(lpvert, na.rm = TRUE),
    sprint = mean(sprint, na.rm = TRUE)
  )

master_combine <- master %>%
  filter(!is.na(hgt))

master_combine %>%
  group_by(secondgen) %>%
  summarise(
    n = n(),
    
    height = mean(hgt, na.rm = TRUE),
    
    wingspan = mean(wngspn, na.rm = TRUE),
    
    vert = mean(lpvert, na.rm = TRUE),
    
    sprint = mean(sprint, na.rm = TRUE)
  )

master_combine %>%
  group_by(secondgen) %>%
  summarise(
    avg_rsci = mean(rsci),
    med_rsci = median(rsci),
    .groups = "drop"
  )

nba_model5 <- glm(
  nba_flag ~
    secondgen +
    factor(rsci_tier) +
    hgt +
    wngspn +
    lpvert +
    sprint,
  family = binomial,
  data = master_combine
)

summary(nba_model5)

nba_model6 <- glm(
  nba_flag ~
    secondgen +
    factor(rsci_tier),
  family = binomial,
  data = master_combine
)

summary(nba_model6)


master_old <- master %>%
  filter(hs_class <= 2020)


nba_model7 <- glm(
  nba_flag ~
    secondgen +
    factor(rsci_tier) +
    factor(hs_class),
  family = binomial,
  data = master
)

summary(nba_model7)

nba_model7b <- glm(
  nba_flag ~
    secondgen +
    factor(rsci_tier) +
    factor(hs_class),
  family = binomial,
  data = master_old
)

summary(nba_model7b)

master_combine <- master_combine %>%
  mutate(
    
    wingspan_adv =
      wngspn - hgt,
    
    plus6_wingspan =
      ifelse(
        wingspan_adv >= 6,
        1,
        0
      )
    
  )

master_combine %>%
  group_by(secondgen) %>%
  summarise(
    avg_advantage = mean(wingspan_adv, na.rm = TRUE),
    pct_plus6 = mean(plus6_wingspan, na.rm = TRUE)
  )

nba_model8 <- glm(
  
  nba_flag ~
    
    secondgen +
    factor(rsci_tier) +
    plus6_wingspan,
  
  family = binomial,
  
  data = master_combine
  
)

summary(nba_model8)

exp(coef(nba_model8))

nba_model9 <- glm(
  
  nba_flag ~
    
    secondgen +
    factor(rsci_tier) +
    wingspan_adv,
  
  family = binomial,
  
  data = master_combine
  
)

summary(nba_model9)

exp(coef(nba_model9))

# among rsci ranked recruits from 1998 - 2020 classes, second generation players
# were significantly more likely to reach the NBA.  this relationship remained strong
# after controlling for recruting tier and graduation class/cohort.  Estimated odds
# of reaching the nba were roughly seven times higher for second generation players
# than for comparable recruits who do not have NBA parentage

master_old$combine_flag <- ifelse(!is.na(master_old$hgt), 1, 0)

combine_model <- glm(
  combine_flag ~
    secondgen +
    factor(rsci_tier) +
    factor(hs_class),
  family = binomial,
  data = master_old
)

summary(combine_model)

####### productivity models ########

nba_players <- master %>%
  filter(nba_flag == 1)

ws_model <- lm(
  nba_ws ~
    secondgen +
    rsci,
  data = nba_players
)

summary(ws_model)

vorp_model <- lm(
  nba_vorp ~
    secondgen +
    rsci,
  data = nba_players
)

summary(vorp_model)

##### earnings models ######

master$log_earnings <- log(master$adj_car_earn)

nba_avg_earn <- nba_players |>
  group_by(secondgen) |>
  summarise(n = n(),
            avg_earn = mean(adj_car_earn, na.rm = TRUE))

print(nba_avg_earn)

earnings_model1 <- lm(
  log_earnings ~ secondgen + rsci,
  data = nba_players
)

summary(earnings_model1)

earnings_model2 <- lm(
  log_earnings ~ secondgen + rsci + nba_ws,
  data = nba_players
)

summary(earnings_model2)

###### career longevity ######

nba_players <- master_old %>%
  group_by(secondgen) %>%
  summarise(
    mean_games = mean(nba_gp, na.rm = TRUE),
    median_games = median(nba_gp, na.rm = TRUE),
    #mean_years = mean(years, na.rm = TRUE),
    #median_years = median(years, na.rm = TRUE),
    n = n()
  )


nba_players <- master_old |>
  filter(nba_flag == 1) |>
  dplyr::mutate(
    event =
      ifelse(
        to >= 2024,
        0,
        1
      )
  )

games_model <- lm(
  nba_gp ~ secondgen + rsci,
  data = nba_players
)

summary(games_model)

# survival analysis

library(survival)
library(survminer)

km_fit <- survfit(
  Surv(nba_gp, event) ~ secondgen,
  data = nba_players
)

ggsurvplot(
  km_fit,
  data = nba_players,
  risk.table = TRUE,
  pval = TRUE
)

summary(km_fit)$table

#log-rank test
survdiff(
  Surv(nba_gp, event) ~ secondgen,
  data = nba_players
)

cox_model <- coxph(
  Surv(nba_gp, event) ~
    secondgen +
    rsci,
  data = nba_players
)

summary(cox_model)

##### bonus:  backing out probabilities of playing in the nba based solely on rsci ran####

nba_probit <- glm(
  nba_flag ~ rsci,
  family = binomial(link = "probit"),
  data = master_old
)

summary(nba_probit)

library(pscl)

pR2(nba_probit)

pred_df <- data.frame(
  rsci = 1:100
)

pred_df$pr_nba <- predict(
  nba_probit,
  newdata = pred_df,
  type = "response"
)

head(pred_df)

library(ggplot2)

ggplot(
  pred_df,
  aes(rsci, pr_nba)
) +
  geom_line(size = 1.2) +
  labs(
    x = "RSCI Rank",
    y = "Estimated Probability of NBA Career",
    title = "Predicted NBA Probability by RSCI Rank"
  )

nba_probit2 <- glm(
  nba_flag ~ rsci + secondgen,
  family = binomial(link = "probit"),
  data = master_old
)

summary(nba_probit2)

pR2(nba_probit)


pred_heir <- expand.grid(
  rsci = 1:100,
  secondgen = 1
)

pred_nonheir <- expand.grid(
  rsci = 1:100,
  secondgen = 0
)

pred_heir$pr <- predict(
  nba_probit2,
  newdata = pred_heir,
  type = "response"
)

pred_nonheir$pr <- predict(
  nba_probit2,
  newdata = pred_nonheir,
  type = "response"
)


library(dplyr)
library(ggplot2)

pred_heir <- pred_heir %>%
  mutate(group = "NBA Heir")

pred_nonheir <- pred_nonheir %>%
  mutate(group = "Non-Heir")

pred_plot <- bind_rows(
  pred_heir,
  pred_nonheir
)

ggplot(
  pred_plot,
  aes(
    x = rsci,
    y = pr,
    color = group
  )
) +
  geom_line(linewidth = 1.2) +
  scale_x_reverse() +
  scale_y_continuous(
    labels = scales::percent
  ) +
  scale_color_manual(
    values = c(
      "Non-Heir" = "steelblue",
      "NBA Heir" = "firebrick"
    )
  ) +
  labs(
    title = "Predicted Probability of Reaching the NBA",
    subtitle = "Probit model using RSCI rank and NBA lineage",
    x = "RSCI Rank (Lower = Better Recruit)",
    y = "Predicted NBA Probability",
    color = NULL
  ) +
  theme_minimal(base_size = 14)


#equivalent rank for heirs and non-heirs
equiv_rank <- pred_heir %>%
  rowwise() %>%
  mutate(
    equivalent_nonheir_rank =
      pred_nonheir$rsci[
        which.min(
          abs(
            pred_nonheir$pr - pr
          )
        )
      ]
  ) %>%
  ungroup()

# lineage bonus
equiv_rank <- equiv_rank %>%
  mutate(
    lineage_bonus =
      rsci - equivalent_nonheir_rank
  )

# build table including nonheir prob and rsci rank
equiv_rank <- equiv_rank %>%
  mutate(
    nonheir_prob =
      pred_nonheir$pr[
        match(
          equivalent_nonheir_rank,
          pred_nonheir$rsci
        )
      ]
  ) |>
  ungroup()

### note:  model predicts some heirs have nba probabilities higher than any
### non-hear can achieve even within the top 100 rsci universe

pred_plot %>%
  filter(
    rsci %in% c(1,10,25,50,75,100)
  ) %>%
  select(
    rsci,
    group,
    pr
  )

equiv_heir <- pred_nonheir %>%
  rowwise() %>%
  mutate(
    equivalent_heir_rank =
      pred_heir$rsci[
        which.min(
          abs(
            pred_heir$pr - pr
          )
        )
      ]
  ) %>%
  ungroup()

equiv_heir %>%
  filter(
    rsci %in% c(1,10,25,50,75,100)
  ) %>%
  select(
    nonheir_rank = rsci,
    nba_prob = pr,
    equivalent_heir_rank
  )

# based on the probit model 2, an nba heir ranked approx 75th has a
# similar predicted probability of reaching the NBA as a non-heir ranked
# approximately 25th.  Across much of the RSCI distribution NBA lineage
# appears equivalent to roughly 50 ranking positions.