### Initialize libraries ----
suppressWarnings(suppressMessages(library(httr)))
suppressWarnings(suppressMessages(library(jsonlite)))
options(dplyr.summarise.inform = FALSE)
suppressWarnings(suppressMessages(library(tidyverse)))
suppressWarnings(suppressMessages(library(ggpubr)))
suppressWarnings(suppressMessages(library(readr)))

### Important! REQUIRED FOR API REQUESTS! RUN THIS EVERY TIME SCRIPT IS LOADED! ----
api_key = 'ENTER IN USER GENERATED API KEY'

### Function Creation for Data Collection ----
choose_challengers <- function(num_sampled) {
  "
  A function that takes an integer, num_sampled, as input and requests a list of challenger players in the 
  North American region from Riot's API and then returns a list of num_sampled randomly chosen players.
  "
  
  # Do an API request for challenger players in NA.
  na_challenger = GET(paste('https://na1.api.riotgames.com/lol/league/v4/challengerleagues/by-queue/RANKED_SOLO_5x5?api_key=', 
                            api_key, 
                            sep = ''))
  # Obtain each player's summoner ID and name and add an empty column for account ID.
  dat = fromJSON(rawToChar(na_challenger$content))
  players = dat$entries[, 1:2]
  players['accountId'] = NA
  
  # Randomly select num_sampled challenger players
  randomly_selected_players = sample_n(players, num_sampled)
  
  # Loop through each player and perform an API request for their account ID.
  for (i in 1:nrow(randomly_selected_players)) {
    single_player = GET(paste('https://na1.api.riotgames.com/lol/summoner/v4/summoners/',
                              randomly_selected_players[i, 1],
                              '?api_key=',
                              api_key,
                              sep = ''))
    account_id = fromJSON(rawToChar(single_player$content))$accountId
    randomly_selected_players[i, 3] = account_id
    Sys.sleep(1.2)
  }
  
  return(randomly_selected_players)
}

# We then use the account ID from our list of NA challengers to get a random match from their match history
# for ranked 5v5 games. We then process the data from the match by extracting our desired variables for both the winning and 
# losing team and then storing this data in a new dataframe. The end result should be a dataframe with 2 rows and 27 columns
# where each row corresponds to data for the winning team in the match and data for the losing team in the match.
match_data_collector <- function(account_id) {
  "
  A function that takes as input a player's account ID. It requests their match history for ranked solo/duo queue games, chooses
  a random match and then forms a dataframe with two rows that contains aggregated team data for the winning and losing team 
  in that match.
  "
  
  # Do an API request for the player's match list using account ID.
  matchlist = GET(paste('https://na1.api.riotgames.com/lol/match/v4/matchlists/by-account/',
                        account_id,
                        '?queue=420&api_key=',
                        api_key,
                        sep = ''))
  
  # Obtain match ID of every match in the matchlist.
  match_ids = fromJSON(rawToChar(matchlist$content))$matches[, 2]
  
  # Sample a random match to obtain data for
  # Note, some matches do not last a long time and may be missing certain necessary data. To overcome this issue,
  # run a while loop, sample a random match and see if that match has missing data. If it does, resample another match, 
  # if it does not, exit the while loop and begin collecting data from that sampled match.
  while (TRUE) {
    match_index = sample(1:length(match_ids), 1)
    random_match = match_ids[match_index]
    match = GET(paste('https://na1.api.riotgames.com/lol/match/v4/matches/',
                      random_match,
                      '?api_key=',
                      api_key,
                      sep = ''))
    match_details = fromJSON(rawToChar(match$content))
    if (typeof(match_details$participants$timeline) != 'list') {
      Sys.sleep(0.05)
      next
    }
    table_check = do.call(data.frame, match_details$participants$timeline)
    
    if ('goldPerMinDeltas.0.10' %in% colnames(table_check) & 'goldPerMinDeltas.10.20' %in% colnames(table_check)) {
      break 
    } else {
      Sys.sleep(0.05)
      next
    }
  }
  
  # Filter out match_details, obtain variables of interest, form final dataframe containing variables of interest for both
  # winning and losing team
  team_data_1 = match_details[['teams']] %>% select(win, teamId, firstBlood, firstTower, firstInhibitor,
                                                    firstBaron, firstDragon, firstRiftHerald, towerKills, inhibitorKills,
                                                    baronKills, dragonKills, riftHeraldKills)
  
  team_data_2_stats = do.call(data.frame, match_details$participants$stats) %>% 
    select(-participantId)
  team_data_2 = dplyr::bind_cols(match_details$participants[, 1:5],
                                 team_data_2_stats)
  team_data_2 = team_data_2 %>% group_by(teamId) %>% 
    summarise(kills = sum(kills),
              deaths = sum(deaths),
              goldEarned = sum(goldEarned),
              goldSpent = sum(goldSpent),
              gold_efficiency = sum(goldEarned) - sum(goldSpent),
              totalDamageDealt = sum(totalDamageDealt),
              totalDamageDealtToChampions = sum(totalDamageDealtToChampions),
              visionScore = sum(visionScore),
              totalMinionsKilled = sum(totalMinionsKilled),
              neutralMinionsKilledTeamJungle = sum(neutralMinionsKilledTeamJungle),
              neutralMinionsKilledEnemyJungle = sum(neutralMinionsKilledEnemyJungle)) %>% 
    select(-teamId)
  
  team_data_3 = do.call(data.frame, match_details$participants$timeline) %>% 
    add_column(teamId = c(rep(100, 5), rep(200, 5)), .before = 1)
  team_data_3 = team_data_3 %>% group_by(teamId) %>% 
    summarise(gold_10_mins = sum(goldPerMinDeltas.0.10) * 10,
              gold_20_mins = (sum(goldPerMinDeltas.0.10) + sum(goldPerMinDeltas.10.20)) * 10) %>% 
    select(-teamId)
  
  aggregated_data = dplyr::bind_cols(team_data_1, team_data_2, team_data_3) %>% 
    relocate(gold_10_mins:gold_20_mins, .before = goldEarned) %>% 
    add_column(match_id = rep(random_match), .before = 1)
  
  return(aggregated_data)
}

### Data Collection ----

# Loop through account IDs in our NA challengers list using the match_data_collection function to
# populate a dataframe with (number of account IDS * 2) rows of match data.

na_challengers = choose_challengers(300)
match_data = data.frame(matrix(NA, nrow = 600, ncol = 27))
j = 1
for (i in seq(1, nrow(match_data), 2)) {
  single_match = match_data_collector(na_challengers[j, 3])
  match_data[i:(i+1), ] = single_match
  j = j + 1
  Sys.sleep(2)
}
colnames(match_data) = colnames(single_match)

# Change categorical variables into factors in order to be used as binary explanatory variables in model.
match_data$teamId = ifelse(match_data$teamId == 100, 'Blue', 'Red')
match_data$win[match_data$win == 'Fail'] = 'Loss'

match_data$win = as.factor(match_data$win)
match_data$teamId = as.factor(match_data$teamId)
match_data$firstBlood = as.factor(match_data$firstBlood)
match_data$firstTower = as.factor(match_data$firstTower)
match_data$firstInhibitor = as.factor(match_data$firstInhibitor)
match_data$firstBaron = as.factor(match_data$firstBaron)
match_data$firstDragon = as.factor(match_data$firstDragon)
match_data$firstRiftHerald = as.factor(match_data$firstRiftHerald)

# Create some additional variables for experimentation in logistic regression model.
gold_diff_10 = c()
for (i in seq(1, nrow(match_data), 2)) {
  diff1 = match_data[i, c('gold_10_mins')] - match_data[(i+1), c('gold_10_mins')]
  gold_diff_10 = append(gold_diff_10, c(diff1, -diff1))
}

gold_diff_20 = c()
for (i in seq(1, nrow(match_data), 2)) {
  diff1 = match_data[i, c('gold_20_mins')] - match_data[(i+1), c('gold_20_mins')]
  gold_diff_20 = append(gold_diff_20, c(diff1, -diff1))
}

gold_diff = c()
for (i in seq(1, nrow(match_data), 2)) {
  diff1 = match_data[i, c('goldEarned')] - match_data[(i+1), c('goldEarned')]
  gold_diff = append(gold_diff, c(diff1, -diff1))
}

gold_spent_percentage_diff = c()
for (i in seq(1, nrow(match_data), 2)) {
  diff1 = (match_data[i, c('goldSpent')] - match_data[(i+1), c('goldSpent')]) / 
    mean(c(match_data[i, c('goldSpent')], match_data[(i+1), c('goldSpent')]))
  gold_spent_percentage_diff = append(gold_spent_percentage_diff, c(diff1, -diff1))
}

baron_diff = c()
dragon_diff = c()
for (i in seq(1, nrow(match_data), 2)) {
  diff1 = match_data[i, c('baronKills')] - match_data[(i+1), c('baronKills')]
  diff2 = match_data[(i+1), c('baronKills')] - match_data[i, c('baronKills')]
  diff3 = match_data[i, c('dragonKills')] - match_data[(i+1), c('dragonKills')]
  diff4 = match_data[(i+1), c('dragonKills')] - match_data[i, c('dragonKills')]
  
  baron_diff = append(baron_diff, c(diff1, diff2))
  dragon_diff = append(dragon_diff, c(diff3, diff4))
}

# Add all additional variables to match data frame and export as csv.
match_data_extra = match_data %>% 
  add_column(gold_diff_10, .before = 'gold_10_mins') %>% 
  add_column(gold_diff_20, .before = 'gold_20_mins') %>% 
  add_column(gold_diff, .before = 'goldEarned') %>% 
  add_column(gold_spent_percentage_diff, .before = 'goldSpent') %>% 
  add_column(baron_diff, .before = 'baronKills') %>% 
  add_column(dragon_diff, .before = 'dragonKills')

write_csv(match_data_extra, 'match_data/aggregated_match_data.csv')

### Creating the logistic regression model ----

# Split data into training/test set
training_indices = sample(nrow(match_data_extra), 300)

training = match_data_extra[training_indices, ] %>% 
  select(-c(match_id))
test = match_data_extra[-training_indices, ] %>% 
  select(-c(match_id))

# The logistic regression model
logistic = glm(win ~ 
                 towerKills + baronKills + deaths + gold_spent_percentage_diff,
               data = training, 
               family = 'binomial')
summary(logistic)

# Use model to make predictions on test and training set. Graph the S-curve and make confusion matrices for both.
# Test Set
pred_test = predict(logistic, test, type = 'response')

prediction_test_data = data.frame(
  win_prob = pred_test,
  result = test$win
)

prediction_test_data = prediction_test_data[
  order(prediction_test_data$win_prob), ]
prediction_test_data$rank = 1:nrow(prediction_test_data)


ggplot(data = prediction_test_data, aes(x = rank, y = win_prob)) +
  geom_point(aes(color = result), alpha = 1, shape = 4, stroke = 2) +
  scale_fill_discrete(guide = guide_legend(reverse = TRUE)) + 
  xlab('Index') +
  ylab('Probability') +
  ggtitle('Probability of Winning vs Actual Win Result (Test Set)') + 
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_hline(yintercept = 0.5)

table(prediction_test_data$result, prediction_test_data$win_prob > 0.5)

# Training Set
pred_training = predict(logistic, training, type = 'response')

prediction_training_data = data.frame(
  win_prob_training = pred_training,
  result_training = training$win
)

prediction_training_data = prediction_training_data[
  order(prediction_training_data$win_prob_training), ]
prediction_training_data$rank = 1:nrow(prediction_training_data)

ggplot(data = prediction_training_data, aes(x = rank, y = win_prob_training)) +
  geom_point(aes(color = result_training), alpha = 1, shape = 4, stroke = 2) +
  scale_fill_discrete(guide = guide_legend(reverse = TRUE)) + 
  xlab('Index') +
  ylab('Probability') +
  ggtitle('Probability of Winning vs Actual Win Result (Train)') + 
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_hline(yintercept = 0.5)

table(prediction_training_data$result, prediction_training_data$win_prob_training > 0.5)

### Condorcet Section ----

# C9 Data (recent 10 games)
c9 = data.frame('win' = as.factor(c('Loss', 'Loss', 'Win', 'Loss', 'Win',
                                    'Win', 'Win', 'Loss', 'Loss', 'Win')),
                'towerKills' = c(1, 6, 8, 1, 9,
                                 10, 8, 4, 6, 7),
                'baronKills' = c(0, 2, 1, 0, 1,
                                 2, 0, 0, 1, 0),
                'deaths' = c(18, 18, 5, 13, 15,
                             9, 20, 11, 13, 15),
                'gold_spent_percentage_diff' = c(-0.120482, 0.006711, 0.1409, -0.1027, 0.14167,
                                                 0.18959, -0.08354, 0.027778, 0.02403, -0.02549)
)
write_csv(c9, 'match_data/c9_recent10.csv')

# TSM Data (recent 10 games)
tsm = data.frame('win' = as.factor(c('Win', 'Loss', 'Loss', 'Win', 'Win',
                                     'Win', 'Win', 'Loss', 'Loss', 'Win')),
                 'towerKills' = c(11, 2, 6, 10, 11,
                                  10, 8, 4, 1, 9),
                 'baronKills' = c(1, 0, 0, 3, 2,
                                  2, 1, 0, 0, 1),
                 'deaths' = c(11, 19, 14, 7, 4,
                              9, 13, 14, 17, 10),
                 'gold_spent_percentage_diff' = c(0.137324, -0.123377, 0.021475, 0.2238, 0.13628,
                                                  0.065606, -0.00771, -0.15525, -0.091743, 0.114379)
)
write_csv(tsm, 'match_data/tsm_recent10.csv')

# Apply logistic regression model to C9 & TSM data to generate win probabilities for the 10 most recent matches for both teams.
c9_win_prob = predict(logistic, c9, type = 'response')
tsm_win_prob = predict(logistic, tsm, type = 'response')

# Average the win probability of last 10 matches for each team to obtain win probability for a future match.
c9_avg = round(mean(c9_win_prob), 3)
tsm_avg = round((tsm_win_prob), 3)

# Combine win probabilities of both teams for use in Condorcet's model using special method.
c9_beat_tsm = round((c9_avg * (1-tsm_avg)) / ((c9_avg * (1-tsm_avg)) + ((1-c9_avg) * tsm_avg)), 3)
tsm_beat_c9 = round(((1-c9_avg) * tsm_avg) / ((c9_avg * (1-tsm_avg)) + ((1-c9_avg) * tsm_avg)), 3)

# Create binomial pmfs of different series length with probability = c9_beat_tsm
pmf_list = list()
for (i in c(3, 7, 11, 101)){
  if (i == 101) {
    x = 0:i
    binom_dat = data.frame(matches_won = factor(x), prob = dbinom(x, size = i, prob = c9_beat_tsm))
    
    my_plot = ggplot(binom_dat, aes(x = matches_won, y = prob)) +
      geom_bar(stat = 'identity', col = 'dodgerblue1', fill = 'dodgerblue1') +
      scale_x_discrete(breaks = seq(0, 101, by = 10)) +
      labs(title = paste('Binomial Distribution with n =', i),
           subtitle = paste('Bin(', i, ', ', c9_beat_tsm, ')', sep = ''),
           x = '# Matches Won',
           y = 'Probability')
  } else {
    x = 0:i
    binom_dat = data.frame(matches_won = factor(x), prob = dbinom(x, size = i, prob = c9_beat_tsm))
    
    my_plot = ggplot(binom_dat, aes(x = matches_won, y = prob)) +
      geom_bar(stat = 'identity', col = 'dodgerblue1', fill = 'dodgerblue1') +
      labs(title = paste('Binomial Distribution with n =', i),
           subtitle = paste('Bin(', i, ', ', c9_beat_tsm, ')', sep = ''),
           x = '# Matches Won',
           y = 'Probability')
  }
  
  pmf_list = c(pmf_list, list(my_plot))
}
ggarrange(pmf_list[[1]], pmf_list[[2]], pmf_list[[3]], pmf_list[[4]], ncol = 2, nrow = 2)

# Dataframe displaying probability of C9 winning the majority of their games in a best of n series
data.frame(x = c(3, 7, 11, 101), y = c(pbinom(q = 1, size = 3, prob = c9_beat_tsm, lower.tail = FALSE),
                                       pbinom(q = 3, size = 7, prob = c9_beat_tsm, lower.tail = FALSE),
                                       pbinom(q = 5, size = 11, prob = c9_beat_tsm, lower.tail = FALSE),
                                       pbinom(q = 50, size = 101, prob = c9_beat_tsm, lower.tail = FALSE))) %>% 
  rename('Series Length' = x,
         'Probability c9 wins majority of the matches' = y)
