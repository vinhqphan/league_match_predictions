### Initialize libraries ###
suppressWarnings(suppressMessages(library(httr)))
suppressWarnings(suppressMessages(library(jsonlite)))
options(dplyr.summarise.inform = FALSE)
suppressWarnings(suppressMessages(library(tidyverse)))
suppressWarnings(suppressMessages(library(xlsx)))

### Important! REQUIRED FOR API REQUESTS! RUN THIS EVERY TIME SCRIPT IS LOADED! ###
api_key = 'RGAPI-1e7c045e-3c77-4c3c-8935-83b0126106c3'

choose_challengers <- function(num_sampled) {
  "
  A function that takes an integer, num_sampled, as input and requests a list of challenger players from Riot's API and 
  then returns a list of num_sampled randomly chosen players.
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

na_challengers = choose_challengers(300)

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

# Loop through account IDs in our NA challengers list using the match_data_collection function to
# populate a dataframe with (number of account IDS * 2) rows of match data.
# match_data = data.frame(matrix(NA, nrow = 600, ncol = 27))
# j = 1
# for (i in seq(1, nrow(match_data), 2)) {
#   single_match = match_data_collector(na_challengers[j, 3])
#   match_data[i:(i+1), ] = single_match
#   j = j + 1
#   Sys.sleep(2)
# }
colnames(match_data) = colnames(single_match)

# remaining = match_data[515:nrow(match_data), ]
# remaining_challs = na_challengers[258:nrow(na_challengers), ]
# j = 1
# for (i in seq(1, nrow(remaining), 2)) {
#   single_match = match_data_collector(remaining_challs[j, 3])
#   remaining[i:(i+1), ] = single_match
#   j = j + 1
#   Sys.sleep(1.2)
# }

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

match_data_extra = match_data %>% 
  add_column(gold_diff_10, .before = 'gold_10_mins') %>% 
  add_column(gold_diff_20, .before = 'gold_20_mins') %>% 
  add_column(gold_diff, .before = 'goldEarned') %>% 
  add_column(gold_spent_percentage_diff, .before = 'goldSpent') %>% 
  add_column(baron_diff, .before = 'baronKills') %>% 
  add_column(dragon_diff, .before = 'dragonKills')

write.xlsx(match_data_extra, 'aggregated_match_data.xlsx')

### Creating the logistic regression model ###
training_indices = sample(nrow(match_data_extra), 300)
training = match_data_extra[training_indices, ] %>% 
  select(-c(match_id))

test = match_data_extra[-training_indices, ] %>% 
  select(-c(match_id))

logistic = glm(win ~ 
                 towerKills + baronKills + deaths + gold_spent_percentage_diff,
               data = training, 
               family = 'binomial')
summary(logistic)

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
  xlab('index') +
  ylab('prob') +
  ggtitle('prob of winning vs actual win for test data') + 
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_hline(yintercept = 0.5)

table(prediction_test_data$result, prediction_test_data$win_prob > 0.5)

# get data from two teams, get their last 10 most recent matches, grab variables i want for logistic model, average those values


### TEST CODE ###
