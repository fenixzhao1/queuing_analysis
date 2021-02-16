##### data preparation #####
# load packages
library(dplyr)
library(ggplot2)
library(xtable)
library(gridExtra)
library(foreign)

# combine session data
df_session_1 = read.csv("D:/Dropbox/Working Papers/Mechanism in Queueing/data/pilot2/queuing_experiment_2021-02-12.csv", header = T)
df_trade_1 = read.csv("D:/Dropbox/Working Papers/Mechanism in Queueing/data/pilot2/queuing_experiment Events (accessed 2021-02-12).csv", header = T)

df_session = merge(df_session_1)
df_trade = merge(df_trade_1)
rm(df_session_1)
rm(df_trade_1)

# document the session data
df_session = df_session %>% select(-c(participant.label, participant._is_bot, participant._index_in_pages,
                                      participant._max_page_index, participant._current_app_name, participant._current_page_name,
                                      participant.time_started, participant.visited, participant.mturk_worker_id,
                                      participant.mturk_assignment_id, player.silo_num, player._initial_decision,
                                      group.ran_ready_function, session.label, session.mturk_HITGroupId, session.mturk_HITId,
                                      session.comment, session.is_demo))
df_session = df_session %>% mutate(session_round_group_id = paste(as.character(session.code), as.character(subsession.round_number),
                                                            as.character(group.id_in_subsession), sep = '_'))

# document the exchange data


##### Swap: acceptance, efficiency, messaging #####






