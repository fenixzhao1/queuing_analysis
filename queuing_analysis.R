##### data preparation #####
# load packages
library(dplyr)
library(ggplot2)
library(xtable)
library(gridExtra)
library(foreign)

# combine session data
df_session_1 = read.csv("D:/Dropbox/Working Papers/Mechanism in Queueing/data/pilot2/queuing_experiment_2021-02-12.csv", header = T, stringsAsFactors = FALSE)
df_session_2 = read.csv("D:/Dropbox/Working Papers/Mechanism in Queueing/data/pilot2/queuing_experiment_2021-02-18.csv", header = T, stringsAsFactors = FALSE)
df_trade_1 = read.csv("D:/Dropbox/Working Papers/Mechanism in Queueing/data/pilot2/queuing_experiment Events (accessed 2021-02-12).csv", header = T, stringsAsFactors = FALSE)
df_trade_2 = read.csv("D:/Dropbox/Working Papers/Mechanism in Queueing/data/pilot2/queuing_experiment Events (accessed 2021-02-18).csv", header = T, stringsAsFactors = FALSE)

df_session = rbind(df_session_1, df_session_2)
df_trade = rbind(df_trade_1, df_trade_2)
rm(df_session_1, df_session_2)
rm(df_trade_1, df_trade_2)

# add and drop variables to the session data
df_session = df_session %>% select(-c(participant.label, participant._is_bot, participant._index_in_pages,
                                      participant._max_page_index, participant._current_app_name, participant._current_page_name,
                                      participant.time_started, participant.visited, participant.mturk_worker_id,
                                      participant.mturk_assignment_id, player.silo_num, player._initial_decision,
                                      group.ran_ready_function, session.label, session.mturk_HITGroupId, session.mturk_HITId,
                                      session.comment, session.is_demo, player.final_payoff))
df_session = df_session %>% mutate(
  session_round_group_id = paste(as.character(session.code), as.character(subsession.round_number),
                                 as.character(group.id_in_subsession), sep = '_')
  )

# add variables to the trade data
df_trade = df_trade %>% mutate(communication = ifelse(messaging==TRUE|messaging=='True', 'Message', 'NoMessage'))

df_trade = df_trade %>% mutate(
  treatment = paste(as.character(swap_method), as.character(communication), sep = '_'),
  session_round_group_id = paste(as.character(session_code), as.character(round_number),
                                 as.character(id_in_subsession), sep = '_'),
  value2 = ifelse(players_per_group==6, strsplit(substr(value,2,14),split=','), strsplit(substr(value,2,11),split=','))
)

# add the values for each initial position to the trade data
for (i in 1:length(df_trade$value)){
  df_trade$pos1value[i] = as.numeric(df_trade$value2[[i]][1])
  df_trade$pos2value[i] = as.numeric(df_trade$value2[[i]][2])
  df_trade$pos3value[i] = as.numeric(df_trade$value2[[i]][3])
  df_trade$pos4value[i] = as.numeric(df_trade$value2[[i]][4])
  df_trade$pos5value[i] = as.numeric(df_trade$value2[[i]][5])
  df_trade$pos6value[i] = ifelse(df_trade$players_per_group[i]==6, as.numeric(df_trade$value2[[i]][6]), NA)
}

# add treatment variables to the session data
for (i in 1:length(df_session$participant.code)){
  df_temp = filter(df_trade, session_round_group_id == df_session$session_round_group_id[i])
  df_session$duration[i] = df_temp$duration[1]
  df_session$group_size[i] = df_temp$players_per_group[1]
  df_session$swap_method[i] = df_temp$swap_method[1]
  df_session$communication[i] = df_temp$communication[1]
  df_session$endowment[i] = df_temp$endowment[1]
  df_session$practice[i] = df_temp$practice[1]
  df_session$treatment[i] = df_temp$treatment[1]
  df_session$pos1value[i] = df_temp$pos1value[1]
  df_session$pos2value[i] = df_temp$pos2value[1]
  df_session$pos3value[i] = df_temp$pos3value[1]
  df_session$pos4value[i] = df_temp$pos4value[1]
  df_session$pos5value[i] = df_temp$pos5value[1]
  df_session$pos6value[i] = df_temp$pos6value[1]
}

# add payoff variables to session data
df_session = df_session %>% mutate(
  net_payoff = player.payoff - endowment,
  avg_efficient_pay = ifelse(group_size==5, 22, 30.33)
)

# remove practice rounds and temporary datasets
df_session = filter(df_session, practice == FALSE | practice == 'False')
df_trade = filter(df_trade, practice == FALSE | practice == 'False')
rm(df_temp)


##### Summary stats #####
# create summary table
treatmenttype = unique(df_trade$treatment)
summary = matrix(NA, nrow = 2, ncol = 4)
rownames(summary) = c('accept rate', 'efficiency')
colnames(summary) = treatmenttype

# loop over treatments to fill out the table
for (i in 1:length(treatmenttype)){
  df_treat = filter(df_trade, treatment == treatmenttype[i])
  df_treat_session = filter(df_session, treatment == treatmenttype[i])
  
  # calculate the acceptance rate
  accept = table(df_treat$event_type)[1]
  reject = table(df_treat$event_type)[2] + table(df_treat$event_type)[3] 
  summary[1,i] = round(accept/(accept+reject), digits = 3)
  
  # calculate the efficiency
  actual_pay = sum(df_treat_session$net_payoff)
  max_pay = sum(df_treat_session$avg_efficient_pay)
  summary[2,i] = round(actual_pay/max_pay, digits = 3)
}
xtable(summary)

# statistical significance of payoff difference
df1 = filter(df_session, treatment == 'swap_NoMessage')
df2 = filter(df_session, treatment == 'swap_Message')
df3 = filter(df_session, treatment == 'TL_NoMessage')
df4 = filter(df_session, treatment == 'TL_Message')

t.test(df1$net_payoff, df2$net_payoff, alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)$p.value
t.test(df3$net_payoff, df4$net_payoff, alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)$p.value
t.test(df1$net_payoff, df3$net_payoff, alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)$p.value
t.test(df2$net_payoff, df4$net_payoff, alternative = 'two.sided', mu = 0, conf.level = 0.95, var.equal = FALSE)$p.value

# view message content
df_message_swap = filter(df_trade, swap_method == 'swap' & communication == 'Message' & message != 'N/A' & message != '')
df_message_tl = filter(df_trade, swap_method == 'TL' & communication == 'Message' & message != 'N/A' & message != '')

# remove temporary datasets
rm(summary, df1, df2, df3, df4, df_message_swap, df_message_tl, df_treat, df_treat_session)

