##### generate cost matrix #####
x =  c(2,4,6,8,10,12)
matrix = matrix(0, nrow = 12, ncol = 6)

for (i in 1:12){
  matrix[i,] = sample(x, 6, replace = FALSE)
}

##### data preparation #####
# load packages
library(dplyr)
library(ggplot2)
library(xtable)
library(gridExtra)
library(foreign)

# check the payoff calculation
df = read.csv("D:/Dropbox/Working Papers/Mechanism in Queueing/data/pilot/Lines_Queueing_2020-08-07.csv", header = T)
df = df %>% mutate(manual_payoff = player.endowment + player.pay_rate - player.cost)
df = df %>% mutate(diff = player.round_payoff - manual_payoff)
df_swap = filter(df, player.swap_method == 'swap')
summary(df_swap$diff)
df_token = filter(df, player.swap_method == 'token')
summary(df_token$diff)





