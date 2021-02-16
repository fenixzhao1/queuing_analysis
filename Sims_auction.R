##### generate cost matrix #####
x =  c(2,4,6,8,10,12)
matrix = matrix(0, nrow = 12, ncol = 6)

for (i in 1:12){
  matrix[i,] = sample(x, 6, replace = FALSE)
}


##### algorithm for zero intelligence agents #####
# double auction
double_auction = function(iteration, num_player){
  
  # generate initial line
  line = runif(num_player, 0, 1)
  
  # loop over iteration
  for (i in 1:iteration){
    
    # randomly select two nearby players and set their bidding price
    requester = sample(2:num_player, 1)
    requestee = requester - 1
    bid_requester = runif(1, 0, line[requester])
    bid_requestee = runif(1, line[requestee], 1)
    
    # compare bidding price and if possible switch positions
    if (bid_requester >= bid_requestee){
      
      intermediate = line[requestee]
      line[requestee] = line[requester]
      line[requester] = intermediate
    }
  }
  
  # calculate current payoff
  V = num_player
  
  payoff_current = 0
  for (j in 1:num_player){
    payoff = V - j*line[j]
    payoff_current = payoff_current + payoff
  }
  
  # calculate best possible payoff
  payoff_best = 0
  line_best = sort(line, decreasing=TRUE)
  for (j in 1:num_player){
    payoff = V - j*line_best[j]
    payoff_best = payoff_best + payoff
  }
  
  # return efficiency
  effi = payoff_current / payoff_best
  return(effi)
}


# take-it-leave-it price
tili_auction = function(iteration, num_player){
  
  # generate initial line
  line = runif(num_player, 0, 1)
  
  # loop over iteration
  for (i in 1:iteration){
    
    # randomly select two nearby players and set their bidding price
    requester = sample(2:num_player, 1)
    requestee = requester - 1
    bid_requester = runif(1, 0, line[requester])
    
    # compare bidding price and if possible switch positions
    if (bid_requester >= line[requestee]){
      
      intermediate = line[requestee]
      line[requestee] = line[requester]
      line[requester] = intermediate
    }
  }
  
  # calculate current payoff
  V = num_player
  
  payoff_current = 0
  for (j in 1:num_player){
    payoff = V - j*line[j]
    payoff_current = payoff_current + payoff
  }
  
  # calculate best possible payoff
  payoff_best = 0
  line_best = sort(line, decreasing=TRUE)
  for (j in 1:num_player){
    payoff = V - j*line_best[j]
    payoff_best = payoff_best + payoff
  }
  
  # return efficiency
  effi = payoff_current / payoff_best
  return(effi)
}


# try some simulations to calculate efficiency
num_sim = 1000
efficiency_double = rep(0, num_sim)
efficiency_tili = rep(0, num_sim)
for (k in 1:num_sim){
  efficiency_double[k] = double_auction(100, 16)
  efficiency_tili[k] = tili_auction(100, 16)
}

# print results from simulations
print('mean efficiency in double auction:')
print(mean(efficiency_double))
print('mean efficiency in TILI auction:')
print(mean(efficiency_tili))





