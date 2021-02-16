##### Simulation parameters #####
sim <- function(iters,subs,servTime, shade){
iteration = iters  # number of iteration
n = subs  # length of line
T = servTime  # service time
V = (n-1)  # value of service     ####OTHER OPTION: V=0.5*T*(n-1) to ensure non-negativity
efficiency = c()  # store efficiency of final line
efficiency_ori = c()  # store efficiency of original line
improvement = c()  # store improvement in efficiency

##### Simulation process #####
for (t in 1:iteration){
  
  # Parameters
  m = n  # current swaping
  replace = 0  # used for swaping
  ####try log normal distribution as well
  line_ori = round(rlnorm(n, exp(0.5+0.5(0.01)),exp(0.5+0.5(0.01))*sqrt(exp(0.01)-1)), digits = (nchar(as.character(n))+1))  # original line    #could you use sample (the r command) to create a permutation of the vector 1:4?
  line_cur = line_ori  # current line
  line = line_ori  # final line
  line_opt = sort(line_ori, decreasing = TRUE)  # optimal line
  
  # Swapping
  for (j in 1:n){
    for (i in m:2){ ### only allows one pass through, need to nest more ifs
      if (line[i] >= line[i-1] + shade){
        replace = line[i-1]
        line[i-1] = line[i]
        line[i] = replace
      }
    }
    if (max(abs(line-line_cur)) == 0){
      break
    }
    else{
      m = n
      line_cur = line
      next
    }
  }
  
  # Calculate efficiency
  # payoff for final line
  payoff_line = rep(0, n)
  payoff = 0
  for (k in 1:n){
    payoff_line[k] = V - line[k]*T*(k-1)
    payoff = payoff + payoff_line[k]
  }
  # payoff for original line
  payoff_line_ori = rep(0, n)
  payoff_ori = 0
  for (k in 1:n){
    payoff_line_ori[k] = V - line_ori[k]*T*(k-1)
    payoff_ori = payoff_ori + payoff_line_ori[k]
  }
  # payoff for optimal line
  payoff_line_opt = rep(0, n)
  payoff_opt = 0
  for (k in 1:n){
    payoff_line_opt[k] = V - line_opt[k]*T*(k-1)
    payoff_opt = payoff_opt + payoff_line_opt[k]
  }
  # efficiency
  eff = payoff/payoff_opt
  eff_ori = payoff_ori/payoff_opt
  imp = eff - eff_ori
  efficiency = c(efficiency, eff)
  efficiency_ori = c(efficiency_ori, eff_ori)
  improvement = c(improvement, imp)
  
}
return(c(mean(efficiency),mean(efficiency_ori),mean(improvement)))
}

#Plot of efficiency by length
numList <- rep(0,99)
effList <- rep(0,99)
impList <- rep(0,99)
for (i in 2:100){
  numList[i-1]<- i
  sim_i <- sim(1000,i,2,0.25)
  effList[i-1]<- sim_i[1]
  impList[i-1]<- sim_i[3]
}
lengthTestDat <- data.frame(cbind(numList,effList,impList))
lnormLength<-ggplot(lengthTestDat,aes(x=numList,y=effList,colour=impList))+ 
  geom_point(size=1.4)+
  labs(x="Length of Queue",y="Efficiency",colour="Improvement")

#Plot of changing shades
lengthList <- c(5,10,50,100)
shadeList <- rep(0,188)
effList <- rep(0,188)
numList <- rep(0,188)
impList <- rep(0,188)
for (i in 1:length(lengthList)){
  queue <- lengthList[i]
  for (j in 4:50){
    shadeList[(i-1)*47 +(j-3)]<-1/j
    simu<-sim(1000,queue,2,(1/j))
    numList[(i-1)*47 +(j-3)]<-queue
    effList[(i-1)*47 +(j-3)]<-simu[1]
    impList[(i-1)*47 +(j-3)]<- simu[3]
  }
}
shadeTestDat <- data.frame(cbind(numList,shadeList,effList,impList))
b<-ggplot(shadeTestDat,aes(x=shadeList,y=effList,colour=impList))+ 
  geom_point(size=1.4)+
  labs(x="Size of Shade",y="Efficiency",colour="Improvement")+
  facet_grid(cols=vars(numList))


### plots for changing shading and number of people
#check types of distributions for cosst
#maybe try distribution for V draws as well instead 
#solve if buyers distribution is non strictly normal, ie how does shading change if buyers have prob .5 of having value 10 and 