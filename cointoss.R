cointoss = function(numtosses)
{
  #function computes payoff streams for two contingencies (given random coinflips):
  #1: number of consecutive heads (HH)
  #2: number of HTs (HT)
  # i.e.: sequence of 'HTHHH' would count as 1 HT and 2 HH
  #returns dataframe of payoff per flip and two series of cumulative payoffs
  
  #generate sequence of coin tosses. 1 = H; -1 = T
  toss_seq = sample(c(1,-1),numtosses,replace=TRUE)

  #go through sequence and compute payoffs for strategy 1 and 2

  strat_HH = rep(NA,length(toss_seq) - 1)
  strat_HT = rep(NA, length(toss_seq) -1)

  i = 1
  while(i <= numtosses-1)
  {
    if(toss_seq[i+1] == toss_seq[i]){
      
      strat_HH[i] = 1
      strat_HT[i] = 0
      
    } else if(toss_seq[i] == 1 & toss_seq[i+1] == -1){
      
      strat_HT[i] = 1
      strat_HH[i] = 0
      
    } else{
      
      strat_HH[i] = 0
      strat_HT[i] = 0
    }
    
    
    i = i + 1
  }
  strat_HH = cumsum(strat_HH)
  strat_HT = cumsum(strat_HT)
  strat_HH_payoff = max(strat_HH)/ numtosses
  strat_HT_payoff = max(strat_HT)/ numtosses
  payoffs = as.data.frame(cbind(strat_HH_payoff,strat_HT_payoff))
  results = list('Payoffs' = payoffs, 'HH' = strat_HH, 'HT' = strat_HT)
  return(results)
}
