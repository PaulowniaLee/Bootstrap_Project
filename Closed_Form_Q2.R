#### Environment ####

# Huizhu's data
suicide <- c(6,10,10) 
s.prob <- c(1/3, 1/3, 1/3)
compromise <- c(10,5,1,1,1,0,7)
d.prob <- c(1/6, 1/6, 1/8, 1/8, 1/8, 1/8, 1/6)


#####

# Q1 by Closed Form

#### Estimation ####
# basic probabilities 
q = 1/8 #prob for 0
# suicides 
ps6 = 1/3 
ps10 = 2/3
ps11 = 0
# compromises 
pc1 = 1/8 * 3 
pc5 = 1/6
pc7 = 1/6
pc10 = 1/6

# compute P1 - P12
{
  # P1
  P1 = ps6 * (pc7+pc10)
  
  # P2
  P2 = ps6^2 * (pc1*(pc7+pc10) + pc5*pc5) + 
    ps6*ps10 * (pc1*(pc7+pc10) + pc5*(pc5+pc7+pc10)) + 
    ps6*ps11 * (pc1*(pc7+pc10) + pc5*(pc5+pc7+pc10)) + 
    ps10*ps6 * (pc1*pc10 + pc5*(pc7+pc10) + pc7*(pc5+pc7) + pc10*(pc1+pc5)) + 
    ps10^2 * (pc1*pc10 + pc5*(pc7+pc10) + pc7*(pc5+pc7+pc10) + pc10*(pc1+pc5+pc7)) +
    ps10*ps11 * (pc1*pc10 + pc5*(pc7+pc10) + pc7*(pc5+pc7+pc10) + pc10*(pc1+pc5+pc7+pc10)) +
    ps11*ps6 * (pc5*(pc7+pc10) + pc7*(pc5+pc7) + pc10*(pc5)) +
    ps11*ps10 * (pc5*(pc7+pc10) + pc7*(pc5+pc7+pc10) + pc10*(pc5+pc7+pc10)) +
    ps11^2 * (pc5*(pc7+pc10) + pc7*(pc5+pc7+pc10) + pc10*(pc5+pc7+pc10))
  
  # P3 
  P3 = ps6^2 * (pc1^2*(pc5+pc7) + pc1*pc5*2*(pc1+pc5)) + 
    ps6*ps10 * (pc1^2*(pc5+pc7+pc10) + pc1*pc5*2*(pc1+pc5+pc7)) + 
    ps6*ps11 * (pc1^2*(pc5+pc7+pc10) + pc1*pc5*2*(pc1+pc5+pc7+pc10)) +
    ps10*ps6 * (pc1^2*pc10 + pc1*pc5*2*(pc5+pc7) + pc1*pc7*2*(pc5+pc7) + pc5^2*(pc1+pc5)) + 
    ps10^2 * (pc1^2*pc10 + pc1*pc5*2*(pc5+pc7+pc10) + pc1*pc7*2*(pc5+pc7+pc10) + pc5^2*(pc1+pc5+pc7)) + 
    ps10*ps11 * (pc1^2*pc10 + pc1*pc5*2*(pc5+pc7+pc10) + pc1*pc7*2*(pc5+pc7+pc10) + pc5^2*(pc1+pc5+pc7+pc10)) +
    ps11*ps6 * (pc1^2*pc10 + pc1*pc5*2*(pc7+pc10) + pc1*pc7*2*(pc5+pc7) + pc1*pc10*2*(pc1+pc5) + pc5^2*(pc5)) + 
    ps11*ps10 * (pc1^2*pc10 + pc1*pc5*2*(pc7+pc10) + pc1*pc7*2*(pc5+pc7+pc10) + pc1*pc10*2*(pc1+pc5+pc7) + pc5^2*(pc5+pc7+pc10)) + 
    ps11^2 * (pc1^2 *pc10 + pc1*pc5*2*(pc7+pc10) + pc1*pc7*2*(pc5+pc7+pc10) + pc1*pc10*2*(pc1+pc5+pc7+pc10) + pc5^2*(pc5+pc7+pc10)) 
  
  # P4
  P4 = ps6^2 * (pc1^3*(pc5+pc7)) + 
    ps6*ps10 * (pc1^3*(pc5+pc7+pc10)) +
    ps6*ps11 * (pc1^3*(pc5+pc7+pc10)) +
    ps10*ps6 * (pc1^3*pc10 + pc1^2*pc5*3*(pc5+pc7) + pc1^2*pc7*3*(pc5)) +
    ps10^2 * (pc1^3*pc10 + pc1^2*pc5*3*(pc5+pc7+pc10) + pc1^2*pc7*3*(pc5+pc7+pc10)) +
    ps10*ps11 * (pc1^3*pc10 + pc1^2*pc5*3*(pc5+pc7+pc10) + pc1^2*pc7*3*(pc5+pc7+pc10)) +
    ps11*ps6 * (pc1^3*pc10 + pc1^2*pc5*3*(pc5+pc7) + pc1*pc5^2*3*(pc1+pc5) + pc1^2*pc7*3*(pc5+pc7)) +
    ps11*ps10 * (pc1^3*pc10 + pc1^2*pc5*3*(pc5+pc7+pc10) + pc1*pc5^2*3*(pc1+pc5+pc7) + pc1^2*pc7*3*(pc5+pc7+pc10)) +
    ps11^2 * (pc1^3*pc10 + pc1^2*pc5*3*(pc5+pc7+pc10) + pc1*pc5^2*3*(pc1+pc5+pc7+pc10) + pc1^2*pc7*3*(pc5+pc7+pc10)) 
  
  # P5
  P5 = ps6^2 * (pc1^4*(pc5+pc7)) + 
    ps6*ps10 * (pc1^4*(pc5+pc7+pc10)) +
    ps6*ps11 * (pc1^4*(pc5+pc7+pc10)) +
    ps10*ps6 * (pc1^4*(pc7+pc10) + pc1^3*pc5*4*(pc5+pc7) + pc1^3*pc7*4*(pc1+pc5)) +
    ps10^2 * (pc1^4*(pc7+pc10) + pc1^3*pc5*4*(pc5+pc7+pc10) + pc1^3*pc7*4*(pc1+pc5+pc7)) +
    ps10*ps11 * (pc1^4*(pc7+pc10) + pc1^3*pc5*4*(pc5+pc7+pc10) + pc1^3*pc7*4*(pc1+pc5+pc7+pc10)) +
    ps11*ps6 * (pc1^4*(pc10) + pc1^3*pc5*4*(pc5+pc7) + pc1^3*pc7*4*(pc5)) +
    ps11*ps10 * (pc1^4*(pc10) + pc1^3*pc5*4*(pc5+pc7+pc10) + pc1^3*pc7*4*(pc5+pc7+pc10)) +
    ps11^2 * (pc1^4*(pc10) + pc1^3*pc5*4*(pc5+pc7+pc10) + pc1^3*pc7*4*(pc5+pc7+pc10)) 
  
  # P6
  P6 = ps6^2 * (pc1^5*(pc5)) +
    ps6*ps10 * (pc1^5*(pc5+pc7+pc10)) +
    ps6*ps11 * (pc1^5*(pc5+pc7+pc10)) +
    ps10*ps6 * (pc1^5*(pc7+pc10) + pc1^4*pc5*5*(pc5)) +
    ps10^2 * (pc1^5*(pc7+pc10) + pc1^4*pc5*5*(pc5+pc7+pc10)) +
    ps10*ps11 * (pc1^5*(pc7+pc10) + pc1^4*pc5*5*(pc5+pc7+pc10)) +
    ps11*ps6 * (pc1^5*(pc7+pc10) + pc1^4*pc5*5*(pc5+pc7) + pc1^4*pc7*5*(pc1+pc5)) +
    ps11*ps10 * (pc1^5*(pc7+pc10) + pc1^4*pc5*5*(pc5+pc7+pc10) + pc1^4*pc7*5*(pc1+pc5+pc7)) +
    ps11^2 * (pc1^5*(pc7+pc10) + pc1^4*pc5*5*(pc5+pc7+pc10) + pc1^4*pc7*5*(pc1+pc5+pc7+pc10)) 
  
  # P7
  P7 = ps6^2 * (pc1^6*(pc1+pc5)) +
    ps6*ps10 * (pc1^6*(pc1+pc5+pc7)) +
    ps6*ps11 * (pc1^6*(pc1+pc5+pc7+pc10)) +
    ps10*ps6 * (pc1^6*(pc5+pc7) + pc1^5*pc5*6*(pc1+pc5)) +
    ps10^2 * (pc1^6*(pc5+pc7+pc10) + pc1^5*pc5*6*(pc1+pc5+pc7)) +
    ps10*ps11 * (pc1^6*(pc5+pc7+pc10) + pc1^5*pc5*6*(pc1+pc5+pc7+pc10)) +
    ps11*ps6 * (pc1^6*(pc7+pc10) + pc1^5*pc5*6*(pc5)) +
    ps11*ps10 * (pc1^6*(pc7+pc10) + pc1^5*pc5*6*(pc5+pc7+pc10)) +
    ps11^2 * (pc1^6*(pc7+pc10) + pc1^5*pc5*6*(pc5+pc7+pc10)) 
  
  # P8
  P8 = ps10*ps6 * (pc1^7*(pc5+pc7)) + 
    ps10^2 * (pc1^7*(pc5+pc7+pc10)) + 
    ps10*ps11 * (pc1^7*(pc5+pc7+pc10)) + 
    ps11*ps6 * (pc1^7*(pc5+pc7) + pc1^6*pc5*7*(pc1+pc5)) + 
    ps11*ps10 * (pc1^7*(pc5+pc7+pc10) + pc1^6*pc5*7*(pc1+pc5+pc7)) + 
    ps11^2 * (pc1^7*(pc5+pc7+pc10) + pc1^6*pc5*7*(pc1+pc5+pc7+pc10))  
  
  # P9
  P9 = ps10*ps6 * (pc1^8*(pc5+pc7)) + 
    ps10^2 * (pc1^8*(pc5+pc7+pc10)) +
    ps10*ps11 * (pc1^8*(pc5+pc7+pc10)) +
    ps11*ps6 * (pc1^8*(pc5+pc7)) +
    ps11*ps10 * (pc1^8*(pc5+pc7+pc10)) +
    ps11^2 * (pc1^8*(pc5+pc7+pc10)) 
  
  # P10
  P10 = ps10*ps6 * (pc1^9*(pc5)) +
    ps10^2 * (pc1^9*(pc5+pc7+pc10)) +
    ps10*ps11 * (pc1^9*(pc5+pc7+pc10)) +
    ps11*ps6 * (pc1^9*(pc5+pc7)) +
    ps11*ps10 * (pc1^9*(pc5+pc7+pc10)) +
    ps11^2 * (pc1^9*(pc5+pc7+pc10)) 
  
  # P11
  P11 = ps10*ps6 * (pc1^10*(pc1+pc5)) +
    ps10^2 * (pc1^10*(pc1+pc5+pc7)) +
    ps10*ps11 * (pc1^10*(pc1+pc5+pc7+pc10)) +
    ps11*ps6 * (pc1^10*(pc5)) +
    ps11*ps10 * (pc1^10*(pc5+pc7+pc10)) +
    ps11^2 * (pc1^10*(pc5+pc7+pc10)) 
  
  # P12
  P12 = ps11*ps6 * (pc1^11*(pc1+pc5)) +
    ps11*ps10 * (pc1^11*(pc1+pc5+pc7)) +
    ps11^2 * (pc1^11*(pc1+pc5+pc7+pc10)) 
}
PP <- c(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12)
remove(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12)

# Fine-tuning part (find appropriate n)
{
  potential_n <- c(50, 100, 500, 1000, 5000, 10^4, 1.5*10^4)
  # start while loop
  epsilon = 100
  i = 1
  # control the error within 10^(-10)
  # use largest t = 11 get the bound
  while(epsilon >= 10^(-10)){
    n = potential_n[i]
    coeff = prod(seq(from = n+1, to = n+11, by = 1)) # product of the vector
    epsilon = coeff * q^n * ( q/((n+1)/(11+n+1) - q) - q/(1-q) ) # error of sum
    i = i + 1
  }
  remove(i, coeff, epsilon, potential_n)
}

# use partial sum to approximate infinite sum
{
  i <- 1:n
  PPC <- vector()
  # P1 coefficient (P1C)
  PPC <- append(PPC, 1/(1-q)) 
  # P2
  PPC <- append(PPC, q/(1-q)^2 + 1/(1-q) ) 
  # P3 - P12 (by approximation)
  PPT <- sapply(seq(from = 2, to = 11, by = 1), function(t){
    # factorial part
    coeff <- sapply(i, function(k){
      prod(seq(from = k+1, to = k+t, by = 1))
    })
    # q^k part
    product <- rep(q, n)^i
    # combine
    return(1 + 1/factorial(t) * ( sum(coeff * product) + 
                                    prod(seq(from = n+1, to = n+t, by = 1)) * q^n * q/(1-q)) )
  })
  PPC <- append(PPC, PPT)
  remove(n, PPT, i)
}
remove(q, ps6, ps10, ps11, pc1, pc5, pc7, pc10)

# get probability: compromise between first and second suicides
pr_target <- 1 - sum(PP*PPC)


#####

#### Bootstrap ####

# get bootstrap cdf of estimation error
# that is, first construct 10^5 bootstrap samples, 
BB = 10^5 
# list containing bootstrap samples
SS_data <- list()
CC_data <- list()
# construct 10^5 bootstrap samples
set.seed(1019)
for (j in 1:BB){
  # construct three suicides
  SS <- sample(suicide, size = 3, replace = T, prob = NULL)
  # construct compromises
  i = 0
  CC <- vector()
  while (i <= sum(SS)){
    CC <- append(CC, sample(compromise, size = 1, replace = T, prob = d.prob))
    i <- sum(CC)
  }
  # append
  SS_data[[j]] <- SS
  CC_data[[j]] <- CC[1:length(CC)-1]
}
remove(CC, SS, i, j)


## get the target probability from each of these sample
# vector containing errors 
error <- vector()
for (u in 1:BB){
  # prepare probability for suicides
  {
  deltaS <- 26 - sum(SS_data[[u]])
  diffS <- SS_data[[u]] - deltaS
  SNplus <- sum(diffS > 0)
  # how many terms of S are strictly larger than delta?
  # with this, get probabilities
  lowprobS <- 1/(length(SS_data[[u]]) + 1 )
  # there is a case, that we don't get any C larger than delta
  # under this case, Nplus is 0, highprob is infinity 
  highprobS <- 1/(length(SS_data[[u]]) + 1 ) * (1 + 1/SNplus )
  ss.prob <- rep(lowprobS, times = length(SS_data[[u]]))
  # but no term satisfies diff > 0 if Nplus is 0, so every term is (1/N+1)
  ss.prob[which(diffS > 0)] <- highprobS
  
  # we will append an arbitrary term. 
  # To keep our closed form, let it be the largest one of the range
  append_valueS = 10
  # fix the case where no S is higher than delta 
  # (each term has prob 1/(N+1) but only N terms to sample)
  if (SNplus > 0){
    SS_update <- SS_data[[u]]
    ss.prob_update <- ss.prob
  } else if (SNplus == 0) {
    SS_update <- append(SS_data[[u]], append_valueS)
    ss.prob_update <- append(ss.prob, lowprobS)
  } else {
    print("error SNplus")
  } 
  }

  # prepare probability for compromises
  {
  delta <- sum(SS_data[[u]]) - sum(CC_data[[u]])
  diff <- CC_data[[u]] - delta
  Nplus <- sum(diff > 0)
  # how many terms of C are strictly larger than delta?
  # with this, get probabilities
  lowprob <- 1/(length(CC_data[[u]]) + 1 )
  # there is a case, that we don't get any C larger than delta
  # under this case, Nplus is 0, highprob is infinity 
  highprob <- 1/(length(CC_data[[u]]) + 1 ) * (1 + 1/Nplus )
  dd.prob <- rep(lowprob, times = length(CC_data[[u]]))
  # but no term satisfies diff > 0 if Nplus is 0, so every term is (1/N+1)
  dd.prob[which(diff > 0)] <- highprob
  
  # because previous calculation is carried with 0,1,5,7,10 in mind
  # we make the arbitrary term to be 10 for the case
  # "no term has diff > 0"
  append_value = 10
  if (Nplus > 0){
    CC_update <- CC_data[[u]]
    dd.prob_update <- dd.prob
  } else if (Nplus == 0) {
    CC_update <- append(CC_data[[u]], append_value)
    dd.prob_update <- append(dd.prob, lowprob) 
    # add low prob s.t. total probability is 1
  } else {
    print("error Nplus")
  }
  }

  # basic probabilities 
  {
  q = sum(dd.prob_update[which(CC_data[[u]] == 0)]) #prob for 0
  # suicides 
  ps6 = sum(ss.prob[which(SS_data[[u]] == 6)]) 
  ps10 = sum(ss.prob[which(SS_data[[u]] == 10)]) 
  ps11 = sum(ss.prob[which(SS_data[[u]] == 11)]) 
  # compromises 
  pc1 = sum(dd.prob_update[which(CC_data[[u]] == 1)]) 
  pc5 = sum(dd.prob_update[which(CC_data[[u]] == 5)])
  pc7 = sum(dd.prob_update[which(CC_data[[u]] == 7)])
  pc10 = sum(dd.prob_update[which(CC_data[[u]] == 10)])
  }
  
  # compute P1 - P12
  {
    # P1
    P1 = ps6 * (pc7+pc10)
    
    # P2
    P2 = ps6^2 * (pc1*(pc7+pc10) + pc5*pc5) + 
      ps6*ps10 * (pc1*(pc7+pc10) + pc5*(pc5+pc7+pc10)) + 
      ps6*ps11 * (pc1*(pc7+pc10) + pc5*(pc5+pc7+pc10)) + 
      ps10*ps6 * (pc1*pc10 + pc5*(pc7+pc10) + pc7*(pc5+pc7) + pc10*(pc1+pc5)) + 
      ps10^2 * (pc1*pc10 + pc5*(pc7+pc10) + pc7*(pc5+pc7+pc10) + pc10*(pc1+pc5+pc7)) +
      ps10*ps11 * (pc1*pc10 + pc5*(pc7+pc10) + pc7*(pc5+pc7+pc10) + pc10*(pc1+pc5+pc7+pc10)) +
      ps11*ps6 * (pc5*(pc7+pc10) + pc7*(pc5+pc7) + pc10*(pc5)) +
      ps11*ps10 * (pc5*(pc7+pc10) + pc7*(pc5+pc7+pc10) + pc10*(pc5+pc7+pc10)) +
      ps11^2 * (pc5*(pc7+pc10) + pc7*(pc5+pc7+pc10) + pc10*(pc5+pc7+pc10))
    
    # P3 
    P3 = ps6^2 * (pc1^2*(pc5+pc7) + pc1*pc5*2*(pc1+pc5)) + 
      ps6*ps10 * (pc1^2*(pc5+pc7+pc10) + pc1*pc5*2*(pc1+pc5+pc7)) + 
      ps6*ps11 * (pc1^2*(pc5+pc7+pc10) + pc1*pc5*2*(pc1+pc5+pc7+pc10)) +
      ps10*ps6 * (pc1^2*pc10 + pc1*pc5*2*(pc5+pc7) + pc1*pc7*2*(pc5+pc7) + pc5^2*(pc1+pc5)) + 
      ps10^2 * (pc1^2*pc10 + pc1*pc5*2*(pc5+pc7+pc10) + pc1*pc7*2*(pc5+pc7+pc10) + pc5^2*(pc1+pc5+pc7)) + 
      ps10*ps11 * (pc1^2*pc10 + pc1*pc5*2*(pc5+pc7+pc10) + pc1*pc7*2*(pc5+pc7+pc10) + pc5^2*(pc1+pc5+pc7+pc10)) +
      ps11*ps6 * (pc1^2*pc10 + pc1*pc5*2*(pc7+pc10) + pc1*pc7*2*(pc5+pc7) + pc1*pc10*2*(pc1+pc5) + pc5^2*(pc5)) + 
      ps11*ps10 * (pc1^2*pc10 + pc1*pc5*2*(pc7+pc10) + pc1*pc7*2*(pc5+pc7+pc10) + pc1*pc10*2*(pc1+pc5+pc7) + pc5^2*(pc5+pc7+pc10)) + 
      ps11^2 * (pc1^2 *pc10 + pc1*pc5*2*(pc7+pc10) + pc1*pc7*2*(pc5+pc7+pc10) + pc1*pc10*2*(pc1+pc5+pc7+pc10) + pc5^2*(pc5+pc7+pc10)) 
    
    # P4
    P4 = ps6^2 * (pc1^3*(pc5+pc7)) + 
      ps6*ps10 * (pc1^3*(pc5+pc7+pc10)) +
      ps6*ps11 * (pc1^3*(pc5+pc7+pc10)) +
      ps10*ps6 * (pc1^3*pc10 + pc1^2*pc5*3*(pc5+pc7) + pc1^2*pc7*3*(pc5)) +
      ps10^2 * (pc1^3*pc10 + pc1^2*pc5*3*(pc5+pc7+pc10) + pc1^2*pc7*3*(pc5+pc7+pc10)) +
      ps10*ps11 * (pc1^3*pc10 + pc1^2*pc5*3*(pc5+pc7+pc10) + pc1^2*pc7*3*(pc5+pc7+pc10)) +
      ps11*ps6 * (pc1^3*pc10 + pc1^2*pc5*3*(pc5+pc7) + pc1*pc5^2*3*(pc1+pc5) + pc1^2*pc7*3*(pc5+pc7)) +
      ps11*ps10 * (pc1^3*pc10 + pc1^2*pc5*3*(pc5+pc7+pc10) + pc1*pc5^2*3*(pc1+pc5+pc7) + pc1^2*pc7*3*(pc5+pc7+pc10)) +
      ps11^2 * (pc1^3*pc10 + pc1^2*pc5*3*(pc5+pc7+pc10) + pc1*pc5^2*3*(pc1+pc5+pc7+pc10) + pc1^2*pc7*3*(pc5+pc7+pc10)) 
    
    # P5
    P5 = ps6^2 * (pc1^4*(pc5+pc7)) + 
      ps6*ps10 * (pc1^4*(pc5+pc7+pc10)) +
      ps6*ps11 * (pc1^4*(pc5+pc7+pc10)) +
      ps10*ps6 * (pc1^4*(pc7+pc10) + pc1^3*pc5*4*(pc5+pc7) + pc1^3*pc7*4*(pc1+pc5)) +
      ps10^2 * (pc1^4*(pc7+pc10) + pc1^3*pc5*4*(pc5+pc7+pc10) + pc1^3*pc7*4*(pc1+pc5+pc7)) +
      ps10*ps11 * (pc1^4*(pc7+pc10) + pc1^3*pc5*4*(pc5+pc7+pc10) + pc1^3*pc7*4*(pc1+pc5+pc7+pc10)) +
      ps11*ps6 * (pc1^4*(pc10) + pc1^3*pc5*4*(pc5+pc7) + pc1^3*pc7*4*(pc5)) +
      ps11*ps10 * (pc1^4*(pc10) + pc1^3*pc5*4*(pc5+pc7+pc10) + pc1^3*pc7*4*(pc5+pc7+pc10)) +
      ps11^2 * (pc1^4*(pc10) + pc1^3*pc5*4*(pc5+pc7+pc10) + pc1^3*pc7*4*(pc5+pc7+pc10)) 
    
    # P6
    P6 = ps6^2 * (pc1^5*(pc5)) +
      ps6*ps10 * (pc1^5*(pc5+pc7+pc10)) +
      ps6*ps11 * (pc1^5*(pc5+pc7+pc10)) +
      ps10*ps6 * (pc1^5*(pc7+pc10) + pc1^4*pc5*5*(pc5)) +
      ps10^2 * (pc1^5*(pc7+pc10) + pc1^4*pc5*5*(pc5+pc7+pc10)) +
      ps10*ps11 * (pc1^5*(pc7+pc10) + pc1^4*pc5*5*(pc5+pc7+pc10)) +
      ps11*ps6 * (pc1^5*(pc7+pc10) + pc1^4*pc5*5*(pc5+pc7) + pc1^4*pc7*5*(pc1+pc5)) +
      ps11*ps10 * (pc1^5*(pc7+pc10) + pc1^4*pc5*5*(pc5+pc7+pc10) + pc1^4*pc7*5*(pc1+pc5+pc7)) +
      ps11^2 * (pc1^5*(pc7+pc10) + pc1^4*pc5*5*(pc5+pc7+pc10) + pc1^4*pc7*5*(pc1+pc5+pc7+pc10)) 
    
    # P7
    P7 = ps6^2 * (pc1^6*(pc1+pc5)) +
      ps6*ps10 * (pc1^6*(pc1+pc5+pc7)) +
      ps6*ps11 * (pc1^6*(pc1+pc5+pc7+pc10)) +
      ps10*ps6 * (pc1^6*(pc5+pc7) + pc1^5*pc5*6*(pc1+pc5)) +
      ps10^2 * (pc1^6*(pc5+pc7+pc10) + pc1^5*pc5*6*(pc1+pc5+pc7)) +
      ps10*ps11 * (pc1^6*(pc5+pc7+pc10) + pc1^5*pc5*6*(pc1+pc5+pc7+pc10)) +
      ps11*ps6 * (pc1^6*(pc7+pc10) + pc1^5*pc5*6*(pc5)) +
      ps11*ps10 * (pc1^6*(pc7+pc10) + pc1^5*pc5*6*(pc5+pc7+pc10)) +
      ps11^2 * (pc1^6*(pc7+pc10) + pc1^5*pc5*6*(pc5+pc7+pc10)) 
    
    # P8
    P8 = ps10*ps6 * (pc1^7*(pc5+pc7)) + 
      ps10^2 * (pc1^7*(pc5+pc7+pc10)) + 
      ps10*ps11 * (pc1^7*(pc5+pc7+pc10)) + 
      ps11*ps6 * (pc1^7*(pc5+pc7) + pc1^6*pc5*7*(pc1+pc5)) + 
      ps11*ps10 * (pc1^7*(pc5+pc7+pc10) + pc1^6*pc5*7*(pc1+pc5+pc7)) + 
      ps11^2 * (pc1^7*(pc5+pc7+pc10) + pc1^6*pc5*7*(pc1+pc5+pc7+pc10))  
    
    # P9
    P9 = ps10*ps6 * (pc1^8*(pc5+pc7)) + 
      ps10^2 * (pc1^8*(pc5+pc7+pc10)) +
      ps10*ps11 * (pc1^8*(pc5+pc7+pc10)) +
      ps11*ps6 * (pc1^8*(pc5+pc7)) +
      ps11*ps10 * (pc1^8*(pc5+pc7+pc10)) +
      ps11^2 * (pc1^8*(pc5+pc7+pc10)) 
    
    # P10
    P10 = ps10*ps6 * (pc1^9*(pc5)) +
      ps10^2 * (pc1^9*(pc5+pc7+pc10)) +
      ps10*ps11 * (pc1^9*(pc5+pc7+pc10)) +
      ps11*ps6 * (pc1^9*(pc5+pc7)) +
      ps11*ps10 * (pc1^9*(pc5+pc7+pc10)) +
      ps11^2 * (pc1^9*(pc5+pc7+pc10)) 
    
    # P11
    P11 = ps10*ps6 * (pc1^10*(pc1+pc5)) +
      ps10^2 * (pc1^10*(pc1+pc5+pc7)) +
      ps10*ps11 * (pc1^10*(pc1+pc5+pc7+pc10)) +
      ps11*ps6 * (pc1^10*(pc5)) +
      ps11*ps10 * (pc1^10*(pc5+pc7+pc10)) +
      ps11^2 * (pc1^10*(pc5+pc7+pc10)) 
    
    # P12
    P12 = ps11*ps6 * (pc1^11*(pc1+pc5)) +
      ps11*ps10 * (pc1^11*(pc1+pc5+pc7)) +
      ps11^2 * (pc1^11*(pc1+pc5+pc7+pc10)) 
  }
  PP <- c(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12)
  remove(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12)
  
  # Fine-tuning part (find appropriate n)
  {
    potential_n <- c(50, 100, 500, 1000, 5000, 10^4, 1.5*10^4)
    # start while loop
    epsilon = 100
    i = 1
    # control the error within 10^(-10)
    # use largest t = 11 get the bound
    while(epsilon >= 10^(-10)){
      n = potential_n[i]
      coeff = prod(seq(from = n+1, to = n+11, by = 1)) # product of the vector
      epsilon = coeff * q^n * ( q/((n+1)/(11+n+1) - q) - q/(1-q) ) # error of sum
      i = i + 1
    }
    remove(i, coeff, epsilon, potential_n)
  }
  
  # use partial sum to approximate infinite sum
  {
    i <- 1:n
    PPC <- vector()
    # P1 coefficient (P1C)
    PPC <- append(PPC, 1/(1-q)) 
    # P2
    PPC <- append(PPC, q/(1-q)^2 + 1/(1-q) ) 
    # P3 - P12 (by approximation)
    PPT <- sapply(seq(from = 2, to = 11, by = 1), function(t){
      # factorial part
      coeff <- sapply(i, function(k){
        prod(seq(from = k+1, to = k+t, by = 1))
      })
      # q^k part
      product <- rep(q, n)^i
      # combine
      return(1 + 1/factorial(t) * ( sum(coeff * product) + 
                                      prod(seq(from = n+1, to = n+t, by = 1)) * q^n * q/(1-q)) )
    })
    PPC <- append(PPC, PPT)
    remove(n, PPT, i)
  }
  remove(q, ps6, ps10, ps11, pc1, pc5, pc7, pc10)
  
  # get probability: compromise between first and second suicides
  pr_boot <- 1 - sum(PP*PPC)
  error <- append(error, pr_boot - pr_target)
  remove(PP, PPC)
  
  
  
  # Progress Indicator 
  if (u%%5000==0) {cat(" *",u)} 
}

#####

# error distribution 

saveRDS(error, "error_distribution_closeQ2.rds")
write.csv(error, file = "error_distribution_closeQ2.csv")

# plot 
CDF <- ecdf(error)
plot(CDF)

{
  # Bias 
  mean(error)
  # MSE
  mean(error^2)
  # sd
  sd(error)
  # MAD
  mean(abs(error))
}