theme_ggplot <- function(base_size = 12, base_family = "") { 
  # Starts with theme_grey and then modify some parts 
  theme_grey(base_size = base_size, base_family = base_family) %+replace% 
    theme( 
      axis.title.x         = element_text(size=rel(1.2), face="bold.italic", vjust=-0.2),
      axis.title.y         = element_text(size=rel(1.2), angle = 90, face="bold.italic", vjust=0.2), 
      axis.text            = element_text(size   = rel(1.2), colour = "black"),
      axis.line            = element_line(colour="black", size=rel(2)), 
      axis.ticks           = element_line(colour = "black", size=rel(1.3)),
      legend.title         = element_text(size = rel(1.3), face = "bold", hjust = 0),
      legend.text          = element_text(size = rel(1.2)), 
      panel.background     = element_rect(fill = "white", colour = NA),
      panel.grid.major     = element_line(colour = "gray94"), 
      panel.grid.minor     = element_line(colour = "gray94", size = 0.5),
      strip.text           = element_text(size = rel(1.5), colour = "black"),
      plot.margin          = grid::unit(c(1, 1, 1, 1), "lines"),  #top, right, bottom, left
      plot.title           = element_text(size = rel(1.5), face="bold",hjust=0.5),
      plot.subtitle        = element_text(size = rel(1.2),hjust=0.5)
   ) 
} 

################################
#start utility functions


get_act <- function(eps, n_act, q_vec, time, count_vec, 
           ucbparm=0, grad=FALSE){
  
  u <- runif(1)
  if(u < eps) {
    result <- sample(1:n_act, 1)
  }else if(grad == FALSE){
    est <- q_vec + ucbparm * sqrt(log(time + 1) / (count_vec + 1) )
    result <- which.max(est)
  }else{
    expest <- exp(q_vec)
    prob <- expest / sum(expest)
    result <- sample(1:n_act, 1, prob=prob)
  }
  return(result)   
}

normal_sim_bandit <- function(n_act, #number of arms
                       initial, #initial estimation for each action
                       mu_true, #true mean values for actions
                       sd_true, #true sd for actions
                       target_error, #target standard error
                       eps = 0,   #prob for eps-greedy exploration
                       ucbparm = 0, #use UCB if greater than 0
                       stepsize = 0, #constant rate to update q values
                                     #if 0 then update by sample averages
                       gradient = FALSE, #use gradient if TRUE,
                       nmax = 10000, #maximum number of trials
                       print = FALSE
){
  #initialization
  q_cur <- initial
  r_est <- lapply(1:n_act, function(k) q_cur[k])
  act_best <- which.max(q_cur)
  width_cur <- rep(nmax, n_act)
  width_best <- width_cur[act_best]
  act_count <- rep(0, n_act)
  rewardvec <- NULL
  widthvec <- NULL 
  n <- 1

  #iterations
  while(width_best > target_error & n <= nmax){
      
      if(n <= n_act) {
          act_cur <- n
      }else if(n <= 5*n_act){
          act_cur <- get_act(1, n_act, q_cur, n, act_count, ucbparm, gradient)
      }else{          
        act_cur <- get_act(eps, n_act, q_cur, n, act_count, ucbparm, gradient)
      }

      act_count[act_cur] <- act_count[act_cur] + 1
      
      #get reward from act_cur
      reward <- rnorm(1, mu_true[act_cur], sd_true[act_cur])
      
      if(stepsize ==0){#update by sample average
         q_cur[act_cur] <- q_cur[act_cur] + (reward - q_cur[act_cur]) / act_count[act_cur]
      }else if(gradient == TRUE){ #update by SGD
         onehot <- rep(0, n_act)
         onehot[act_cur] <- 1
         expest <- exp(q_cur)
         prob <- expest / sum(expest)
         q_cur <- q_cur + stepsize * reward * (onehot - prob)
      }else{ #constant step update
         q_cur[act_cur] <- q_cur[act_cur] + stepsize * (reward - q_cur[act_cur])
      }      
      
      #append the q functions
      r_est[[act_cur]] <- c(r_est[[act_cur]], reward)  
      width_cur[act_cur] <- ifelse(act_count[act_cur]>1, sd(r_est[[act_cur]])/sqrt(act_count[act_cur]), nmax)

      act_best <- which.max(q_cur)
      width_best <- width_cur[act_best]
      
      rewardvec <- c(rewardvec, reward)
      widthvec <- c(widthvec, width_best) 
      if(print==TRUE)
        cat("iter =",n,"; reward =",reward,"; act =", act_cur,"; error =", width_best,"; Qbest =", q_cur[act_best],"\n")
      n <- n + 1
  }
 
  return(list(act_best=act_best,
              act_count=act_count, rewardvec=rewardvec, 
              widthvec=widthvec, qval=q_cur))
}

##############################################################

binary_sim_bandit <- function(n_act, #number of arms
                              initial, #initial estimation for each action
                              prob_true, #vector of true probs 
                              target_error, #target standard error
                              eps = 0,   #prob for eps-greedy exploration
                              ucbparm = 0, #use UCB if greater than 0
                              stepsize = 0, #constant rate to update q values
                              #if 0 then update by sample averages
                              gradient = FALSE, #use gradient if TRUE,
                              nmax = 10000, #maximum number of trials
                              print = FALSE
){
  
  #initialization
  q_cur <- initial
  r_est <- lapply(1:n_act, function(k) q_cur[k])
  act_best <- which.max(q_cur)
  width_cur <- rep(nmax, n_act)
  width_best <- width_cur[act_best]
  act_count <- rep(0, n_act)
  rewardvec <- NULL
  widthvec <- NULL  
  n <- 1

  #iterations
  while(width_best > target_error & n <= nmax){
      
      if(n <= n_act) {
          act_cur <- n
      }else if(n <= 5*n_act){
          act_cur <- get_act(1, n_act, q_cur, n, act_count, ucbparm, gradient)
      }else{          
        act_cur <- get_act(eps, n_act, q_cur, n, act_count, ucbparm, gradient)
      }

      act_count[act_cur] <- act_count[act_cur] + 1
      
      #get reward from act_cur
      reward <- rbinom(1, 1, prob_true[act_cur])
      
      if(stepsize ==0){#update by sample average
         q_cur[act_cur] <- q_cur[act_cur] + (reward - q_cur[act_cur]) / act_count[act_cur]
      }else if(gradient == TRUE){ #update by SGD
         onehot <- rep(0, n_act)
         onehot[act_cur] <- 1
         expest <- exp(q_cur)
         prob <- expest / sum(expest)
         q_cur <- q_cur + stepsize * reward * (onehot - prob)
      }else{ #constant step update
         q_cur[act_cur] <- q_cur[act_cur] + stepsize * (reward - q_cur[act_cur])
      }      
      
      #append the q functions
      r_est[[act_cur]] <- c(r_est[[act_cur]], reward)  
      width_cur[act_cur] <- ifelse(act_count[act_cur]>1, sd(r_est[[act_cur]])/sqrt(act_count[act_cur]), nmax)

      act_best <- which.max(q_cur)
      width_best <- width_cur[act_best]
      
      rewardvec <- c(rewardvec, reward)
      widthvec <- c(widthvec, width_best)
       
      if(print==TRUE)
        cat("iter =",n,"; rewardsum =",rewardsum,"; act =", act_cur,"; error =", width_best,"; Qbest =", q_cur[act_best],"\n")
      n <- n + 1
  }
 
  return(list(act_best=act_best,
              act_count=act_count, rewardvec=rewardvec, 
              widthvec=widthvec, qval=q_cur))

}

#sd / sqrt(n) = error
n_norm <- function(sd,error,n_act) 
  return(n_act * (sd / error)^2)

#sqrt( p*(1-p)/n ) = error
n_bin <- function(p,error,n_act)
  return(n_act * p * (1-p) / error^2)

#end utility functions
###################################################
