sim_data<-function(alpha, beta, g, r, gamma, sigma, distribution){
  #'
  #' simulates data from a hierarchical data structure for normal or poisson distributions
  #' @param alpha the true intercept of the fixed term in the model
  #' @param beta the true treatment effect of the fixed term in the model
  #' @param g the number of clusters in the data, multiple of 2
  #' @param r the number of times to sample from each cluster
  #' @param gamma the standard deviation of the random intercept 
  #' @param sigma the standard deviation of the error term
  #' @distribution one of "normal" or "poisson", the distribution to sample from
  #' @return a list of the data and the estimated beta hat
  #'
  
  # generate treatment assignment for clusters, resample if all treated or all
  # control
  x<-rep(c(0,1), g/2)
  # sum=0
  # while(sum==0|sum==g){
  #   x<-rbinom(g, 1, .5)
  #   sum<-sum(x)
  # }
  # generate treatment assignment for observations (r from g groups)
  x_i<-rep(x, r)
  
  # save the cluster assignments for each observation
  j<-rep(1:g, r)
  
  # fixed effect mu_0 for each cluster
  mu_i0<-alpha+beta*x
  
  # random effect mu_i
  mu_i<-rnorm(g*r, mean=mu_i0, sd=gamma)
  
  if (distribution=="normal"){
    
    # generate normal outcomes
    y<-mu_i+rnorm(r*g, 0, sigma)
    
    #fit linear model if r=1 (no clusters)
    if (r==1){
      fit<-lm(y~x_i)
    }
    # fit mixed effects model with random intercept
    else{
      fit<-lmer(y~x_i+(1|j))
    }
    
    # extract beta estimate
    beta_hat<-summary(fit)$coefficients[2]
    
  }
  
  else {
    # log link, so exponentiate the normal 
    mu_i<-exp(mu_i)
    
    # generate poisson outcome
    y<-rpois(r*g, mu_i)
    
    # fit linear if r==1
    err=FALSE
    if (r==1){
      fit<-tryCatch({glm(y~x_i, family=poisson)}, error=function(e){
        err<<-TRUE
      })
    }
    # fit poisson mixed effects model with random intercept, NA if errof
    else{
      fit<-tryCatch({glmer(y~x_i+(1|j), family = poisson)}, error=function(e){
        err<<-TRUE
      })
    }
    
    
    # extract coefficient
    beta_hat<-ifelse(err==TRUE, NA, summary(fit)$coefficients[2])
    
  }
  
  data=data.frame(y=y, x=x_i, cluster=j)
  
  return(list(data, beta_hat, num_clusters=g, num_samples=r))
}




gen_data<-function(alpha, beta, g, r, gamma, sigma, distribution, folder, filename){
  #' 
  #' Generate data and saves two files (one with data, one with coefficients and settings)
  #' @param alpha the true intercept of the fixed term in the model
  #' @param beta the true treatment effect of the fixed term in the model
  #' @param g the number of clusters in the data
  #' @param r the number of times to sample from each cluster
  #' @param gamma the standard deviation of the random intercept 
  #' @param sigma the standard deviation of the error term
  #' @distribution one of "normal" or "poisson", the distribution to sample from
  #' @param folder Path to folder in which to save files (include ending slash).
  #' @param filename File name prefix.
  #' @return saves 2 csv files with _data.csv and _coef_setting.csv suffixes.
  #' 
  
  #simulate data
  data<-sim_data(alpha, beta, g, r, gamma, sigma, distribution)
  
  # data file
  df<-data[[1]]
  write.csv(df, paste0(folder, filename,"_data.csv"), row.names=FALSE)
  
  # coef and settings file
  setting_df<-data.frame(alpha, beta, g, r, gamma, sigma, coef_est=data[[2]])
  write.csv(setting_df, paste0(folder,filename,"_coef_setting.csv"), row.names=FALSE)
  
}


get_results_row<-function(iter, alpha, beta, g, r, gamma, sigma, distribution){
  #' 
  #' returns the data settings and the evaluation metrics
  #' @iter number of iterations
  #' @param alpha the true intercept of the fixed term in the model
  #' @param beta the true treatment effect of the fixed term in the model
  #' @param g the number of clusters in the data
  #' @param r the number of times to sample from each cluster
  #' @param gamma the standard deviation of the random intercept 
  #' @param sigma the standard deviation of the error term
  #' @distribution one of "normal" or "poisson", the distribution to sample from
  #' @return data frame row containing the settings used and the variance of the estimator
  #' 
  # initialize vector of beta alphas
  beta_hats<-c()
  
  
  # simulate iter times and extract values
  for (i in 1:iter){
    simulation<-sim_data(alpha, beta, g, r, gamma, sigma, distribution)
    beta_hats<-c(beta_hats, simulation[[2]])
  }
  
  # calculate variance of estimates
  variance<-var(beta_hats, na.rm = TRUE)
  icc<-gamma^2/(gamma^2+sigma^2)
  
  return(data.frame(iterations=iter, alpha, beta, g, r, gamma, sigma, distribution, variance, icc))
  
}

calc_g_r<-function(B, c1, c2){
  #' calculates the range of g values and maximum number of samples from each cluster 
  #' to max out the budget
  #' @param B the budget
  #' @param c1 the cost of the first sample from each cluster
  #' @param c2 the cost of each successive sample from each cluster
  #' @return the sequence of g and r pairs that maximize the budget
  #'
  g<-seq(4, floor(B/c1), 2)
  r<-floor(B/(g*c2)-c1/c2+1)
  
  return(list(g, r))
}

run_experiments<-function(B, c1, c2, iter, alpha, beta, gamma, sigma, distribution, filename){
  #'
  #' runs the simulation for all given settingsn and saves the results to file
  #' @param B the total budget
  #' @param c1 the cost of sampling a cluster for the first time
  #' @param c2 the cost of sampling a cluster after the first time
  #' @param iter number of iterations
  #' @param alpha the true intercept of the fixed term in the model
  #' @param beta the true treatment effect of the fixed term in the model
  #' @param gamma the standard deviation of the random intercept 
  #' @param sigma the standard deviation of the error term
  #' @param one of "normal" or "poisson", the distribution to sample from
  #' @param filename the name of the file to save to, if NA, will not save
  #' @return data frame containing the settings and evaluation measures for each experiment
  
  # calculate g and r pairs
  pairs<-calc_g_r(B, c1, c2)
  
  #extract
  g<-pairs[[1]]
  r<-pairs[[2]]
  
  results<-data.frame()
  # run simulations
  for (i in iter){
    for (a in alpha){
      for(b in beta){
        for(g1 in g){
          for(gam in gamma){
            for(s in sigma){
              row<-get_results_row(i, a, b, g1, r[which(g==g1)], gam, s, distribution)
              results<-rbind(results, row)
            }
          }
          
        }
      }
    }
  }
  
  if(!is.na(filename)){
    write.csv(results, paste0(filename, ".csv"))
  }
  return(results)
}