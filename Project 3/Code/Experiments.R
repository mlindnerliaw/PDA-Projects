#######################Normal
set.seed(1)
vary_gamma1<-run_experiments(B=5000, c1=100, c2=95, 300, 1, 2, c(1/3, 1, 3), 1, "normal", "95_c2_vary_gamma")
vary_sigma1<-run_experiments(B=5000, c1=100, c2=95, 300, 1, 2, 1, c(1/3, 1, 3), "normal", "95_c2_vary_sigma")
vary_alpha1<-run_experiments(B=5000, c1=100, c2=95, 300, c(.5, 2, 10), 2, 1, 1, "normal", "95_c2_vary_alpha")
vary_beta1<-run_experiments(B=5000, c1=100, c2=95, 300, 1, c(.5, 2, 10), 1, 1, "normal", "95_c2_vary_beta")

# c2=20
vary_gamma2<-run_experiments(B=5000, c1=100, c2=20, 300, 1, 2, c(1/3, 1, 3), 1, "normal", "20_c2_vary_gamma")
vary_sigma2<-run_experiments(B=5000, c1=100, c2=20, 300, 1, 2, 1, c(1/3, 1, 3), "normal", "20_c2_vary_sigma")
vary_alpha2<-run_experiments(B=5000, c1=100, c2=20, 300, c(.5, 2, 10), 2, 1, 1, "normal", "20_c2_vary_alpha")
vary_beta2<-run_experiments(B=5000, c1=100, c2=20, 300, 1, c(.5, 2, 10), 1, 1, "normal", "20_c2_vary_beta")

# c2=5
vary_gamma3<-run_experiments(B=5000, c1=100, c2=5, 300, 1, 2, c(1/3, 1, 3), 1, "normal", "5_c2_vary_gamma")
vary_sigma3<-run_experiments(B=5000, c1=100, c2=5, 300, 1, 2, 1, c(1/3, 1, 3), "normal", "5_c2_vary_sigma")
vary_alpha3<-run_experiments(B=5000, c1=100, c2=5, 300, c(.5, 2, 10), 2, 1, 1, "normal", "5_c2_vary_alpha")
vary_beta3<-run_experiments(B=5000, c1=100, c2=5, 300, 1, c(.5, 2, 10), 1, 1, "normal", "5_c2_vary_beta")

################Poisson
set.seed(1)
vary_gamma4<-run_experiments(B=5000, c1=100, c2=95, 300, 1, 2, c(1/3, 1, 3), 1, "poisson", 
                             "95_c2_vary_gamma_pois")
vary_sigma4<-run_experiments(B=5000, c1=100, c2=95, 300, 1, 2, 1, c(1/3, 1, 3), "poisson", 
                             "95_c2_vary_sigma_pois")
vary_alpha4<-run_experiments(B=5000, c1=100, c2=95, 300, c(.5,2, 10), 2, 1, 1, "poisson", 
                             "95_c2_vary_alpha_pois")
vary_beta4<-run_experiments(B=5000, c1=100, c2=95, 300, 1, c(.5, 2, 10), 1, 1, "poisson", 
                            "95_c2_vary_beta_pois")

# c2=20
vary_gamma5<-run_experiments(B=5000, c1=100, c2=20, 300, 1, 2, c(1/3, 1, 3), 1, "poisson", 
                             "20_c2_vary_gamma_pois")
vary_sigma5<-run_experiments(B=5000, c1=100, c2=20, 300, 1, 2, 1, c(1/3, 1, 3), "poisson", 
                             "20_c2_vary_sigma_pois")
vary_alpha5<-run_experiments(B=5000, c1=100, c2=20, 300, c(.5,2, 10), 2, 1, 1, "poisson",
                             "20_c2_vary_alpha_pois")
vary_beta5<-run_experiments(B=5000, c1=100, c2=20, 300, 1, c(.5, 2, 10), 1, 1, "poisson", 
                            "20_c2_vary_beta_pois")