# read in saved poisson results 
vary_gamma4<-read.csv("95_c2_vary_gamma_pois.csv")
vary_sigma4<-read.csv("95_c2_vary_sigma_pois.csv")
vary_alpha4<-read.csv("95_c2_vary_alpha_pois.csv")
vary_beta4<-read.csv("95_c2_vary_beta_pois.csv")

vary_gamma5<-read.csv("20_c2_vary_gamma_pois.csv")
vary_sigma5<-read.csv("20_c2_vary_sigma_pois.csv")
vary_alpha5<-read.csv("20_c2_vary_alpha_pois.csv")
vary_beta5<-read.csv("20_c2_vary_beta_pois.csv")

vary_gamma6<-read.csv("5_c2_vary_gamma_pois.csv")
vary_sigma6<-read.csv("5_c2_vary_sigma_pois.csv")
vary_alpha6<-read.csv("5_c2_vary_alpha_pois.csv")
vary_beta6<-read.csv("5_c2_vary_beta_pois.csv")


## collect results
opt_gamma1<-rbind(cbind(vary_gamma4, c2=95), cbind(vary_gamma5, c2=20), cbind(vary_gamma6, c2=5))%>%group_by(c2, gamma)%>%filter(variance==min(variance))

opt_alpha1<-rbind(cbind(vary_alpha4, c2=95), cbind(vary_alpha5, c2=20), cbind(vary_alpha6, c2=5))%>%group_by(c2, alpha)%>%filter(variance==min(variance))

opt_beta1<-rbind(cbind(vary_beta4, c2=95), cbind(vary_beta5, c2=20), cbind(vary_beta6, c2=5))%>%group_by(c2, beta)%>%filter(variance==min(variance))


####
####Low Cost Ratio
gamma.plot4 <- ggplot(data = vary_gamma4, aes(x = g, y=variance, color=factor(round(gamma, digits=2)))) +
  geom_line()+
  geom_point(data=opt_gamma1[opt_gamma1$c2==95,], aes(x=g, y=variance, color="optimal (g,r)"))+
  annotate("text", 24, .5, label=paste0("(",as.character(opt_gamma1$g[2]),",", as.character(opt_gamma1$r[2]), ")"), 
           color="red", size=3)+
  annotate("text", 22, 1.4, label=paste0("(",as.character(opt_gamma1$g[1]),",", as.character(opt_gamma1$r[1]), ")"), 
           color="red", size=3)+
  annotate("text", 44, .5, label=paste0("(",as.character(opt_gamma1$g[3]),",", as.character(opt_gamma1$r[3]), ")"), 
           color="red", size=3)+
  labs(
    title = "Varying Gamma",
    x = "Number of Clusters (g)",
    y = "Variance") +
  theme_minimal() +
  scale_color_manual(name="Gamma",values=c("0.33"="orange", "1"="green","3"="blue","optimal (g,r)"="red"))+
  theme(legend.position = "bottom")
#gamma.plot4



alpha.plot4 <- ggplot(data = vary_alpha4, aes(x = g, y=variance, color=factor(alpha))) +
  geom_line()+
  geom_point(data=opt_alpha1[opt_alpha1$c2==95,], aes(x=g, y=variance, color="optimal (g,r)"))+
  annotate("text", 15, .1, label=paste0("(",as.character(opt_alpha1$g[2]),",", as.character(opt_alpha1$r[2]), ")"), 
           color="red", size=3)+
  annotate("text", 27, .12, label=paste0("(",as.character(opt_alpha1$g[1]),",", as.character(opt_alpha1$r[1]), ")"), 
           color="red", size=3)+
  annotate("text", 4, .08, label=paste0("(",as.character(opt_alpha1$g[3]),",", as.character(opt_alpha1$r[3]), ")"), 
           color="red", size=3)+
  labs(
    title = "Varying Alpha",
    x = "Number of Clusters (g)",
    y = "Variance") +
  theme_minimal() +
  scale_color_manual(name="Alpha",values=c("0.5"="orange", "2"="green", "10"="blue","optimal (g,r)"="red"))+
  theme(legend.position = "bottom")
#alpha.plot4


beta.plot4 <- ggplot(data = vary_beta4, aes(x = g, y=variance, color=factor(beta))) +
  geom_line()+
  geom_point(data=opt_beta1[opt_beta1$c2==95,], aes(x=g, y=variance, color="optimal (g,r)"))+
  annotate("text", 6, .1, label=paste0("(",as.character(opt_beta1$g[2]),",", as.character(opt_beta1$r[2]), ")"), 
           color="red", size=3)+
  annotate("text", 22, .1, label=paste0("(",as.character(opt_beta1$g[1]),",", as.character(opt_beta1$r[1]), ")"), 
           color="red", size=3)+
  annotate("text", 27.5, .11, label=paste0("(",as.character(opt_beta1$g[3]),",", as.character(opt_beta1$r[3]), ")"), 
           color="red", size=3)+
  labs(
    title = "Varying Beta",
    x = "Number of Clusters (g)",
    y = "Variance") +
  theme_minimal() +
  scale_color_manual(name="Alpha",values=c("0.5"="orange", "2"="green", "10"="blue","optimal (g,r)"="red"))+
  theme(legend.position = "bottom")
#beta.plot4

grid.arrange(gamma.plot4, alpha.plot4, beta.plot4, nrow=2)



####
####Moderate Cost Ratio
gamma.plot5 <- ggplot(data = vary_gamma5, aes(x = g, y=variance, color=factor(round(gamma, digits=2)))) +
  geom_line()+
  geom_point(data=opt_gamma1[opt_gamma1$c2==20,], aes(x=g, y=variance, color="optimal (g,r)"))+
  annotate("text", 4, .3, label=paste0("(",as.character(opt_gamma1$g[4]),",", as.character(opt_gamma1$r[4]), ")"), 
           color="red", size=3)+
  annotate("text", 8.3, .3, label=paste0("(",as.character(opt_gamma1$g[5]),",", as.character(opt_gamma1$r[5]), ")"), 
           color="red", size=3)+
  annotate("text", 30, .8, label=paste0("(",as.character(opt_gamma1$g[6]),",", as.character(opt_gamma1$r[6]), ")"), 
           color="red", size=3)+
  labs(
    title = "Varying Gamma",
    x = "Number of Clusters (g)",
    y = "Variance") +
  theme_minimal() +
  scale_color_manual(name="Gamma",values=c("0.33"="orange", "1"="green","3"="blue","optimal (g,r)"="red"))+
  theme(legend.position = "bottom")
#gamma.plot5



alpha.plot5<- ggplot(data = vary_alpha5, aes(x = g, y=variance, color=factor(alpha))) +
  geom_line()+
  geom_point(data=opt_alpha1[opt_alpha1$c2==20,], aes(x=g, y=variance, color="optimal (g,r)"))+
  annotate("text", 8, .05, label=paste0("(",as.character(opt_alpha1$g[4]),",", as.character(opt_alpha1$r[4]), ")"), 
           color="red", size=3)+
  annotate("text", 15, 0.02, label=paste0("(",as.character(opt_alpha1$g[6]),",", as.character(opt_alpha1$r[6]), ")"), 
           color="red", size=3)+
  labs(
    title = "Varying Alpha",
    x = "Number of Clusters (g)",
    y = "Variance") +
  theme_minimal() +
  scale_color_manual(name="Alpha",values=c("0.5"="orange", "2"="green", "10"="blue","optimal (g,r)"="red"))+
  theme(legend.position = "bottom")
#alpha.plot5


beta.plot5 <- ggplot(data = vary_beta5, aes(x = g, y=variance, color=factor(beta))) +
  geom_line()+
  geom_point(data=opt_beta1[opt_beta1$c2==20,], aes(x=g, y=variance, color="optimal (g,r)"))+
  annotate("text", 4, .04, label=paste0("(",as.character(opt_beta1$g[4]),",", as.character(opt_beta1$r[4]), ")"), 
           color="red", size=3)+
  annotate("text", 8, .02, label=paste0("(",as.character(opt_beta1$g[6]),",", as.character(opt_beta1$r[6]), ")"), 
           color="red", size=3)+
  labs(
    title = "Varying Beta",
    x = "Number of Clusters (g)",
    y = "Variance") +
  theme_minimal() +
  scale_color_manual(name="Alpha",values=c("0.5"="orange", "2"="green", "10"="blue","optimal (g,r)"="red"))+
  theme(legend.position = "bottom")
#beta.plot5


grid.arrange(gamma.plot5, alpha.plot5, beta.plot5, nrow=2)




####
##### High Cost Ratio
gamma.plot6 <- ggplot(data = vary_gamma6, aes(x = g, y=variance, color=factor(round(gamma, digits=2)))) +
  geom_line()+
  geom_point(data=opt_gamma1[opt_gamma1$c2==5,], aes(x=g, y=variance, color="optimal (g,r)"))+
  annotate("text", 10, .2, label=paste0("(",as.character(opt_gamma1$g[7]),",", as.character(opt_gamma1$r[7]), ")"), 
           color="red", size=3)+
  annotate("text", 32, .5, label=paste0("(",as.character(opt_gamma1$g[1]),",", as.character(opt_gamma1$r[1]), ")"), 
           color="red", size=3)+
  labs(
    title = "Varying Gamma",
    x = "Number of Clusters (g)",
    y = "Variance") +
  theme_minimal() +
  scale_color_manual(name="Gamma",values=c("0.33"="orange", "1"="green","3"="blue","optimal (g,r)"="red"))+
  theme(legend.position = "bottom")
#gamma.plot6



alpha.plot6 <- ggplot(data = vary_alpha6, aes(x = g, y=variance, color=factor(alpha))) +
  geom_line()+
  geom_point(data=opt_alpha1[opt_alpha1$c2==5,], aes(x=g, y=variance, color="optimal (g,r)"))+
  annotate("text", 10, .0175, label=paste0("(",as.character(opt_alpha1$g[8]),",", as.character(opt_alpha1$r[8]), ")"), 
           color="red", size=3)+
  annotate("text", 4, .0175, label=paste0("(",as.character(opt_alpha1$g[7]),",", as.character(opt_alpha1$r[7]), ")"), 
           color="red", size=3)+
  annotate("text", 6, 0, label=paste0("(",as.character(opt_alpha1$g[9]),",", as.character(opt_alpha1$r[9]), ")"), 
           color="red", size=3)+
  labs(
    title = "Varying Alpha",
    x = "Number of Clusters (g)",
    y = "Variance") +
  theme_minimal() +
  scale_color_manual(name="Alpha",values=c("0.5"="orange", "2"="green", "10"="blue","optimal (g,r)"="red"))+
  theme(legend.position = "bottom")
#alpha.plot6


beta.plot6 <- ggplot(data = vary_beta6, aes(x = g, y=variance, color=factor(beta))) +
  geom_line()+
  geom_point(data=opt_beta1[opt_beta1$c2==5,], aes(x=g, y=variance, color="optimal (g,r)"))+
  annotate("text", 4.5, .02, label=paste0("(",as.character(opt_beta1$g[7]),",", as.character(opt_beta1$r[7]), ")"), 
           color="red", size=3)+
  labs(
    title = "Varying Beta",
    x = "Number of Clusters (g)",
    y = "Variance") +
  theme_minimal() +
  scale_color_manual(name="Alpha",values=c("0.5"="orange", "2"="green", "10"="blue","optimal (g,r)"="red"))+
  theme(legend.position = "bottom")
#beta.plot6

#grid.arrange(gamma.plot6, alpha.plot6, beta.plot6, nrow=2)


# summary table
full2<-rbind(opt_alpha1, opt_beta1, opt_gamma1)%>%filter(c2==5)%>%
  select(g, r, variance)
full3<-cbind(value=c("0.5", "2", "10", "0.5", "2", "10", "1","3","0.33"), full2,
             parameter=c(rep("alpha", 3), 
                         rep("beta", 3), rep("gamma", 3)))

kable(full3[,-c(2, 3, 7)], digits=c(2, 0, 0,3), caption="Optimal Designs For Varying Parameters under High Cost Ratio, Poisson") %>% 
  pack_rows(index = table(full3$parameter))
