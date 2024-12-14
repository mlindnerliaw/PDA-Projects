# read in saved normal results
vary_gamma1<-read.csv("95_c2_vary_gamma.csv")
vary_sigma1<-read.csv("95_c2_vary_sigma.csv")
vary_alpha1<-read.csv("95_c2_vary_alpha.csv")
vary_beta1<-read.csv("95_c2_vary_beta.csv")

vary_gamma2<-read.csv("20_c2_vary_gamma.csv")
vary_sigma2<-read.csv("20_c2_vary_sigma.csv")
vary_alpha2<-read.csv("20_c2_vary_alpha.csv")
vary_beta2<-read.csv("20_c2_vary_beta.csv")

vary_gamma3<-read.csv("5_c2_vary_gamma.csv")
vary_sigma3<-read.csv("5_c2_vary_sigma.csv")
vary_alpha3<-read.csv("5_c2_vary_alpha.csv")
vary_beta3<-read.csv("5_c2_vary_beta.csv")

# collect optimals for each parameter
opt_gamma<-rbind(cbind(vary_gamma1, c2=95), cbind(vary_gamma2, c2=20), cbind(vary_gamma3, c2=5))%>%group_by(c2, gamma)%>%filter(variance==min(variance))

opt_sigma<-rbind(cbind(vary_sigma1, c2=95), cbind(vary_sigma2, c2=20), cbind(vary_sigma3, c2=5))%>%group_by(c2, sigma)%>%filter(variance==min(variance))

opt_alpha<-rbind(cbind(vary_alpha1, c2=95), cbind(vary_alpha2, c2=20), cbind(vary_alpha3, c2=5))%>%group_by(c2, alpha)%>%filter(variance==min(variance))

opt_beta<-rbind(cbind(vary_beta1, c2=95), cbind(vary_beta2, c2=20), cbind(vary_beta3, c2=5))%>%group_by(c2, beta)%>%filter(variance==min(variance))


######
###### Low Cost Ratio
gamma.plot1 <- ggplot(data = vary_gamma1, aes(x = g, y=variance, color=factor(round(gamma, digits=2)))) +
  geom_line()+
  geom_point(data=opt_gamma[opt_gamma$c2==95,], aes(x=g, y=variance, color="optimal (g,r)"))+
  annotate("text", 5, .7, label=paste0("(",as.character(opt_gamma$g[2]),",", as.character(opt_gamma$r[2]), ")"), 
           color="red", size=3)+
  annotate("text", 5, .25, label=paste0("(",as.character(opt_gamma$g[1]),",", as.character(opt_gamma$r[1]), ")"), 
           color="red", size=3)+
  annotate("text", 50, .2, label=paste0("(",as.character(opt_gamma$g[3]),",", as.character(opt_gamma$r[3]), ")"), 
           color="red", size=3)+
  labs(
    title = "Varying Gamma",
    x = "Number of Clusters (g)",
    y = "Variance") +
  theme_minimal() +
  scale_color_manual(name="Gamma",values=c("0.33"="orange", "1"="green","3"="blue","optimal (g,r)"="red"))+
  theme(legend.position = "bottom")
#gamma.plot1


sigma.plot1 <- ggplot(data = vary_sigma1, aes(x = g, y=variance, color=factor(round(sigma, digits=2)))) +
  geom_line()+
  geom_point(data=opt_sigma[opt_sigma$c2==95,], aes(x=g, y=variance, color="optimal (g,r)"))+
  annotate("text", 5, .65, label=paste0("(",as.character(opt_sigma$g[2]),",", as.character(opt_sigma$r[2]), ")"), 
           color="red", size=3)+
  annotate("text", 5, .25, label=paste0("(",as.character(opt_sigma$g[1]),",", as.character(opt_sigma$r[1]), ")"), 
           color="red", size=3)+
  annotate("text", 16, .2, label=paste0("(",as.character(opt_sigma$g[3]),",", as.character(opt_sigma$r[3]), ")"), 
           color="red", size=3)+
  labs(
    title = "Varying Sigma",
    x = "Number of Clusters (g)",
    y = "Variance") +
  theme_minimal() +
  scale_color_manual(name="Sigma",values=c("0.33"="orange", "1"="green","3"="blue","optimal (g,r)"="red"))+
  theme(legend.position = "bottom")
#sigma.plot1


alpha.plot1 <- ggplot(data = vary_alpha1, aes(x = g, y=variance, color=factor(alpha))) +
  geom_line()+
  geom_point(data=opt_alpha[opt_alpha$c2==95,], aes(x=g, y=variance, color="optimal (g,r)"))+
  annotate("text", 24, .13, label=paste0("(",as.character(opt_alpha$g[2]),",", as.character(opt_alpha$r[2]), ")"), 
           color="red", size=3)+
  annotate("text", 10, .127, label=paste0("(",as.character(opt_alpha$g[1]),",", as.character(opt_alpha$r[1]), ")"), 
           color="red", size=3)+
  annotate("text", 6, .135, label=paste0("(",as.character(opt_alpha$g[3]),",", as.character(opt_alpha$r[3]), ")"), 
           color="red", size=3)+
  labs(
    title = "Varying Alpha",
    x = "Number of Clusters (g)",
    y = "Variance") +
  theme_minimal() +
  scale_color_manual(name="Alpha",values=c("0.5"="orange", "2"="green", "10"="blue","optimal (g,r)"="red"))+
  theme(legend.position = "bottom")
#alpha.plot1


beta.plot1 <- ggplot(data = vary_beta1, aes(x = g, y=variance, color=factor(beta))) +
  geom_line()+
  geom_point(data=opt_beta[opt_beta$c2==95,], aes(x=g, y=variance, color="optimal (g,r)"))+
  annotate("text", 12.75, .142, label=paste0("(",as.character(opt_beta$g[2]),",", as.character(opt_beta$r[2]), ")"), 
           color="red", size=3)+
  annotate("text", 7.9, .1375, label=paste0("(",as.character(opt_beta$g[1]),",", as.character(opt_beta$r[1]), ")"), 
           color="red", size=3)+
  annotate("text", 50, .13, label=paste0("(",as.character(opt_beta$g[3]),",", as.character(opt_beta$r[3]), ")"), 
           color="red", size=3)+
  labs(
    title = "Varying Beta",
    x = "Number of Clusters (g)",
    y = "Variance") +
  theme_minimal() +
  scale_color_manual(name="Alpha",values=c("0.5"="orange", "2"="green", "10"="blue","optimal (g,r)"="red"))+
  theme(legend.position = "bottom")
#beta.plot1

grid.arrange(gamma.plot1, alpha.plot1, beta.plot1, nrow=2)


######
###### Moderate cost ratio
gamma.plot2 <- ggplot(data = vary_gamma2, aes(x = g, y=variance, color=factor(round(gamma, digits=2)))) +
  geom_line()+
  geom_point(data=opt_gamma[opt_gamma$c2==20,], aes(x=g, y=variance, color="optimal (g,r)"))+
  annotate("text", 5.5, .08, label=paste0("(",as.character(opt_gamma$g[4]),",", as.character(opt_gamma$r[4]), ")"), 
           color="red", size=3)+
  annotate("text", 10.5, .1, label=paste0("(",as.character(opt_gamma$g[5]),",", as.character(opt_gamma$r[5]), ")"), 
           color="red", size=3)+
  labs(
    title = "Varying Gamma",
    x = "Number of Clusters (g)",
    y = "Variance") +
  theme_minimal() +
  scale_color_manual(name="Gamma",values=c("0.33"="orange", "1"="green","3"="blue","optimal (g,r)"="red"))+
  theme(legend.position = "bottom")
#gamma.plot2


alpha.plot2 <- ggplot(data = vary_alpha2, aes(x = g, y=variance, color=factor(alpha))) +
  geom_line()+
  geom_point(data=opt_alpha[opt_alpha$c2==20,], aes(x=g, y=variance, color="optimal (g,r)"))+
  annotate("text", 10, .02, label=paste0("(",as.character(opt_alpha$g[4]),",", as.character(opt_alpha$r[4]), ")"), 
           color="red", size=3)+
  annotate("text", 6, .05, label=paste0("(",as.character(opt_alpha$g[5]),",", as.character(opt_alpha$r[5]), ")"), 
           color="red", size=3)+
  labs(
    title = "Varying Alpha",
    x = "Number of Clusters (g)",
    y = "Variance") +
  theme_minimal() +
  scale_color_manual(name="Alpha",values=c("0.5"="orange", "2"="green", "10"="blue","optimal (g,r)"="red"))+
  theme(legend.position = "bottom")
#alpha.plot1


beta.plot2 <- ggplot(data = vary_beta2, aes(x = g, y=variance, color=factor(beta))) +
  geom_line()+
  geom_point(data=opt_beta[opt_beta$c2==20,], aes(x=g, y=variance, color="optimal (g,r)"))+
  annotate("text", 4, .05, label=paste0("(",as.character(opt_beta$g[4]),",", as.character(opt_beta$r[4]), ")"), 
           color="red", size=3)+
  annotate("text", 10, .025, label=paste0("(",as.character(opt_beta$g[6]),",", as.character(opt_beta$r[6]), ")"), 
           color="red", size=3)+
  
  labs(
    title = "Varying Beta",
    x = "Number of Clusters (g)",
    y = "Variance") +
  theme_minimal() +
  scale_color_manual(name="Alpha",values=c("0.5"="orange", "2"="green", "10"="blue","optimal (g,r)"="red"))+
  theme(legend.position = "bottom")
#beta.plot1

grid.arrange(gamma.plot2, alpha.plot2, beta.plot2, nrow=2)



####
#### High Cost ratio
gamma.plot3 <- ggplot(data = vary_gamma3, aes(x = g, y=variance, color=factor(round(gamma, digits=2)))) +
  geom_line()+
  geom_point(data=opt_gamma[opt_gamma$c2==5,], aes(x=g, y=variance, color="optimal (g,r)"))+
  annotate("text", 4, .009, label=paste0("(",as.character(opt_gamma$g[7]),",", as.character(opt_gamma$r[7]), ")"), 
           color="red", size=3)+
  annotate("text", 6, .045, label=paste0("(",as.character(opt_gamma$g[8]),",", as.character(opt_gamma$r[8]), ")"), 
           color="red", size=3)+
  annotate("text", 10, .005, label=paste0("(",as.character(opt_gamma$g[9]),",", as.character(opt_gamma$r[9]), ")"), 
           color="red", size=3)+
  labs(
    title = "Varying Gamma",
    x = "Number of Clusters (g)",
    y = "Variance") +
  theme_minimal() +
  scale_color_manual(name="Gamma",values=c("0.33"="orange", "1"="green","3"="blue","optimal (g,r)"="red"))+
  theme(legend.position = "bottom")
#gamma.plot3


alpha.plot3 <- ggplot(data = vary_alpha3, aes(x = g, y=variance, color=factor(alpha))) +
  geom_line()+
  geom_point(data=opt_alpha[opt_alpha$c2==5,], aes(x=g, y=variance, color="optimal (g,r)"))+
  annotate("text", 4, .007, label=paste0("(",as.character(opt_alpha$g[8]),",", as.character(opt_alpha$r[8]), ")"), 
           color="red", size=3)+
  annotate("text", 6, .009, label=paste0("(",as.character(opt_alpha$g[7]),",", as.character(opt_alpha$r[7]), ")"), 
           color="red", size=3)+
  labs(
    title = "Varying Alpha",
    x = "Number of Clusters (g)",
    y = "Variance") +
  theme_minimal() +
  scale_color_manual(name="Alpha",values=c("0.5"="orange", "2"="green", "10"="blue","optimal (g,r)"="red"))+
  theme(legend.position = "bottom")
#alpha.plot3


beta.plot3 <- ggplot(data = vary_beta3, aes(x = g, y=variance, color=factor(beta))) +
  geom_line()+
  geom_point(data=opt_beta[opt_beta$c2==5,], aes(x=g, y=variance, color="optimal (g,r)"))+
  annotate("text", 4, .008, label=paste0("(",as.character(opt_beta$g[7]),",", as.character(opt_beta$r[7]), ")"), 
           color="red", size=3)+
  annotate("text", 10, .01, label=paste0("(",as.character(opt_beta$g[8]),",", as.character(opt_beta$r[8]), ")"), 
           color="red", size=3)+
  
  labs(
    title = "Varying Beta",
    x = "Number of Clusters (g)",
    y = "Variance") +
  theme_minimal() +
  scale_color_manual(name="Alpha",values=c("0.5"="orange", "2"="green", "10"="blue","optimal (g,r)"="red"))+
  theme(legend.position = "bottom")
#beta.plot3

#grid.arrange(gamma.plot3, alpha.plot3, beta.plot3, nrow=2)


## create data frame for summary table
full<-rbind(opt_alpha, opt_beta, opt_gamma)%>%filter(c2==5)%>%
  select(g, r, variance)
full1<-cbind(value=c("0.5", "2", "10", "0.5", "2", "10", "1","3","0.33"), full,
             parameter=c(rep("alpha", 3), 
                         rep("beta", 3), rep("gamma", 3)))

kable(full1[,-c(2, 3, 7)], digits=c(2, 0, 0,3), caption="Optimal Designs For Varying Parameters under High Cost Ratio") %>% 
  pack_rows(index = table(full1$parameter))