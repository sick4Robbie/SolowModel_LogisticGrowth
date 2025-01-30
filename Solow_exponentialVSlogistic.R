##### Solow Model: population growth comparison #####
library(dplyr)
library(tidyverse)
library(patchwork)
gc() #garbage collection for memory purposes
rm(list=ls()) #clear memory
cat("\014")  #clear console
  

#### parameters and initial variable vectors ####
Period = c(1:200) #discrete time
alpha = 1/3 #capital share of production
delta = 0.1 #depreciation rate
s = 0.2 #constant savings rate of households

## exponential population growth
  n = 0.053 #exponential growth rate of (working) population
## Logistic population growth
  # function: L(t) = L_final / (1+exp(b*(x_m-t)))
  L_init = 0.0001
  L_final = L_init*exp(n*length(Period)) #both models approach the same L-value
  b = 0.1 #slope of logistic population growth
  x_m = 0.5*length(Period) #turning point of the logistic function

##initial vectors 
Y=K=Kdot=L=C=r=w=c(rep(L_init,length(Period))) # exponential growth variables
Yl=Kl=Kdotl=Ll=Cl=rl=wl=c(rep(L_init,length(Period))) # logistic growth variables

#### exponential growth block ####
for (t in 1:length(Period)){
  #population evolution
  L[t] = L_init*exp(n*t)
  #aggregate output
  Y[t] = K[t]^alpha*L[t]^(1-alpha)
  #capital growth
  Kdot[t] = s*Y[t]-delta*K[t]
  #capital accumulation
  K[t+1] = K[t]+Kdot[t]
  #aggregate consumption
  C[t] = (1-s)*Y[t]
  #interest rate / MPK
  r[t] = alpha*(K[t]/L[t])^(alpha-1)
  # wages / MPL
  w[t] = (1-alpha)*(K[t]/L[t])^alpha
  }
K <- K[-length(K)] #remove the n+1-th entry of K
results_exponential <- data.frame(Y,L,(K-lag(K))/K,K,C,K/L,Y/L,C/L,r,w)
names(results_exponential) <- c("Y","L","g_K","K","C","K/L","Y/L","C/L","r","w")

#### logistic growth block ####
for (t in 1:length(Period)){
  #population evolution
  Ll[t] = L_final/(1+exp(b*(x_m-t)))
  #aggregate output
  Yl[t] = Kl[t]^alpha*Ll[t]^(1-alpha)
  #capital growth
  Kdotl[t] = s*Yl[t]-delta*Kl[t]
  #capital accumulation
  Kl[t+1] <- Kl[t]+Kdotl[t]
  #aggregate consumption
  Cl[t] = (1-s)*Yl[t]
  #interest rate / MPK
  rl[t] = alpha*(Kl[t]/Ll[t])^(alpha-1)
  # wages / MPL
  wl[t]= (1-alpha)*(Kl[t]/Ll[t])^alpha
  }
Kl<-Kl[-length(Kl)] # remove 201st entry

results_logistic <- data.frame(Yl,Ll,(Kl-lag(Kl))/lag(Kl),Kl,Cl,Kl/Ll,Yl/Ll,Cl/Ll,rl,wl)
names(results_logistic) <- c("Y","L","g_K","K","C","K/L","Y/L","C/L","r","w")


#### Rate of Convergence ####
kss_exp = (s/(delta+n))^(1/(1-alpha))
kss_log = (s/delta)^(1/(1-alpha)) 
## create roc values
results_exponential %>% mutate(roc = (lag(`K/L`)-`K/L`)/(`K/L`-kss_exp)) -> results_exponential
results_logistic %>% mutate(roc = (lag(`K/L`)-`K/L`)/(`K/L`-kss_log)) -> results_logistic
results_exponential %>% mutate(Period) -> results_exponential
results_logistic %>% mutate(Period) -> results_logistic

#### Plots ####
#long-tables for plotting
results_exponential %>%
  pivot_longer(!Period, names_to="Variables", values_to= "Values") -> long_exponential
results_logistic %>%
  pivot_longer(!Period, names_to="Variables", values_to= "Values") -> long_logistic

data.plot <-  subset(long_exponential,Variables %in% c("Period","Y","L","K","C"))
plot_exptotals <- ggplot(data.plot,aes(x=Period,y=Values,color=Variables)) +
  theme_classic() +
  labs(title="Exponential Population Growth: Total Values",y="") +
  geom_point()+ scale_color_hue(l=40, c=35)
data.plot <-  subset(long_logistic,Variables %in% c("Period","L","Y","C","K"))
plot_logtotals <- ggplot(data.plot, aes(x=Period,y=Values,color=Variables)) +
  theme_classic() +
  labs(title="Logistic Population Growth: Total Values",y="") +
  geom_point()+ scale_color_hue(l=40, c=35)

data.plot <-  subset(long_exponential,Variables %in% c("Period","L","Y/L","C/L","K/L"))
plot_exponential<- ggplot(data.plot,aes(x=Period,y=Values,color=Variables)) +
  theme_classic() +
  labs(title="Exponential Population Growth: Per Capita Values",y="") +
  geom_point()
data.plot <-  subset(long_logistic,Variables %in% c("Period","L","Y/L","C/L","K/L"))
plot_logistic<- ggplot(data.plot,aes(x=Period,y=Values,color=Variables)) +
  theme_classic() +
  labs(title="Logistic Population Growth: Per Capita Values",y="") +
  geom_point()

data.plot <-  subset(long_exponential,Variables %in% c("Period","w","r"))
plot_expmarginal<- ggplot(data.plot,aes(x=Period,y=Values,color=Variables)) +
  theme_classic() +
  labs(title="Exponential Population Growth: Marginal Products",y="") +
  geom_point() +  geom_point() + scale_color_manual(values=c("chartreuse4", "blue4"))
data.plot <-  subset(long_logistic,Variables %in% c("Period","w","r"))
plot_logmarginal<- ggplot(data.plot,aes(x=Period,y=Values,color=Variables)) +
  theme_classic() +
  labs(title="Logistic Population Growth: Marginal Products",y="") +
  geom_point() + scale_color_manual(values=c("chartreuse4", "blue4"))

data.plot <-  subset(long_exponential,Variables %in% c("roc","g_K"))
plot_rocexp <- ggplot(data.plot, aes(x=Period,y=Values, color = Variables))+
  theme_classic() +
  labs(title="Exponential Population Growth: Rate of Convergence & Capital Growth Rate",y="") +
  geom_point() + scale_color_manual(values=c("chartreuse2", "burlywood3"))
data.plot <-  subset(long_logistic,Variables %in% c("roc","g_K"))
plot_roclog <- ggplot(data.plot, aes(x=Period,y=Values, color = Variables))+
  theme_classic() +
  labs(title="Logistic Population Growth: Rate of Convergence & Capital Growth Rate",y="") +
  geom_point() + scale_color_manual(values=c("chartreuse2", "burlywood3"))

rm(long_exponential,long_logistic,data.plot)
#### Output ####
 #print graphs
(plot_exponential + plot_logistic) /
(plot_exptotals + plot_logtotals) /
(plot_expmarginal + plot_logmarginal)  /
(plot_rocexp + plot_roclog)
## statistics
## Summary
summary(results_exponential[,-c(11:12)])
summary(results_logistic[,-c(11:12)])
## final period values
results_exponential[length(Period),]
paste("k_ss in the exponential growth scenario is ", kss_exp)
results_logistic[length(Period),]
paste("k_ss in the logistic growth scenario is ", kss_log)

