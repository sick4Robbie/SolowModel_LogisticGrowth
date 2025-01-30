#This version plots the per capita consumption rate as a function of the savings rate.



##### Solow Model: population growth comparison #####
library(dplyr)
library(tidyverse)
library(patchwork)
gc() #garbage collection for memory purposes
rm(list=ls()) #clear memory
cat("\014")  #clear console

#### parameters and initial variable vectors ####
Period = c(1:200) #discrete time
alpha = 0.4 #capital share of production
delta = 0.35 #depreciation rate
# s = sav #constant savings rate of households

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

c_ss_exp <- function(s) {
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
  #### Rate of Convergence ####
  kss_exp = (s/(delta+n))^(1/(1-alpha))
  kss_log = (s/delta)^(1/(1-alpha)) 
  ## create roc values
  results_exponential %>% mutate(roc = (lag(`K/L`)-`K/L`)/(`K/L`-kss_exp)) -> results_exponential
  results_exponential %>% mutate(Period) -> results_exponential

  
  
  return (results_exponential$`C/L`[length(Period)])
  }


css <- data.frame(c(rep(0,201)),c(rep(0,201)))
names(css)<- c("Savings Rate","Steady State per Capita Consumption")
    for(i in 1:201){
    css[i,1]<- i/200
    css[i,2]<-c_ss_exp(i/200)      
    }

ggplot(data=css,aes(x=css[,1], y=css[,2]))+
  geom_point()+  
  labs(x="Savings Rate",y="",title="Steady State per Capita Consumption")
summary(css)
