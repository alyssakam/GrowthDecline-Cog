# Growth and Decline of Cognitive Ability----
# Author: Kam et al.
# Date: July 23 2026

#Run required packages
require(ggplot2)
require(nlme)
require(psych)
require(dplyr)


#Ability score/theta estimates will be centered to be 0 at age 7 
#Age is centered at 7

#Read in data
theta.all <- read.csv("subtesttheta_5_7_25.csv", header = TRUE, na = ".")

#Center age at 7
theta.all$Age7 <- theta.all$Age - 7

#Information----

#Center ability score/theta estimates to be 0 at age 7
theta.all$INF_Theta1 <- theta.all$INF_Theta - mean(theta.all$INF_Theta[theta.all$Age == 7], na.rm=T)

#Linear----
linear_inf <- nlme(INF_Theta1 ~ b1*Age7, 
                   data = theta.all, 
                   subset = !is.na(INF_Theta1),
                   fixed = list(b1 ~ 1),
                   random = b1  ~ 1|FAMID/ID,
                   start = c(b1 = .5))
summary(linear_inf)
intervals(linear_inf, which = "fixed")

#Exponential----
expo_inf <- nlme(INF_Theta1 ~ a0 - (a0 - b0)*exp(-1*b1*Age7), 
                 data = theta.all, 
                 subset = !is.na(INF_Theta1),
                 fixed = list(a0 ~ 1, 
                              b0 ~ 1, 
                              b1 ~ 1), 
                 random = a0 + b0 ~ 1|FAMID/ID,  
                 start = c(a0 = .5, b0 = -.1, b1 = .5))
summary(expo_inf)
intervals(expo_inf, which = "fixed")

#Quadratic----
quad_inf <- nlme(INF_Theta1 ~ b0 + b1*Age7 + b2*(Age7^2), 
                 data = theta.all, 
                 subset = !is.na(INF_Theta1),
                 fixed = list(b0 ~ 1, b1 ~ 1, b2 ~ 1),
                 random = b0  ~ 1|FAMID/ID,
                 start = list(fixed = c(b0 = -1, b1 = .5, b2 = .2)))
summary(quad_inf)
intervals(quad_inf, which = "fixed")

#Discontinuous----
#Information vertex at 39
theta.all$Age7 <- theta.all$Age - 7
theta.all$Age4 <- ifelse(theta.all$Age7 > 32, 32, theta.all$Age7)
theta.all$Age5 <- ifelse(theta.all$Age7 > 32, theta.all$Age7-32, 0)
theta.allx <- theta.all[,c("Age", "Age7", "Age4", "Age5")]
theta.allx <- unique(theta.allx)
print(arrange(theta.allx, Age7))

disc_inf <- nlme(INF_Theta1 ~ a0 - (a0 - b0)*exp(-b1*Age4) + b2*Age5, 
                  data = theta.all, subset = !is.na(INF_Theta1), 
                  fixed = list(a0 ~ 1, b0 ~ 1, b1 ~ 1, b2 ~ 1), 
                  random = a0 + b0  ~ 1|FAMID/ID,  
                  start = list(fixed = c(a0 = .5, b0 = .1, b1 = .1, b2 = .1)))

summary(disc_inf)
intervals(disc_inf, which = "fixed")

#Dual Exponential----
dual_inf <- nlme(INF_Theta1 ~ exp(-1*g0*Age7) - exp(-1*g1*Age7), 
                 data = theta.all, 
                 subset = !is.na(INF_Theta1),
                 fixed = g0 + g1~ 1,
                 random = g0 + g1~ 1,   
                 groups = ~ID,
                 start = c(g0=0, g2 = 1))

summary(dual_inf)
intervals(dual_inf, which = "fixed")




