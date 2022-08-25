library(tidyverse)
library(dplyr)


#Normality test - change to total number of observation (42 in the male dataset)
# Question 1
prop_vec_Q1 <- c(0.6, (2/3), (5/6), 0.8, (2/3), (5/6), 0.6)
shapiro.test(prop_vec_Q1)

#qqnorm(prop_vec_Q1, pch = 1, frame = FALSE)
#qqline(prop_vec_Q1, col = "steelblue", lwd = 2)

# Question 2
prop_vec_Q2 <- c(0, (1/2), (4/6), (11/20), (7/15), 0.475, (8/12))
shapiro.test(prop_vec_Q2)

#qqnorm(prop_vec_Q2, pch = 1, frame = FALSE)
#qqline(prop_vec_Q2, col = "pink", lwd = 2)

# Question 3
prop_vec_Q3 <- c(0, 0, 1, (2/3), (1/3), 0.5, 1)
shapiro.test(prop_vec_Q3)

#qqnorm(prop_vec_Q3, pch = 1, frame = FALSE)
#qqline(prop_vec_Q3, col = "seagreen", lwd = 2)

vec_for_qqplot <- c(prop_vec_Q1, prop_vec_Q2, prop_vec_Q3)
qqnorm(vec_for_qqplot, pch = 1, frame = FALSE)
qqline(prop_vec_Q3, col = "seagreen", lwd = 2)


#Analysis - two sided proportions Z-test
alpha <- 0.05
#Male
# Question 1 - Given a graph G = (V,E) we chose a vertex v_i. What is the probability that v_j and v_k are connect by one edge where each v_j,v_k connects to v_i by one edge? 
n1 <- 42
n2 <- 42
Q1_p2_hat = (5/7) #mean of proportions vector 
Q1_p1_hat = (24/42) #number of edges in the given graph 
Q1_p_hat = (n1*Q1_p1_hat + n2*Q1_p2_hat)/(n1+n2)

# H0: p1 = p2 - null hypothesis
# H1: p2  !=  p1 - research hypothesis
#Z-test for 2 population proportions
z_observed_Q1 = ((Q1_p1_hat- Q1_p2_hat) - 0)/sqrt(Q1_p_hat*(1-Q1_p_hat)*((1/n1)+(1/n2)))
z_observed_Q1
z_observed_Q1 <= -qnorm(1-(alpha/2)) | z_observed_Q1 >= qnorm(1-(alpha/2)) #the difference is not significant at 5%

p_val_Q1 = 2*pnorm(z_observed_Q1,lower.tail = TRUE)
p_val_Q1

# Question 2 - Given a graph G = (V,E) we chose a vertex v_i. What is the probability that v_j and v_k are connect by one edge where both v_j,v_k are v_i's neighbors (there is an edge from v_i to v_j,v_k)?
n1 = 42
n2 = 42
Q2_p2_hat = 0.475
Q2_p1_hat = (24/42)
Q2_p_hat = (n1*Q2_p1_hat + n2*Q2_p2_hat)/(n1+n2)

# H0: p1 = p2 - null hypothesis
# H1: p2 != p1 - research hypothesis
#Z-test for 2 population proportions
z_observed_Q2 = ((Q2_p1_hat- Q2_p2_hat) - 0)/(sqrt(Q2_p_hat*(1-Q2_p_hat)*((1/n1)+(1/n2))))
z_observed_Q2
z_observed_Q2 <=  -qnorm(1-(alpha/2)) | z_observed_Q2 >= qnorm(1-(alpha/2)) #the difference is not significant at 5%

p_val_Q2 = 2*pnorm(z_observed_Q2,lower.tail = FALSE)
p_val_Q2

# Question 3 - Given a graph G = (V,E) we chose a vertex v_i. What is the probability that v_j and v_k are connect by two-sided edge where both v_j,v_k have two-sided edge with v_i?
Q3_p2_hat = (1/2)
Q3_p1_hat = (8/21)
Q3_p_hat = (n1*Q3_p1_hat + n2*Q3_p2_hat)/(n1+n2)

# H0: p1 = p2 - null hypothesis
# H1: p2 != p1 - research hypothesis
#Z-test for 2 population proportions
n1 <- 21
n2 <- 21
z_observed_Q3 = ((Q3_p1_hat- Q3_p2_hat) - 0)/sqrt(Q3_p_hat*(1-Q3_p_hat)*((1/n1)+(1/n2)))
z_observed_Q3
z_observed_Q3 <= -qnorm(1-(alpha/2)) | z_observed_Q3 >= qnorm(1-(alpha/2)) #the difference is not significant at 5%

p_val_Q3 = 2*pnorm(z_observed_Q3,lower.tail = TRUE)
p_val_Q3
