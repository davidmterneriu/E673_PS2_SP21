# Question 4 

rm(list=ls())

library(readxl)
library(tidyverse)
library(ggplot2)
library(kableExtra)
library(texreg)
library(lfe)
library(stargazer)
library(readr)

swiss <- read_table2("swiss.txt", col_names = FALSE)
colnames(swiss)<-c("id","y","income","age","education","children_under","children_over","citizen")

#Part (a): Linear Probability Model

q4_a_mod=lm(data=swiss,y~income+poly(age,2)+education+
              children_under+children_over+as.factor(citizen))
summary(q4_a_mod)

screenreg(q4_a_mod,digits = 3,stars = c(0.01,0.05,0.1))
texreg(q4_a_mod,digits = 3,stars = c(0.01,0.05,0.1))


#Part (b): Hard coding logit model 

