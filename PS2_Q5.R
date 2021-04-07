# Question 5 

rm(list=ls())

library(readxl)
library(tidyverse)
library(ggplot2)
library(kableExtra)
library(texreg)
library(lfe)
library(stargazer)
library(readr)

###########################################################################################
# (1) Descriptive Stats
###########################################################################################


automobile <- read_csv("automobile.csv")

automobile%>%group_by(cla)%>%
  summarise(qty=n(),
         price=mean(pr),
         p_sum=sum(pr))
