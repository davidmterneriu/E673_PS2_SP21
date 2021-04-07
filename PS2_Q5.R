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
library(haven)
library(kableExtra)

warn = getOption("warn")
options(warn=-1)

###########################################################################################
# (1) Descriptive Stats
###########################################################################################

#automobile <- read_csv("automobile.csv")

automobile <- read_dta("automobile.dta")



desc<-data.frame(sapply(automobile, attr,"label"))
desc$var_name=rownames(desc)
rownames(desc)<-c()
colnames(desc)[1]<-"desc"

desc=select(desc,var_name,desc)

automobile=automobile%>%mutate(size=le*wi*(0.01)^2,
                               hp_wt=hp/we)

auto_sum1=automobile%>%group_by(ye)%>%
  summarise(`Number of Models`=unique(co)%>%length(),
         Sales=sum(qu)/1000,
         `Average Price`=weighted.mean(x=pr,w=qu)/1000,
         `HP/Wt`=weighted.mean(x=hp_wt,w=qu),
         Size=weighted.mean(x=size,w=qu),
         `Max Speed`=weighted.mean(x=sp,w=qu))%>%
  rename(Year=ye)

rownames(auto_sum1)<-c()

which_per <- function(x,p) which.min(abs(x - quantile(x,p)))

auto_sum2=automobile%>%select(type,qu,pr,hp_wt,size,sp)%>%
  gather(key=var.name,value=v,-1)%>%
  group_by(var.name)%>%
  summarise(q0_v=min(v),
            q0_name=type[which.min(v)],
            q25_v=quantile(v,0.25),
            q25_name=type[which_per(x=v,p=0.25)],
            q50_v=median(v),
            q50_name=type[which_per(x=v,p=0.5)],
            q75_v=quantile(v,0.75),
            q75_name=type[which_per(x=v,p=0.75)],
            qm100_v=max(v),
            qm100_name=type[which.max(v)])%>%
  gather(key=v1,value=v2,-1)%>%
  spread(key=var.name,value=v2)%>%
  mutate(hp_wt=ifelse(is.na(as.numeric(hp_wt)),hp_wt,as.numeric(hp_wt)%>%round(3)),
         pr=ifelse(is.na(as.numeric(pr)),pr,as.numeric(pr)/1000%>%round(3)),
         qu=ifelse(is.na(as.numeric(qu)),qu,as.numeric(qu)/1000%>%round(3)),
         size=ifelse(is.na(as.numeric(size)),qu,as.numeric(size)%>%round(3)),
         v1=ifelse(grepl("_v",v1),"",v1))%>%
  select(v1,Price=pr,Sales=qu,`HP/Wt`=hp_wt,Size=size,Speed=sp)

rownames(auto_sum2)<-c()

kableExtra::kable(auto_sum1,format = "latex",booktabs = T, linesep = "",digits = 2)
kableExtra::kable(auto_sum2,format = "latex",booktabs = T, linesep = "",digits = 2)

###########################################################################################
# (2) Logit Model 
###########################################################################################

#Part 2.a 

