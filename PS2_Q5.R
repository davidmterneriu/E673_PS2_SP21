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

#automobile2 <- read_csv("automobile.csv")

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


automobile=automobile%>%mutate(rev_t=qu*pr)%>%group_by(ye)%>%
  mutate(year_sales=sum(qu)%>%as.numeric(),
         year_rev=sum(rev_t))%>%
  ungroup()%>%
  mutate(mkt_share_t=qu/year_sales,
         mkt_share_t_v=rev_t/year_rev,
         delta_jt=log(mkt_share_t)-log(1-mkt_share_t),
         delta_jt_v=log(mkt_share_t_v)-log(1-mkt_share_t_v))


automobile%>%select(ye,co,delta_jt,delta_jt_v)%>%
  gather(key="delta_type",v="utility",-c(1,2))%>%
  ggplot(aes(x=utility,color=delta_type))+
  geom_density()+
  theme_bw()+
  labs(x="Delta")+
  scale_color_manual(name="Mkt Share Type",labels=c("Units Sold","Revenue"),
                     values=c("blue","red"))


automobile%>%ggplot(aes(x=delta_jt,y=delta_jt_v))+
  geom_point()+
  labs(x="Units Sold",y="Revenue")+
  theme_bw()+
  stat_smooth(method = "lm")


automobile%>%group_by(ye)%>%
  mutate(mkt_rank=rank(mkt_share_t),
         mkt_rank_v=rank(mkt_share_t_v))%>%
  ungroup()%>%
  mutate(mkt_dif=mkt_rank-mkt_rank_v)%>%
  group_by(ye)%>%
  summarise(mean(mkt_dif))

#ranks stay the same on average.




automobile=automobile%>%group_by(ye,cla)%>%
  mutate(cla_share=qu/sum(qu))%>%
  ungroup()%>%
  mutate(delta_jt_c=log(cla_share)-log(1-cla_share),
         pr_s=pr/1000)


q2_a_mod=lm(data=automobile,delta_jt~hp_wt+size+sp+pr_s)
q2_a_mod2=lm(data=automobile,delta_jt~hp_wt+size+sp+pr_s+cla_share)



stargazer(q2_a_mod,q2_a_mod2,digits = 3)


#Part 2.b 

automobile%>%group_by(ye)%>%
  mutate(special_mean = (sum(pr_s) - pr_s)/(n()-1))%>%
  select(co,ye,special_mean,pr_s)%>%
  summarise(cor=cor(special_mean,pr_s))%>%View()

library(AER)



