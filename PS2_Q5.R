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
q2_a_mod3=lm(data=automobile,delta_jt~hp_wt+size+sp+pr_s+log(cla_share))


stargazer(q2_a_mod,q2_a_mod2,q2_a_mod3,digits = 3)


#Part 2.b 

automobile%>%group_by(ye)%>%
  mutate(special_mean = (sum(pr_s) - pr_s)/(n()-1))%>%
  select(co,ye,special_mean,pr_s)%>%
  summarise(cor=cor(special_mean,pr_s))%>%View()


year_list=automobile$ye%>%unique()
model_list=automobile$co%>%unique()



temp_df=data.frame()

for(i in 1:length(year_list)){
  for(j in 1:length(model_list)){
    te_df=automobile%>%filter(ye %in% year_list[i] & co %in% model_list[j]==F)
    te_res=te_df%>%summarise(speed_z=mean(sp),size_z=mean(size),hp_wt_z=mean(hp_wt))
    te_res=te_res%>%mutate(ye=year_list[i],co=model_list[j])
    temp_df=rbind(temp_df,te_res)
  }
}


automobile=automobile%>%inner_join(temp_df)


library(AER)

iv_mod1=ivreg(data=automobile,delta_jt~hp_wt+size+sp+pr_s
              |hp_wt+size+sp+hp_wt_z+size_z+speed_z)
iv_mod2=ivreg(data=automobile,delta_jt~hp_wt+size+sp+pr_s+log(cla_share)
              |hp_wt+size+sp+hp_wt_z+size_z+speed_z)


summary(iv_mod2)
stargazer(iv_mod1,iv_mod2,digits=3)
b1=summary(iv_mod1, vcov = sandwich, df = Inf, diagnostics = TRUE)
b2=summary(iv_mod2, vcov = sandwich, df = Inf, diagnostics = TRUE)

kable(b1[["diagnostics"]],format = "latex",digits = 3,booktabs = T, linesep = "")
kable(b2[["diagnostics"]],format = "latex",digits = 3,booktabs = T, linesep = "")

auto_95=automobile%>%filter(ye==95)

which_per<-Vectorize(which_per,vectorize.args = "p")


cars_95=auto_95$type[which_per(auto_95$pr_s,seq(0.1,1,by=0.1))]

auto_95f=auto_95%>%filter(type %in% cars_95)%>%
  select(type,pr_s,mkt_share_t,cla,cla_share)



elastic_df=expand.grid(j=auto_95f$type,k=auto_95f$type)%>%as.data.frame()

elastic_df=elastic_df%>%inner_join(select(auto_95f,j=type,pr_j=pr_s,mkt_j=mkt_share_t,
                               cla_j=cla,cla_share_j=cla_share))%>%
  inner_join(select(auto_95f,k=type,pr_k=pr_s,mkt_k=mkt_share_t,
                    cla_k=cla,cla_share_k=cla_share))




nested_elastic=function(alpha,sig,p_j,p_k,share_j,share_k,share_jc,share_kc,case_n){
  #browser()
  if(case_n=="C1"){
    res=alpha*p_j/(1-sig)*(1-sig*share_jc-(1-sig)*share_j)
    return(res)
  }else if (case_n=="C2"){
    res=-alpha*p_k/(1-sig)*(sig*share_jc+(1-sig)*share_k)
    return(res)
  }else{
    res=-alpha*p_k*share_k
    return(res)
  }
}


# Extracting alpha/sigma
iv_alpha=coef(iv_mod2)[5]%>%as.numeric()
iv_sig=coef(iv_mod2)[6]%>%as.numeric()



base_alpha=coef(q2_a_mod3)[5]%>%as.numeric()
base_sig=coef(q2_a_mod3)[6]%>%as.numeric()

elastic_df=elastic_df%>%
  mutate(case_n=ifelse(j==k,"C1",ifelse(cla_j==cla_k,"C2","C3")))

elastic_df$elastic_b=0
elastic_df$elastic_iv=0

for(i in 1:nrow(elastic_df)){
  elastic_df$elastic_b[i]=nested_elastic(alpha=base_alpha,sig=base_sig,
                                         p_j=elastic_df$pr_j[i],p_k=elastic_df$pr_k[i],
                           share_j=elastic_df$mkt_j[i],share_k=elastic_df$mkt_k[i],
                           share_jc=elastic_df$cla_share_j[i],
                           share_kc=elastic_df$cla_share_k[i],
                           case_n=elastic_df$case_n[i])
  elastic_df$elastic_iv[i]=nested_elastic(alpha=iv_alpha,sig=iv_sig,
                                         p_j=elastic_df$pr_j[i],p_k=elastic_df$pr_k[i],
                                         share_j=elastic_df$mkt_j[i],share_k=elastic_df$mkt_k[i],
                                         share_jc=elastic_df$cla_share_j[i],
                                         share_kc=elastic_df$cla_share_k[i],
                                         case_n=elastic_df$case_n[i])
  
}



base_mat<-elastic_df%>%select(j,k,elastic_b)%>%
  pivot_wider(names_from = j, values_from = elastic_b)


iv_mat<-elastic_df%>%select(j,k,elastic_iv)%>%
  pivot_wider(names_from = j, values_from = elastic_iv)




ggplot(elastic_df, aes(x=reorder(j,pr_j), y=reorder(k,pr_k))) +
  geom_tile(aes(fill = elastic_b), colour = "grey50")+
  geom_label(aes(label=round(elastic_b,3),color=case_n))+
  labs(x="",y="",fill="Elasticity",title="Part A Own/Cross Price Elasticity ")+
  ggthemes::theme_clean()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_color_manual(name="Case",
                     values=c("Red", "Blue", "Green"),
                     labels=c("Own Price", "W/in Basket", "W/o Basket"))


ggplot(elastic_df, aes(x=reorder(j,pr_j), y=reorder(k,pr_k))) +
  geom_tile(aes(fill = elastic_iv), colour = "grey50")+
  geom_label(aes(label=round(elastic_iv,3),color=case_n))+
  labs(x="",y="",fill="Elasticity",title="Part B Own/Cross Price Elasticity ")+
  ggthemes::theme_clean()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_color_manual(name="Case",
                     values=c("Red", "Blue", "Green"),
                     labels=c("Own Price", "W/in Basket", "W/o Basket"))


mod2=lm(data=elastic_df,elastic_iv~elastic_b)
summary(mod2)

kable(base_mat,format = "latex",digits = 3,booktabs = T, linesep = "")
kable(b2[["diagnostics"]],format = "latex",digits = 3, linesep = "")

#part c: markups 

elastic_df2=expand.grid(j=auto_95$type,k=auto_95$type)%>%as.data.frame()

elastic_df2=elastic_df2%>%inner_join(select(auto_95,j=type,pr_j=pr_s,mkt_j=mkt_share_t,
                                          cla_j=cla,cla_share_j=cla_share,frm_j=frm))%>%
  inner_join(select(auto_95,k=type,pr_k=pr_s,mkt_k=mkt_share_t,
                    cla_k=cla,cla_share_k=cla_share,frm_k=frm))%>%
  mutate(case_n=ifelse(j==k,"C1",ifelse("cla_j"=="cla_k","C2","C3")))




elastic_df2$elastic_b=0
elastic_df2$elastic_iv=0

for(i in 1:nrow(elastic_df2)){
  print(i/nrow(elastic_df2))
  elastic_df2$elastic_b[i]=nested_elastic(alpha=base_alpha,sig=base_sig,
                                         p_j=elastic_df2$pr_j[i],p_k=elastic_df2$pr_k[i],
                                         share_j=elastic_df2$mkt_j[i],share_k=elastic_df2$mkt_k[i],
                                         share_jc=elastic_df2$cla_share_j[i],
                                         share_kc=elastic_df2$cla_share_k[i],
                                         case_n=elastic_df2$case_n[i])
  elastic_df2$elastic_iv[i]=nested_elastic(alpha=iv_alpha,sig=iv_sig,
                                          p_j=elastic_df2$pr_j[i],p_k=elastic_df2$pr_k[i],
                                          share_j=elastic_df2$mkt_j[i],share_k=elastic_df2$mkt_k[i],
                                          share_jc=elastic_df2$cla_share_j[i],
                                          share_kc=elastic_df2$cla_share_k[i],
                                          case_n=elastic_df2$case_n[i])
  
}

elastic_df2=elastic_df2%>%mutate(delta_jr_base=ifelse(frm_j==frm_k,-1*elastic_b*mkt_j/pr_k,0),
                     delta_jr_iv=ifelse(frm_j==frm_k,-1*elastic_iv*mkt_j/pr_k,0))


delta_jr_base_mat=elastic_df2%>%select(j,k,delta_jr_base)%>%
  pivot_wider(names_from = j, values_from = delta_jr_base)%>%
  select(-k)%>%
  as.matrix()

delta_jr_iv_mat=elastic_df2%>%select(j,k,delta_jr_iv)%>%
  pivot_wider(names_from = j, values_from = delta_jr_iv)%>%
  select(-k)%>%
  as.matrix()


sp_df<-elastic_df2%>%select(j,frm_j,mkt_j,pr_j)%>%unique.data.frame()

sp_df$markup_base=solve(delta_jr_base_mat)%*%sp_df$mkt_j/sp_df$pr_j*100
sp_df$markup_iv=solve(delta_jr_iv_mat)%*%sp_df$mkt_j/sp_df$pr_j*100

sp_df<-sp_df%>%mutate(price_p=percent_rank(pr_j)*100)


sp_df_super=rbind(sp_df%>%select(j,markup_base)%>%top_n(markup_base,n=5),
      sp_df%>%select(j,markup_base)%>%top_n(-markup_base,n=5))%>%arrange(-markup_base)

sp_df_super2=rbind(sp_df%>%select(j,markup_iv)%>%top_n(markup_iv,n=5),
                    sp_df%>%select(j,markup_iv)%>%top_n(-markup_iv,n=5))%>%arrange(-markup_iv)

sp_df_super$markup_base=sp_df_super$markup_base%>%as.numeric()


sp_df_super<-sp_df_super%>%inner_join(select(j=type,auto_95,pr_s,hp_wt,size,sp))


sp_df_super2$markup_iv<-sp_df_super2$markup_iv%>%as.numeric()
sp_df_super2<-sp_df_super2%>%inner_join(select(j=type,auto_95,pr_s,hp_wt,size,sp))


#sp_df_superf=cbind(sp_df_superf$markup_base,sp_df_superf$markup_iv)%>%as.data.frame()

kable(sp_df_super,format = "latex",digits = 2,booktabs = T, linesep = "")
kable(sp_df_super2,format = "latex",digits = 2,booktabs = T, linesep = "")


###########################################################################################
# (3) Logit Model 
###########################################################################################

