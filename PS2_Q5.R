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

#automobile2%>%group_by(ye)%>%
#  summarise(sum(mshare))
#  
#  NEED TO RE-DO MKT SHARE VAR

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
# (3) Random coecients logit model
###########################################################################################

if(FALSE){
#install.packages("BLPestimatoR")
library(BLPestimatoR)


int_delta=automobile$delta_jt%>%as.numeric()

nevos_mod<-as.formula("mkt_share_t~0+pr_s+hp_wt+size+sp|0+hp_wt+size+sp|0+pr_s+size+sp|0+speed_z+size_z+hp_wt_z")





auto_data<-BLP_data(model=nevos_mod,
                    market_identifier = "ye",
                    product_identifier = "co",
                    productData = automobile,
                    par_delta = "delta_jt",
                    integration_method = "MLHS",
                    integration_accuracy = 20, integration_seed = 213)

gmm_obj_wrap(blp_data=auto_data,par_theta2=starting_theta)


auto_data$data$X_rand

starting_theta=c("pr_s"=-1,"size"=1,"sp"=1)
starting_theta=as.matrix(starting_theta)
colnames(starting_theta)<-"unobs_sd"

auto_est<-estimateBLP(blp_data = auto_data,par_theta2 = starting_theta,
                      solver_method = "L-BFGS-B", solver_maxit = 1000, solver_reltol = 1e-6,
                      standardError = "heteroskedastic",
                      extremumCheck = FALSE,
                      printLevel = 1)



nevos_model <- as.formula("share ~  price + productdummy |
    0+ productdummy |
    price + sugar + mushy |
    0+ IV1 + IV2 + IV3 + IV4 + IV5 + IV6 + IV7 + IV8 + IV9 + IV10 + 
    IV11 + IV12 + IV13 + IV14 + IV15 + IV16 + IV17 + IV18 + IV19 + IV20")


names(originalDraws_cereal)

b1=originalDraws_cereal
names(originalDraws_cereal)[1] <- "(Intercept)"

productData_cereal$startingGuessesDelta <- c(log(w_guesses_cereal)) # include orig. draws in the product data

cereal_data <- BLP_data(
  model = nevos_model,
  market_identifier = "cdid",
  par_delta = "startingGuessesDelta",
  product_identifier = "product_id",
  productData = productData_cereal,
  demographic_draws = demographicData_cereal,
  blp_inner_tol = 1e-6, blp_inner_maxit = 5000,
  integration_draws = originalDraws_cereal,
  integration_weights = rep(1 / 20, 20)
)

theta_guesses_cereal[theta_guesses_cereal == 0] <- NA
colnames(theta_guesses_cereal) <- c("unobs_sd", "income", "incomesq", "age", "child")
rownames(theta_guesses_cereal) <- c("(Intercept)", "price", "sugar", "mushy")

theta_guesses_cereal[,-1]<-NA



cereal_est <- estimateBLP(
  blp_data = cereal_data,
  par_theta2 = theta_guesses_cereal,
  solver_method = "BFGS", solver_maxit = 1000, solver_reltol = 1e-6,
  standardError = "heteroskedastic",
  extremumCheck = FALSE,
  printLevel = 1
)


summary(cereal_est)
}



###########################################################################################
# (3) BLP by hand
###########################################################################################


#######################################
# New Share definition 
#######################################



# Main guides: 
# Rasmusen (IU prof with crazy tweets): https://www.rasmusen.org/published/blp-rasmusen.pdf
# Sudhir: https://faculty.fuqua.duke.edu/econometrics/presentations/2013/Sudhir-Demand%20Estimation-Aggregate%20Data%20Workshop-updated%202013.pdf


automobile=automobile%>%mutate(mshare=qu/pop)

m_share_df<-select(automobile,mshare,ye,co)%>%
  group_by(ye)%>%
  mutate(mshare_0=1-sum(mshare))%>%
  ungroup()%>%
  mutate(delta_jt=log(mshare)-log(mshare_0))


#X1: price, hp_wt, and prod dummies:  explanatory variables common to all consumers
#X2: price, size, and speed: observed explanatory variables with random coefficients 

brd_dummy=automobile$brd%>%as.numeric()



brd_dummy_df=stack(attr(automobile$brd, 'labels'))%>%as.data.frame()

prod_dummy=fastDummies::dummy_cols(brd_dummy)
prod_dummy=prod_dummy[,-1]%>%as.matrix()


X1=cbind(price=automobile$pr_s,hp_wt=automobile$hp_wt,prod_dummy)%>%as.matrix()
X1_np=X1[,-1]

num_X1=dim(X1)[2]

#solve(t(X1)%*%X1)

X2<-cbind(price=automobile$pr_s,size=automobile$size,speed=automobile$sp)%>%
  as.matrix()

#number of random coefficients 3= 1 (price) + 2 characteristics (size+speed)
K=dim(xlin)[2]

#number of simulations/consumers 
n_cons=20

automobile=automobile%>%mutate(cons=1)


#IV matrix: speed_z,hp_wt_z,size_z
iv_mat<-automobile%>%select(cons,speed_z,hp_wt_z,size_z)%>%as.matrix()
weight_mat=solve(t(iv_mat)%*%iv_mat)

first_stage=lm(data=automobile,pr_s~speed_z+hp_wt_z+size_z)
fitted_price=fitted(first_stage)

beta_guess=rep(0,num_X1+3)

beta_guess=c(0,0,0)

GMMM_fun<-function(betas,ns){
 
  beta_u=betas[1:3]
  #beta_lame=betas[-c(1:3)]
  
  options(dplyr.summarise.inform = FALSE)
  if(dim(X2)[2]!=length(beta_u)){
    print("beta_u must contain 3 parameters!")
    invokeRestart("abort")
  }
  K=dim(X2)[2]
  set.seed(1234)
  nu_mat=matrix( rnorm(ns*K,mean=0,sd=1), K, ns) 
  
  mu_mat=matrix(0,nrow(X2),ns)
  for(j in 1:ns){
    for(i in 1:nrow(X2)){
      mu_mat[i,j]<-sum(X2[i,]%*%t(nu_mat[,j])%*%beta_u)
    }
  }
  #browser()
  delta_jt=m_share_df$delta_jt%>%as.numeric()
  
  counter=0
  err_tol=1e-7
  current_error=1000
 
  while(counter<1000 & current_error>err_tol){
   
    counter=counter+1
   # print(paste("Inner loop error: ",current_error))
    top=mu_mat
    for(i in 1:nrow(mu_mat)){
      top[i,]<-delta_jt[i]+top[i,]
    }
    top_df=data.frame(ye=m_share_df$ye,co=m_share_df$co,top)
    pred_share_df=top_df%>%gather(key="sim_number",value=v,-c(1,2))%>%
      group_by(ye)%>%
      mutate(bot=1+sum(exp(v)))%>%
      ungroup()%>%
      group_by(ye,co)%>%
      summarise(pred_share=mean(exp(v)/bot))
    pred_share_df=pred_share_df%>%inner_join(select(m_share_df,-c(mshare_0,delta_jt)),by=c("ye","co"))
    pred_share_df$delta_jt=delta_jt%>%as.numeric()
    pred_share_df=pred_share_df%>%mutate(delta_jt_1=delta_jt+log(mshare)-log(abs(pred_share)),
                           error_term=abs(delta_jt_1-delta_jt))
    current_error=max(pred_share_df$error_term)
    
    if(is.nan(current_error)){
      print("Problem with inner loop!")
      invokeRestart("abort")
    }
    
    delta_jt=pred_share_df$delta_jt_1%>%as.numeric()
  }
  #browser()
  delta=delta_jt%>%as.numeric()
  automobile_t=automobile%>%mutate(delta_est=delta)
  #xi_model=lm(delta~0+X1)
  xi_model=ivreg(data=automobile_t,delta_est~0+pr_s+hp_wt+as.factor(brd)|
                   hp_wt+as.factor(brd)+hp_wt_z+speed_z+size_z)
  xi=resid(xi_model)
  
  #GMM Time!
  gmm_val=t(xi)%*%iv_mat%*%weight_mat%*%t(iv_mat)%*%xi
  gmm_val=as.numeric(gmm_val)
  #print(paste("Current GMM value: ",gmm_val,sep=""))
  return(gmm_val)
}

#GMMM_fun(betas=beta_guess,ns=20)









#blp_demand=optim(par=beta_guess,ns=20,fn=GMMM_fun,control=list(trace=2))
#blp_demand=nlm(p=beta_guess,ns=20,f=GMMM_fun,print.level = 2)


#end_time-start_time

blp_demand_est=function(par,ns){
  start_time <- Sys.time()
  init_est=optim(par=par,ns=ns,fn=GMMM_fun,control=list(trace=2))
  end_time <- Sys.time()
  #browser()
  time_dif=end_time-start_time
  beta_u=init_est$par
  gmm_val=init_est$value
  
  K=dim(X2)[2]
  set.seed(1234)
  nu_mat=matrix( rnorm(ns*K,mean=0,sd=1), K, ns) 
  
  
  mu_mat=matrix(0,nrow(X2),ns)
  for(j in 1:ns){
    for(i in 1:nrow(X2)){
      mu_mat[i,j]<-sum(X2[i,]%*%t(nu_mat[,j])%*%beta_u)
    }
  }
  delta_jt=m_share_df$delta_jt%>%as.numeric()
  
  counter=0
  err_tol=1e-6
  current_error=1000
  
  while(counter<1000 & current_error>err_tol){
    
    counter=counter+1
    # print(paste("Inner loop error: ",current_error))
    top=mu_mat
    for(i in 1:nrow(mu_mat)){
      top[i,]<-delta_jt[i]+top[i,]
    }
    top_df=data.frame(ye=m_share_df$ye,co=m_share_df$co,top)
    pred_share_df=top_df%>%gather(key="sim_number",value=v,-c(1,2))%>%
      group_by(ye)%>%
      mutate(bot=1+sum(exp(v)))%>%
      ungroup()%>%
      group_by(ye,co)%>%
      summarise(pred_share=mean(exp(v)/bot))
    pred_share_df=pred_share_df%>%inner_join(select(m_share_df,-c(mshare_0,delta_jt)),by=c("ye","co"))
    pred_share_df$delta_jt=delta_jt%>%as.numeric()
    pred_share_df=pred_share_df%>%mutate(delta_jt_1=delta_jt+log(mshare)-log(abs(pred_share)),
                                         error_term=abs(delta_jt_1-delta_jt))
    current_error=max(pred_share_df$error_term)
    
    if(is.nan(current_error)){
      print("Problem with inner loop!")
      invokeRestart("abort")
    }
    con_share=top_df%>%gather(key="sim_number",value=v,-c(1,2))%>%
      group_by(ye)%>%
      mutate(bot=1+sum(exp(v)))%>%
      ungroup()%>%
      mutate(i_est_share=exp(v)/bot)
    delta_jt=pred_share_df$delta_jt_1%>%as.numeric()
  }
  delta=delta_jt%>%as.numeric()
  automobile_t=automobile%>%mutate(delta_est=delta)
  #xi_model=lm(delta~0+X1)
  xi_model=ivreg(data=automobile_t,delta_est~0+pr_s+hp_wt+as.factor(brd)|
                   hp_wt+as.factor(brd)+hp_wt_z+speed_z+size_z)
  
  xi=resid(xi_model)
  
  names(beta_u)<-c("price","size","speed")
  
 
  rand_df=data.frame(sim_number=paste("X",seq(1,ns,by=1),sep=""),
                           alpha_i=nu_mat[1,]*beta_u[1],
                     size_i=nu_mat[2,]*beta_u[2],
                     speed_i=nu_mat[3,]*beta_u[3])
  
  return_l=list(mean_coeff=xi_model,beta_u=beta_u,gmm_val=gmm_val,time=time_dif,mean_u=delta,
                con_share=con_share,consumer_betas=rand_df,xi=xi)
  return(return_l)
}

ns1=20
demand_res=blp_demand_est(par=beta_guess,ns=ns1)

demand_res$beta_u[1]

demand_res$mean_coeff$coefficients[1]+demand_res$beta_u[1]


demand_res$beta_u%>%round(5)
demand_res$beta_u%>%round(3)
demand_res$gmm_val%>%round(4)
demand_res$time%>%as.numeric()%>%round(3)

brd_dummy_df$values=as.character(brd_dummy_df$values)
brd_dummy_df$ind=as.character(brd_dummy_df$ind)

gmm_model_print=demand_res$mean_coeff%>%broom::tidy()%>%
  select(term,estimate)%>%
  mutate(term=gsub("as.factor\\(brd\\)","",term))%>%
  left_join(brd_dummy_df,by=c("term"="values"))%>%
  mutate(term=ifelse(is.na(ind),term,ind))%>%
  select(-ind)
  
kableExtra::kable(gmm_model_print,format = "latex",booktabs = T, linesep = "",digits = 3)




demand_res$con_share%>%group_by(ye,sim_number)%>%
  summarise(share=sum(i_est_share))%>%
  ungroup()%>%
  group_by(ye)%>%
  summarise(tot=sum(share))

ye_list=demand_res$con_share$ye%>%as.numeric()%>%unique()
co_list=demand_res$con_share$co%>%as.numeric()%>%unique()

rand_co_elas=expand.grid(co_1=co_list,
                         co_2=co_list)%>%
  as.data.frame()

rand_co_elas$elastic=0

demand_res_95=demand_res$con_share%>%filter(ye==95)


ye_list=demand_res_95$ye%>%as.numeric()%>%unique()
co_list=demand_res_95$co%>%as.numeric()%>%unique()

rand_co_elas=expand.grid(co_1=co_list,
                         co_2=co_list)%>%
  as.data.frame()

rand_co_elas$elas=0

demand_res_95f=demand_res_95%>%inner_join(select(auto_95,co,pr_s))
demand_res_95f=demand_res_95f%>%inner_join(demand_res$consumer_betas)
###############################################################################
#Trying new definition of elasticity (04/14/21)
###############################################################################



#See: https://journals.sagepub.com/doi/pdf/10.1177/1536867X1501500317
alpha_price=demand_res$mean_coeff$coefficients[1]%>%as.numeric()

hp_coef=demand_res$mean_coeff$coefficients[2]


rand_bits=demand_res$consumer_betas


automobile_test=automobile
automobile_test$mean_u=demand_res$mean_u
automobile_test$ms=automobile_test$qu/automobile_test$pop

automobile_test=automobile_test%>%filter(ye==95)

p1_list=demand_res_95f$co%>%unique()
master_elas1=expand.grid(j=p1_list,sim_number=paste("X",seq(1,ns1,by=1),sep=""))%>%as.data.frame()

master_elas1%>%inner_join(select(automobile_test,j=co,ms_j=ms,mean_u,pr_s,size,sp,hp_wt))%>%
  inner_join(rand_bits)%>%
  mutate(top_est=mean_u+pr_s*alpha_i+size*size_i+sp*speed_i)%>%
  group_by(j)%>%
  mutate(bot=1+sum(exp(top_est)),
         share_ij=exp(top_est)/bot,
         share_2=sum(share_ij/bot),
         inner_share=alpha_i*share_ij*(1-share_ij))%>%
  summarise(elas=unique(pr_s)/share_2*sum(inner_share))%>%
  unique.data.frame()%>%View()

real_blp_elastic=master_elas1%>%inner_join(select(automobile_test,j=co,ms_j=ms,mean_u,pr_s,size,sp,hp_wt))%>%
  inner_join(rand_bits)%>%
  mutate(top_est=mean_u+pr_s*alpha_i+size*size_i+sp*speed_i)%>%
  group_by(j)%>%
  mutate(bot=1+sum(exp(top_est)),
         share_ij=exp(top_est)/bot,
         share_2=mean(share_ij/bot))%>%
  ungroup()


p1_list=demand_res_95f$co%>%unique()
master_elas=expand.grid(j=p1_list,k=p1_list)%>%as.data.frame()


master_elas=master_elas%>%
  inner_join(select(real_blp_elastic,sim_number,alpha_i,pr_j=pr_s,j,ms_j,share_ij,share_j=share_2))%>%
  inner_join(select(real_blp_elastic,sim_number,pr_k=pr_s,j,ms_k=ms_j,share_ik=share_ij,share_k=share_2))%>%
  mutate(type=ifelse(j==k,"own","cross"))


own_elastic=master_elas%>%filter(type=="own")%>%
  group_by(j)%>%
  summarise(elas=unique(pr_j)/share_j*sum((alpha_price+alpha_i)*share_ij*(1-share_ij)))%>%
  unique.data.frame()%>%
  mutate(type="own")%>%
  mutate(k=j)


cross_elastic=master_elas%>%filter(type!="own")%>%
  group_by(j,k)%>%
  summarise(elas=-unique(pr_k)/share_j*sum((alpha_price+alpha_i)*share_ij*share_ik))%>%
  unique.data.frame()%>%
  mutate(type="cross")



blp_elastic=rbind(select(own_elastic,j,k,elas,type),select(cross_elastic,j,k,elas,type))

blp_elastic=blp_elastic%>%inner_join(select(automobile_test,j=co,type_j=type,pr_j=pr_s,ms_j=ms,frm_j=frm,loc_j=loc))%>%
  inner_join(select(automobile_test,k=co,type_k=type,pr_k=pr_s,ms_k=ms,frm_k=frm,loc_k=loc))%>%
  mutate(delta_jr_blp=ifelse(frm_j==frm_k,-elas*pr_j/ms_j,0))


auto_95_list=auto_95f$type%>%as.character()

blp_elastic%>%filter(type_j %in% auto_95_list&type_k %in% auto_95_list)%>%
  ungroup()%>%
  select(j=type_j,k=type_k,elas,pr_j,pr_k)%>%
  unique.data.frame()%>%
  ggplot(aes(x=reorder(j,pr_j), y=reorder(k,pr_k))) +
  geom_tile(aes(fill = elas), colour = "grey50")+
  geom_label(aes(label=round(elas,4)))+
  labs(x="",y="",fill="Elasticity",title="Random Coefficients Elasticities")+
  ggthemes::theme_clean()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


blp_elastic=blp_elastic%>%mutate(delta_jr_mat=ifelse(frm_j==frm_k,-1*elas*ms_j/pr_j,0))
delta_jr_blp_mat=blp_elastic%>%select(j,k,delta_jr_mat)%>%
  pivot_wider(names_from = j, values_from = delta_jr_mat)%>%
  select(-k)%>%
  as.matrix()

sp_df<-blp_elastic%>%select(j,frm_j,ms_j,pr_j)%>%unique.data.frame()


sp_df$markup_iv=solve(delta_jr_blp_mat)%*%sp_df$ms_j%>%as.numeric()
sp_df$marginal_cost=sp_df$pr_j-solve(delta_jr_blp_mat)%*%sp_df$ms_j%>%as.numeric()
sp_df$lerner=sp_df$markup_iv/sp_df$pr_j*100

sp_df=sp_df%>%inner_join(select(auto_95,j=co,type,size,hp_wt,size,sp))%>%ungroup()

blp_mc=select(sp_df,j=type,frm_j,marginal_cost,markup_iv)

sp_df_super=rbind(sp_df%>%select(type,lerner,pr_j,hp_wt,size,sp)%>%top_n(lerner,n=5),
                  sp_df%>%select(type,lerner,pr_j,hp_wt,size,sp)%>%top_n(-lerner,n=5))%>%arrange(-lerner)


kable(sp_df_super,format = "latex",digits = 2,booktabs = T, linesep = "")

blp_elastic_sum=blp_elastic%>%select(j,k,elas)%>%
  mutate(model="blp")


###############################################################################

if(FALSE){
demand_res_95f=demand_res_95%>%inner_join(select(auto_95,co,pr_s))
demand_res_95f=demand_res_95f%>%inner_join(demand_res$consumer_betas)


p1_list=demand_res_95f$co%>%unique()

master_elas=expand.grid(j=p1_list,k=p1_list)%>%as.data.frame()

master_elas=master_elas%>%inner_join(select(demand_res_95f,sim_number,j=co,est_share_ij=i_est_share,pr_j=pr_s,alpha_i))%>%
  inner_join(select(demand_res_95f,sim_number,k=co,est_share_ik=i_est_share,pr_k=pr_s))

master_elas=master_elas%>%mutate(elas_type=ifelse(j==k,"own","cross"))

own_price_rando=master_elas%>%filter(elas_type=="own")%>%
  group_by(j)%>%
  summarise(intregal=unique(pr_j)*sum(alpha_i*est_share_ij*(1-est_share_ij)),
            tot_share=sum(est_share_ij))%>%
  ungroup()%>%
  mutate(elas=intregal/tot_share)%>%
  select(j=j,k=j,elas)

cross_price_rando=master_elas%>%filter(elas_type!="own")%>%
  mutate(inside=alpha_i*est_share_ij*est_share_ik)%>%
  group_by(j,k)%>%
  summarise(intg_p=unique(pr_k)*sum(inside),
            tot=sum(est_share_ij))%>%
  mutate(elas=-intg_p/tot)%>%
  select(j=j,k=k,elas)

rando_all_elastic=rbind(own_price_rando,cross_price_rando)


auto_95=auto_95%>%mutate(ms=qu/pop)

elastic_df2_all=expand.grid(j=auto_95$type%>%unique(),k=auto_95$type%>%unique())%>%as.data.frame()

elastic_df2_all=elastic_df2_all%>%inner_join(select(auto_95,j=type,co_1=co,pr_j=pr_s,firm_j=frm,loc_j=loc,ms_j=ms))%>%
  inner_join(select(auto_95,k=type,co_2=co,pr_k=pr_s,firm_k=frm,loc_k=loc,ms_k=ms))%>%
  inner_join(rando_all_elastic,by=c("co_1"="j","co_2"="k"))


elastic_df2_all=elastic_df2_all%>%mutate(delta_jr_mat=ifelse(firm_j==firm_k,-1*elas*ms_j/pr_j,0))
delta_jr_blp_mat=elastic_df2_all%>%select(j,k,delta_jr_mat)%>%
  pivot_wider(names_from = j, values_from = delta_jr_mat)%>%
  select(-k)%>%
  as.matrix()

sp_df<-elastic_df2_all%>%select(j,firm_j,ms_j,pr_j)%>%unique.data.frame()


sp_df$markup_iv=solve(delta_jr_blp_mat)%*%sp_df$ms_j
sp_df$marginal_cost=(sp_df$pr_j-sp_df$markup_iv%>%as.numeric())


sp_df=sp_df%>%inner_join(select(auto_95,j=type,firm_j=frm,hp_wt,size,sp))

#blp_mc=select(sp_df,j=type,firm_j,marginal_cost)


sp_df_blp=sp_df%>%mutate(lerner=markup_iv%>%as.numeric()/pr_j*100)

sp_df_super=rbind(sp_df_blp%>%select(j,lerner,pr_j,hp_wt,size,sp)%>%top_n(lerner,n=5),
                  sp_df_blp%>%select(j,lerner,pr_j,hp_wt,size,sp)%>%top_n(-lerner,n=5))%>%arrange(-lerner)

kable(sp_df_super,format = "latex",digits = 2,booktabs = T, linesep = "")
  

elastic_df2=expand.grid(j=auto_95f$type,k=auto_95f$type)%>%as.data.frame()




elastic_df2=elastic_df2%>%inner_join(select(auto_95,j=type,co_1=co,pr_j=pr_s,firm_j=frm))%>%
  inner_join(select(auto_95,k=type,co_2=co,pr_k=pr_s,firm_k=frm))%>%
  inner_join(rando_all_elastic,by=c("co_1"="j","co_2"="k"))


ggplot(elastic_df2, aes(x=reorder(j,pr_j), y=reorder(k,pr_k))) +
  geom_tile(aes(fill = elas), colour = "grey50")+
  geom_label(aes(label=round(elas,4)))+
  labs(x="",y="",fill="Elasticity",title="Random Coefficients Elasticities")+
  ggthemes::theme_clean()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))}



###########################################################################################
# (2.1) Logit Model w/ correct market share definition 
###########################################################################################

automobile=automobile%>%mutate(ms=qu/pop)%>%
  group_by(ye,cla)%>%
  mutate(ms_nest=ms/sum(ms))%>%
  ungroup()%>%
  group_by(ye)%>%
  mutate(outside_s=1-sum(ms))%>%
  ungroup()%>%
  mutate(delta_init=log(ms)-log(outside_s))



bas_model_1=lm(data=automobile,delta_init~hp_wt+size+sp+pr_s)
nest_model_1=lm(data=automobile,delta_init~hp_wt+size+sp+pr_s+log(ms_nest))

iv_mod1=ivreg(data=automobile,delta_init~hp_wt+size+sp+pr_s
              |hp_wt+size+sp+hp_wt_z+size_z+speed_z)
iv_mod2=ivreg(data=automobile,delta_init~hp_wt+size+sp+pr_s+log(cla_share)
              |hp_wt+size+sp+hp_wt_z+size_z+speed_z)




stargazer(bas_model_1,nest_model_1,iv_mod1,iv_mod2,digits = 3)




base_elas=expand.grid(co_1=co_list,
                         co_2=co_list)%>%
  as.data.frame()

base_elas$elas=0

alpha_price_base=bas_model_1$coefficients[names(bas_model_1$coefficients)=="pr_s"]%>%as.numeric()
alpha_price_iv=iv_mod1$coefficients[names(bas_model_1$coefficients)=="pr_s"]%>%as.numeric()


cv_iv_df=automobile%>%mutate(xi_iv=resid(iv_mod1),
                    x_betas=delta_init-xi_iv-alpha_price_iv*pr_s)%>%
  filter(ye==95)%>%
  select(co,x_betas,xi_iv)


auto_95=filter(automobile,ye==95)

base_elas=base_elas%>%inner_join(select(auto_95,co_1=co,pr_1=pr_s,ms_1=ms))%>%
  inner_join(select(auto_95,co_2=co,pr_2=pr_s,ms_2=ms))%>%
  mutate(base_elas=ifelse(co_1==co_2,alpha_price_base*pr_1*(1-ms_1),
                          -alpha_price_base*pr_2*ms_2),
         iv_elas=ifelse(co_1==co_2,alpha_price_iv*pr_1*(1-ms_1),
                        -alpha_price_iv*pr_2*ms_2))
                         


elastic_df2=expand.grid(j=auto_95$type,k=auto_95$type)%>%as.data.frame()

elastic_df2=elastic_df2%>%inner_join(select(auto_95,j=type,pr_j=pr_s,ms_1=ms,frm_j=frm,co_1=co))%>%
  inner_join(select(auto_95,k=type,pr_k=pr_s,ms_2=ms,frm_k=frm,co_2=co))%>%
  mutate(case_n=ifelse(j==k,"C1",ifelse("cla_j"=="cla_k","C2","C3")))






elastic_df2=elastic_df2%>%inner_join(select(base_elas,co_1,co_2,base_elas,iv_elas))
elastic_df2=elastic_df2%>%mutate(delta_jr_mat=ifelse(frm_j==frm_k,-1*iv_elas*ms_1/pr_k,0))


delta_jr_iv_mat=elastic_df2%>%select(j,k,delta_jr_mat)%>%
  pivot_wider(names_from = j, values_from = delta_jr_mat)%>%
  select(-k)%>%
  as.matrix()


sp_df<-elastic_df2%>%select(j,frm_j,ms_1,pr_j)%>%unique.data.frame()


sp_df$markup_iv=solve(delta_jr_iv_mat)%*%sp_df$ms_1/sp_df$pr_j*100
sp_df$mc=sp_df$pr_j-solve(delta_jr_iv_mat)%*%sp_df$ms_1%>%as.numeric()


sp_df=sp_df%>%inner_join(select(auto_95,j=type,frm_j=frm,hp_wt,size,sp))
sp_df_log=sp_df


mc_base_15=sp_df$pr_j*(1-sp_df$markup_iv%>%as.numeric()/100)

sp_df_super=rbind(sp_df%>%select(j,markup_iv,pr_j,hp_wt,size,sp)%>%top_n(markup_iv,n=5),
                  sp_df%>%select(j,markup_iv,pr_j,hp_wt,size,sp)%>%top_n(-markup_iv,n=5))%>%arrange(-markup_iv)

sp_df_super$markup_iv=sp_df_super$markup_iv%>%as.numeric()

kable(sp_df_super,format = "latex",digits = 2,booktabs = T, linesep = "")


elastic_df2%>%ggplot(aes(x=-1*base_elas,y=iv_elas))+
  geom_point()


elastic_df2%>%filter(j %in% auto_95f$type & k %in% auto_95f$type)%>%
  gather(key="model",value="elas",c("iv_elas","base_elas"))%>%ggplot(aes(x=reorder(j,pr_j), y=reorder(k,pr_k))) +
  geom_tile(aes(fill = elas), colour = "grey50")+
  geom_label(aes(label=round(elas,4)))+
  labs(x="",y="",fill="Elasticity",title="Logit Own/Cross Price Elasticity ")+
  ggthemes::theme_clean()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  facet_wrap(~model)

logit_mc=select(sp_df,j,frm_j,marginal_cost=mc,markup_iv)


###########################################################################################
# (4) Comparison 
###########################################################################################


iv_elastic=elastic_df2%>%select(j=co_1,k=co_2,elas=iv_elas)
iv_elastic$model="logit_iv"




all_elastic_df=rbind(iv_elastic,blp_elastic_sum)


all_elastic_df%>%spread(key=model,value=elas)%>%
  mutate(type=ifelse(j==k,"Own price", "Cross price"),
         ratio=blp/logit_iv)%>%
  ggplot(aes(x=type,fill=type,y=ratio))+
  geom_boxplot()+
  facet_wrap(~type,scales = "free")+
  labs(x="",y="BLP/Logit (IV) Ratio",
       title="BLP vs Logit (IV) Elasticities")+
  ggthemes::theme_clean()+
  theme(legend.position = "none")



all_elastic_df_table=all_elastic_df%>%spread(key=model,value=elas)%>%
  mutate(type=ifelse(j==k,"Own price", "Cross price"),
         ratio=blp/logit_iv)%>%
  group_by(type)%>%
  summarise(min=min(ratio),
            q25=quantile(ratio,0.25),
            median=median(ratio),
            mean=mean(ratio),
            q75=quantile(ratio,0.75),
            max=max(ratio))


kable(all_elastic_df_table,format = "latex",digits = 2,booktabs = T, linesep = "")




price_comp=data.frame(sim_number=demand_res$consumer_betas[,1],blp_price=demand_res$consumer_betas[,2]+alpha_price,
                      logit_price=alpha_price_iv)
price_comp=price_comp%>%mutate(ratio=blp_price/logit_price)

ggplot(data=price_comp,aes(x=ratio))+
  geom_histogram(bins=10,aes(y=..count../sum(..count..)),
                 color="black",fill="pink")+
  theme_bw()+
  labs(title="Ratio of BLP/Logit (IV) pricing coefficients",
       x="ratio",
       y="Share of BLP Simulations")


markup_iv=sp_df_log%>%select(j,markup_iv)
markup_iv%>%inner_join(select(blp_mc,j,markup_blp=markup_iv))%>%
  mutate(ratio=markup_blp/markup_iv)%>%
  ggplot(aes(x=ratio))+
  geom_histogram(bins=20,aes(y=..count../sum(..count..)),
                 color="black",fill="cyan")+
  theme_bw()+
  labs(title="Ratio of BLP/Logit (IV) Markups (P-MC)",
       x="ratio",
       y="Share of 1995 Models")

  


###########################################################################################
# (6) Merger work 
###########################################################################################


blp_merger_df=blp_mc%>%inner_join(select(auto_95,j=type,co,loc,ms,pr_s))%>%
  inner_join(filter(blp_elastic_sum,j==k)%>%select(co=j,elas))

blp_merger_cross=select(blp_elastic_sum,j,k,elas)%>%
  inner_join(select(auto_95,loc_j=loc,frm_j=frm,j=co))%>%
  inner_join(select(auto_95,loc_k=loc,frm_k=frm,k=co))


logit_merger_cross=elastic_df2%>%select(j=co_1,k=co_2,elas=iv_elas)%>%
  inner_join(select(auto_95,loc_j=loc,frm_j=frm,j=co))%>%
  inner_join(select(auto_95,loc_k=loc,frm_k=frm,k=co))

cv_blp_df=demand_res$consumer_betas
cv_blp_df$alpha_i=cv_blp_df$alpha_i
cv_blp_betas=select(auto_95,j=co,hp_wt)
cv_blp_betas$hp_wt_beta=cv_blp_betas$hp_wt*hp_coef

ind_95=which(automobile$ye==95)

cv_blp_betas$xi=demand_res$xi[ind_95]
cv_blp_betas$mean_u=demand_res$mean_u[ind_95]

auto_95%>%group_by(loc)%>%
  summarise(product_count=n(),
            mkt_share=100*sum(ms),
            tot_sales=sum(qu*eurpr)/10^9)

cv_consumer_df=expand.grid(sim_number=cv_blp_df$sim_number,j=cv_blp_betas$j)%>%as.data.frame()%>%
  inner_join(select(cv_blp_betas,j,hp_wt_beta,xi,mean_u))%>%
  inner_join(cv_blp_df)%>%
  inner_join(select(auto_95,j=co,size,speed=sp))%>%
  mutate(size_beta=size_i*size,speed_beta=speed*speed_i)



share_logit=function(price_df,raw_data,alpha_p){
  temp_df=raw_data
  temp_df=temp_df%>%rename(j=co)
  temp_df=temp_df%>%inner_join(price_df,by="j")
  
  
  temp_df=temp_df%>%mutate(price_alpha=price*alpha_p,
                           top=x_betas+xi_iv+price_alpha,
                           bot=1+sum(exp(top)),
                           share_j=exp(top)/bot)%>%
    select(j,share_j)
  return(temp_df)
}


price_df_1=data.frame(j=auto_95$co,price=auto_95$pr_s)

r1_log=share_logit(price_df = price_df_1,raw_data = cv_iv_df,alpha_p = alpha_price_iv)
sum(r1_log$share_j)

View(cv_consumer_df)

share_blp=function(price_df,raw_data,alpha_p){
  #browser()
  temp_df=raw_data
  temp_df=temp_df%>%inner_join(price_df,by="j")
  temp_df=temp_df%>%mutate(price_alpha_i=price*(alpha_i+alpha_p),
                   v_ij=mean_u+size_beta+speed_beta+price_alpha_i,
                   top=exp(v_ij))%>%
    group_by(j)%>%
    summarise(share_j=mean(top/(1+sum(top))))%>%
    ungroup()%>%
    select(j,share_j)
  return(temp_df)
}


r1_blp=share_blp(price_df = price_df_1,raw_data = cv_consumer_df,alpha_p = alpha_price)
sum(r1_blp$share_j)



merger_min=function(par,delta_jr,mc,price_index,model){
  warn = getOption("warn")
  options(warn=-1)
  price_df=data.frame(j=price_index,price=par)
  #browser()
 if(model=="BLP"){
   est_share=share_blp(price_df = price_df,raw_data = cv_consumer_df,alpha_p = alpha_price)
   delta_mat=delta_jr
   delta_mat=delta_mat%>%inner_join(select(est_share,j,share_j),by="j")%>%
     inner_join(select(est_share,k=j,share_k=share_j),by="k")%>%
     inner_join(select(price_df,j=j,pr_j=price),by="j")%>%
     inner_join(select(price_df,k=j,pr_k=price),by="k")%>%
     mutate(delta_jr_v=omega_jr*elastic*share_j/pr_k)
   delta_jr_mat=delta_mat%>%select(j,k,delta_jr_v)%>%
     pivot_wider(names_from = j, values_from = delta_jr_v)%>%
     select(-k)%>%
     as.matrix()
 }else{
   est_share=share_logit(price_df =price_df,raw_data = cv_iv_df,alpha_p = alpha_price_iv)
   delta_mat=delta_jr
   delta_mat=delta_mat%>%inner_join(select(est_share,j,share_j),by="j")%>%
     inner_join(select(est_share,k=j,share_k=share_j),by="k")%>%
     inner_join(select(price_df,j=j,pr_j=price),by="j")%>%
     inner_join(select(price_df,k=j,pr_k=price),by="k")%>%
     mutate(delta_jr_v=omega_jr*elastic*share_j/pr_k)
   delta_jr_mat=delta_mat%>%select(j,k,delta_jr_v)%>%
     pivot_wider(names_from = j, values_from = delta_jr_v)%>%
     select(-k)%>%
     as.matrix()
 }
  price_guess=price_df$price
  res=(mc+solve(delta_jr_mat)%*%est_share$share_j)%>%as.numeric()
  #error_vec=abs((price_guess-res)/res)
  
  error_vec=abs(price_guess-res)
  error_res=max(error_vec)
  return(error_res)
  
}


compen_var=function(raw_data,price_df,model){

  temp_df=raw_data
  
  if(model=="BLP"){
    temp_df=inner_join(temp_df,price_df,by="j")
    
    alpha_p=alpha_price
    temp_df=temp_df%>%mutate(alpha_tots=alpha_i+alpha_p,
                     old_price_beta=old_price*alpha_tots,
                     new_price_beta=new_price*alpha_tots,
                     v_old=exp(old_price_beta+mean_u+size_beta+speed_beta),
                     v_new=exp(new_price_beta+mean_u+size_beta+speed_beta))
    
    alpha_vec=raw_data%>%select(sim_number,alpha_i)%>%unique.data.frame()%>%
      mutate(alpha_tots=alpha_i+alpha_p)%>%
      select(sim_number,alpha_tots)
    
    cv=temp_df%>%group_by(sim_number)%>%
      summarise(v_1i=log(1+sum(v_new)),
                           v_0i=log(1+sum(v_old)))%>%
      ungroup()%>%
      inner_join(alpha_vec,by="sim_number")%>%
      summarise(cv=mean(1/abs(alpha_tots)*v_1i-v_0i))
    cv=cv$cv
    return(cv)
    
    
  }else{
    temp_df=raw_data
    temp_df=rename(temp_df,j=co)
    temp_df=inner_join(temp_df,price_df,by="j")
    alph_p=alpha_price_iv
    cv=temp_df%>%mutate(v_old=exp(old_price*alph_p+x_betas+xi_iv),
                     v_new=exp(new_price*alph_p+x_betas+xi_iv))%>%
      summarise(v_1=log(1+sum(v_new)),
                v_0=log(1+sum(v_old)))%>%
      mutate(cv=1/abs(alph_p)*(v_1-v_0))
    cv=cv$cv
    return(cv)
  }
}


auto_95$loc
country_labels <- as.data.frame(attr(auto_95$loc,"labels"))
colnames(country_labels)<-"loc_code"
country_labels$name=rownames(country_labels)
rownames(country_labels)<-c()


loc_code_list=country_labels$loc_code

merger_function=function(country,demand_model,all_loc=F){
  #browser()
  require(tictoc)
  tic()
  warn = getOption("warn")
  options(warn=-1)
  
  old_price_df=select(auto_95,j=co,price=pr_s,ms)
  
  
  if(all_loc==F){
  
  owner_temp=expand.grid(j=auto_95$co%>%unique(),k=auto_95$co%>%unique())%>%
    as.data.frame%>%
    inner_join(select(auto_95,j=co,frm_j=frm,loc_j=loc),by="j")%>%
    inner_join(select(auto_95,k=co,frm_k=frm,loc_k=loc),by="k")%>%
    mutate(new_j=ifelse(loc_j==country,"super",frm_j),
           new_k=ifelse(loc_k==country,"super",frm_k),
           omega_jr=ifelse(new_j==new_k,1,0),
           omega_jr_old=ifelse(frm_j==frm_k,1,0))}
  else{
    owner_temp=expand.grid(j=auto_95$co%>%unique(),k=auto_95$co%>%unique())%>%
      as.data.frame%>%
      inner_join(select(auto_95,j=co,frm_j=frm,loc_j=loc),by="j")%>%
      inner_join(select(auto_95,k=co,frm_k=frm,loc_k=loc),by="k")%>%
      mutate(new_j="super",
             new_k="super",
             omega_jr=ifelse(new_j==new_k,1,0),
             omega_jr_old=ifelse(frm_j==frm_k,1,0))}
  
  
  
  delta_jr_temp=expand.grid(j=auto_95$co%>%unique(),k=auto_95$co%>%unique())%>%
    as.data.frame()%>%
    inner_join(select(owner_temp,j,k,new_j,new_k,omega_jr))
  
  non_zero=owner_temp%>%summarise(new_share=mean(omega_jr),old_share=mean(omega_jr_old))
  #browser()
  if(demand_model=="BLP"){

    elast_df=select(blp_elastic_sum,j,k,elastic=elas)
    mc_est=select(blp_mc,j,mc=marginal_cost)%>%
      inner_join(select(auto_95,j=type,co))%>%
      select(j=co,mc)%>%
      unique.data.frame()
    
    old_shares=share_blp(price_df = old_price_df,raw_data = cv_consumer_df,alpha_p = alpha_price)
    
    p_approx=delta_jr_temp%>%inner_join(elast_df)%>%
      inner_join(select(old_price_df,j=j,pr_j=price))%>%
      inner_join(select(old_price_df,k=j,pr_k=price))%>%
      inner_join(select(old_shares,j=j,share_j=share_j))%>%
      inner_join(select(old_shares,k=j,share_k=share_j))%>%
      mutate(delta_jr=-omega_jr*elastic*share_j/pr_k)
    
    
    delta_jr_blp_mat=p_approx%>%select(j,k,delta_jr)%>%
      pivot_wider(names_from = j, values_from = delta_jr)%>%
      select(-k)%>%
      as.matrix()
    
    p_init_f=(mc_est$mc+solve(delta_jr_blp_mat)%*%old_shares$share_j)%>%as.numeric()
    
    delta_df=select(p_approx,j,k,omega_jr,elastic)
    
    mc2=delta_df%>%inner_join(old_price_df)%>%
      inner_join(mc_est)%>%
      mutate(mc2=price*(1+1/elastic))%>%
      filter(j==k)%>%
      select(j,price,elastic,mc,mc2)%>%
      unique.data.frame()
    
    #p_init_f=(mc2$mc2+solve(delta_jr_blp_mat)%*%old_shares$share_j)%>%as.numeric()
    
    test_price=data.frame(j=old_price_df$j,price=p_init_f,old_p=old_price_df$price)
    
    
    
    #test_price=data.frame(j=old_price_df$j,price=p_init_f)
    
    #merger_min(par=test_price$price,mc=mc_est$mc,price_index = test_price$j,
    #           delta_jr = delta_df,model = demand_model)
    
    
    optimal_p=optim(par=test_price$price,fn=merger_min,mc=mc_est$mc,
                  price_index=test_price$j,delta_jr=delta_df,model=demand_model,
                  control = list(trace=4,maxit = 10^5))
    
    
    price_return=data.frame(j=old_price_df$j,old_price=old_price_df$price,new_price=optimal_p$par)
    
    cv1=compen_var(raw_data =cv_consumer_df,price_df = price_return,model = demand_model )
    
    country_name=country_labels$name[country_labels$loc_code==country]
    
    exectime <- toc()
    exectime <- exectime$toc - exectime$tic
    
    
    res_obj=list(price_return=price_return,cv=cv1,country_name=country_name,
                 demand_model=demand_model,runtime=exectime,
                 error=optimal_p$value)
    
    return(res_obj)

    
  }else{
    elast_df=select(elastic_df2,j=co_1,k=co_2,elastic=iv_elas)
    mc_est=select(logit_mc,j,mc=marginal_cost)%>%
      inner_join(select(auto_95,j=type,co))%>%
      select(j=co,mc)%>%
      unique.data.frame()
    
    old_shares=share_logit(price_df =old_price_df,raw_data = cv_iv_df,alpha_p = alpha_price_iv)
    p_approx=delta_jr_temp%>%inner_join(elast_df)%>%
      inner_join(select(old_price_df,j=j,pr_j=price))%>%
      inner_join(select(old_price_df,k=j,pr_k=price))%>%
      inner_join(select(old_shares,j=j,share_j=share_j))%>%
      inner_join(select(old_shares,k=j,share_k=share_j))%>%
      mutate(delta_jr=-omega_jr*elastic*share_j/pr_k)
    
    delta_jr_logit_mat=p_approx%>%select(j,k,delta_jr)%>%
      pivot_wider(names_from = j, values_from = delta_jr)%>%
      select(-k)%>%
      as.matrix()
    
    p_init_f=(mc_est$mc+solve(delta_jr_logit_mat)%*%old_shares$share_j)%>%as.numeric()
    
    delta_df=select(p_approx,j,k,omega_jr,elastic)
    
    
    mc2=delta_df%>%inner_join(old_price_df)%>%
      inner_join(mc_est)%>%
      mutate(mc2=price*(1+1/elastic))%>%
      filter(j==k)%>%
      select(j,price,elastic,mc,mc2)%>%
      unique.data.frame()
    
    #p_init_f=(mc2$mc2+solve(delta_jr_logit_mat)%*%old_shares$share_j)%>%as.numeric()
    
    test_price=data.frame(j=old_price_df$j,price=p_init_f,old_p=old_price_df$price)
    
    
    
    optimal_p=optim(par=test_price$price,fn=merger_min,mc=mc_est$mc,
                    price_index=test_price$j,delta_jr=delta_df,model=demand_model,
                    control = list(trace=4,maxit = 10^5))
    
    
    price_return=data.frame(j=old_price_df$j,old_price=old_price_df$price,new_price=optimal_p$par)
    
    cv1=compen_var(raw_data =cv_iv_df,price_df = price_return,model = demand_model )
    
    if(all_loc==F){
    country_name=country_labels$name[country_labels$loc_code==country]
    }else{
      country_name="All"
    }
    
    exectime <- toc()
    exectime <- exectime$toc - exectime$tic
    
    res_obj=list(price_return=price_return,cv=cv1,country_name=country_name,
                 demand_model=demand_model,runtime=exectime,
                 error=optimal_p$value)
    
    return(res_obj)
  }
  
  
}


b1=merger_function(country=4,demand_model = "BLP",all_loc = F)
#91657 

b2=merger_function(country=4,demand_model = "Logit",all_loc = F)
#82931

b3=merger_function(country=4,demand_model = "BLP",all_loc = F)
b4=merger_function(country=4,demand_model = "Logit",all_loc = F)


merge_df_blp=b3$price_return
merge_df_blp=merge_df_blp%>%inner_join(select(auto_95,j=co,loc_code=loc))%>%
  inner_join(country_labels)%>%
  mutate(ratio=new_price/old_price,
         change=ifelse(name=="Germany","Merge","None"))


merge_blp_mod=lm(data=merge_df_blp,new_price~old_price*change)
summary(merge_blp_mod)


b3$price_return%>%
  mutate(ratio=new_price/old_price)%>%
  ggplot(aes(x=ratio))+
  geom_histogram(bins = 86)


b4$price_return%>%
  mutate(ratio=new_price/old_price)%>%
  ggplot(aes(x=ratio))+
  geom_histogram()



demand_list=c("BLP","Logit")

res_df_final=data.frame()
price_vec_df=data.frame()

start_time<-Sys.time()
for(d in 1:length(demand_list)){
  for(c in 1:length(loc_code_list)){
    m1=paste("Country ",loc_code_list[c], " || Model ",demand_list[d])
    print(m1)
    welfare=merger_function(country=loc_code_list[c],demand_model = demand_list[d])
    res_df=data.frame(country_name=welfare$country_name,
                      demand_model=welfare$demand_model,
                      cv=welfare$cv,
                      runtime=welfare$runtime)
    price_vec=welfare$price_return
    price_vec=price_vec%>%mutate(price_delta=new_price/old_price,
                       country_name=welfare$country_name,
                       demand_model=welfare$demand_model)
    res_df_final=rbind(res_df_final,res_df)
    price_vec_df=rbind(price_vec_df,price_vec)
    
  }
}

end_time<-Sys.time()

time_dif=end_time-start_time

country_labels

country_ms=auto_95%>%group_by(loc)%>%
  summarise(ms=sum(ms))%>%
  inner_join(country_labels,by=c("loc"="loc_code"))%>%
  ungroup()%>%
  select(name,ms)%>%
  arrange(-ms)



res_final_ms=res_df_final%>%
  inner_join(country_ms,by=c("country_name"="name"))


ggplot(data=res_final_ms,aes(x=reorder(country_name,ms),y=cv,fill=demand_model))+
  geom_bar(stat="identity",position = "dodge")+
  labs(x="Production Country",y="Compensating CV",fill="Demand Model")+
  ggthemes::theme_clean()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



price_vec_df%>%filter(country_name%in%c("Germany","Spain","France"))%>%ggplot(aes(x=price_delta,fill=demand_model))+
  geom_histogram(bins=20,position = "dodge")+
  facet_wrap(~country_name,scales = "free")+
  ggthemes::theme_clean()+
  labs(y="Density",x="New/Old Price Ratio",fill="Demand Model")+
  theme(legend.position = "top")



