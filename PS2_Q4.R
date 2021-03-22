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



logit_hand=function(df,formula){
  browser()
  mm=model.matrix(formula,df)
  predictor_names=colnames(mm)
  form_string=as.character(formula)
  dv=form_string[form_string%in%predictor_names==F]
  dv=dv[dv!="~"]
  dv=df[,colnames(df)%in%dv]
  
  n=dim(mm)[1]
  p_1=dim(mm)[2]
  par_1=rep(0.1,p_1)%>%as.matrix()
  
  
  mle_function=function(d_v,model_mat,par_1){
    adj_model_mat=sweep(model_mat,2,par_1,FUN="*")
    adj2=d_v*(colSums(adj_model_mat)-log(1+exp(colSums(adj_model_mat))))
    res=-1*sum(adj2)
    return(res)
  }
  
  #y2=mle_function(par_1,d_v=dv,model_mat=mm)
 
  test_1=optim(par=par_1,fn=mle_function,d_v=dv,model_mat=mm,hessian = T)
  par_est=test_1$par
  hessian=test_1$hessian
  val_test=test_1$value
  se_val=diag(hessian)%>%sqrt()
  z_val=par_est/se_val
  p_val=2*pnorm(-abs(z_val))
  result_df=data.frame(term=colnames(mm),estimate=par_est,std.err=se_val,z.value=z_val,
                       p.value=p_val)
  return(result_df)
  

}

logit_hand(df=swiss,y~age+I(age^2))

mod2=glm(data=swiss,y~age+I(age^2),family = binomial(link="logit"))
summary(mod2)
