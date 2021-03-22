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
  #browser()
  mm=model.matrix(formula,df)
  predictor_names=colnames(mm)
  form_string=as.character(formula)
  dv=form_string[form_string%in%predictor_names==F]
  dv=dv[dv!="~"]
  dv=df[,colnames(df)%in%dv]
  
  n=dim(mm)[1]
  p_1=dim(mm)[2]
  par_1=rep(0,p_1)%>%as.matrix()
  
  
  
  mle_function=function(par,x,y){
    loglik_v<- sum(-y*log(1 + exp(-(x%*%par))) - (1-y)*log(1 + exp(x%*%par)))
    return(-1*loglik_v)
  }
  
 
  test_1=optim(par=par_1,fn=mle_function,y=dv,x=mm)
  beta=test_1$par
  p = 1/(1+exp(-mm%*%beta))
  V = array(0,dim=c(dim(mm)[1],dim(mm)[1]))
  diag(V) = p*(1-p)
  IB = t(mm)%*%V%*%mm
 
  se_val=diag(solve(IB))%>%sqrt()
  z_val=beta/se_val
  p_val=2*pnorm(-abs(z_val))
  result_df=data.frame(term=colnames(mm),estimate=beta,std.err=se_val,z.value=z_val,
                       p.value=p_val)
  rownames(result_df)<-c()
  return(result_df)
  

}


#colnames(swiss)

logit_hand(df=swiss,y~age+I(age^2)+income+education+children_under+children_over+as.factor(citizen))

mod2=glm(data=swiss,y~age+I(age^2)+income+education+children_under+children_over+as.factor(citizen),family = binomial)
summary(mod2)


mod2$qr$qr


