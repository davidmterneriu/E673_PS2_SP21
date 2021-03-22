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

q4_a_mod=lm(data=swiss,y~income+age+I(age^2)+education+
              children_under+children_over+as.factor(citizen))
summary(q4_a_mod)

screenreg(q4_a_mod,digits = 3,stars = c(0.01,0.05,0.1))
texreg(q4_a_mod,digits = 3,stars = c(0.01,0.05,0.1))


#Part (b): Hard coding logit model 



logit_hand=function(df,formula,opts=T){
  #browser()
  
  mm=model.matrix(formula,df)
  predictor_names=colnames(mm)
  form_string=as.character(formula)
  dv=form_string[form_string%in%predictor_names==F]
  dv=dv[dv!="~"]
  dv=df[,colnames(df)%in%dv]
  
  n=dim(mm)[1]
  p_1=dim(mm)[2]
  par_1=rep(1,p_1)%>%as.matrix()
  p_null=1%>%as.matrix()
  
  options(warn=-1)
  mle_function=function(par,x,y){
    loglik_v<- sum(-y*log(1 + exp(-(x%*%par))) - (1-y)*log(1 + exp(x%*%par)))
    return(-1*loglik_v)
  }
  mle_nll=function(par,y){
    x=rep(1,length(y))%>%as.matrix()
    loglik_v<- sum(-y*log(1 + exp(-(x%*%par))) - (1-y)*log(1 + exp(x%*%par)))
    return(-1*loglik_v)
  }
  
  if(opts){
    test_1=nlm(p=par_1,f=mle_function,y=dv,x=mm)
    beta=test_1$estimate
    ll=test_1$minimum
    test_null=nlm(p=p_null,f=mle_nll,y=dv)
    ll_null=test_null$minimum
  }else{
    test_o=optim(par=par_1,fn=mle_function,y=dv,x=mm)
    beta=test_o$par
    ll=test_o$value
    test_null=optim(par=p_null,fn=mle_nll,y=dv)
    ll_null=test_null$value
  }
  p = 1/(1+exp(-mm%*%beta))
  V = array(0,dim=c(dim(mm)[1],dim(mm)[1]))
  diag(V) = p*(1-p)
  IB = t(mm)%*%V%*%mm
  R2=1-ll/ll_null
  se_val=diag(solve(IB))%>%sqrt()
  z_val=beta/se_val
  p_val=2*pnorm(-abs(z_val))
  result_df=data.frame(term=colnames(mm),estimate=beta,std.err=se_val,z.value=z_val,
                       p.value=p_val)
  rownames(result_df)<-c()
  
  AIC=2*ll+2*p_1
  BIC=2*ll+log(n)*p_1
  dev=2*ll
  gof=c("AIC"=AIC,"BIC"=BIC,"Log Likelihood"=(-1)*ll,"Deviance"=dev,"McFadden pR2"=R2,"Num. obs."=n)
  gof.names = names(gof)
  decimal.places <- c(TRUE, TRUE, TRUE, TRUE,TRUE,FALSE) 
  t <- createTexreg(coef.names = as.character(result_df$term),
                    coef = result_df$estimate,
                    se = result_df$std.err,
                    pvalues = result_df$p.value,
                    gof.names = gof.names,
                    gof = gof,
                    gof.decimal = decimal.places)
  print(screenreg(t,digits = 3,stars = c(0.01,0.05,0.1)))
  res=list(res=result_df,tex=t,fitted=p)
}

b1_nlm=logit_hand(df=swiss,y~age+I(age^2)+income+education+children_under+children_over+as.factor(citizen),
           opts=T)

b1_opt=logit_hand(df=swiss,y~age+I(age^2)+income+education+children_under+children_over+as.factor(citizen),
                  opts=F)

mod2=glm(data=swiss,y~age+I(age^2)+income+education+children_under+children_over+as.factor(citizen),
         family = binomial(link = "logit"))


texreg(list(mod2,b1_nlm$tex,b1_opt$tex),digits = 3,stars = c(0.01,0.05,0.1))



summary(mod2)

#Part (c): Logit marginal effect of education E on labor force participation

library(margins)

b3=margins(mod2)

summary(b3)%>%as.data.frame()%>%kable(format = "latex",digits=3)


#Part (d): Probit

probit_hand=function(df,formula,opts=T){
  #browser()
  
  mm=model.matrix(formula,df)
  predictor_names=colnames(mm)
  form_string=as.character(formula)
  dv=form_string[form_string%in%predictor_names==F]
  dv=dv[dv!="~"]
  dv=df[,colnames(df)%in%dv]
  
  n=dim(mm)[1]
  p_1=dim(mm)[2]
  par_1=rep(-0.1,p_1)%>%as.matrix()
  p_null=-0.1%>%as.matrix()
  
  options(warn=-1)
  mle_function=function(par,x,y){
    loglik_v<- sum(y*log(pnorm(x%*%par)))+sum((1-y)*log(1-pnorm(x%*%par)))
    return(-1*loglik_v)
  }
  mle_nll=function(par,y){
    x=rep(1,length(y))%>%as.matrix()
    loglik_v<- sum(y*log(pnorm(x%*%par)))+sum((1-y)*log(1-pnorm(x%*%par)))
    return(-1*loglik_v)
  }
  if(opts){
    test_1=nlm(p=par_1,f=mle_function,y=dv,x=mm)
    beta=test_1$estimate
    ll=test_1$minimum
    
    test_null=nlm(p=p_null,f=mle_nll,y=dv)
    ll_null=test_null$minimum
  }else{
    test_o=optim(par=par_1,fn=mle_function,y=dv,x=mm)
    beta=test_o$par
    ll=test_o$value
    test_null=optim(par=p_null,fn=mle_nll,y=dv)
    ll_null=test_null$value
  }
  p = 1/(1+exp(-mm%*%beta))
  V = array(0,dim=c(dim(mm)[1],dim(mm)[1]))
  diag(V) = p*(1-p)
  IB = t(mm)%*%V%*%mm
  R2=1-ll/ll_null
  
  se_val=diag(solve(IB))%>%sqrt()
  z_val=beta/se_val
  p_val=2*pnorm(-abs(z_val))
  result_df=data.frame(term=colnames(mm),estimate=beta,std.err=se_val,z.value=z_val,
                       p.value=p_val)
  rownames(result_df)<-c()
  
  AIC=2*ll+2*p_1
  BIC=2*ll+log(n)*p_1
  dev=2*ll
  gof=c("AIC"=AIC,"BIC"=BIC,"Log Likelihood"=(-1)*ll,"Deviance"=dev,"McFadden pR2"=R2,"Num. obs."=n)
  gof.names = names(gof)
  decimal.places <- c(TRUE, TRUE, TRUE, TRUE,TRUE,FALSE) 
  t <- createTexreg(coef.names = as.character(result_df$term),
                    coef = result_df$estimate,
                    se = result_df$std.err,
                    pvalues = result_df$p.value,
                    gof.names = gof.names,
                    gof = gof,
                    gof.decimal = decimal.places)
  #print(screenreg(t,digits = 3,stars = c(0.01,0.05,0.1)))
  res=list(res=result_df,tex=t,fitted=p,McFadden_r2=R2)
}

b2_nlm=probit_hand(df=swiss,y~age+I(age^2)+income+education+children_under+children_over+as.factor(citizen),
                  opts=T)

b2_opt=probit_hand(df=swiss,y~age+I(age^2)+income+education+children_under+children_over+as.factor(citizen),
                  opts=F)



mod3=glm(data=swiss,y~age+I(age^2)+income+education+children_under+children_over+as.factor(citizen),
         family = binomial(link = "probit"))

texreg(list(mod3,b2_nlm$tex,b2_opt$tex),stars = c(0.01,0.05,0.1),digits = 3)
 

#Part (e): Probit-margins

b3_f=margins(mod3)

summary(b3_f)%>%as.data.frame()%>%kable(format = "latex",digits=3)

