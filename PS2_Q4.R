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
  
  
  options(warn=-1)
  mle_function=function(par,x,y){
    loglik_v<- sum(-y*log(1 + exp(-(x%*%par))) - (1-y)*log(1 + exp(x%*%par)))
    return(-1*loglik_v)
  }
  if(opts){
    test_1=nlm(p=par_1,f=mle_function,y=dv,x=mm)
    beta=test_1$estimate
    ll=test_1$minimum
  }else{
    test_o=optim(par=par_1,fn=mle_function,y=dv,x=mm)
    beta=test_o$par
    ll=test_o$value
  }
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
  
  AIC=2*ll+2*p_1
  BIC=2*ll+log(n)*p_1
  dev=2*ll
  gof=c("AIC"=AIC,"BIC"=BIC,"Log Likelihood"=(-1)*ll,"Deviance"=dev,"Num. obs."=n)
  gof.names = names(gof)
  decimal.places <- c(TRUE, TRUE, TRUE, TRUE,FALSE) 
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

b1=logit_hand(df=swiss,y~age+I(age^2)+income+education+children_under+children_over+as.factor(citizen),
           opts=T)

mod2=glm(data=swiss,y~age+I(age^2)+income+education+children_under+children_over+as.factor(citizen),
         family = binomial(link = "logit"))


texreg(list(mod2,b1$tex))



summary(mod2)

#Part (c): Logit marginal effect of education E on labor force participation

library(margins)

b3=margins(mod2)

summary(b3)%>%as.data.frame()%>%kable(format = "latex",digits=3)

#colnames(b3)

#b3%>%mutate(prod_edu_hi=dydx_education+1.96*sqrt(Var_dydx_education),
#            prod_edu_lo=dydx_education-1.96*sqrt(Var_dydx_education))%>%
#  ggplot(aes(x=education,y=dydx_education))+
#  geom_ribbon(aes(ymax=prod_edu_hi,ymin=prod_edu_lo),color="black",fill="gray")+
#  geom_line()+
#  geom_point()


#b3%>%as.data.frame()%>%select(education,dydx_education,attempt1)%>%
#  mutate(error=attempt1/dydx_education)

#library(ggeffects)
#mod2_logit_df=ggpredict(mod2,terms="education")
#ggplot(mod2_logit_df, aes(x, predicted)) + geom_line()


#Part (d): Probit

