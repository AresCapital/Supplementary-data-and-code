rm(list=ls())
library(tidyverse)
library(tsModel)
library(dlnm)
library(splines)
library(mvmeta)
library(FluMoDL)
library(lubridate)
library(this.path)
setwd(dirname(this.path()))

## Data preparation
## The holidays
## Spring festival holiday
sf1 = ymd("2015-02-18"):ymd("2015-02-24")
sf2 = ymd("2016-02-07"):ymd("2016-02-13")
sf3 = ymd("2017-01-27"):ymd("2017-02-02")
sf4 = ymd("2018-02-15"):ymd("2018-02-21")
sf5 = ymd("2019-02-04"):ymd("2019-02-10")
sf = as.Date(c(sf1,sf2,sf3,sf4,sf5),origin = "1970-01-01")
## National day holiday
nd1 = ymd("2015-10-01"):ymd("2015-10-07")
nd2 = ymd("2016-10-01"):ymd("2016-10-07")
nd3 = ymd("2017-10-01"):ymd("2017-10-07")
nd4 = ymd("2018-10-01"):ymd("2018-10-07")
nd5 = ymd("2019-10-01"):ymd("2019-10-07")
nd = as.Date(c(nd1,nd2,nd3,nd4,nd5),origin = "1970-01-01")
holy = as.Date(c(nd,sf),origin = "1970-01-01")
runSum <- function(v,
                   lags = 0,
                   na.rm = F) {
  lagMat <- Lag(v, lags)
  rowSums(lagMat, na.rm = na.rm)
}

city_info <- read.csv("city.csv") 
city = city_info$citycode
m = length(city)

d0 <- readr::read_csv("data.csv", locale = locale(encoding = "GBK")) %>%
  left_join(city_info %>% select(citycode, pop)) %>% 
  group_by(citycode) %>% 
  mutate(tem1 = runSum(tem,1:7)/7,
         rhu1 = runSum(rhu,1:7)/7) %>% 
  mutate(tt = as.numeric(as.factor(date))) %>% 
  ungroup() %>% 
  mutate(holiday = case_when(
    date %in% sf ~ "spring festival holiday",
    date %in% nd ~ "national day holiday",
    !(date %in% holy) ~ "common days"
  ))
xdata <- d0 %>%
  group_by(citycode) %>%
  mutate(ac = runSum(total,1:3)+0.00000000000001) %>% 
  ungroup() %>% 
  mutate(CO = 1000*CO) %>% 
  mutate_at(vars(`PM2.5`:`CO`),function(x)x/10) 


## Choose the very air pollutant
xdata <- xdata %>%
  mutate(plu = CO) 

func1 = function(){
subdata <- lapply(city, function(x)
  xdata[xdata$citycode == x,])
coef.plu <-
  matrix(NA, m, 1, dimnames = list(city, paste0("x", seq(1))))
vcov.plu <- vector("list", m)
lag = c(1,7)
arglag = list(fun = "ns", df = 3)
argvar <- list(fun = "lin")

fx = as.formula(
  "total~offset(log(pop))+ns(tt,df=35)+log(ac)+factor(dow)+factor(holiday)+ns(tem1,3)+ns(rhu1,3)+cb.plu"
)

for (i in 1:m) {
  data <- subdata[[i]]
  cb.plu <-
    crossbasis(data$plu,
               lag = lag,
               argvar = argvar,
               arglag = arglag)
  model = glm(formula = fx,
              family = quasipoisson(link = "log"),
              data = data)
  red.plu <- crossreduce(cb.plu, model)
  coef.plu[i, ] <- coef(red.plu)
  vcov.plu[[i]] <- vcov(red.plu)
}
## calculate the pooled estimate cumulative rr
mv.plu <- mvmeta(coef.plu, vcov.plu, method = "reml")
rr_est = exp(coef(summary(mv.plu))[1]) %>% round(., 4) 
rr_lci = exp(coef(summary(mv.plu))[5]) %>% round(., 4)  
rr_uci = exp(coef(summary(mv.plu))[6]) %>% round(., 4)  
cum_rr = paste0(rr_est, " (",rr_lci, "—", rr_uci, ")")

## Calculation for the attributable fractions with pooled effects
af <- matrix(NA, m, 3, dimnames = list(city, c("mean","lci","uci")))
for (j in 1:m) {
  data <- subdata[[j]]
  cb.plu <-
    crossbasis(data$plu,
               lag = lag,
               argvar = argvar,
               arglag = arglag)
  af[j,"mean"] = attrdl(data$plu, cb.plu, data$total, coef = coef(mv.plu),type = "af",dir = "forw", cen=0) 
  af[j,"lci"] = attrdl(data$plu, cb.plu, data$total, coef = coef(mv.plu)-1.96*sqrt(vcov(mv.plu)[[1]]),type = "af",dir = "forw", cen=0) 
  af[j,"uci"] = attrdl(data$plu, cb.plu, data$total, coef = coef(mv.plu)+1.96*sqrt(vcov(mv.plu)[[1]]),type = "af",dir = "forw", cen=0)
}



## Lag-specific effect estimate
rr_lag = data.frame()
for(kkk in 1:7){
for(i in 1:m){
  data <- subdata[[i]]
  cb.plu <- crossbasis(data$plu,lag=lag,argvar=argvar,arglag=arglag)
  model = glm(
    formula = fx,
    family = quasipoisson(link = "log"),
    data = data
  )
  red.plu <- crossreduce(cb.plu,model)
  red.plu =  crossreduce(cb.plu,model,type = "lag",value = kkk,cen=0)
  coef.plu[i,] <- coef(red.plu)
  vcov.plu[[i]] <- vcov(red.plu)
}
mv.plu <- mvmeta(coef.plu,vcov.plu,method = "reml")
## 污染物浓度每增加10ug/m3 的 RR
rr_est = exp(coef(summary(mv.plu))[1])  ## 估计值
rr_lci = exp(coef(summary(mv.plu))[5])  ## 95%CI下限
rr_uci = exp(coef(summary(mv.plu))[6])  ## 95%CI上限

ff = c("rr_est" = rr_est, "rr_lci" = rr_lci,"rr_uci" = rr_uci,"lag" = kkk)
rr_lag = rbind(rr_lag,ff)
}
colnames(rr_lag) = c("rr_est","rr_lci","rr_uci","lag")
plot_lag <- rr_lag %>% ggplot()+
  geom_hline(aes(yintercept=1),size=1)+
  geom_segment(aes(x = factor(lag),y = rr_lci,xend = factor(lag),yend = rr_uci))+
  geom_line(aes(x = lag,y = rr_est)) +
  geom_point(aes(x = factor(lag),y = rr_est)) +
  theme_classic()+
  xlab("Lag days")+
  ylab("RR")

result = list(
  "plot_lag" = plot_lag,
  "cum_rr" = cum_rr,
  "af" = af)
return(result)
}
ff = func1()

ff$cum_rr
ff$plot_lag
ff$af
