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

func1 = function(xitem = "pm25"){
  subdata <- lapply(city, function(x)
    xdata[xdata$citycode == x,])
  coef.plu <-
    matrix(NA, m, 1, dimnames = list(city, paste0("x", seq(1))))
  vcov.plu <- vector("list", m)
  ## 这里改模型参数----
  lag = c(1,7)
  arglag = list(fun = "ns", df = 3)
  argvar <- list(fun = "lin")
  
  item = paste(c("cb.pm25","cb.pm10","cb.no2","cb.so2","cb.co","cb.o3"),collapse = "+")
  
  fx = paste0(
    "total~offset(log(pop))+ns(tt,df=35)+log(ac)+factor(dow)+factor(holiday)+ns(tem1,3)+ns(rhu1,3)+",item
  )
  
  ## 计算7天累积的RR----
  for (i in 1:m) {
    data <- subdata[[i]]
    cb.pm25 <-
      crossbasis(data$PM2.5,
                 lag = lag,
                 argvar = argvar,
                 arglag = arglag)
    cb.pm10 <-
      crossbasis(data$PM10,
                 lag = lag,
                 argvar = argvar,
                 arglag = arglag)
    cb.no2 <-
      crossbasis(data$NO2,
                 lag = lag,
                 argvar = argvar,
                 arglag = arglag)
    cb.so2 <-
      crossbasis(data$SO2,
                 lag = lag,
                 argvar = argvar,
                 arglag = arglag)
    cb.o3 <-
      crossbasis(data$O3,
                 lag = lag,
                 argvar = argvar,
                 arglag = arglag)
    cb.co <-
      crossbasis(data$CO,
                 lag = lag,
                 argvar = argvar,
                 arglag = arglag)
    
    
    model = glm(formula = as.formula(fx),
                family = quasipoisson(link = "log"),
                data = data)
    red.pm25 <- crossreduce(cb.pm25, model)
    red.pm10 <- crossreduce(cb.pm10, model)
    red.no2 <- crossreduce(cb.no2, model)
    red.so2 <- crossreduce(cb.so2, model)
    red.co <- crossreduce(cb.co, model)
    red.o3 <- crossreduce(cb.o3, model)
    
    red.plu = get(paste0("red.",xitem))
    coef.plu[i, ] <- coef(red.plu)
    vcov.plu[[i]] <- vcov(red.plu)
  }
  mv.plu <- mvmeta(coef.plu, vcov.plu, method = "reml")
  ## 污染物浓度每增加10ug/m3 的 RR
  rr_est = exp(coef(summary(mv.plu))[1]) %>% round(., 4) 
  rr_lci = exp(coef(summary(mv.plu))[5]) %>% round(., 4)
  rr_uci = exp(coef(summary(mv.plu))[6]) %>% round(., 4) 
  cum_rr = paste0(xitem,": ",rr_est, " (",rr_lci, "—", rr_uci, ")")
  return(cum_rr)
}
func1("pm25")
func1("pm10")
func1("no2")
func1("so2")
func1("co")
func1("o3")
