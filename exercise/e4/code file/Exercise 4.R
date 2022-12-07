library(readr)
library(readxl)
library(tidyverse)
hcris <- read.delim("C:/Users/phadmin/Desktop/econ 771/e1/HCRIS_Data.txt")
market<- readRDS('C:/Users/phadmin/Desktop/econ 771/e4/hospital_markets.rds') 
hrr <- read_excel("C:/Users/phadmin/Desktop/econ 771/e4/ZipHsaHrr15.xls")
zctact <- read_csv("C:/Users/phadmin/Desktop/econ 771/e4/zcta-to-county.csv")

hcris.final <- hcris %>% 
  filter(year>=2000 & year<=2017) %>% 
  mutate(zip=substr(zip,1,5))
 
zctact.final<-zctact %>%
  filter(row_number() != 1) %>% 
  mutate(zip=zcta5,
         fips=county)

hrr.final<-hrr%>%
  mutate(zip=sprintf("%05d",zipcode15))

mergedt<-hcris.final %>% 
  left_join(hrr.final,by="zip")%>% 
  left_join(zctact.final,by="zip")%>% 
  left_join(market,by="fips")%>% 
  mutate(yearc=as.factor(year),zipc=as.factor(zip)) %>%
  mutate(hrrnumc=as.factor(hrrnum)) %>%
  mutate(yearc=as.factor(year))%>%
  mutate(price=(tot_charges-tot_discounts)/tot_discharges)

#######################################################################

#Q1

#######################################################################
library(ggplot2)
q1.data <- mergedt %>% 
  group_by(yearc, zip) %>% 
  summarise(tot_discharges_zipsum=sum(tot_discharges)) %>%
  ungroup() %>%
  left_join(mergedt,by=c("yearc","zip")) %>%
  mutate(zippct=tot_discharges/tot_discharges_zipsum) %>%
  filter(zip != '00000'  & !is.na(zip))

q1.fig <- ggplot(data=q1.data,aes(x=yearc, y=zippct)) + 
  geom_violin() + 
  scale_y_continuous(limit=c(0,1)) +
  xlab("Year") + ylab("Hospital market shares") + 
  theme_bw()
q1.fig
#######################################################################

#Q2

#######################################################################
library(fixest)
q2.data <- q1.data %>% 
  mutate(zippct100=zippct*100,
         sqrzippct100=zippct100*zippct100)%>% 
  group_by(yearc, zip) %>% 
  summarise(hhizip=sum(sqrzippct100)) %>%
  ungroup() %>%
  left_join( q1.data,by=c("yearc","zip"))

q2.datar<- q2.data%>%
  group_by(yearc, zip) %>% 
  summarise(numberhosp=n_distinct(provider)) %>%
  ungroup() %>%
  left_join( q2.data,by=c("yearc","zip"))
q2modela <- feols(price ~ hhizip | provider+year, data=q2.datar)
summary(q2modela)
q2model <- feols(price ~ hhizip+tot_discharges+numberhosp | provider+year, data=q2.datar)
summary(q2model)

#######################################################################

#Q3

#######################################################################
q3.data <- mergedt %>% 
  group_by(yearc, hrrnumc) %>% 
  summarise(tot_discharges_hrrsum=sum(tot_discharges)) %>%
  ungroup() %>%
  left_join(mergedt,by=c("yearc","hrrnumc")) %>%
  mutate(hrrpct=tot_discharges/tot_discharges_hrrsum)

q3.fig <- ggplot(data=q3.data,aes(x=yearc, y=hrrpct)) + 
  geom_violin() + 
  scale_y_continuous(limit=c(0,1)) +
  xlab("Year") + ylab("Hospital market shares") + 
  theme_bw()
q3.fig
#######################################################################

#Q4

#######################################################################
library(fixest)
q4.data <- q3.data %>% 
  mutate(hrrpct100=hrrpct*100,
         sqrhrrpct100=hrrpct100*hrrpct100)%>% 
  group_by(yearc, hrrnumc) %>% 
  summarise(hhihrr=sum(sqrhrrpct100)) %>%
  ungroup() %>%
  left_join( q3.data,by=c("yearc","hrrnumc"))

q4.datar<- q4.data%>%
  group_by(yearc, hrrnumc) %>% 
  summarise(numberhosp=n_distinct(provider)) %>%
  ungroup() %>%
  left_join( q4.data,by=c("yearc","hrrnumc"))

q4modela <- feols(price ~ hhihrr | provider+year, data=q4.datar)
summary(q4modela)
q4model <- feols(price ~ hhihrr+tot_discharges+numberhosp | provider+year, data=q4.datar)
summary(q4model)

#######################################################################

#Q5

#######################################################################
q5.data <- mergedt %>% 
  group_by(yearc, mkt) %>% 
  summarise(tot_discharges_mktsum=sum(tot_discharges)) %>%
  ungroup() %>%
  left_join(mergedt,by=c("yearc","mkt")) %>%
  mutate(mktpct=tot_discharges/tot_discharges_mktsum)

q5.fig <- ggplot(data=q5.data,aes(x=yearc, y=mktpct)) + 
  geom_violin() + 
  scale_y_continuous(limit=c(0,1)) +
  xlab("Year") + ylab("Hospital market shares") + 
  theme_bw()
q5.fig
#######################################################################

#Q6

#######################################################################
library(fixest)
q6.data <- q5.data %>% 
  mutate(mktpct100=mktpct*100,
         sqrmktpct100=mktpct100*mktpct100)%>% 
  group_by(yearc, mkt) %>% 
  summarise(hhimkt=sum(sqrmktpct100)) %>%
  ungroup() %>%
  left_join( q5.data,by=c("yearc","mkt"))

q6.datar<- q6.data%>%
  group_by(yearc, mkt) %>% 
  summarise(numberhosp=n_distinct(provider)) %>%
  ungroup() %>%
  left_join( q6.data,by=c("yearc","mkt"))

q6modela <- feols(price ~ hhimkt | provider+year, data=q6.datar)
summary(q6modela)
q6model <- feols(price ~ hhimkt+tot_discharges+numberhosp | provider+year, data=q6.datar)
summary(q6model)
#######################################################################

#Q7

#######################################################################
q7.zip<- feols( log(zippct) ~ log(price)|provider+year, data=q1.data)
summary(q7.zip)
q7.hrr<- feols( log(hrrpct) ~ log(price)|provider+year, data=q3.data)
summary(q7.hrr)
q7.mkt<- feols( log(mktpct) ~ log(price)|provider+year, data=q5.data)
summary(q7.mkt)
