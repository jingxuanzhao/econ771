
library(tidyverse)
library(haven)
library(foreign)
maindt <- read.dta("C:/Users/phadmin/Desktop/econ 771/e3/Data_main.dta") 
lisdt <- read.dta("C:/Users/phadmin/Desktop/econ 771/e3/Data_subsidyinfo.dta") 

#change lis from wide to long
lisdtlong<- reshape(data=lisdt, idvar="PDPregion",
                    varying = c("s2006","s2007","s2008","s2009","s2010"),
                    v.name=c("lisline"),
                    times=c("s2006","s2007","s2008","s2009","s2010"),
                    direction="long")

lisdtlong<-lisdtlong%>%
  mutate(year=parse_number(time))%>%
  select("PDPregion","year","lisline")


#merge data and data cleanning
mergedt<-maindt %>%
  inner_join(lisdtlong, by=c("PDPregion","year"))%>%
  
  #Q1
  group_by(uniqueID) %>%
  mutate(yearmin=min(year))%>%
  ungroup() %>%
  mutate(includeq1=ifelse(yearmin==year, 1, 0))%>%
  
  mutate(eben=ifelse(benefit=="E",1,0))%>%
  
  group_by(orgParentCode) %>%
  mutate(yearminfirm=min(year))%>%
  ungroup()%>%
  mutate(first_us=ifelse(yearminfirm==year, 0, 1)) %>%
  group_by(orgParentCode,state) %>%
  mutate(yearminfirmstate=min(year))%>%
  ungroup()%>%
  mutate(first_state=ifelse(yearminfirmstate==year, 0, 1))%>%
  
  
  #Q2
  group_by(state, year) %>%
  mutate(enroll_stateagg = sum(enrollment, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(enrollpct = enrollment/enroll_stateagg,
         ln_enrollpct = log(enrollpct),
         lispremium = premium - lisline,
         lispremium = ifelse(benefit=="E",NA,lispremium))%>%

  #Q6
  mutate(rdwindow=ifelse(lispremium>=-10&lispremium<=10, 1, 0),
         belowbench=ifelse(lispremium<=0&rdwindow==1, 1, 0),
         belowbench2006temp=ifelse(belowbench==1&year==2006, 1, 0),
         rdwindow2006temp=ifelse(rdwindow==1&year==2006, 1, 0),
         lissubsidy2006temp=ifelse(year==2006, lisline, 0))%>%
  
  group_by(uniqueID) %>%
  mutate(belowbench2006=max(belowbench2006temp),
         rdwindow2006=max(rdwindow2006temp),
         lissubsidy2006=max(lissubsidy2006temp)) %>%
  ungroup() %>%
  
  mutate(rdwindow2=ifelse(lispremium>=-4&lispremium<=4, 1, 0),
         belowbench2=ifelse(lispremium<=0&rdwindow2==1, 1, 0),
         belowbench2006temp2=ifelse(belowbench2==1&year==2006, 1, 0),
         rdwindow2006temp2=ifelse(rdwindow2==1&year==2006, 1, 0))%>%
  
  group_by(uniqueID) %>%
  mutate(belowbench20062=max(belowbench2006temp2),
         rdwindow20062=max(rdwindow2006temp2)) %>%
  ungroup() %>%
  
  mutate(lispremiumneg=ifelse(lispremium<=0, lispremium, 0),
         lispremiumpos=ifelse(lispremium>=0, lispremium, 0),
         lispremiumnegsq=lispremiumneg*lispremiumneg,
         lispremiumpossq=lispremiumpos*lispremiumpos)%>%
  
  mutate(ba_0=ifelse(deductible==0&btypedetail=="BA", 1, 0),
         ba_1_99=ifelse(deductible>0&deductible<100&btypedetail=="BA", 1, 0),
         ba_100=ifelse(deductible==100&btypedetail=="BA", 1, 0),
         ba_101_99=ifelse(deductible>100&deductible<200&btypedetail=="BA", 1, 0),
         ba_200_49=ifelse(deductible>=200&deductible<250&btypedetail=="BA", 1, 0),
         ba_250up=ifelse(deductible>=2500&btypedetail=="BA", 1, 0))%>%
  
  mutate(bench062007=ifelse(belowbench2006==1&LIS==1&year==2007, 1, 0),
         bench062008=ifelse(belowbench2006==1&LIS==1&year==2008, 1, 0),
         bench062009=ifelse(belowbench2006==1&LIS==1&year==2009, 1, 0),
         bench062010=ifelse(belowbench2006==1&LIS==1&year==2010, 1, 0),
         bench06not2007=ifelse(belowbench2006==1&LIS==0&year==2007, 1, 0),
         bench06not2008=ifelse(belowbench2006==1&LIS==0&year==2008, 1, 0),
         bench06not2009=ifelse(belowbench2006==1&LIS==0&year==2009, 1, 0),
         bench06not2010=ifelse(belowbench2006==1&LIS==0&year==2010, 1, 0),
         benchnot06yes2007=ifelse(belowbench2006==0&LIS==1&year==2007, 1, 0),
         benchnot06yes2008=ifelse(belowbench2006==0&LIS==1&year==2008, 1, 0),
         benchnot06yes2009=ifelse(belowbench2006==0&LIS==1&year==2009, 1, 0),
         benchnot06yes2010=ifelse(belowbench2006==0&LIS==1&year==2010, 1, 0),)%>%
       

  #Q8
  mutate(lnpremium=log(premium))

#############################################

#Q1

#############################################
q1 <- mergedt %>%
  mutate(marker=1)%>%
  ungroup() %>%
  group_by(year,includeq1) %>%
  summarise(
    Mean.premium = mean(premium, na.rm = T),
    Se.premium = sd(premium, na.rm = T),
    Mean.deductible = mean(deductible, na.rm = T),
    Se.deductible = sd(deductible, na.rm = T),
    mean.eben=mean(eben, na.rm = T),
    frac_us=mean(first_us, na.rm = T),
    frac_state=mean(first_state, na.rm = T),
    firm=n_distinct(orgParentCode),
    sumplanno = sum(marker, na.rm = T),
    
    
  ) %>%
  arrange(includeq1,year)

q1
write_csv(q1,'C:/Users/phadmin/Desktop/econ 771/e3/output/q1.csv',append=FALSE,col_names=TRUE)

#############################################

#Q2

#############################################
library(rdrobust)
q2 <- mergedt %>% 
  filter(year==2006 & !is.na(ln_enrollpct) &lispremium >=-10 & lispremium<=10 ) 

q2.fig <-
  rdplot(q2$ln_enrollpct, q2$lispremium, 
         c=0,
         nbins=c(20, 20),
         p=4,
         col.lines = "black",
         col.dots = "black", 
         title="",
         x.label="Monthly premium - LIS subsidy, 2006", 
         y.label="log enrollment share, 2006")
q2.fig
packageVersion("rdrobust")
#############################################

#Q3

#############################################
q3.fig_10 <-
  rdplot(q2$ln_enrollpct, q2$lispremium, 
         c=0,
         nbins=c(10, 10),
         p=4,
         col.lines = "black",
         col.dots = "black", 
         title="",
         x.label="Monthly premium - LIS subsidy, 2006", 
         y.label="log enrollment share, 2006")
q3.fig_10

q3.fig_30 <-
  rdplot(q2$ln_enrollpct, q2$lispremium, 
         c=0,
         nbins=c(30, 30),
         p=4,
         col.lines = "black",
         col.dots = "black", 
         title="",
         x.label="Monthly premium - LIS subsidy, 2006", 
         y.label="log enrollment share, 2006")
q3.fig_30

#############################################

#Q4

#############################################
#https://cran.microsoft.com/snapshot/2015-02-09/web/packages/rdrobust/rdrobust.pdf


#a=rdbinselect(q2$ln_enrollpct, q2$lispremium, data=q2, subset = q2, c = 0, p = 4,
#            numbinl = NULL, numbinr = NULL, binselect = "es",
#            lowerend = NULL, upperend = NULL, scale = 1, hide = FALSE,
#            par=NULL, title = NULL, x.label = NULL, y.label = NULL,
#            x.lim = NULL, y.lim = NULL, model = FALSE, frame = FALSE)

#https://www.rdocumentation.org/packages/rdrobust/versions/2.0.3/topics/rdplot
rd.result <- rdplot(q2$ln_enrollpct, q2$lispremium, 
                    c=0)
summary(rd.result)

q4.fig_es <-
  rdplot(q2$ln_enrollpct, q2$lispremium, 
         c=0,
         nbins = NULL, binselect = "es",
         p=4,
         col.lines = "black",
         col.dots = "black", 
         title="",
         x.label="Monthly premium - LIS subsidy, 2006", 
         y.label="log enrollment share, 2006")
q4.fig_es 

q4.fig_espr <-
  rdplot(q2$ln_enrollpct, q2$lispremium, 
         c=0,
         nbins = NULL, binselect = "espr",
         p=4,
         col.lines = "black",
         col.dots = "black", 
         title="",
         x.label="Monthly premium - LIS subsidy, 2006", 
         y.label="log enrollment share, 2006")
q4.fig_espr

q4.fig_esmv <-
  rdplot(q2$ln_enrollpct, q2$lispremium, 
         c=0,
         nbins = NULL, binselect = "esmv",
         p=4,
         col.lines = "black",
         col.dots = "black", 
         title="",
         x.label="Monthly premium - LIS subsidy, 2006", 
         y.label="log enrollment share, 2006")
q4.fig_esmv

q4.fig_esmvpr <-
  rdplot(q2$ln_enrollpct, q2$lispremium, 
         c=0,
         nbins = NULL, binselect = "esmvpr",
         p=4,
         col.lines = "black",
         col.dots = "black", 
         title="",
         x.label="Monthly premium - LIS subsidy, 2006", 
         y.label="log enrollment share, 2006")
q4.fig_esmvpr

#############################################

#Q5

#############################################
library(rddensity)
#https://www.rdocumentation.org/packages/rddensity/versions/2.2/topics/rddensity
q5_unrestricted <- rddensity(q2$lispremium,
                         c=0,
                         p=4,
                         fitselect="unrestricted",
                         h=c(10,10),
                         vce="jackknife")
q5_unrestricted$test$p_jk 

q5_restricted <- rddensity(q2$lispremium,
                             c=0,
                             p=4,
                             fitselect="restricted",
                             h=c(10,10),
                             vce="jackknife")
q5_restricted$test$p_jk
#############################################

#Q6

#############################################
library(lmerTest)
library(miceadds)
library(modelsummary)
q6dt<-mergedt[order(mergedt$uniqueID,mergedt$year, decreasing = FALSE), ] 
q6dt<-q6dt%>%
  mutate(l1_lispremiumneg=lag(lispremiumneg,1),
         l2_lispremiumneg=lag(lispremiumneg,2),
         l3_lispremiumneg=lag(lispremiumneg,3),
         l4_lispremiumneg=lag(lispremiumneg,4),
         l1_lispremiumpos=lag(lispremiumpos,1),
         l2_lispremiumpos=lag(lispremiumpos,2),
         l3_lispremiumpos=lag(lispremiumpos,3),
         l4_lispremiumpos=lag(lispremiumpos,4),
         
         l1_lispremiumnegsq=lag(lispremiumnegsq,1),
         l2_lispremiumnegsq=lag(lispremiumnegsq,2),
         l3_lispremiumnegsq=lag(lispremiumnegsq,3),
         l4_lispremiumnegsq=lag(lispremiumnegsq,4),
         l1_lispremiumpossq=lag(lispremiumpossq,1),
         l2_lispremiumpossq=lag(lispremiumpossq,2),
         l3_lispremiumpossq=lag(lispremiumpossq,3),
         l4_lispremiumpossq=lag(lispremiumpossq,4),
         
         l1_ba_0=lag(ba_0,1),
         l2_ba_0=lag(ba_0,2),
         l3_ba_0=lag(ba_0,3),
         l4_ba_0=lag(ba_0,4),
         
         l1_ba_1_99=lag(ba_1_99,1),
         l2_ba_1_99=lag(ba_1_99,2),
         l3_ba_1_99=lag(ba_1_99,3),
         l4_ba_1_99=lag(ba_1_99,4),
         
         l1_ba_100=lag(ba_100,1),
         l2_ba_100=lag(ba_100,2),
         l3_ba_100=lag(ba_100,3),
         l4_ba_100=lag(ba_100,4),
         
         l1_ba_101_99=lag(ba_101_99,1),
         l2_ba_101_99=lag(ba_101_99,2),
         l3_ba_101_99=lag(ba_101_99,3),
         l4_ba_101_99=lag(ba_101_99,4),
         
         l1_ba_200_49=lag(ba_200_49,1),
         l2_ba_200_49=lag(ba_200_49,2),
         l3_ba_200_49=lag(ba_200_49,3),
         l4_ba_200_49=lag(ba_200_49,4),
         
         l1_ba_250up=lag(ba_250up,1),
         l2_ba_250up=lag(ba_250up,2),
         l3_ba_250up=lag(ba_250up,3),
         l4_ba_250up=lag(ba_250up,4),
         
         l1_btypedetail=lag(btypedetail,1),
         l2_btypedetail=lag(btypedetail,2),
         l3_btypedetail=lag(btypedetail,3),
         l4_btypedetail=lag(btypedetail,4))

###A
q6_2006<-q6dt%>%
  filter(year==2006&rdwindow20062==1)
model2006 = lm.cluster(ln_enrollpct~belowbench2006+lispremiumneg+lispremiumpos,cluster="orgParentCode",data=q6_2006)
summary(model2006)
modelsummary(model2006)
summodel2006est=rbind(summary(model2006)[2,1],summary(model2006)[3,1],summary(model2006)[4,1])
summodel2006se=rbind(summary(model2006)[2,2],summary(model2006)[3,2],summary(model2006)[4,2])
summodel2006=cbind(summodel2006est,summodel2006se)

q6_2007<-q6dt%>%
  filter(year==2007&rdwindow20062==1)
model2007 = lm.cluster(ln_enrollpct~belowbench2006+l1_lispremiumneg+l1_lispremiumpos,cluster="orgParentCode",data=q6_2007)
summary(model2007)
modelsummary(model2007)
summodel2007est=rbind(summary(model2007)[2,1],summary(model2007)[3,1],summary(model2007)[4,1])
summodel2007se=rbind(summary(model2007)[2,2],summary(model2007)[3,2],summary(model2007)[4,2])
summodel2007=cbind(summodel2007est,summodel2007se)

q6_2008<-q6dt%>%
  filter(year==2008&rdwindow20062==1)
model2008 = lm.cluster(ln_enrollpct~belowbench2006+l2_lispremiumneg+l2_lispremiumpos,cluster="orgParentCode",data=q6_2008)
summary(model2008)
modelsummary(model2008)
summodel2008est=rbind(summary(model2008)[2,1],summary(model2008)[3,1],summary(model2008)[4,1])
summodel2008se=rbind(summary(model2008)[2,2],summary(model2008)[3,2],summary(model2008)[4,2])
summodel2008=cbind(summodel2008est,summodel2008se)

q6_2009<-q6dt%>%
  filter(year==2009&rdwindow20062==1)
model2009 = lm.cluster(ln_enrollpct~belowbench2006+l3_lispremiumneg+l3_lispremiumpos,cluster="orgParentCode",data=q6_2009)
summary(model2009)
modelsummary(model2009)
summodel2009est=rbind(summary(model2009)[2,1],summary(model2009)[3,1],summary(model2009)[4,1])
summodel2009se=rbind(summary(model2009)[2,2],summary(model2009)[3,2],summary(model2009)[4,2])
summodel2009=cbind(summodel2009est,summodel2009se)

q6_2010<-q6dt%>%
  filter(year==2010&rdwindow20062==1)
model2010 = lm.cluster(ln_enrollpct~belowbench2006+l4_lispremiumneg+l4_lispremiumpos,cluster="orgParentCode",data=q6_2010)
summary(model2010)
modelsummary(model2010)
summodel2010est=rbind(summary(model2010)[2,1],summary(model2010)[3,1],summary(model2010)[4,1])
summodel2010se=rbind(summary(model2010)[2,2],summary(model2010)[3,2],summary(model2010)[4,2])
summodel2010=cbind(summodel2010est,summodel2010se)

summodelq6a=as.data.frame(cbind(summodel2006,summodel2007,summodel2008,summodel2009,summodel2010))

###B
model2006b = lm.cluster(ln_enrollpct~belowbench2006+lispremiumneg+lispremiumpos+lispremiumnegsq+lispremiumpossq+ba_0+ba_1_99+ba_100+ba_101_99+ba_200_49+ba_250up+factor(btypedetail)+factor(state)+factor(orgParentCode),cluster="orgParentCode",data=q6_2006)
a=summary(model2006b)
summodel2006b=cbind(summary(model2006b)[2,1],summary(model2006b)[2,2])

model2007b = lm.cluster(ln_enrollpct~belowbench2006+l1_lispremiumneg+l1_lispremiumpos+l1_lispremiumnegsq+l1_lispremiumpossq+l1_ba_0+l1_ba_1_99+l1_ba_100+l1_ba_101_99+l1_ba_200_49+l1_ba_250up+factor(l1_btypedetail)+factor(state)+factor(orgParentCode),cluster="orgParentCode",data=q6_2007)
summary(model2007b)
summodel2007b=cbind(summary(model2007b)[2,1],summary(model2007b)[2,2])

model2008b = lm.cluster(ln_enrollpct~belowbench2006+l2_lispremiumneg+l2_lispremiumpos+l2_lispremiumnegsq+l2_lispremiumpossq+l2_ba_0+l2_ba_1_99+l2_ba_100+l2_ba_101_99+l2_ba_200_49+l2_ba_250up+factor(l2_btypedetail)+factor(state)+factor(orgParentCode),cluster="orgParentCode",data=q6_2008)
summary(model2008b)
summodel2008b=cbind(summary(model2008b)[2,1],summary(model2008b)[2,2])

model2009b = lm.cluster(ln_enrollpct~belowbench2006+l3_lispremiumneg+l3_lispremiumpos+l3_lispremiumnegsq+l3_lispremiumpossq+l3_ba_0+l3_ba_1_99+l3_ba_100+l3_ba_101_99+l3_ba_200_49+l3_ba_250up+factor(l3_btypedetail)+factor(state)+factor(orgParentCode),cluster="orgParentCode",data=q6_2009)
summary(model2009b)
summodel2009b=cbind(summary(model2009b)[2,1],summary(model2009b)[2,2])

model2010b = lm.cluster(ln_enrollpct~belowbench2006+l4_lispremiumneg+l4_lispremiumpos+l4_lispremiumnegsq+l4_lispremiumpossq+l4_ba_0+l4_ba_1_99+l4_ba_100+l4_ba_101_99+l4_ba_200_49+l4_ba_250up+factor(l4_btypedetail)+factor(state)+factor(orgParentCode),cluster="orgParentCode",data=q6_2010)
summary(model2010b)
summodel2010b=cbind(summary(model2010b)[2,1],summary(model2010b)[2,2])

summodelq6b=as.data.frame(cbind(summodel2006b,summodel2007b,summodel2008b,summodel2009b,summodel2010b))

###C
model2007c = lm.cluster(ln_enrollpct~bench062007+bench06not2007+benchnot06yes2007+l1_lispremiumneg+l1_lispremiumpos,cluster="orgParentCode",data=q6_2007)
summary(model2007c)
summodel2007cest=rbind(summary(model2007c)[2,1],summary(model2007c)[3,1],summary(model2007c)[4,1])
summodel2007cse=rbind(summary(model2007c)[2,2],summary(model2007c)[3,2],summary(model2007c)[4,2])
summodel2007c=cbind(summodel2007cest,summodel2007cse)

model2008c = lm.cluster(ln_enrollpct~bench062008+bench06not2008+benchnot06yes2008+l2_lispremiumneg+l2_lispremiumpos,cluster="orgParentCode",data=q6_2008)
summary(model2008c)
summodel2008cest=rbind(summary(model2008c)[2,1],summary(model2008c)[3,1],summary(model2008c)[4,1])
summodel2008cse=rbind(summary(model2008c)[2,2],summary(model2008c)[3,2],summary(model2008c)[4,2])
summodel2008c=cbind(summodel2008cest,summodel2008cse)

model2009c = lm.cluster(ln_enrollpct~bench062009+bench06not2009+benchnot06yes2009+l3_lispremiumneg+l3_lispremiumpos,cluster="orgParentCode",data=q6_2009)
summary(model2009c)
summodel2009cest=rbind(summary(model2009c)[2,1],summary(model2009c)[3,1],summary(model2009c)[4,1])
summodel2009cse=rbind(summary(model2009c)[2,2],summary(model2009c)[3,2],summary(model2009c)[4,2])
summodel2009c=cbind(summodel2009cest,summodel2009cse)

model2010c = lm.cluster(ln_enrollpct~bench062010+bench06not2010+benchnot06yes2010+l4_lispremiumneg+l4_lispremiumpos,cluster="orgParentCode",data=q6_2010)
summary(model2010c)
summodel2010cest=rbind(summary(model2010c)[2,1],summary(model2010c)[3,1],summary(model2010c)[4,1])
summodel2010cse=rbind(summary(model2010c)[2,2],summary(model2010c)[3,2],summary(model2010c)[4,2])
summodel2010c=cbind(summodel2010cest,summodel2010cse)



summodelq6c=as.data.frame(cbind(summodel2007c,summodel2008c,summodel2009c,summodel2010c))

write_csv(summodelq6a,'C:/Users/phadmin/Desktop/econ 771/e3/output/q6a.csv',append=FALSE,col_names=TRUE)
write_csv(summodelq6b,'C:/Users/phadmin/Desktop/econ 771/e3/output/q6b.csv',append=FALSE,col_names=TRUE)
write_csv(summodelq6c,'C:/Users/phadmin/Desktop/econ 771/e3/output/q6c.csv',append=FALSE,col_names=TRUE)


#############################################

#Q7

#############################################
y<-q6_2006$ln_enrollpct
x<-q6_2006$lispremium
q7<-rdbwselect(y,x,c = 0,  p = 1, q = 2, deriv = 0,kernel = "tri",bwselect = "cerrd")
q7$bws

mergedtq7<-maindt %>%
  inner_join(lisdtlong, by=c("PDPregion","year"))%>%
  
  #Q1
  group_by(uniqueID) %>%
  mutate(yearmin=min(year))%>%
  ungroup() %>%
  mutate(includeq1=ifelse(yearmin==year, 1, 0))%>%
  
  mutate(eben=ifelse(benefit=="E",1,0))%>%
  
  group_by(orgParentCode) %>%
  mutate(yearminfirm=min(year))%>%
  ungroup()%>%
  mutate(first_us=ifelse(yearminfirm==year, 0, 1)) %>%
  group_by(orgParentCode,state) %>%
  mutate(yearminfirmstate=min(year))%>%
  ungroup()%>%
  mutate(first_state=ifelse(yearminfirmstate==year, 0, 1))%>%
  
  
  #Q2
  group_by(state, year) %>%
  mutate(enroll_stateagg = sum(enrollment, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(enrollpct = enrollment/enroll_stateagg,
         ln_enrollpct = log(enrollpct),
         lispremium = premium - lisline,
         lispremium = ifelse(benefit=="E",NA,lispremium))%>%
  
  #Q6
  mutate(rdwindow=ifelse(lispremium>=-10&lispremium<=10, 1, 0),
         belowbench=ifelse(lispremium<=0&rdwindow==1, 1, 0),
         belowbench2006temp=ifelse(belowbench==1&year==2006, 1, 0),
         rdwindow2006temp=ifelse(rdwindow==1&year==2006, 1, 0),
         lissubsidy2006temp=ifelse(year==2006, lisline, 0))%>%
  
  group_by(uniqueID) %>%
  mutate(belowbench2006=max(belowbench2006temp),
         rdwindow2006=max(rdwindow2006temp),
         lissubsidy2006=max(lissubsidy2006temp)) %>%
  ungroup() %>%
  
  mutate(rdwindow2=ifelse(lispremium>=-1.72&lispremium<=1.72, 1, 0),
         belowbench2=ifelse(lispremium<=0&rdwindow2==1, 1, 0),
         belowbench2006temp2=ifelse(belowbench2==1&year==2006, 1, 0),
         rdwindow2006temp2=ifelse(rdwindow2==1&year==2006, 1, 0))%>%
  
  group_by(uniqueID) %>%
  mutate(belowbench20062=max(belowbench2006temp2),
         rdwindow20062=max(rdwindow2006temp2)) %>%
  ungroup() %>%
  
  mutate(lispremiumneg=ifelse(lispremium<=0, lispremium, 0),
         lispremiumpos=ifelse(lispremium>=0, lispremium, 0),
         lispremiumnegsq=lispremiumneg*lispremiumneg,
         lispremiumpossq=lispremiumpos*lispremiumpos)%>%
  
  mutate(ba_0=ifelse(deductible==0&btypedetail=="BA", 1, 0),
         ba_1_99=ifelse(deductible>0&deductible<100&btypedetail=="BA", 1, 0),
         ba_100=ifelse(deductible==100&btypedetail=="BA", 1, 0),
         ba_101_99=ifelse(deductible>100&deductible<200&btypedetail=="BA", 1, 0),
         ba_200_49=ifelse(deductible>=200&deductible<250&btypedetail=="BA", 1, 0),
         ba_250up=ifelse(deductible>=2500&btypedetail=="BA", 1, 0))%>%
  
  mutate(bench062007=ifelse(belowbench2006==1&LIS==1&year==2007, 1, 0),
         bench062008=ifelse(belowbench2006==1&LIS==1&year==2008, 1, 0),
         bench062009=ifelse(belowbench2006==1&LIS==1&year==2009, 1, 0),
         bench062010=ifelse(belowbench2006==1&LIS==1&year==2010, 1, 0),
         bench06not2007=ifelse(belowbench2006==1&LIS==0&year==2007, 1, 0),
         bench06not2008=ifelse(belowbench2006==1&LIS==0&year==2008, 1, 0),
         bench06not2009=ifelse(belowbench2006==1&LIS==0&year==2009, 1, 0),
         bench06not2010=ifelse(belowbench2006==1&LIS==0&year==2010, 1, 0),
         benchnot06yes2007=ifelse(belowbench2006==0&LIS==1&year==2007, 1, 0),
         benchnot06yes2008=ifelse(belowbench2006==0&LIS==1&year==2008, 1, 0),
         benchnot06yes2009=ifelse(belowbench2006==0&LIS==1&year==2009, 1, 0),
         benchnot06yes2010=ifelse(belowbench2006==0&LIS==1&year==2010, 1, 0))



q7dt<-mergedtq7[order(mergedt$uniqueID,mergedt$year, decreasing = FALSE), ] 
q7dt<-q7dt%>%
  mutate(l1_lispremiumneg=lag(lispremiumneg,1),
         l2_lispremiumneg=lag(lispremiumneg,2),
         l3_lispremiumneg=lag(lispremiumneg,3),
         l4_lispremiumneg=lag(lispremiumneg,4),
         l1_lispremiumpos=lag(lispremiumpos,1),
         l2_lispremiumpos=lag(lispremiumpos,2),
         l3_lispremiumpos=lag(lispremiumpos,3),
         l4_lispremiumpos=lag(lispremiumpos,4),
         
         l1_lispremiumnegsq=lag(lispremiumnegsq,1),
         l2_lispremiumnegsq=lag(lispremiumnegsq,2),
         l3_lispremiumnegsq=lag(lispremiumnegsq,3),
         l4_lispremiumnegsq=lag(lispremiumnegsq,4),
         l1_lispremiumpossq=lag(lispremiumpossq,1),
         l2_lispremiumpossq=lag(lispremiumpossq,2),
         l3_lispremiumpossq=lag(lispremiumpossq,3),
         l4_lispremiumpossq=lag(lispremiumpossq,4),
         
         l1_ba_0=lag(ba_0,1),
         l2_ba_0=lag(ba_0,2),
         l3_ba_0=lag(ba_0,3),
         l4_ba_0=lag(ba_0,4),
         
         l1_ba_1_99=lag(ba_1_99,1),
         l2_ba_1_99=lag(ba_1_99,2),
         l3_ba_1_99=lag(ba_1_99,3),
         l4_ba_1_99=lag(ba_1_99,4),
         
         l1_ba_100=lag(ba_100,1),
         l2_ba_100=lag(ba_100,2),
         l3_ba_100=lag(ba_100,3),
         l4_ba_100=lag(ba_100,4),
         
         l1_ba_101_99=lag(ba_101_99,1),
         l2_ba_101_99=lag(ba_101_99,2),
         l3_ba_101_99=lag(ba_101_99,3),
         l4_ba_101_99=lag(ba_101_99,4),
         
         l1_ba_200_49=lag(ba_200_49,1),
         l2_ba_200_49=lag(ba_200_49,2),
         l3_ba_200_49=lag(ba_200_49,3),
         l4_ba_200_49=lag(ba_200_49,4),
         
         l1_ba_250up=lag(ba_250up,1),
         l2_ba_250up=lag(ba_250up,2),
         l3_ba_250up=lag(ba_250up,3),
         l4_ba_250up=lag(ba_250up,4),
         
         l1_btypedetail=lag(btypedetail,1),
         l2_btypedetail=lag(btypedetail,2),
         l3_btypedetail=lag(btypedetail,3),
         l4_btypedetail=lag(btypedetail,4))

###A
q7_2006<-q7dt%>%
  filter(year==2006&rdwindow20062==1)
model2006 = lm.cluster(ln_enrollpct~belowbench2006+lispremiumneg+lispremiumpos,cluster="orgParentCode",data=q7_2006)
summary(model2006)
modelsummary(model2006)
summodel2006est=rbind(summary(model2006)[2,1],summary(model2006)[3,1],summary(model2006)[4,1])
summodel2006se=rbind(summary(model2006)[2,2],summary(model2006)[3,2],summary(model2006)[4,2])
summodel2006=cbind(summodel2006est,summodel2006se)

q7_2007<-q7dt%>%
  filter(year==2007&rdwindow20062==1)
model2007 = lm.cluster(ln_enrollpct~belowbench2006+l1_lispremiumneg+l1_lispremiumpos,cluster="orgParentCode",data=q7_2007)
summary(model2007)
modelsummary(model2007)
summodel2007est=rbind(summary(model2007)[2,1],summary(model2007)[3,1],summary(model2007)[4,1])
summodel2007se=rbind(summary(model2007)[2,2],summary(model2007)[3,2],summary(model2007)[4,2])
summodel2007=cbind(summodel2007est,summodel2007se)

q7_2008<-q7dt%>%
  filter(year==2008&rdwindow20062==1)
model2008 = lm.cluster(ln_enrollpct~belowbench2006+l2_lispremiumneg+l2_lispremiumpos,cluster="orgParentCode",data=q7_2008)
summary(model2008)
modelsummary(model2008)
summodel2008est=rbind(summary(model2008)[2,1],summary(model2008)[3,1],summary(model2008)[4,1])
summodel2008se=rbind(summary(model2008)[2,2],summary(model2008)[3,2],summary(model2008)[4,2])
summodel2008=cbind(summodel2008est,summodel2008se)

q7_2009<-q7dt%>%
  filter(year==2009&rdwindow20062==1)
model2009 = lm.cluster(ln_enrollpct~belowbench2006+l3_lispremiumneg+l3_lispremiumpos,cluster="orgParentCode",data=q7_2009)
summary(model2009)
modelsummary(model2009)
summodel2009est=rbind(summary(model2009)[2,1],summary(model2009)[3,1],summary(model2009)[4,1])
summodel2009se=rbind(summary(model2009)[2,2],summary(model2009)[3,2],summary(model2009)[4,2])
summodel2009=cbind(summodel2009est,summodel2009se)

q7_2010<-q7dt%>%
  filter(year==2010&rdwindow20062==1)
model2010 = lm.cluster(ln_enrollpct~belowbench2006+l4_lispremiumneg+l4_lispremiumpos,cluster="orgParentCode",data=q7_2010)
summary(model2010)
modelsummary(model2010)
summodel2010est=rbind(summary(model2010)[2,1],summary(model2010)[3,1],summary(model2010)[4,1])
summodel2010se=rbind(summary(model2010)[2,2],summary(model2010)[3,2],summary(model2010)[4,2])
summodel2010=cbind(summodel2010est,summodel2010se)

summodelq7a=as.data.frame(cbind(summodel2006,summodel2007,summodel2008,summodel2009,summodel2010))

###B
model2006b = lm.cluster(ln_enrollpct~belowbench2006+lispremiumneg+lispremiumpos+lispremiumnegsq+lispremiumpossq+ba_0+ba_1_99+ba_100+ba_101_99+ba_200_49+ba_250up+factor(btypedetail)+factor(state)+factor(orgParentCode),cluster="orgParentCode",data=q7_2006)
summary(model2006b)
summodel2006b=cbind(summary(model2006b)[2,1],summary(model2006b)[2,2])

model2007b = lm.cluster(ln_enrollpct~belowbench2006+l1_lispremiumneg+l1_lispremiumpos+l1_lispremiumnegsq+l1_lispremiumpossq+l1_ba_0+l1_ba_1_99+l1_ba_100+l1_ba_101_99+l1_ba_200_49+l1_ba_250up+factor(l1_btypedetail)+factor(state)+factor(orgParentCode),cluster="orgParentCode",data=q7_2007)
summary(model2007b)
summodel2007b=cbind(summary(model2007b)[2,1],summary(model2007b)[2,2])

model2008b = lm.cluster(ln_enrollpct~belowbench2006+l2_lispremiumneg+l2_lispremiumpos+l2_lispremiumnegsq+l2_lispremiumpossq+l2_ba_0+l2_ba_1_99+l2_ba_100+l2_ba_101_99+l2_ba_200_49+l2_ba_250up+factor(l2_btypedetail)+factor(state)+factor(orgParentCode),cluster="orgParentCode",data=q7_2008)
summary(model2008b)
summodel2008b=cbind(summary(model2008b)[2,1],summary(model2008b)[2,2])

model2009b = lm.cluster(ln_enrollpct~belowbench2006+l3_lispremiumneg+l3_lispremiumpos+l3_lispremiumnegsq+l3_lispremiumpossq+l3_ba_0+l3_ba_1_99+l3_ba_100+l3_ba_101_99+l3_ba_200_49+l3_ba_250up+factor(l3_btypedetail)+factor(state)+factor(orgParentCode),cluster="orgParentCode",data=q7_2009)
summary(model2009b)
summodel2009b=cbind(summary(model2009b)[2,1],summary(model2009b)[2,2])

model2010b = lm.cluster(ln_enrollpct~belowbench2006+l4_lispremiumneg+l4_lispremiumpos+l4_lispremiumnegsq+l4_lispremiumpossq+l4_ba_0+l4_ba_1_99+l4_ba_100+l4_ba_101_99+l4_ba_200_49+l4_ba_250up+factor(l4_btypedetail)+factor(state)+factor(orgParentCode),cluster="orgParentCode",data=q7_2010)
summary(model2010b)
summodel2010b=cbind(summary(model2010b)[2,1],summary(model2010b)[2,2])

summodelq7b=as.data.frame(cbind(summodel2006b,summodel2007b,summodel2008b,summodel2009b,summodel2010b))

###C
model2007c = lm.cluster(ln_enrollpct~bench062007+bench06not2007+benchnot06yes2007+l1_lispremiumneg+l1_lispremiumpos,cluster="orgParentCode",data=q7_2007)
summary(model2007c)
summodel2007cest=rbind(summary(model2007c)[2,1],summary(model2007c)[3,1],summary(model2007c)[4,1])
summodel2007cse=rbind(summary(model2007c)[2,2],summary(model2007c)[3,2],summary(model2007c)[4,2])
summodel2007c=cbind(summodel2007cest,summodel2007cse)

model2008c = lm.cluster(ln_enrollpct~bench062008+bench06not2008+benchnot06yes2008+l2_lispremiumneg+l2_lispremiumpos,cluster="orgParentCode",data=q7_2008)
summary(model2008c)
summodel2008cest=rbind(summary(model2008c)[2,1],summary(model2008c)[3,1],summary(model2008c)[4,1])
summodel2008cse=rbind(summary(model2008c)[2,2],summary(model2008c)[3,2],summary(model2008c)[4,2])
summodel2008c=cbind(summodel2008cest,summodel2008cse)

model2009c = lm.cluster(ln_enrollpct~bench062009+bench06not2009+benchnot06yes2009+l3_lispremiumneg+l3_lispremiumpos,cluster="orgParentCode",data=q7_2009)
summary(model2009c)
summodel2009cest=rbind(summary(model2009c)[2,1],summary(model2009c)[3,1],summary(model2009c)[4,1])
summodel2009cse=rbind(summary(model2009c)[2,2],summary(model2009c)[3,2],summary(model2009c)[4,2])
summodel2009c=cbind(summodel2009cest,summodel2009cse)

model2010c = lm.cluster(ln_enrollpct~bench062010+bench06not2010+benchnot06yes2010+l4_lispremiumneg+l4_lispremiumpos,cluster="orgParentCode",data=q7_2010)
summary(model2010c)
summodel2010cest=rbind(summary(model2010c)[2,1],summary(model2010c)[3,1],summary(model2010c)[4,1])
summodel2010cse=rbind(summary(model2010c)[2,2],summary(model2010c)[3,2],summary(model2010c)[4,2])
summodel2010c=cbind(summodel2010cest,summodel2010cse)



summodelq7c=as.data.frame(cbind(summodel2007c,summodel2008c,summodel2009c,summodel2010c))

write_csv(summodelq7a,'C:/Users/phadmin/Desktop/econ 771/e3/output/q7a.csv',append=FALSE,col_names=TRUE)
write_csv(summodelq7b,'C:/Users/phadmin/Desktop/econ 771/e3/output/q7b.csv',append=FALSE,col_names=TRUE)
write_csv(summodelq7c,'C:/Users/phadmin/Desktop/econ 771/e3/output/q7c.csv',append=FALSE,col_names=TRUE)
#############################################

#Q8

#############################################
library(ggplot2)
library(fixest)
q8dt<-q6dt%>%
  filter(benefit=="B")%>%
  mutate(l1_premium=lag(premium,1),
         l2_premium=lag(premium,2),
         l3_premium=lag(premium,3),
         l4_premium=lag(premium,4),
         
         premium_2007_2006=(premium-l1_premium),
         premium_2008_2006=(premium-l2_premium),
         premium_2009_2006=(premium-l3_premium),
         premium_2010_2006=(premium-l4_premium),
         
         premium_2008_2007=(l1_premium-l2_premium),
         premium_2009_2008=(l2_premium-l3_premium),
         premium_2010_2009=(l3_premium-l4_premium))
q8_2007<-q8dt%>%
  filter(year==2007&benefit=="B")
q8_2008<-q8dt%>%
  filter(year==2008&benefit=="B")
q8_2009<-q8dt%>%
  filter(year==2009&benefit=="B")
q8_2010<-q8dt%>%
  filter(year==2010&benefit=="B")

summary(mergedt)



q8a <- ggplot(q8_2007,aes(x=ln_enrollpct,y=premium_2007_2006)) +
  geom_point() +
  scale_color_manual(values=c("#FFCC00", "#000066"))+
  labs(x="log 2006 share", y = "Premium 2007-Premium 2006")+
  theme_classic()
q8a

q8b <- ggplot(q8_2008,aes(x=ln_enrollpct,y=premium_2008_2007)) +
  geom_point() +
  scale_color_manual(values=c("#FFCC00", "#000066"))+
  labs(x="log 2006 share", y = "Premium 2008-Premium 2007")+
  theme_classic()
q8b

q8c <- ggplot(q8_2009,aes(x=ln_enrollpct,y=premium_2009_2008)) +
  geom_point() +
  scale_color_manual(values=c("#FFCC00", "#000066"))+
  labs(x="log 2006 share", y = "Premium 2009-Premium 2008")+
  theme_classic()
q8c

q8d <- ggplot(q8_2010,aes(x=ln_enrollpct,y=premium_2010_2009)) +
  geom_point() +
  scale_color_manual(values=c("#FFCC00", "#000066"))+
  labs(x="log 2006 share", y = "Premium 2010-Premium 2009")+
  theme_classic()
q8d


q8_reg_2007 <- feols(premium_2007_2006 ~ ln_enrollpct + btypedetail | state+orgParentCode , data=q8_2007)
summary(q8_reg_2007)
q8_reg_2008 <- feols(premium_2008_2007 ~ ln_enrollpct + btypedetail | state+orgParentCode , data=q8_2008)
summary(q8_reg_2008)
q8_reg_2009 <- feols(premium_2009_2008 ~ ln_enrollpct + btypedetail | state+orgParentCode , data=q8_2009)
summary(q8_reg_2009)
q8_reg_2010 <- feols(premium_2010_2009 ~ ln_enrollpct + btypedetail | state+orgParentCode , data=q8_2010)
summary(q8_reg_2010)

mod.fs_2007 <- feols(ln_enrollpct~LIS |  state+orgParentCode, data=q8_2007)
summary(mod.fs_2007)
mod.rf_2007 <- feols(premium_2007_2006 ~ LIS |  state+orgParentCode, data=q8_2007)
summary(mod.rf_2007)

mod.iv_2007 <- feols(premium_2007_2006 ~ 0 | state+orgParentCode | ln_enrollpct~LIS, data=q8_2007)
summary(mod.iv_2007)
modelsummary(mod.iv_2007)

mod.iv_2008 <- feols(premium_2008_2007 ~ 0 | state+orgParentCode | ln_enrollpct~LIS, data=q8_2008)
summary(mod.iv_2008)
modelsummary(mod.iv_2008)

mod.iv_2009 <- feols(premium_2009_2008 ~ 0 | state+orgParentCode | ln_enrollpct~LIS, data=q8_2009)
summary(mod.iv_2009)
modelsummary(mod.iv_2009)

mod.iv_2010 <- feols(premium_2010_2009 ~ 0 | state+orgParentCode | ln_enrollpct~LIS, data=q8_2010)
summary(mod.iv_2010)
modelsummary(mod.iv_2010)

