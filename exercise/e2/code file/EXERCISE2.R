rm(final.data)
MDPPAS <- read.delim("C:/Users/phadmin/Desktop/econ 771/e2/MDPPAS.txt")
MUPD <- read.delim("C:/Users/phadmin/Desktop/econ 771/e2/MUPD.txt")
PFS_update_data <- read.delim("C:/Users/phadmin/Desktop/econ 771/e2/PFS_update_data.txt")

MUPD<-MUPD%>%
  filter(NPPES_CREDENTIALS=="M.D.")
write_tsv(MUPD,'C:/Users/phadmin/Desktop/econ 771/e2/MUPD_MD.txt',append=FALSE,col_names=TRUE)

PFS<-PFS_update_data%>%
  mutate(HCPCS_CODE=hcpcs)%>%
  filter(year==2013)



final.data<-MDPPAS %>%
  inner_join(MUPD, by=c("NPI", "year"))%>%
  inner_join(PFS, by=c("HCPCS_CODE"))




write_tsv(final.data,'C:/Users/phadmin/Desktop/econ 771/e2/final.data.txt',append=FALSE,col_names=TRUE)

table(final.data$year.x)
names(final.data)

memory.limit(1E12)
final.data <- read.delim("C:/Users/phadmin/Desktop/econ 771/e2/final.data.txt")

library(tidyverse)
final.data <- final.data %>%
  filter(year.x!=2018)%>%
  mutate(BENE_UNIQUE_CNT=as.numeric(BENE_UNIQUE_CNT),
         claim=LINE_SRVC_CNT,
         TOT_MEDICARE_PAYMENT_AMT=AVERAGE_MEDICARE_ALLOWED_AMT*LINE_SRVC_CNT)%>% 
  filter(year.x!=2018)  %>%
  mutate(int=ifelse(pos_opd+pos_office+pos_asc==0,0,pos_opd/(pos_opd+pos_office+pos_asc)))%>%
  mutate(int75r=ifelse(int>=0.75, "Integrated", "Non-integrated"))%>% 
  mutate(int75=ifelse(int>=0.75, 1, 0))%>% 
  mutate(birthyear= as.numeric(str_sub(birth_dt,-4,-1)),
         age=year.x-birthyear)

price.shock <-final.data %>%
  mutate_at(vars(dprice_rel_2010, price_nonfac_orig_2010, price_nonfac_orig_2007), replace_na, 0) %>%
  mutate(i=as.numeric(year.x))%>%
  mutate(price_shock = case_when(
    i<=2013 ~ ((i-2009)/4)*dprice_rel_2010,
    i>2013  ~ dprice_rel_2010),
    denom = LINE_SRVC_CNT*price_nonfac_orig_2010,
    numer = price_shock*LINE_SRVC_CNT*price_nonfac_orig_2010) %>%
  group_by(NPI) %>%
  summarize(phy_numer=sum(numer, na.rm=TRUE), phy_denom=sum(denom, na.rm=TRUE), tax_id=first(group1)) %>%
  ungroup() %>%
  mutate(phy_rev_change=phy_numer/phy_denom)   %>%
  group_by(tax_id) %>%
  summarize(practice_rev_change=sum(phy_rev_change, na.rm=TRUE)) %>%
  ungroup()

final.data_iv<-final.data%>%
  inner_join(price.shock , by=c("group1"="tax_id"))
write_tsv(final.data_iv,'C:/Users/phadmin/Desktop/econ 771/e2/final.data_iv.txt',append=FALSE,col_names=TRUE)

final.data_iv_agg<-final.data_iv%>%
  group_by(NPI,year.x) %>%
  summarize(pts=sum(BENE_UNIQUE_CNT, na.rm=TRUE), ttclaim=sum(claim, na.rm=TRUE), payment=sum(TOT_MEDICARE_PAYMENT_AMT)) %>%
  ungroup() 

formerge<-final.data_iv%>%
  group_by(NPI,year.x) %>%
  filter(row_number()==1) %>%
  ungroup() 

final.data_iv_agg_final<-final.data_iv_agg%>%
  inner_join(formerge, by=c("NPI", "year.x"))%>%
  mutate(logclaim=log(ttclaim+0.1))

write_tsv(final.data_iv_agg_final,'C:/Users/phadmin/Desktop/econ 771/e2/final.data_iv_agg_final.txt',append=FALSE,col_names=TRUE)
#############################################

#Q1

#############################################
q1 <- final.data_iv_agg_final%>%
  summarise(
    Mean.pts = mean(pts, na.rm = T),
    Std.pts= sd(pts, na.rm = T),
    Max.pts= max(pts, na.rm = T),
    Min.pts= min(pts, na.rm = T),
    Mean.ttclaim = mean(ttclaim, na.rm = T),
    Std.ttclaim = sd(ttclaim, na.rm = T),
    Max.ttclaim = max(ttclaim, na.rm = T),
    Min.ttclaim = min(ttclaim, na.rm = T),
    Mean.payment = mean(payment, na.rm = T),
    Std.payment = sd(payment, na.rm = T),
    Max.payment = max(payment, na.rm = T),
    Min.payment = min(payment, na.rm = T)
  ) 

q1

write_csv(q1,'C:/Users/phadmin/Desktop/econ 771/e2/output/q1.csv',append=FALSE,col_names=TRUE)

#############################################

#Q2

#############################################
library(tidyverse)
library(ggplot2)


q2_75<- final.data_iv_agg_final%>%
ungroup() %>%
  group_by(int75r,year.x) %>%
  summarise(
    Mean.ttclaim = mean(ttclaim, na.rm = T))

q2_75.fig <- ggplot(q2_75,aes(x=year.x, y=Mean.ttclaim, group=int75r, color=int75r)) +
  geom_line() +
  scale_color_manual(values=c("#FFCC00", "#000066"))+
  labs(x="Year", y = "Total physician-level claims")+
  ggtitle("Figure 1. Mean of total physician-level claims for integrated versus non-integrated physicians") +
  theme_classic()+
  theme(legend.position="right")+ 
  labs(col='Physician integration status')
q2_75.fig



#############################################

#Q3

#############################################
library(tidyverse)
library(fixest)
library(modelsummary)


q3<-feols(logclaim~int75+age|year.x+NPI, data= final.data_iv_agg_final)
summary(q3)
modelsummary(q3)
#############################################

#Q4

#############################################
library(robomit)
final.data_q4<-final.data_iv_agg_final[!is.na(final.data_iv_agg_final$age),]


a_00_05=o_beta(y = "logclaim", # dependent variable
       x = "int75", # independent treatment variable
       con = "age", # related control variables
       delta = 0, # delta
       R2max = 0.5, # maximum R-square
       type = "lm", # model type
       data = final.data_q4) # dataset
a_00_05_beta<-a_00_05[1,2]

a_00_06=o_beta(y = "logclaim", # dependent variable
               x = "int75", # independent treatment variable
               con = "age", # related control variables
               delta = 0, # delta
               R2max = 0.6, # maximum R-square
               type = "lm", # model type
               data = final.data_q4) # dataset
a_00_06_beta<-a_00_06[1,2]

a_00_07=o_beta(y = "logclaim", # dependent variable
               x = "int75", # independent treatment variable
               con = "age", # related control variables
               delta = 0, # delta
               R2max = 0.7, # maximum R-square
               type = "lm", # model type
               data = final.data_q4) # dataset
a_00_07_beta<-a_00_07[1,2]

a_00_08=o_beta(y = "logclaim", # dependent variable
               x = "int75", # independent treatment variable
               con = "age", # related control variables
               delta = 0, # delta
               R2max = 0.8, # maximum R-square
               type = "lm", # model type
               data = final.data_q4) # dataset
a_00_08_beta<-a_00_08[1,2]

a_00_09=o_beta(y = "logclaim", # dependent variable
               x = "int75", # independent treatment variable
               con = "age", # related control variables
               delta = 0, # delta
               R2max = 0.9, # maximum R-square
               type = "lm", # model type
               data = final.data_q4) # dataset
a_00_09_beta<-a_00_09[1,2]

a_00_10=o_beta(y = "logclaim", # dependent variable
               x = "int75", # independent treatment variable
               con = "age", # related control variables
               delta = 0, # delta
               R2max = 1, # maximum R-square
               type = "lm", # model type
               data = final.data_q4) # dataset
a_00_10_beta<-a_00_10[1,2]

a_05_05=o_beta(y = "logclaim", # dependent variable
               x = "int75", # independent treatment variable
               con = "age", # related control variables
               delta = 0.5, # delta
               R2max = 0.5, # maximum R-square
               type = "lm", # model type
               data = final.data_q4) # dataset
a_05_05_beta<-a_05_05[1,2]

a_05_06=o_beta(y = "logclaim", # dependent variable
               x = "int75", # independent treatment variable
               con = "age", # related control variables
               delta = 0.5, # delta
               R2max = 0.6, # maximum R-square
               type = "lm", # model type
               data = final.data_q4) # dataset
a_05_06_beta<-a_05_06[1,2]

a_05_07=o_beta(y = "logclaim", # dependent variable
               x = "int75", # independent treatment variable
               con = "age", # related control variables
               delta = 0.5, # delta
               R2max = 0.7, # maximum R-square
               type = "lm", # model type
               data = final.data_q4) # dataset
a_05_07_beta<-a_05_07[1,2]

a_05_08=o_beta(y = "logclaim", # dependent variable
               x = "int75", # independent treatment variable
               con = "age", # related control variables
               delta = 0.5, # delta
               R2max = 0.8, # maximum R-square
               type = "lm", # model type
               data = final.data_q4) # dataset
a_05_08_beta<-a_05_08[1,2]

a_05_09=o_beta(y = "logclaim", # dependent variable
               x = "int75", # independent treatment variable
               con = "age", # related control variables
               delta = 0.5, # delta
               R2max = 0.9, # maximum R-square
               type = "lm", # model type
               data = final.data_q4) # dataset
a_05_09_beta<-a_05_09[1,2]

a_05_10=o_beta(y = "logclaim", # dependent variable
               x = "int75", # independent treatment variable
               con = "age", # related control variables
               delta = 0.5, # delta
               R2max = 1, # maximum R-square
               type = "lm", # model type
               data = final.data_q4) # dataset
a_05_10_beta<-a_05_10[1,2]

a_10_05=o_beta(y = "logclaim", # dependent variable
               x = "int75", # independent treatment variable
               con = "age", # related control variables
               delta = 1, # delta
               R2max = 0.5, # maximum R-square
               type = "lm", # model type
               data = final.data_q4) # dataset
a_10_05_beta<-a_10_05[1,2]

a_10_06=o_beta(y = "logclaim", # dependent variable
               x = "int75", # independent treatment variable
               con = "age", # related control variables
               delta = 1, # delta
               R2max = 0.6, # maximum R-square
               type = "lm", # model type
               data = final.data_q4) # dataset
a_10_06_beta<-a_10_06[1,2]

a_10_07=o_beta(y = "logclaim", # dependent variable
               x = "int75", # independent treatment variable
               con = "age", # related control variables
               delta = 1, # delta
               R2max = 0.7, # maximum R-square
               type = "lm", # model type
               data = final.data_q4) # dataset
a_10_07_beta<-a_10_07[1,2]

a_10_08=o_beta(y = "logclaim", # dependent variable
               x = "int75", # independent treatment variable
               con = "age", # related control variables
               delta = 1, # delta
               R2max = 0.8, # maximum R-square
               type = "lm", # model type
               data = final.data_q4) # dataset
a_10_08_beta<-a_10_08[1,2]

a_10_09=o_beta(y = "logclaim", # dependent variable
               x = "int75", # independent treatment variable
               con = "age", # related control variables
               delta = 1, # delta
               R2max = 0.9, # maximum R-square
               type = "lm", # model type
               data = final.data_q4) # dataset
a_10_09_beta<-a_10_09[1,2]

a_10_10=o_beta(y = "logclaim", # dependent variable
               x = "int75", # independent treatment variable
               con = "age", # related control variables
               delta = 1, # delta
               R2max = 1, # maximum R-square
               type = "lm", # model type
               data = final.data_q4) # dataset
a_10_10_beta<-a_10_10[1,2]

a_15_05=o_beta(y = "logclaim", # dependent variable
               x = "int75", # independent treatment variable
               con = "age", # related control variables
               delta = 1.5, # delta
               R2max = 0.5, # maximum R-square
               type = "lm", # model type
               data = final.data_q4) # dataset
a_15_05_beta<-a_15_05[1,2]

a_15_06=o_beta(y = "logclaim", # dependent variable
               x = "int75", # independent treatment variable
               con = "age", # related control variables
               delta = 1.5, # delta
               R2max = 0.6, # maximum R-square
               type = "lm", # model type
               data = final.data_q4) # dataset
a_15_06_beta<-a_15_06[1,2]

a_15_07=o_beta(y = "logclaim", # dependent variable
               x = "int75", # independent treatment variable
               con = "age", # related control variables
               delta = 1.5, # delta
               R2max = 0.7, # maximum R-square
               type = "lm", # model type
               data = final.data_q4) # dataset
a_15_07_beta<-a_15_07[1,2]

a_15_08=o_beta(y = "logclaim", # dependent variable
               x = "int75", # independent treatment variable
               con = "age", # related control variables
               delta = 1.5, # delta
               R2max = 0.8, # maximum R-square
               type = "lm", # model type
               data = final.data_q4) # dataset
a_15_08_beta<-a_15_08[1,2]

a_15_09=o_beta(y = "logclaim", # dependent variable
               x = "int75", # independent treatment variable
               con = "age", # related control variables
               delta = 1.5, # delta
               R2max = 0.9, # maximum R-square
               type = "lm", # model type
               data = final.data_q4) # dataset
a_15_09_beta<-a_15_09[1,2]

a_15_10=o_beta(y = "logclaim", # dependent variable
               x = "int75", # independent treatment variable
               con = "age", # related control variables
               delta = 1.5, # delta
               R2max = 1, # maximum R-square
               type = "lm", # model type
               data = final.data_q4) # dataset
a_15_10_beta<-a_15_10[1,2]

a_20_05=o_beta(y = "logclaim", # dependent variable
               x = "int75", # independent treatment variable
               con = "age", # related control variables
               delta = 2, # delta
               R2max = 0.5, # maximum R-square
               type = "lm", # model type
               data = final.data_q4) # dataset
a_20_05_beta<-a_20_05[1,2]

a_20_06=o_beta(y = "logclaim", # dependent variable
               x = "int75", # independent treatment variable
               con = "age", # related control variables
               delta = 2, # delta
               R2max = 0.6, # maximum R-square
               type = "lm", # model type
               data = final.data_q4) # dataset
a_20_06_beta<-a_20_06[1,2]

a_20_07=o_beta(y = "logclaim", # dependent variable
               x = "int75", # independent treatment variable
               con = "age", # related control variables
               delta = 2, # delta
               R2max = 0.7, # maximum R-square
               type = "lm", # model type
               data = final.data_q4) # dataset
a_20_07_beta<-a_20_07[1,2]

a_20_08=o_beta(y = "logclaim", # dependent variable
               x = "int75", # independent treatment variable
               con = "age", # related control variables
               delta = 2, # delta
               R2max = 0.8, # maximum R-square
               type = "lm", # model type
               data = final.data_q4) # dataset
a_20_08_beta<-a_20_08[1,2]

a_20_09=o_beta(y = "logclaim", # dependent variable
               x = "int75", # independent treatment variable
               con = "age", # related control variables
               delta = 2, # delta
               R2max = 0.9, # maximum R-square
               type = "lm", # model type
               data = final.data_q4) # dataset
a_20_09_beta<-a_20_09[1,2]

a_20_10=o_beta(y = "logclaim", # dependent variable
               x = "int75", # independent treatment variable
               con = "age", # related control variables
               delta = 2, # delta
               R2max = 1, # maximum R-square
               type = "lm", # model type
               data = final.data_q4) # dataset
a_20_10_beta<-a_20_10[1,2]

a_beta=cbind(a_00_05_beta,a_00_06_beta,a_00_07_beta,a_00_08_beta,a_00_09_beta,a_00_10_beta,
             a_05_05_beta,a_05_06_beta,a_05_07_beta,a_05_08_beta,a_05_09_beta,a_05_10_beta,
             a_10_05_beta,a_10_06_beta,a_10_07_beta,a_10_08_beta,a_10_09_beta,a_10_10_beta,
             a_15_05_beta,a_15_06_beta,a_15_07_beta,a_15_08_beta,a_15_09_beta,a_15_10_beta,
             a_20_05_beta,a_20_06_beta,a_20_07_beta,a_20_08_beta,a_20_09_beta,a_20_10_beta)

a_beta

write_csv(a_beta,'C:/Users/phadmin/Desktop/econ 771/e2/output/q4.csv',append=FALSE,col_names=TRUE)


#############################################

#Q5

#############################################
library(lfe)

ivmodel=felm(logclaim~age|year.x+NPI|(int75~practice_rev_change)|NPI, data=final.data_iv_agg_final)
modelsummary(ivmodel)
summary(ivmodel)
step1 <- felm(int75~practice_rev_change+age|year.x+NPI, data=final.data_iv_agg_final)
modelsummary(step1)
summary(step1)
reduce<-felm(logclaim~practice_rev_change+age|year.x+NPI, data=final.data_iv_agg_final)
modelsummary(reduce)
summary(reduce)

#############################################

#Q6

#############################################

q6<-feols(int75~practice_rev_change+age|year.x+NPI, data= final.data_q4)
int75.hat <- predict(q6)
final.data_q6=cbind(final.data_q4,int75.hat)
final.data_q6<-final.data_q6%>%
  mutate(intdiff=int75-int75.hat)
  
q6.dif<-feols(logclaim~int75+intdiff+age|year.x+NPI, data= final.data_q6)
modelsummary(q6.dif)
summary(q6.dif)

#############################################

#Q7

#############################################
library(ivmodel)
#https://rdrr.io/cran/ivmodel/man/AR.test.html
final.data_q7<-final.data_q6%>%
  mutate(logclaimn=as.numeric(logclaim))

Y=final.data_q7[,"logclaimn"]
D=final.data_q7[,"int75"]
Z=final.data_q7[,"practice_rev_change"]
Xname=c("age")
X=final.data_q7[,Xname]
q7 = ivmodel(Y=Y,D=D,Z=Z,X=X)
AR.test(q7)
#############################################

#Q8

#############################################
random<-final.data_q7$practice_rev_change
random100<-NULL
for (i in 1:100){
randomx<-sample(random)
random100=as.data.frame(cbind(random100,randomx))
}
random100$revchangemean=rowMeans(random100[-1])
random100r<-random100[,101]
final.data_q8<-cbind(random<-final.data_q7,random100r)
final.data_q8<-final.data_q8%>%
  mutate(rev_changedif=practice_rev_change-random100r)

library(lfe)

ivmodel=felm(logclaim~age|year.x+NPI|(int75~rev_changedif)|NPI, data=final.data_q8)
modelsummary(ivmodel)
summary(ivmodel)
step1 <- felm(int75~rev_changedif+age|year.x+NPI, data=final.data_q8)
modelsummary(step1)
summary(step1)
reduce<-felm(logclaim~rev_changedif+age|year.x+NPI, data=final.data_q8)
modelsummary(reduce)
summary(reduce)
