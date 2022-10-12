memory.limit(1E10)
library(readr)
library(tidyverse)
MUPD2012 <- read.delim("C:/Users/phadmin/Desktop/econ 771/e2/utilization-payment-puf/utilization-payment-puf/2012/Medicare_Provider_Util_Payment_PUF_CY2012.txt")
MUPD2013 <- read.delim("C:/Users/phadmin/Desktop/econ 771/e2/utilization-payment-puf/utilization-payment-puf/2013/Medicare_Provider_Util_Payment_PUF_CY2013.txt")
MUPD2014 <- read.delim("C:/Users/phadmin/Desktop/econ 771/e2/utilization-payment-puf/utilization-payment-puf/2014/Medicare_Provider_Util_Payment_PUF_CY2014.txt")
MUPD2015 <- read.delim("C:/Users/phadmin/Desktop/econ 771/e2/utilization-payment-puf/utilization-payment-puf/2015/Medicare_Provider_Util_Payment_PUF_CY2015.txt")
MUPD2016 <- read.delim("C:/Users/phadmin/Desktop/econ 771/e2/utilization-payment-puf/utilization-payment-puf/2016/Medicare_Provider_Util_Payment_PUF_CY2016.txt")
MUPD2017 <- read.delim("C:/Users/phadmin/Desktop/econ 771/e2/utilization-payment-puf/utilization-payment-puf/2017/Medicare_Provider_Util_Payment_PUF_CY2017.txt")
MUPD2018 <- read.delim("C:/Users/phadmin/Desktop/econ 771/e2/utilization-payment-puf/utilization-payment-puf/2018/Medicare_Provider_Util_Payment_PUF_CY2018.txt")
MUPD2019 <- read.csv("C:/Users/phadmin/Desktop/econ 771/e2/utilization-payment-puf/utilization-payment-puf/2019/Medicare_Physician_Other_Practitioners_by_Provider_2019.csv")
MUPD2020 <- read.csv("C:/Users/phadmin/Desktop/econ 771/e2/utilization-payment-puf/utilization-payment-puf/2020/Medicare_Physician_Other_Practitioners_by_Provider_2020.csv")

MUPD2012r= MUPD2012 %>%
  mutate(AVERAGE_MEDICARE_STANDARD_AMT=0) %>%
  select(NPI,NPPES_CREDENTIALS,NPPES_ENTITY_CODE,NPPES_PROVIDER_ZIP,NPPES_PROVIDER_STATE,
         NPPES_PROVIDER_COUNTRY,PROVIDER_TYPE,MEDICARE_PARTICIPATION_INDICATOR,
         HCPCS_CODE,HCPCS_DRUG_INDICATOR,LINE_SRVC_CNT,BENE_UNIQUE_CNT,BENE_DAY_SRVC_CNT,AVERAGE_MEDICARE_ALLOWED_AMT,
         AVERAGE_SUBMITTED_CHRG_AMT,AVERAGE_MEDICARE_PAYMENT_AMT,AVERAGE_MEDICARE_STANDARD_AMT) %>%
  mutate(year=2012)

MUPD2013r= MUPD2013 %>%
  mutate(AVERAGE_MEDICARE_STANDARD_AMT=0) %>%
  select(NPI,NPPES_CREDENTIALS,NPPES_ENTITY_CODE,NPPES_PROVIDER_ZIP,NPPES_PROVIDER_STATE,
         NPPES_PROVIDER_COUNTRY,PROVIDER_TYPE,MEDICARE_PARTICIPATION_INDICATOR,
         HCPCS_CODE,HCPCS_DRUG_INDICATOR,LINE_SRVC_CNT,BENE_UNIQUE_CNT,BENE_DAY_SRVC_CNT,AVERAGE_MEDICARE_ALLOWED_AMT,
         AVERAGE_SUBMITTED_CHRG_AMT,AVERAGE_MEDICARE_PAYMENT_AMT,AVERAGE_MEDICARE_STANDARD_AMT) %>%
  mutate(year=2013)

MUPD2014r= MUPD2014 %>%
  select(NPI=npi,NPPES_CREDENTIALS=nppes_credentials,NPPES_ENTITY_CODE=nppes_entity_code,NPPES_PROVIDER_ZIP=nppes_provider_zip,NPPES_PROVIDER_STATE=nppes_provider_state,
         NPPES_PROVIDER_COUNTRY=nppes_provider_country,PROVIDER_TYPE=provider_type,MEDICARE_PARTICIPATION_INDICATOR=medicare_participation_indicator,
         HCPCS_CODE=hcpcs_code,HCPCS_DRUG_INDICATOR=hcpcs_drug_indicator,LINE_SRVC_CNT=line_srvc_cnt,BENE_UNIQUE_CNT=bene_unique_cnt,BENE_DAY_SRVC_CNT=bene_day_srvc_cnt,AVERAGE_MEDICARE_ALLOWED_AMT=average_Medicare_allowed_amt,
         AVERAGE_SUBMITTED_CHRG_AMT=average_submitted_chrg_amt,AVERAGE_MEDICARE_PAYMENT_AMT=average_Medicare_payment_amt,AVERAGE_MEDICARE_STANDARD_AMT=average_Medicare_standard_amt) %>%
  mutate(year=2014)

MUPD2015r= MUPD2015 %>%
  select(NPI=npi,NPPES_CREDENTIALS=nppes_credentials,NPPES_ENTITY_CODE=nppes_entity_code,NPPES_PROVIDER_ZIP=nppes_provider_zip,NPPES_PROVIDER_STATE=nppes_provider_state,
         NPPES_PROVIDER_COUNTRY=nppes_provider_country,PROVIDER_TYPE=provider_type,MEDICARE_PARTICIPATION_INDICATOR=medicare_participation_indicator,
         HCPCS_CODE=hcpcs_code,HCPCS_DRUG_INDICATOR=hcpcs_drug_indicator,LINE_SRVC_CNT=line_srvc_cnt,BENE_UNIQUE_CNT=bene_unique_cnt,BENE_DAY_SRVC_CNT=bene_day_srvc_cnt,AVERAGE_MEDICARE_ALLOWED_AMT=average_Medicare_allowed_amt,
         AVERAGE_SUBMITTED_CHRG_AMT=average_submitted_chrg_amt,AVERAGE_MEDICARE_PAYMENT_AMT=average_Medicare_payment_amt,AVERAGE_MEDICARE_STANDARD_AMT=average_Medicare_standard_amt) %>%
  mutate(year=2015)

MUPD2016r= MUPD2016 %>%
  select(NPI,NPPES_CREDENTIALS,NPPES_ENTITY_CODE,NPPES_PROVIDER_ZIP,NPPES_PROVIDER_STATE,
         NPPES_PROVIDER_COUNTRY,PROVIDER_TYPE,MEDICARE_PARTICIPATION_INDICATOR,
         HCPCS_CODE,HCPCS_DRUG_INDICATOR,LINE_SRVC_CNT,BENE_UNIQUE_CNT,BENE_DAY_SRVC_CNT,AVERAGE_MEDICARE_ALLOWED_AMT,
         AVERAGE_SUBMITTED_CHRG_AMT,AVERAGE_MEDICARE_PAYMENT_AMT,AVERAGE_MEDICARE_STANDARD_AMT) %>%
  mutate(year=2016)

MUPD2017r= MUPD2017 %>%
  select(NPI=npi,NPPES_CREDENTIALS=nppes_credentials,NPPES_ENTITY_CODE=nppes_entity_code,NPPES_PROVIDER_ZIP=nppes_provider_zip,NPPES_PROVIDER_STATE=nppes_provider_state,
         NPPES_PROVIDER_COUNTRY=nppes_provider_country,PROVIDER_TYPE=provider_type,MEDICARE_PARTICIPATION_INDICATOR=medicare_participation_indicator,
         HCPCS_CODE=hcpcs_code,HCPCS_DRUG_INDICATOR=hcpcs_drug_indicator,LINE_SRVC_CNT=line_srvc_cnt,BENE_UNIQUE_CNT=bene_unique_cnt,BENE_DAY_SRVC_CNT=bene_day_srvc_cnt,AVERAGE_MEDICARE_ALLOWED_AMT=average_Medicare_allowed_amt,
         AVERAGE_SUBMITTED_CHRG_AMT=average_submitted_chrg_amt,AVERAGE_MEDICARE_PAYMENT_AMT=average_Medicare_payment_amt,AVERAGE_MEDICARE_STANDARD_AMT=average_Medicare_standard_amt) %>%
  mutate(year=2017)

MUPD2018r= MUPD2018 %>%
  select(NPI=npi,NPPES_CREDENTIALS=nppes_credentials,NPPES_ENTITY_CODE=nppes_entity_code,NPPES_PROVIDER_ZIP=nppes_provider_zip,NPPES_PROVIDER_STATE=nppes_provider_state,
         NPPES_PROVIDER_COUNTRY=nppes_provider_country,PROVIDER_TYPE=provider_type,MEDICARE_PARTICIPATION_INDICATOR=medicare_participation_indicator,
         HCPCS_CODE=hcpcs_code,HCPCS_DRUG_INDICATOR=hcpcs_drug_indicator,LINE_SRVC_CNT=line_srvc_cnt,BENE_UNIQUE_CNT=bene_unique_cnt,BENE_DAY_SRVC_CNT=bene_day_srvc_cnt,AVERAGE_MEDICARE_ALLOWED_AMT=average_Medicare_allowed_amt,
         AVERAGE_SUBMITTED_CHRG_AMT=average_submitted_chrg_amt,AVERAGE_MEDICARE_PAYMENT_AMT=average_Medicare_payment_amt,AVERAGE_MEDICARE_STANDARD_AMT=average_Medicare_standard_amt) %>%
  mutate(year=2018)

MUPD2019r= MUPD2019 %>%
  mutate(AVERAGE_MEDICARE_ALLOWED_AMT=Tot_Mdcr_Alowd_Amt/Tot_Benes,
         AVERAGE_SUBMITTED_CHRG_AMT=Tot_Sbmtd_Chrg/Tot_Benes,
         AVERAGE_MEDICARE_PAYMENT_AMT=Tot_Mdcr_Pymt_Amt/Tot_Benes,
         AVERAGE_MEDICARE_STANDARD_AMT=Tot_Mdcr_Stdzd_Amt/Tot_Benes,
         HCPCS_CODE=0,
         HCPCS_DRUG_INDICATOR=0,
         BENE_DAY_SRVC_CNT=0)%>%
  select(NPI=Rndrng_NPI,NPPES_CREDENTIALS=Rndrng_Prvdr_Crdntls,NPPES_ENTITY_CODE=Rndrng_Prvdr_Ent_Cd,NPPES_PROVIDER_ZIP=Rndrng_Prvdr_Zip5,NPPES_PROVIDER_STATE=Rndrng_Prvdr_State_Abrvtn,
         NPPES_PROVIDER_COUNTRY=Rndrng_Prvdr_Cntry,PROVIDER_TYPE=Rndrng_Prvdr_Type,MEDICARE_PARTICIPATION_INDICATOR=Rndrng_Prvdr_Mdcr_Prtcptg_Ind,
         HCPCS_CODE,HCPCS_DRUG_INDICATOR,LINE_SRVC_CNT=Tot_Srvcs,BENE_UNIQUE_CNT=Tot_Benes,BENE_DAY_SRVC_CNT,AVERAGE_MEDICARE_ALLOWED_AMT,
         AVERAGE_SUBMITTED_CHRG_AMT,AVERAGE_MEDICARE_PAYMENT_AMT,AVERAGE_MEDICARE_STANDARD_AMT) %>%
  mutate(year=2019)

MUPD2020r= MUPD2020 %>%
  mutate(AVERAGE_MEDICARE_ALLOWED_AMT=parse_number(Tot_Mdcr_Alowd_Amt)/as.numeric(Tot_Benes),
         AVERAGE_SUBMITTED_CHRG_AMT=parse_number(Tot_Sbmtd_Chrg)/as.numeric(Tot_Benes),
         AVERAGE_MEDICARE_PAYMENT_AMT=parse_number(Tot_Mdcr_Pymt_Amt)/as.numeric(Tot_Benes),
         AVERAGE_MEDICARE_STANDARD_AMT=parse_number(Tot_Mdcr_Stdzd_Amt)/as.numeric(Tot_Benes),
         HCPCS_CODE=0,
         HCPCS_DRUG_INDICATOR=0,
         BENE_DAY_SRVC_CNT=0)%>%
  select(NPI=ï..Rndrng_NPI,NPPES_CREDENTIALS=Rndrng_Prvdr_Crdntls,NPPES_ENTITY_CODE=Rndrng_Prvdr_Ent_Cd,NPPES_PROVIDER_ZIP=Rndrng_Prvdr_Zip5,NPPES_PROVIDER_STATE=Rndrng_Prvdr_State_Abrvtn,
         NPPES_PROVIDER_COUNTRY=Rndrng_Prvdr_Cntry,PROVIDER_TYPE=Rndrng_Prvdr_Type,MEDICARE_PARTICIPATION_INDICATOR=Rndrng_Prvdr_Mdcr_Prtcptg_Ind,
         HCPCS_CODE,HCPCS_DRUG_INDICATOR,LINE_SRVC_CNT=Tot_Srvcs,BENE_UNIQUE_CNT=Tot_Benes,BENE_DAY_SRVC_CNT,AVERAGE_MEDICARE_ALLOWED_AMT,
         AVERAGE_SUBMITTED_CHRG_AMT,AVERAGE_MEDICARE_PAYMENT_AMT,AVERAGE_MEDICARE_STANDARD_AMT) %>%
  mutate(year=2020)

MUPD<-rbind(MUPD2012r,MUPD2013r,MUPD2014r,MUPD2015r,MUPD2016r,MUPD2017r,MUPD2018r,MUPD2019r,MUPD2020r)

write_tsv(MUPD,'C:/Users/phadmin/Desktop/econ 771/e2/MUPD.txt',append=FALSE,col_names=TRUE)

names(MUPD2014)
names(MUPD2019)
