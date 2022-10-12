library(readr)
library(tidyverse)
MDPPAS2009 <- read.csv("C:/Users/phadmin/Desktop/econ 771/e2/MDPPAS/MDPPAS/PhysicianData_2009/PhysicianData_2009.csv")
MDPPAS2010 <- read.csv("C:/Users/phadmin/Desktop/econ 771/e2/MDPPAS/MDPPAS/PhysicianData_2010/PhysicianData_2010.csv")
MDPPAS2011 <- read.csv("C:/Users/phadmin/Desktop/econ 771/e2/MDPPAS/MDPPAS/PhysicianData_2011/PhysicianData_2011.csv")
MDPPAS2012 <- read.csv("C:/Users/phadmin/Desktop/econ 771/e2/MDPPAS/MDPPAS/PhysicianData_2012/PhysicianData_2012.csv")
MDPPAS2013 <- read.csv("C:/Users/phadmin/Desktop/econ 771/e2/MDPPAS/MDPPAS/PhysicianData_2013/PhysicianData_2013.csv")
MDPPAS2014 <- read.csv("C:/Users/phadmin/Desktop/econ 771/e2/MDPPAS/MDPPAS/PhysicianData_2014/PhysicianData_2014.csv")
MDPPAS2015 <- read.csv("C:/Users/phadmin/Desktop/econ 771/e2/MDPPAS/MDPPAS/PhysicianData_2015/PhysicianData_2015.csv")
MDPPAS2016 <- read.csv("C:/Users/phadmin/Desktop/econ 771/e2/MDPPAS/MDPPAS/PhysicianData_2016/PhysicianData_2016.csv")
MDPPAS2017 <- read.csv("C:/Users/phadmin/Desktop/econ 771/e2/MDPPAS/MDPPAS/PhysicianData_2017/PhysicianData_2017.csv")
MDPPAS2018 <- read.csv("C:/Users/phadmin/Desktop/econ 771/e2/MDPPAS/MDPPAS/PhysicianData_2018/PhysicianData_2018.csv")
MDPPAS2019 <- read.csv("C:/Users/phadmin/Desktop/econ 771/e2/MDPPAS/MDPPAS/PhysicianData_2019/PhysicianData_2019.csv")

names(MDPPAS2018)

MDPPAS2009r= MDPPAS2009 %>%
  select(NPI=npi,pos_office,pos_inpat,pos_opd,pos_er,pos_nursing,pos_asc,pos_resid,pos_other,npi_allowed_amt,npi_unq_benes,
         birth_dt,claim_count11,perc_male,perc_female,perc_white,perc_black,perc_asian,perc_hispanic,perc_amerindian,group1,group2) %>%
  mutate(year=2009)

MDPPAS2010r= MDPPAS2010 %>%
  select(NPI=npi,pos_office,pos_inpat,pos_opd,pos_er,pos_nursing,pos_asc,pos_resid,pos_other,npi_allowed_amt,npi_unq_benes,
         birth_dt,claim_count11,perc_male,perc_female,perc_white,perc_black,perc_asian,perc_hispanic,perc_amerindian,group1,group2) %>%
  mutate(year=2010)

MDPPAS2011r= MDPPAS2011 %>%
  select(NPI=npi,pos_office,pos_inpat,pos_opd,pos_er,pos_nursing,pos_asc,pos_resid,pos_other,npi_allowed_amt,npi_unq_benes,
         birth_dt,claim_count11,perc_male,perc_female,perc_white,perc_black,perc_asian,perc_hispanic,perc_amerindian,group1,group2) %>%
  mutate(year=2011)

MDPPAS2012r= MDPPAS2012 %>%
  select(NPI=npi,pos_office,pos_inpat,pos_opd,pos_er,pos_nursing,pos_asc,pos_resid,pos_other,npi_allowed_amt,npi_unq_benes,
         birth_dt,claim_count11,perc_male,perc_female,perc_white,perc_black,perc_asian,perc_hispanic,perc_amerindian,group1,group2) %>%
  mutate(year=2012)

MDPPAS2013r= MDPPAS2013 %>%
  select(NPI=npi,pos_office,pos_inpat,pos_opd,pos_er,pos_nursing,pos_asc,pos_resid,pos_other,npi_allowed_amt,npi_unq_benes,
         birth_dt,claim_count11,perc_male,perc_female,perc_white,perc_black,perc_asian,perc_hispanic,perc_amerindian,group1,group2) %>%
  mutate(year=2013)

MDPPAS2014r= MDPPAS2014 %>%
  select(NPI=npi,pos_office,pos_inpat,pos_opd,pos_er,pos_nursing,pos_asc,pos_resid,pos_other,npi_allowed_amt,npi_unq_benes,
         birth_dt,claim_count11,perc_male,perc_female,perc_white,perc_black,perc_asian,perc_hispanic,perc_amerindian,group1,group2) %>%
  mutate(year=2014)

MDPPAS2015r= MDPPAS2015 %>%
  select(NPI=npi,pos_office,pos_inpat,pos_opd,pos_er,pos_nursing,pos_asc,pos_resid,pos_other,npi_allowed_amt,npi_unq_benes,
         birth_dt,claim_count11,perc_male,perc_female,perc_white,perc_black,perc_asian,perc_hispanic,perc_amerindian,group1,group2) %>%
  mutate(year=2015)

MDPPAS2016r= MDPPAS2016 %>%
  select(NPI=npi,pos_office,pos_inpat,pos_opd,pos_er,pos_nursing,pos_asc,pos_resid,pos_other,npi_allowed_amt,npi_unq_benes,
         birth_dt,claim_count11,perc_male,perc_female,perc_white,perc_black,perc_asian,perc_hispanic,perc_amerindian,group1,group2) %>%
  mutate(year=2016)

MDPPAS2017r= MDPPAS2017 %>%
  select(NPI=npi,pos_office,pos_inpat,pos_opd,pos_er,pos_nursing,pos_asc,pos_resid,pos_other,npi_allowed_amt,npi_unq_benes,
         birth_dt,claim_count11,perc_male,perc_female,perc_white,perc_black,perc_asian,perc_hispanic,perc_amerindian,group1,group2) %>%
  mutate(year=2017)

MDPPAS2018r= MDPPAS2018 %>%
  select(NPI=npi,pos_office,pos_inpat,pos_opd,pos_er,pos_nursing,pos_asc,pos_resid,pos_other,npi_allowed_amt,npi_unq_benes,
         birth_dt,claim_count11,perc_male,perc_female,perc_white,perc_black,perc_asian,perc_hispanic,perc_amerindian,group1,group2) %>%
  mutate(year=2018)

MDPPAS2019r= MDPPAS2019 %>%
  select(NPI=npi,pos_office,pos_inpat,pos_opd,pos_er,pos_nursing,pos_asc,pos_resid,pos_other,npi_allowed_amt,npi_unq_benes,
         birth_dt,claim_count11,perc_male,perc_female,perc_white,perc_black,perc_asian,perc_hispanic,perc_amerindian,group1,group2) %>%
  mutate(year=2019)

MDPPAS<-rbind(MDPPAS2009r, MDPPAS2010r, MDPPAS2011r, MDPPAS2012r, MDPPAS2013r, MDPPAS2014r, MDPPAS2015r, MDPPAS2016r, MDPPAS2017r, MDPPAS2018r, MDPPAS2019r)

write_tsv(MDPPAS,'C:/Users/phadmin/Desktop/econ 771/e2/MDPPAS.txt',append=FALSE,col_names=TRUE)

rm(MDPPAS2009)
rm(MDPPAS2010)
rm(MDPPAS2011)
rm(MDPPAS2012)
rm(MDPPAS2013)
rm(MDPPAS2014)
rm(MDPPAS2015)
rm(MDPPAS2016)
rm(MDPPAS2017)
rm(MDPPAS2018)
rm(MDPPAS2019)

rm(MDPPAS2009r)
rm(MDPPAS2010r)
rm(MDPPAS2011r)
rm(MDPPAS2012r)
rm(MDPPAS2013r)
rm(MDPPAS2014r)
rm(MDPPAS2015r)
rm(MDPPAS2016r)
rm(MDPPAS2017r)
rm(MDPPAS2018r)
rm(MDPPAS2019r)
