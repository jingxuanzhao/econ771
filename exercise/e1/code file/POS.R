library(readr)
library(tidyverse)
#1984-1990
for (i in 1984:1990) {
  pos=read_csv(paste0("C:/Users/phadmin/Desktop/econ 771/e1/pos",i,".csv/pos",i,".csv"))
  final.pos = pos %>%
    select(provider=prov1680,city=prov0115,hosptype=prov2885,category) %>%
    mutate(year=i,
           stater=str_sub(city, start=-2))%>%
    select(provider,stater,hosptype,year,category)
  
  assign(paste("final.pos.",i,sep=""),final.pos)
  
  if (i==1984) {
    final.posr1=final.pos.1984
  } else {
    final.posr1=rbind(final.posr1,get(paste0("final.pos.",i)))
  }
}
#1991
for (i in 1991) {
  pos=read_csv(paste0("C:/Users/phadmin/Desktop/econ 771/e1/pos",i,".csv/pos",i,".csv"))
  final.pos = pos %>%
    select(provider=prvdr_num,city=prov0115,hosptype=gnrl_cntl_type_cd,category=prvdr_ctgry_cd) %>%
    mutate(year=i,
           stater=str_sub(city, start=-2))%>%
    select(provider,stater,hosptype,year,category)
  
  assign(paste("final.pos.",i,sep=""),final.pos)
  
    final.posr2=final.pos.1991

}

#1992-2021 
for (i in 1992:2010) {
  pos=read_csv(paste0("C:/Users/phadmin/Desktop/econ 771/e1/pos",i,".csv/pos",i,".csv"))
  final.pos = pos %>%
    select(provider=prvdr_num,stater=state_cd,hosptype=gnrl_cntl_type_cd,category=prvdr_ctgry_cd) %>%
    mutate(year=i)
  
  assign(paste("final.pos.",i,sep=""),final.pos)
  
  if (i==1992) {
    final.posr3=final.pos.1992
  } else {
    final.posr3=rbind(final.posr3,get(paste0("final.pos.",i)))
  }
}

for (i in 2011) {
  pos=read_csv(paste0("C:/Users/phadmin/Desktop/econ 771/e1/pos",i,".csv/pos",i,".csv"))
  final.pos = pos %>%
    select(provider=PRVDR_NUM,stater=STATE_CD,hosptype=GNRL_CNTL_TYPE_CD,category=PRVDR_CTGRY_CD) %>%
    mutate(year=i)%>%
    select(provider,stater,hosptype,year,category)
  
  assign(paste("final.pos.",i,sep=""),final.pos)
  
  final.posr4=final.pos.2011
  
}

for (i in 2012:2018) {
  pos=read_csv(paste0("C:/Users/phadmin/Desktop/econ 771/e1/pos",i,".csv/pos",i,".csv"))
  final.pos = pos %>%
    select(provider=prvdr_num,stater=state_cd,hosptype=gnrl_cntl_type_cd,category=prvdr_ctgry_cd) %>%
    mutate(year=i)
  
  assign(paste("final.pos.",i,sep=""),final.pos)
  
  if (i==2012) {
    final.posr5=final.pos.2012
  } else {
    final.posr5=rbind(final.posr5,get(paste0("final.pos.",i)))
  }
}

for (i in 2019:2021) {
  pos=read_csv(paste0("C:/Users/phadmin/Desktop/econ 771/e1/pos",i,".csv/pos",i,".csv"))
  final.pos = pos %>%
    select(provider=PRVDR_NUM,stater=STATE_CD,hosptype=GNRL_CNTL_TYPE_CD,category=PRVDR_CTGRY_CD) %>%
    mutate(year=i)
  
  assign(paste("final.pos.",i,sep=""),final.pos)
  
  if (i==2019) {
    final.posr6=final.pos.2019
  } else {
    final.posr6=rbind(final.posr6,get(paste0("final.pos.",i)))
  }
}

final.pos.dataR=rbind(final.posr1,final.posr2,final.posr3,final.posr4,final.posr5,final.posr6)

final.pos.data <- final.pos.dataR %>%
  filter(category=="01") %>%
  filter(provider!='000000') %>%
  arrange(provider, year)

