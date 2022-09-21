library(readxl)
kffstate <- read_excel("C:/Users/phadmin/Desktop/econ 771/e1/KFFstate.xlsx")
final.hcris.data<-HCRIS_Data

final.pos.data$provider <- as.numeric(final.pos.data$provider)

  final.data <- final.pos.data %>%
  left_join(final.hcris.data, by=c("provider", "year"))%>%
  left_join(kffstate, by=c("stater"))%>%
    mutate(hosptyper=ifelse(hosptype %in% c("02"),"Private not for profit", 
                   ifelse(hosptype %in% c("04","09"), "Private for profit","Others")))%>%
    mutate(provider <- as.character(provider))%>%
    mutate(year <- as.character(year))

write_tsv(final.data,'C:/Users/phadmin/Desktop/econ 771/e1/final.data.txt',append=FALSE,col_names=TRUE)
  
table(final.data$stater)

#############################################

#Q1

#############################################
  q1 <- final.hcris.data %>%
    ungroup() %>%
    group_by(year) %>%
    filter(year>2002&year<2020) %>%
    summarise(
      Mean.uncomp_care = mean(uncomp_care, na.rm = T),
      Std.uncomp_care = sd(uncomp_care, na.rm = T),
      Max.uncomp_care = max(uncomp_care, na.rm = T),
      Min.uncomp_care = min(uncomp_care, na.rm = T),
      Mean.tot_pat_rev = mean(tot_pat_rev, na.rm = T),
      Std.tot_pat_rev = sd(tot_pat_rev, na.rm = T),
      Max.tot_pat_rev = max(tot_pat_rev, na.rm = T),
      Min.tot_pat_rev = min(tot_pat_rev, na.rm = T),
    ) %>%
    arrange(year)

q1
  
write_csv(q1,'C:/Users/phadmin/Desktop/econ 771/e1/output/q1.csv',append=FALSE,col_names=TRUE)
#############################################
  
#Q2
  
#############################################
  library(ggplot2)
  q2 <- final.data %>%
    filter(year>2002&year<2020)%>%
    filter(hosptyper!="Others")%>%
    ungroup() %>%
    group_by(hosptyper,year) %>%
    summarise(
      Mean.uncomp_care = mean(uncomp_care, na.rm = T) 

    ) %>%
    arrange(hosptyper,year)
  
  
  q2.fig <- ggplot(q2,aes(x=year, y=Mean.uncomp_care, group=hosptyper, color=hosptyper)) +
    geom_line() +
    scale_color_manual(values=c("#FFCC00", "#000066"))+
    labs(x="Year", y = "Mean hospital uncompensated care")+
    ggtitle("Figure 1. Mean hospital uncompensated care, by hospital ownership type, 2000-2018") +
    theme_classic()+
    theme(legend.position="right")+ 
    labs(col='Hospital ownership type')
  q2.fig
  
#############################################
  
#Q3
  
#############################################
library(fixest)
library(modelsummary)
final.data_q3 <- final.data %>%
  filter(year>2002&year<2020) %>%
  mutate(indicator_all=ifelse(expansion==1 & year>=Expansionyear, 1, 0))%>%
  mutate(indicator_2014=ifelse(expansion==1 & year>=Expansionyear & Expansionyear==2014, 1, 0))%>%
  mutate(indicator_2015=ifelse(expansion==1 & year>=Expansionyear & (Expansionyear==2014|Expansionyear==2015), 1, 0))%>%
  mutate(indicator_2016=ifelse(expansion==1 & year>=Expansionyear & (Expansionyear==2014|Expansionyear==2015|Expansionyear==2016), 1, 0))

final.data_q3_2014<-final.data_q3%>%
  filter(Expansionyear==0|Expansionyear==2014)
final.data_q3_2015<-final.data_q3%>%
  filter(Expansionyear==0|Expansionyear==2014|Expansionyear==2015)
final.data_q3_2016<-final.data_q3%>%
  filter(Expansionyear==0|Expansionyear==2014|Expansionyear==2015|Expansionyear==2016)

q3_all<-feols(uncomp_care~indicator_all|year+provider, data=final.data_q3)
modelsummary(q3_all)
summary(q3_all)

q3_2014<-feols(uncomp_care~indicator_2014|year+provider, data=final.data_q3_2014)
summary(q3_2014)

q3_2015<-feols(uncomp_care~indicator_2015|year+provider, data=final.data_q3_2015)
summary(q3_2015)

q3_2016<-feols(uncomp_care~indicator_2016|year+provider, data=final.data_q3_2016)
summary(q3_2016)
t<-summary(q3_all)
est_all <- as_tibble(q3_all$coefficients)
est_2014 <- as_tibble(q3_2014$coefficients)
est_2015 <- as_tibble(q3_2015$coefficients)
est_2016 <- as_tibble(q3_2016$coefficients)
est_q3<-rbind(est_all,est_2014,est_2015,est_2016)

write_csv(est_q3,'C:/Users/phadmin/Desktop/econ 771/e1/output/q3.csv',append=FALSE,col_names=TRUE)

#############################################

#Q4

#############################################
final.data_q4 <- final.data %>%
  filter(year>2002&year<2020) %>%
  mutate(indexyear=ifelse(expansion==1, year-Expansionyear, -1))%>%
  mutate(index_yn18=ifelse(indexyear==-18, 1, 0))%>%
  mutate(index_yn17=ifelse(indexyear==-17, 1, 0))%>%
  mutate(index_yn16=ifelse(indexyear==-16, 1, 0))%>%
  mutate(index_yn15=ifelse(indexyear==-15, 1, 0))%>%
  mutate(index_yn14=ifelse(indexyear==-14, 1, 0))%>%
  mutate(index_yn13=ifelse(indexyear==-13, 1, 0))%>%
  mutate(index_yn12=ifelse(indexyear==-12, 1, 0))%>%
  mutate(index_yn11=ifelse(indexyear==-11, 1, 0))%>%
  mutate(index_yn10=ifelse(indexyear==-10, 1, 0))%>%
  mutate(index_yn9=ifelse(indexyear==-9, 1, 0))%>%
  mutate(index_yn8=ifelse(indexyear==-8, 1, 0))%>%
  mutate(index_yn7=ifelse(indexyear==-7, 1, 0))%>%
  mutate(index_yn6=ifelse(indexyear==-6, 1, 0))%>%
  mutate(index_yn5=ifelse(indexyear==-5, 1, 0))%>%
  mutate(index_yn4=ifelse(indexyear==-4, 1, 0))%>%
  mutate(index_yn3=ifelse(indexyear==-3, 1, 0))%>%
  mutate(index_yn2=ifelse(indexyear==-2, 1, 0))%>%
  mutate(index_yp0=ifelse(indexyear==0, 1, 0))%>%
  mutate(index_yp1=ifelse(indexyear==1, 1, 0))%>%
  mutate(index_yp2=ifelse(indexyear==2, 1, 0))%>%
  mutate(index_yp3=ifelse(indexyear==3, 1, 0))%>%
  mutate(index_yp4=ifelse(indexyear==4, 1, 0))%>%
  mutate(index_yp5=ifelse(indexyear==5, 1, 0))%>%
  mutate(index_yp6=ifelse(indexyear==6, 1, 0))

table(final.data_q4$indexyear)

final.data_q4_2014 <- final.data_q4 %>%
  filter(Expansionyear==0|Expansionyear==2014)

q4_all<-feols(uncomp_care~index_yn18+index_yn17+index_yn16+index_yn15+index_yn14+index_yn13+index_yn12+index_yn11+index_yn10+
             index_yn9+index_yn8+index_yn7+index_yn6+index_yn5+index_yn4+index_yn3+index_yn2+
             index_yp0+index_yp1+index_yp2+index_yp3+index_yp4+index_yp5+index_yp6|year+provider, data=final.data_q4)
summary(q4_all)


q4_all_2014<-feols(uncomp_care~index_yn18+index_yn17+index_yn16+index_yn15+index_yn14+index_yn13+index_yn12+index_yn11+index_yn10+
               index_yn9+index_yn8+index_yn7+index_yn6+index_yn5+index_yn4+index_yn3+index_yn2+
               index_yp0+index_yp1+index_yp2+index_yp3+index_yp4+index_yp5+index_yp6|year+provider, data=final.data_q4_2014)
summary(q4_all_2014)

#############################################

#Q5

#############################################
final.data_q5 <- final.data %>%
  filter(year>2002&year<2020) %>%
  filter(Expansionyear==0|Expansionyear==2014|Expansionyear==2015|Expansionyear==2016) %>%
  mutate(Expansionyear = ifelse(Expansionyear==0, 10000, Expansionyear),
         time_to_treat = ifelse(expansion==0, -1, year-Expansionyear),
         time_to_treat = ifelse(time_to_treat < -11, -11, time_to_treat))
 

q5<-feols(uncomp_care ~ sunab(Expansionyear, time_to_treat) | year+provider, cluster=~provider,data=final.data_q5)
summary(q5)


#############################################

#Q6

#############################################
library(fixest)
#https://lrberge.github.io/fixest/reference/sunab.html
iplot(q5,
  main     = "Figure 2. Event study of Medicaid expansion on hospital uncompensated care, using Sun and Abraham approach",
  xlab     = "Year to enactment of Medicaid expansion",
  ylab     = "Event study estimates for uncompensated care"
)

#############################################

#Q7

#############################################
# Install some packages

library(devtools)
install_github("bcallaway11/BMisc", dependencies = TRUE)
install_github("bcallaway11/did", dependencies = TRUE)
install_github("asheshrambachan/HonestDiD", dependencies = TRUE)
#--------------------------------------------------------------------------
# Load packages
#--------------------------------------------------------------------------
# Libraries
# Load libraries
library(ggplot2)
library(here)
library(foreign)
library(rlang)
library(tidyverse)
library(CVXR)
library(dplyr)
library(did)
library(DRDID)
library(foreach)
library(doParallel)
library(TruncatedNormal)
library(latex2exp)
library(iterators)
library(lpSolveAPI)
library(purrr)
library(pracma)
library(Matrix)
library(ROI)
library(HonestDiD)

## -----------------------------------------------------------------------------

#' @title honest_did
#'
#' @description a function to compute a sensitivity analysis
#'  using the approach of Rambachan and Roth (2021)
#' @param es an event study
honest_did <- function(es, ...) {
  UseMethod("honest_did", es)
}


#' @title honest_did.AGGTEobj
#'
#' @description a function to compute a sensitivity analysis
#'  using the approach of Rambachan and Roth (2021) when
#'  the event study is estimating using the `did` package
#'
#' @param e event time to compute the sensitivity analysis for.
#'  The default value is `e=0` corresponding to the "on impact"
#'  effect of participating in the treatment.
#' @param type Options are "smoothness" (which conducts a
#'  sensitivity analysis allowing for violations of linear trends
#'  in pre-treatment periods) or "relative_magnitude" (which
#'  conducts a sensitivity analysis based on the relative magnitudes
#'  of deviations from parallel trends in pre-treatment periods).
#' @inheritParams HonestDiD::createSensitivityResults
#' @inheritParams HonestDid::createSensitivityResults_relativeMagnitudes
honest_did.AGGTEobj <- function(es,
                                e=0,
                                type=c("smoothness", "relative_magnitude"),
                                method=NULL,
                                bound="deviation from parallel trends",
                                Mvec=NULL,
                                Mbarvec=NULL,
                                monotonicityDirection=NULL,
                                biasDirection=NULL,
                                alpha=0.05,
                                parallel=FALSE,
                                gridPoints=10^3,
                                grid.ub=NA,
                                grid.lb=NA,
                                ...) {
  
  
  type <- type[1]
  
  # make sure that user is passing in an event study
  if (es$type != "dynamic") {
    stop("need to pass in an event study")
  }
  
  # check if used universal base period and warn otherwise
  if (es$DIDparams$base_period != "universal") {
    warning("it is recommended to use a universal base period for honest_did")
  }
  
  # recover influence function for event study estimates
  es_inf_func <- es$inf.function$dynamic.inf.func.e
  
  # recover variance-covariance matrix
  n <- nrow(es_inf_func)
  V <- t(es_inf_func) %*% es_inf_func / (n*n) 
  
  
  nperiods <- nrow(V)
  npre <- sum(1*(es$egt < 0))
  npost <- nperiods - npre
  
  baseVec1 <- basisVector(index=(e+1),size=npost)
  
  orig_ci <- constructOriginalCS(betahat = es$att.egt,
                                 sigma = V, numPrePeriods = npre,
                                 numPostPeriods = npost,
                                 l_vec = baseVec1)
  
  if (type=="relative_magnitude") {
    if (is.null(method)) method <- "C-LF"
    robust_ci <- createSensitivityResults_relativeMagnitudes(betahat = es$att.egt, sigma = V, 
                                                             numPrePeriods = npre, 
                                                             numPostPeriods = npost,
                                                             bound=bound,
                                                             method=method,
                                                             l_vec = baseVec1,
                                                             Mbarvec = Mbarvec,
                                                             monotonicityDirection=monotonicityDirection,
                                                             biasDirection=biasDirection,
                                                             alpha=alpha,
                                                             gridPoints=100,
                                                             grid.lb=-1,
                                                             grid.ub=1,
                                                             parallel=parallel)
    
  } else if (type=="smoothness") {
    robust_ci <- createSensitivityResults(betahat = es$att.egt,
                                          sigma = V, 
                                          numPrePeriods = npre, 
                                          numPostPeriods = npost,
                                          method=method,
                                          l_vec = baseVec1,
                                          monotonicityDirection=monotonicityDirection,
                                          biasDirection=biasDirection,
                                          alpha=alpha,
                                          parallel=parallel)
  }
  
  list(robust_ci=robust_ci, orig_ci=orig_ci, type=type)
}

#---------------------------------------------------------------------------
# Using covariates and DR DiD with never-treated as comparison group
# Fix the reference time periods
# https://www.rdocumentation.org/packages/did/versions/2.1.2/topics/att_gt
final.data$providernum <- as.numeric(final.data$provider)
final.data_q7 <- final.data %>%
  filter(year>2002&year<2020)%>%
  filter(!is.na(uncomp_care)) %>%
  group_by(providernum) %>%
  mutate(providerq7=cur_group_id()) %>% ungroup()

CS_never_cond <- att_gt(yname="uncomp_care",
                        tname="year",
                        idname="providerq7",
                        gname="Expansionyear",
                        xformla=~1,
                        #xformla = xformla
                        data = final.data_q7,
                        panel = TRUE,
                        est_method="dr")




CS_never_cond

# Now, compute event study
CS_es_never_cond <- aggte(CS_never_cond, type = "dynamic")

summary(CS_es_never_cond)
# Plot event study
fig_CS <- ggdid(CS_es_never_cond,
                title = "Figure 3. Event study of Medicaid expansion on hospital uncompensated care, using Callaway and Sant'Anna approach"  ,
                xlab     = "Year to enactment of Medicaid expansion",
                ylab     = "Event study estimates for uncompensated care")


fig_CS
#############################################

#Q8

#############################################
#https://rdrr.io/github/asheshrambachan/HonestDiD/man/createSensitivityPlot.html

# code for running honest_did
library(slam)
library(Rglpk)

hd_cs_smooth_never <- honest_did(CS_es_never_cond,type="smoothness")
#Error in { : task 1 failed - "attempt to apply non-function"

hd_cs_rm_never <- honest_did(CS_es_never_cond, type="relative_magnitude")
#Error in { : task 1 failed - "a dimension is zero"
hd_cs_rm_never
# Drop 0 as that is not really allowed.
hd_cs_rm_never$robust_ci <- hd_cs_rm_never$robust_ci[-1,]

## -----------------------------------------------------------------------------
# make sensitivity analysis plots
cs_HDiD_smootha <- createSensitivityPlot(hd_cs_smooth_never$robust_ci,
                                        hd_cs_smooth_never$orig_ci,
                                        rescaleFactor = 0)

cs_HDiD_smoothb <- createSensitivityPlot(hd_cs_smooth_never$robust_ci,
                                        hd_cs_smooth_never$orig_ci,
                                        rescaleFactor = 0.5)

cs_HDiD_smoothc <- createSensitivityPlot(hd_cs_smooth_never$robust_ci,
                                        hd_cs_smooth_never$orig_ci,
                                        rescaleFactor = 1)

cs_HDiD_smoothd <- createSensitivityPlot(hd_cs_smooth_never$robust_ci,
                                        hd_cs_smooth_never$orig_ci,
                                        rescaleFactor = 1.5)

cs_HDiD_smoothe <- createSensitivityPlot(hd_cs_smooth_never$robust_ci,
                                        hd_cs_smooth_never$orig_ci,
                                        rescaleFactor = 2)


cs_HDiD_relmag <- createSensitivityPlot_relativeMagnitudes(hd_cs_rm_never$robust_ci,
                                                           hd_cs_rm_never$orig_ci)

cs_HDiD_relmag
#############################################

#Q9

#############################################
#############################################

#Q10

#############################################
#In general, I feel doing data cleaning for POS data is a little bit time consuming. Understanding these variables also took a while. 