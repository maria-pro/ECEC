library(haven)
library(tidyverse)

#files: B6, B8, B10

b6<-haven::read_sav("/Volumes/TOSHIBA EXT/LSAC/9.1_C2 General Release/Survey data/SPSS/lsacgrb6.sav")
b8<-haven::read_sav("/Volumes/TOSHIBA EXT/LSAC/9.1_C2 General Release/Survey data/SPSS/lsacgrb8.sav")
b10<-haven::read_sav("/Volumes/TOSHIBA EXT/LSAC/9.1_C2 General Release/Survey data/SPSS/lsacgrb10.sav")



#literacy
#dtlitr	(tlitr no age): Learning and cognition outcomes	Numeracy	Academic Rating Scale - Language and Literacy	Rasch modelled score from dlc09a1 to 10
b6$dppvt

b6$dtlitr

#numeracy

b6$dtmathr
b6$dmatreas

b6_outcome<-b6%>%select(hicid,
                        dppvt, dtlitr,
                        dtmathr,dmatreas )
# Create a z-score for each chosen outcome measure such that good outcomes are represented by positive scores, and poor outcomes by negative scores.
b6_outcome<-b6_outcome%>%
  mutate(
    dppvt_z=(dppvt - mean(dppvt, na.rm=TRUE))/sd(dppvt, na.rm=TRUE),
    dtlitr_z=(dtlitr - mean(dtlitr, na.rm=TRUE))/sd(dtlitr, na.rm=TRUE),
    dtmathr_z=(dtmathr - mean(dtmathr, na.rm=TRUE))/sd(dtmathr, na.rm=TRUE),
    dmatreas_z=(dmatreas - mean(dmatreas, na.rm=TRUE))/sd(dmatreas, na.rm=TRUE)
    )
#Compute a score for each subdomain by taking the mean of these standardised scores 
#within each subdomain and 
#standardise this to a z-score adjusting for missing data when necessary (see Appendix B for more details on handling missing data).  

b6_outcome<-b6_outcome%>%
  select(hicid,
         dppvt_z, dtlitr_z, #"literacy
         dtmathr_z, dmatreas_z #numeracy
         )%>%
  mutate(
    literacy=(dppvt_z+dtlitr_z)/2,
    numeracy=(dtmathr_z+dmatreas_z)/2,
    academic=(dppvt_z+dtlitr_z+dtmathr_z+dmatreas_z)/4,
  )
  
  
  
  #pivot_longer(
#  cols=c("dppvt_z", "dtlitr_z"),
#  names_to="literacy_var",
#  values_to="literacy_score"
#)%>%
#  pivot_longer(
#    cols=c("dtmathr_z", "dmatreas_z"),
#    names_to="numeracy_var",
#    values_to="numeracy_score"
#  )
  
#Compute an index for each domain by obtaining the mean of the standardised scores within each domain and standardise to have a mean of 100 and a standard deviation of 10, again adjusting for missing data when necessary.

b6_outcome<-b6_outcome%>%
  mutate(
    literacy_index=scale(literacy)*10+100,
    numeracy_index=scale(numeracy)*10+100,
    academic_index=scale(academic)*10+100
  )

mean(b6_outcome$literacy_index, na.rm=TRUE)

mean(b6_outcome$numeracy_index, na.rm=TRUE)
mean(b6_outcome$academic_index, na.rm=TRUE)

summary(b6_outcome)

#Create cut-off variables that identify the 15% of children with lowest performance and the 15% of children performing most highly in each domain (roughly representing the number of children who have scores below and above the sample mean by one standard deviation).



n <- 15
b6_outcome<-b6_outcome%>%
  mutate(
    numeracy_score=case_when(
      numeracy_index > quantile(numeracy_index, prob = 1 - n/100, na.rm=TRUE) ~"positive",
      numeracy_index < quantile(numeracy_index, prob = 1 - n/100, na.rm=TRUE) ~"negative",
      TRUE~"expected"
          ),
    
    literacy_score=case_when(
      literacy_index > quantile(literacy_index, prob = 1 - n/100, na.rm=TRUE) ~"positive",
      literacy_index < quantile(literacy_index, prob = 1 - n/100, na.rm=TRUE) ~"negative",
      TRUE~"expected"
    ),
    academic_score=case_when(
      academic_index > quantile(academic_index, prob = 1 - n/100, na.rm=TRUE) ~"positive",
      academic_index < quantile(academic_index, prob = 1 - n/100, na.rm=TRUE) ~"negative",
      TRUE~"expected"
    )
  )

b6<-left_join(b6, b6_outcome)

b6%>%write_rds("b6_outcomes.rds")
b6%>%write_sav("b6_outcomes.sav")

#---------
#b8

#literacy
#etlitcr	(tlitr no age): Learning and cognition outcomes	Numeracy	Academic Rating Scale - Language and Literacy	Rasch modelled score from dlc09a1 to 10
b8$eppvt

b8$etlitcr

#numeracy

b8$etmathcr
b8$ematreas

b8_outcome<-b8%>%select(hicid,
                        eppvt, etlitcr,
                        etmathcr,ematreas )
# Create a z-score for each chosen outcome measure such that good outcomes are represented by positive scores, and poor outcomes by negative scores.
b8_outcome<-b8_outcome%>%
  mutate(
    eppvt_z=(eppvt - mean(eppvt, na.rm=TRUE))/sd(eppvt, na.rm=TRUE),
    etlitcr_z=(etlitcr - mean(etlitcr, na.rm=TRUE))/sd(etlitcr, na.rm=TRUE),
    etmathcr_z=(etmathcr - mean(etmathcr, na.rm=TRUE))/sd(etmathcr, na.rm=TRUE),
    ematreas_z=(ematreas - mean(ematreas, na.rm=TRUE))/sd(ematreas, na.rm=TRUE)
  )
#Compute a score for each subdomain by taking the mean of these standardised scores 
#within each subdomain and 
#standardise this to a z-score adjusting for missing data when necessary (see Appendix B for more details on handling missing data).  

b8_outcome<-b8_outcome%>%
  select(hicid,
         eppvt_z, etlitcr_z, #"literacy
         etmathcr_z, ematreas_z #numeracy
  )%>%
  mutate(
    literacy=(eppvt_z+etlitcr_z)/2,
    numeracy=(etmathcr_z+ematreas_z)/2,
    academic=(eppvt_z+etlitcr_z+etmathcr_z+ematreas_z)/4,
  )



#pivot_longer(
#  cols=c("eppvt_z", "etlitcr_z"),
#  names_to="literacy_var",
#  values_to="literacy_score"
#)%>%
#  pivot_longer(
#    cols=c("etmathcr_z", "ematreas_z"),
#    names_to="numeracy_var",
#    values_to="numeracy_score"
#  )

#Compute an index for each domain by obtaining the mean of the standardised scores within each domain and standardise to have a mean of 100 and a standard deviation of 10, again adjusting for missing data when necessary.

b8_outcome<-b8_outcome%>%
  mutate(
    literacy_index=scale(literacy)*10+100,
    numeracy_index=scale(numeracy)*10+100,
    academic_index=scale(academic)*10+100
  )

mean(b8_outcome$literacy_index, na.rm=TRUE)

mean(b8_outcome$numeracy_index, na.rm=TRUE)
mean(b8_outcome$academic_index, na.rm=TRUE)

summary(b8_outcome)

#Create cut-off variables that identify the 15% of children with lowest performance and the 15% of children performing most highly in each domain (roughly representing the number of children who have scores below and above the sample mean by one standard deviation).



n <- 15
b8_outcome<-b8_outcome%>%
  mutate(
    numeracy_score=case_when(
      numeracy_index > quantile(numeracy_index, prob = 1 - n/100, na.rm=TRUE) ~"positive",
      numeracy_index < quantile(numeracy_index, prob = 1 - n/100, na.rm=TRUE) ~"negative",
      TRUE~"expected"
    ),
    
    literacy_score=case_when(
      literacy_index > quantile(literacy_index, prob = 1 - n/100, na.rm=TRUE) ~"positive",
      literacy_index < quantile(literacy_index, prob = 1 - n/100, na.rm=TRUE) ~"negative",
      TRUE~"expected"
    ),
    academic_score=case_when(
      academic_index > quantile(academic_index, prob = 1 - n/100, na.rm=TRUE) ~"positive",
      academic_index < quantile(academic_index, prob = 1 - n/100, na.rm=TRUE) ~"negative",
      TRUE~"expected"
    )
  )

b8<-left_join(b8, b8_outcome)

b8%>%write_rds("b8_outcomes.rds")
b8%>%write_sav("b8_outcomes.sav")