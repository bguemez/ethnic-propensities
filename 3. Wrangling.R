
# This R file merges individual-level and
# household-level data from the three waves of the survey

# Load libraries 

library(dplyr)
library(ggplot2)
library(tidyr)
library(statar)
library(here)
library(broom)
library(statar)
library(broom.mixed)
library(data.table)
library(panelr)
theme_set(theme_minimal())

## Set Working Directory 

setwd("C:/Users/braul/OneDrive - Duke University/Papers/Mobility_indig")


## Download all databases from http://www.ennvih-mxfls.org/english/ennvih-1.html
## and http://www.ennvih-mxfls.org/english/weights1.html 


## 1. Open data

library(haven)

  # First wave
  adults1 <- read_dta("data/hh02dta_all/hh02dta_b3a/iiia_ed.dta")
  dem1 <- read_dta("data/hh02dta_all/hh02dta_b3a/iiia_portad.dta") |> 
  select(folio, ls, edad,ent)
  adults1 <- merge(adults1, dem1, by=c("folio","ls"))
  
  adults1$ed03[adults1$ed03==3] <- 0

  ## Include sex
  
  dem2 <- read_dta("data/hh02dta_all/hh02dta_bc/c_ls.dta") |> 
    select(folio,ls, ls04) 
  
  adults1 <- merge(adults1, dem2, by=c("folio","ls"))
  

  # weights 
  weights2 <- read_dta("data/hh02w_all/hh02w_b3a.dta")
  weights2$folio <- as.numeric(weights2$folio) 

  ## Household location
  rur1 <- read_dta("data/hh02dta_all/hh02dta_bc/c_portad.dta") |>
    select(folio, estrato, loc, mpio)

  ## SES Index
  
  sesind <- read_dta("data/wave1_ses.dta")
  adults1 <- merge(adults1, sesind, by=c("folio"))
  
  ## Merge 
  adults1 <- merge(adults1, weights2, by=c("folio","ls"))
  adults1 <- merge(adults1, rur1, by=c("folio"))

  rm("weights2","dem1", "dem2", "rur1",  "sesind")


  adults1 <- adults1 |> rename(ed07=ed07_1)

## 2. Folio Wrangling

ap <- c("AP")

hh_ses <- c("zpca") 

identity <- c("ed03","ed04","edad","ent", "estrato", "loc", "mpio",
              "ed05","ed06","ed07", "ed12", "edad", "ls04")


burec <- c("folio","ls")

# Select relevant vars and Create folio var 
options("scipen"=10)

adults1 <- adults1 |> select(burec,identity, hh_ses, factor_b3a) |>
  mutate(id = case_when(ls<10 ~ paste("0",as.character(ls),sep=""),
                        ls>9 ~ as.character(ls)),
         pid_link=paste(sprintf("%08.0f", folio),sprintf("%02.0f", ls), sep = ""),
              pid_link2=paste(substr(pid_link,1,6), ap, "00", id, sep=""),
         wave_1=1, anio=2,
         duplicatedplink = if_else(duplicated(pid_link)==T, 1,0)) 


# Select and rename for the other two waves

adults1 <- adults1 |> rename(fac_ind=factor_b3a) |> filter(duplicatedplink==0)

        

# Second wave

adults2 <- read_dta("data/hh05dta_all/hh05dta_b3a/iiia_ed.dta")
dem1 <- read_dta("data/hh05dta_all/hh05dta_b3a/iiia_portad.dta") |>
select(folio, ls, edad, ent)
adults2 <- merge(adults2, dem1, by=c("folio","ls"))


dem2 <- read_dta("data/hh05dta_all/hh05dta_bc/c_ls.dta") |> 
  select(folio,ls, ls04,panel,ls01a) 
adults2 <- merge(adults2, dem2, by=c("folio","ls"))


adults2$ed03[adults2$ed03==3] <- 0


## Household location
rur2 <- read_dta("data/hh05dta_all/hh05dta_bc/c_portad.dta") |> select(folio, estrato, loc, mpio) 

adults2 <- merge(adults2, rur2, by=c("folio"))


## SES Index

sesind <- read_dta("data/wave2_ses.dta")
adults2 <- merge(adults2, sesind, by=c("folio"))


rm("dem2", "dem1",  "rur2", "sesind") 



adults2 <- adults2 |> rename(ed07=ed07_1)
adults2 <- adults2 |> select(burec,identity,hh_ses, "folio","foliow1","ls","pid_link", "panel", "ls01a") |> 
  mutate(wave_2=2) 


adults2 <- adults2 |> filter(panel==1)



adults_12 <- merge(adults1,adults2, by = "pid_link", suffixes = c("_1","_2"))  



# Third wave
adults3 <- read_dta("data/hh09dta_all/hh09dta_b3a/iiia_ed.dta") 
dem1 <- read_dta("data/hh09dta_all/hh09dta_b3a/iiia_portad.dta") |>
  select(folio, ls, edad, ent)
adults3 <- merge(adults3, dem1, by=c("folio","ls"))


dem2 <- read_dta("data/hh09dta_all/hh09dta_bc/c_ls.dta") |> 
  select(folio,ls, ls04) 
adults3 <- merge(adults3, dem2, by=c("folio","ls"))

adults3$ed03[adults3$ed03==3] <- 0

## Household location
rur3 <- read_dta("data/hh09dta_all/hh09dta_bc/c_portad.dta") |> select(folio, estrato, loc, mpio)

adults3 <- merge(adults3, rur3, by=c("folio"))

## SES Index

sesind <- read_dta("data/wave3_ses.dta")
adults3 <- merge(adults3, sesind, by=c("folio"))


adults3 <- adults3 |> rename(ed07=ed07_1) |>  
  select(burec,identity,  "ls","pid_link", hh_ses)  |>
  mutate(wave=3) 

rm( "dem1","dem2", "rur3", "sesind") 




 
## Only 2 and 3 



# Note: these are only the people from wave 2 that were also in wave 1 
adults2 <- adults2 |> ungroup() |> mutate(id = case_when(as.numeric(ls)<10 ~ paste("0",as.character(as.numeric(ls)),sep=""),
                                                         as.numeric(ls)>9 ~ as.character(as.numeric(ls))),
                                          pid_link=paste(substr(pid_link,1,6), ap, "00", id, sep=""))

adults_23 <- merge(adults2, adults3, by=c("pid_link"),  suffixes = c("_2","_3"))
adults_23 <- adults_23 |> rename(pid_link2=pid_link, wave_3=wave)


 
adults3 <- adults3 |> rename(pid_link2=pid_link )

## Only 1 and 3 

adults_13 <- merge(adults1, adults3, by = "pid_link2",  suffixes = c("_1","_3")) 

adults_13 <- adults_13 |> rename(wave_3=wave)


## Full panel 



colnames(adults3) <- paste(colnames(adults3),"_3",sep="")
adults3 <- adults3 |> rename(pid_link2=pid_link2_3 )

adults_123 <- merge(adults_12, adults3, by = "pid_link2") 


 


## Weights for wave 2 & wave 3


weights22 <- read_dta("data/hh05lw_all/hh05_lw_b3a.dta")
weights22$folio <- as.numeric(weights22$folio)
weights22$ls <- as.numeric(weights22$ls)

## I need to do this in order to match folio-ls from wave 1 with the weights from wave2 

weights22 <- weights22 |> rename(folio_1=folio)
weights22 <- weights22 |> rename(ls_1=ls)
weights22 <- weights22 |> rename(fac_ind_2=fac_3al)


adults_123 <- merge(adults_123,weights22, by = c("folio_1", "ls_1")) 



weights22 <- read_dta("data/hh05lw_all/hh05_lw_b3a.dta")

weights22 <- weights22 |> rename(folio_2=folio)
weights22 <- weights22 |> rename(ls_2=ls)
weights22 <- weights22 |> rename(fac_ind_2=fac_3al)


rm("weights22") 

## Add suffix to weights of wave1 
adults_123 <- adults_123 |> rename(fac_ind_1=fac_ind)

# Same with wave 3

weights33 <- read_dta("data/hh09lw_all/hh09_lw_b3a.dta")
                                       

weights33 <- weights33 |> rename(folio_3=folio)
weights33 <- weights33 |> rename(ls_3=ls)
weights33 <- weights33 |> rename(fac_ind_3=fac_3al)
weights33 <- weights33 |> mutate(pid_link2=paste(folio_3,ls_3,sep="")) |> select(-c(folio_3,ls_3))

adults_123 <- merge(adults_123,weights33, by = c("pid_link2")) 


rm(weights33)





### 4.2. Include ID number or each individual and its household

## Individual ID


d <- adults_123 |>
  mutate(id = factor(1:nrow(adults_123)))   # add individual ID number 


## weights 

meanfac1 <- mean(d$fac_ind_1,na.rm=T)
meanfac2 <- mean(d$fac_ind_2,na.rm=T)
meanfac3 <- mean(d$fac_ind_3, na.rm=T )

d <- d |> mutate(awt_1=fac_ind_1/meanfac1,
                 awt_2=fac_ind_2/meanfac2,
                 awt_3=fac_ind_3/meanfac3)



## Change values so 3 is zero 

d$ed03_1[d$ed03_1==3] <- 0
d$ed03_2[d$ed03_2==3] <- 0
d$ed03_3[d$ed03_3==3] <- 0

d$ed04_1[d$ed04_1==3] <- 0
d$ed04_2[d$ed04_2==3] <- 0
d$ed04_3[d$ed04_3==3] <- 0


d <- d |> mutate(path = paste(as.character(ed03_1),as.character(ed03_2),as.character(ed03_3) , sep = "") )


## rename folios

d <- d |> rename(folio_n=folio_1)
d <- d |> rename(folio_ns=folio_2)
d <- d |> rename(folio_s=folio_3)


rm( "adults_12","adults_123", "adults1", "adults2", "adults3", "adults_13", "adults_23") 



save(d, file='data/d_wide_ses.rda')

 

### 5. Switch to long format 

d <- long_panel(d,
                id = "id",
                periods = c(1,2,3),
                prefix = "_")

save(d, file='data/d_long_ses.rda')




## Demographic Variables 


## Sex variable

d <- d |> mutate(female=if_else(ls04==3,1,0))


## Education Variable 

d <- d |> ungroup() |> group_by(wave, id) |>  mutate(ed= case_when(
  ed06<=1 | ed05==3 ~ 0,
  ed06==2 | ed06==3 ~ 1,
  ed06==4 | ed06==5 ~ 2,
  ed06==6 | ed06==7 ~ 3,
  ed06>7 ~ 4)) |> ungroup()

## Estimating Years of education 

d <- d |> group_by(wave, id)  |> mutate(grade= case_when(
  ed06==3 & ed07>6 & ed07<90 ~ 6, 
  ed06==4 & ed07>3 & ed07<90 ~ 3, 
  ed06==6 & ed07>3 & ed07<90 ~ 3, 
  is.na(ed07)==F  ~ ed07,
  ed06==0 ~ 0)) |> ungroup()

d <- d |> group_by(wave, id) |>  mutate(anesc = case_when(
  ed05==3 | ed06<3 | (ed06==3 & grade==0) | (ed06==3 & grade==98) ~ 0, 
  ed06==3 & grade==1 ~ 1,
  ed06==3 & grade==2 ~ 2,
  ed06==3 & grade==3 ~ 3,
  ed06==3 & grade==4 ~ 4,
  ed06==3 & grade==5 ~ 5,
  ed06==3 & grade==6 | (ed06==4 & grade==0 ) ~ 6,
  (ed06==4 & grade==1) | ed06==5 | (ed06==4 & grade==98) ~ 7,
  (ed06==4 ) & grade==2 ~ 8,
  (ed06==4 ) & grade==3 | (ed06==6 & grade==0) ~ 9,
  (ed06==6 & grade==1 )| ed06==7 | (ed06==6 & grade==98) ~ 10,
  (ed06==6) & grade==2 ~ 11,
  (ed06==6) & grade==3 | (ed06==8 & grade==0 ) | (ed06==9 & grade==0) ~ 12,
  (ed06==8| ed06==9 | ed06==10) ~ 13)) |> ungroup()

## If there is a missing in wave  3, then I assume they had the same education from the previous wave 

d <- d |> group_by(id) |> mutate(anesc = if_else(wave==3 & is.na(anesc)==T, dplyr::lag(anesc), anesc),
                                 ed = if_else(wave==3 & is.na(ed)==T, dplyr::lag(ed), ed)) |> ungroup()

d <- d |> group_by(id) |>  mutate(anesc = if_else(wave==2 & is.na(anesc)==T, dplyr::lag(anesc), anesc),
                                  ed = if_else(wave==3 & is.na(ed)==T, dplyr::lag(ed), ed)) |> ungroup()


## If there is a missing in wave 2 , then I assume they had the same education from the previous  wave 

d <- d |> group_by(id) |> mutate(anesc = if_else(wave==2 & is.na(anesc)==T, dplyr::lag(anesc), anesc),
                                 ed = if_else(wave==2 & is.na(ed)==T, dplyr::lag(ed), ed)) |> ungroup()

d <- d |> group_by(id) |>  mutate(anesc = if_else(wave==2 & is.na(anesc)==T, dplyr::lag(anesc), anesc),
                                  ed = if_else(wave==2 & is.na(ed)==T, dplyr::lag(ed), ed)) |> ungroup()


## If there is a missing in wave 1 , then I assume they had the same education from the next  wave 

d <- d |> group_by(id) |> mutate(anesc = if_else(wave==1 & is.na(anesc)==T, dplyr::lead(anesc), anesc),
                                 ed = if_else(wave==1 & is.na(ed)==T, dplyr::lead(ed), ed)) |> ungroup()




## Let's see if this worked looking at changes 

d <- d  |> group_by(id) |> 
  mutate(d_anesc= anesc-dplyr::lag(anesc), d_ed= ed-dplyr::lag(ed)) |> ungroup()

## Since you can't decrease your education, I'll set all negative d_anesc to 0 

d <- d |> group_by(id) |> mutate(d_anesc = if_else(d_anesc<0, 0, d_anesc)) |> ungroup()
d <- d |> group_by(id) |> mutate(d_anesc = if_else(is.na(d_anesc)==T, 0, d_anesc)) |> ungroup()


d <- d |> group_by(id) |> mutate(d_anescum = cumsum(d_anesc) ) |> ungroup()


########### Differences in socioeconomic status
library(tidyverse)


## Create Terms 
d <-  d |>  group_by(id) |> mutate(m_zpca= weighted.mean(zpca, fac_ind),
                                   m_anesc= weighted.mean(anesc, fac_ind)) |>
  ungroup() 





# SES on wave 1
d <- d  |> group_by(id) |> 
  mutate(difses = zpca-m_zpca,
         difed = anesc - m_anesc)  |>  # Create first-difference scores for all variables
  mutate(p_ses = difses*(difses>0), n_ses = -difses*(difses<0),
         p_ed = difed*(difed>0)) |> 
  mutate(p_ses = ifelse(is.na(p_ses), 0, p_ses),
         p_ed = ifelse(is.na(p_ed), 0, p_ed),
         n_ses = ifelse(is.na(n_ses), 0, n_ses),
         difses = ifelse(is.na(difses), 0, difses)) |>  
  mutate(pses_cum = cumsum(p_ses), nses_cum = cumsum(n_ses)) 




#### Indigenous municipalities 

mun_ind <- read_dta("data/census2010_estimates/perind_2010.dta")


## rename mpio from the original base
d_l <- d
d_l <- d_l |> rename(mun=mpio) 

## Merge using mun and ent

d_l <- left_join(d_l, mun_ind, by=c("ent","mun"))


d_l <- d_l |>   
  rename(perind21 = perind)


## Recodes of indigenous classification 

d_l <- d_l |> mutate(ind=if_else(ed03==1,1,0),
                     speak=case_when(is.na(ed04)==T & ed03==0 ~ 0,
                                     ed04==1 & ed03==1 ~ 1,
                                     ed04==0 & ed03==1 ~ 0)) 

## Indigenous Speaker 


d_l <- d_l |> group_by(id) |> mutate(speak1=if_else(speak[wave==1 | wave==2 | wave==3]==1,1,0),
                                     speaksum=sum(speak1, na.rm=T),
                                     speak1=if_else(speaksum>0,1,0)) |> ungroup()  

d_l <- d_l |> rename(speak_or=speak) |> rename(speak=speak1)


## Age on wave 1

d_l <-  d_l |> group_by(id) |> mutate(edadw1s = if_else(wave==1,edad,0),
                                      edadw1 = sum(edadw1s))

# region

d_l <- d_l |> mutate(region5=case_when(
  ent== 1 ~ 3,
  ent== 2 ~ 1,
  ent== 3 ~ 2,
  ent== 4 ~ 5,
  ent== 5 ~ 1,
  ent== 6 ~ 3,
  ent== 7 ~ 5,
  ent== 8 ~ 1,
  ent== 9 ~ 4,
  ent== 10 ~ 2,
  ent== 11 ~ 4,
  ent== 12 ~ 5,
  ent== 13 ~ 4,
  ent== 14~ 3,
  ent== 15 ~ 4,
  ent== 16 ~ 3,
  ent== 17 ~ 4,
  ent== 18~ 2,
  ent== 19~ 1,
  ent== 20 ~ 5,
  ent== 21 ~ 4,
  ent== 22 ~ 4,
  ent== 23 ~ 5,
  ent== 24 ~ 3,
  ent== 25 ~ 2,
  ent== 26 ~ 1,
  ent== 27 ~ 5,
  ent== 28 ~ 1,
  ent== 29 ~ 4,
  ent== 30 ~ 5,
  ent== 31 ~ 5,
  ent== 32 ~ 2), 
  south = if_else(region5==5,1,0))

## Region (version of three)

# Create reg_r variable indicating regions
d_l$region <- ifelse(d_l$region5 == 5, 1, 
                     ifelse(d_l$region5 %in% c(3, 4), 2, 
                            ifelse(d_l$region5 <= 2, 3, NA)))

# Recode reg_r levels
d_l$region <- factor(d_l$region, 
                     levels = c(1, 2, 3), labels = c("south", "center", "north"))



## Last recodes 

## sum of waves (if swave==6, then we have information about respondent for the 3 waves)
d_l <- d_l |> group_by(id) |> mutate(swave=sum(wave))


## Define variables of Times a respondent self-ascribed as indigenous 
d_l <- d_l |> mutate(times_ind = case_when(
  path=="000"  ~ 0, 
  path =="001" | path =="010" | path =="100"  ~ 1,
  path =="011" | path =="110" | path =="101"  ~ 2,
  path == "111" ~ 3)) 





d_l$wave <- factor(d_l$wave)


write_dta(d_l, "data/d_l_ses.dta")


