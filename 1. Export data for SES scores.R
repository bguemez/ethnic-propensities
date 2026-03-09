

# Load libraries 
library(dplyr)
library(ggplot2)
library(tidyr)
library(statar)
library(here)
library(broom)
library(statar)
library(broom.mixed)
library(panelr)
library(haven)
library(stringr)


setwd("C:/Users/braul/OneDrive - Duke University/Papers/Mobility_indig")


## Wave 1

hh1 <- read_dta("data/hh02dta_all/hh02dta_b2/ii_ah.dta") |> select(-ls)
ob1 <- read_dta("data/hh02dta_all/hh02dta_bc/c_cv.dta")
obs1 <- read_dta("data/hh02dta_all/hh02dta_bc/c_cvo.dta")
hh_w <- merge(hh1, ob1, by=c("folio")) 
hh_w <- merge(hh_w, obs1, by=c("folio")) 


## Household location
rur1 <- read_dta("data/hh02dta_all/hh02dta_bc/c_portad.dta") |> select(folio, estrato, loc, mpio)
hh_w <- merge(hh_w, rur1, by=c("folio")) 

# Weights
weights1 <- read_dta("data/hh02w_all/hh02w_b2.dta") |> mutate(folio=as.numeric(folio))

hh_w <- merge(hh_w, weights1, by=c("folio")) 
hhi <- hh_w |> select("folio", "factor_b2","ah03b",
                      "ah03c",
                      "ah03d",
                      "ah03e",
                      "ah03f",
                      "ah03g",
                      "ah03h",
                      "cvo06_1",
                      "cvo07_1",
                      "cvo05_1",
                      "cvo04", "cv08_1", "cv16", "cv07")

hhi <- hhi |> mutate(	
  ah03b=as.factor(	(if_else(	ah03b	==3  ,0,	ah03b))),
  ah03c=as.factor(	(if_else(	ah03c	==3  ,0,	ah03c))),
  ah03d=as.factor(	(if_else(	ah03d	==3  ,0,	ah03d))),
  ah03e=as.factor(	(if_else(	ah03e	==3  ,0,	ah03e))),
  ah03f=as.factor(	(if_else(	ah03f	==3  ,0,	ah03f))),
  ah03g=as.factor(	(if_else(	ah03g	==3  ,0,	ah03g))),
  ah03h=as.factor(	(if_else(	ah03h	==3  ,0,	ah03h))),
  cvo06_1=as.factor(	(if_else(	cvo06_1	==1 ,1,	0))),
  cvo07_1=as.factor(	(if_else(	cvo07_1	==1 | cvo07_1==2 ,1,	0))),
  cvo05_1=as.factor(	(if_else(	cvo05_1	==1 | cvo05_1	==2 ,1,	0))),
  cvo04= as.factor( (if_else(	cvo04	==1 ,1,	0))),
  cv08_1= as.factor( (if_else(	cv08_1	==1 | cv08_1==2 ,1,	0))),
  cv16= as.factor( (if_else(	cv16	==1 ,1,	0))),
  cv07= as.factor( (if_else(cv07>2,3,cv07))))

write.csv(hhi, file ="data/seswave_1.csv" )

rm("ob1","obs1","hh_w","rur1", "hh1", "hhi", "weights1") 




## Wave 2

hh1 <- read_dta("data/hh05dta_all/hh05dta_b2/ii_ah.dta") |> select(-ls)
ob1 <- read_dta("data/hh05dta_all/hh05dta_bc/c_cv.dta")
obs1 <- read_dta("data/hh05dta_all/hh05dta_bc/c_cvo.dta")


hh_w <- merge(hh1, ob1, by=c("folio")) 
hh_w <- merge(hh_w, obs1, by=c("folio")) 


## Household location
rur1 <- read_dta("data/hh05dta_all/hh05dta_bc/c_portad.dta") |> select(folio, estrato, loc, mpio)

hh_w <- merge(hh_w, rur1, by=c("folio")) 

hh_w <- hh_w |> mutate(foliow1=paste(substr(folio,1,6),"00", sep="")) ## I have to do this to not lose the people that move to a new home. This is because the longitudinal weights use the folio from wave 1 

## weights

weights2 <- read_dta("data/hh05lw_all/hh05_lw_b2.dta") 

weights2 <- weights2 |> rename(foliow1=folio)

hh_w <- merge(hh_w, weights2, by=c("foliow1")) 



hhi <- hh_w |> select("folio", "foliow1", "fac_2l","ah03b",
                      "ah03c",
                      "ah03d",
                      "ah03e",
                      "ah03f",
                      "ah03g",
                      "ah03h",
                      "cvo06_1",
                      "cvo07_1",
                      "cvo05_1",
                      "cvo04", "cv08_1", "cv16", "cv07")

hhi <- hhi |> mutate(	
  ah03b=as.factor(	(if_else(	ah03b	==3  ,0,	ah03b))),
  ah03c=as.factor(	(if_else(	ah03c	==3  ,0,	ah03c))),
  ah03d=as.factor(	(if_else(	ah03d	==3  ,0,	ah03d))),
  ah03e=as.factor(	(if_else(	ah03e	==3  ,0,	ah03e))),
  ah03f=as.factor(	(if_else(	ah03f	==3  ,0,	ah03f))),
  ah03g=as.factor(	(if_else(	ah03g	==3  ,0,	ah03g))),
  ah03h=as.factor(	(if_else(	ah03h	==3  ,0,	ah03h))),
  cvo06_1=as.factor(	(if_else(	cvo06_1	==1 ,1,	0))),
  cvo07_1=as.factor(	(if_else(	cvo07_1	==1 | cvo07_1==2 ,1,	0))),
  cvo05_1=as.factor(	(if_else(	cvo05_1	==1 | cvo05_1	==2 ,1,	0))),
  cvo04= as.factor( (if_else(	cvo04	==1 ,1,	0))),
  cv08_1= as.factor( (if_else(	cv08_1	==1 | cv08_1==2 ,1,	0))),
  cv16= as.factor( (if_else(	cv16	==1 ,1,	0))),
  cv07= as.factor( (if_else(cv07>2,3,cv07))))




write.csv(hhi, file ="data/seswave_2.csv" )





rm("ob1","obs1","hh_w","rur1", "hh1", "hhi", "weights2") 



## Wave 3


## Household expenditure  

hh1 <- read_dta("data/hh09dta_all/hh09dta_b2/ii_ah.dta") |> select(-ls)
ob1 <- read_dta("data/hh09dta_all/hh09dta_bc/c_cv.dta")
obs1 <- read_dta("data/hh09dta_all/hh09dta_bc/c_cvo.dta")


hh_w <- merge(hh1, ob1, by=c("folio")) 
hh_w <- merge(hh_w, obs1, by=c("folio")) 




## Household location
rur1 <- read_dta("data/hh09dta_all/hh09dta_bc/c_portad.dta") |> select(folio, estrato, loc, mpio)

hh_w <- merge(hh_w, rur1, by=c("folio")) 


hh_w <- hh_w |> mutate(foliow1=paste(substr(folio,1,6),"00", sep="")) ## I have to do this to not lose the people that move to a new home. This is because the longitudinal weights use the folio from wave 1 



weights3 <- read_dta("data/hh09lw_all/hh09_lw_b2.dta") 

weights3 <- weights3 |> rename(foliow1=folio)


hh_w <- merge(hh_w, weights3, by=c("foliow1")) 





hhi <- hh_w |> select("folio","ah03b", "fac_2l",
                      "ah03c",
                      "ah03d",
                      "ah03d1",
                      "ah03e",
                      "ah03f",
                      "ah03g",
                      "ah03h",
                      "cvo06_1",
                      "cvo07_1",
                      "cvo05_1",
                      "cvo04", "cv08_1", "cv16", "cv07")

hhi <- hhi |> mutate(	
  ah03b=as.factor(	(if_else(	ah03b	==3  ,0,	ah03b))),
  ah03c=as.factor(	(if_else(	ah03c	==3  ,0,	ah03c))),
  ah03d=as.factor(	(if_else(	ah03d	==3  ,0,	ah03d))),
  ah03d1=as.factor(	(if_else(	ah03d1	==3  ,0,	ah03d1))),
  ah03e=as.factor(	(if_else(	ah03e	==3  ,0,	ah03e))),
  ah03f=as.factor(	(if_else(	ah03f	==3  ,0,	ah03f))),
  ah03g=as.factor(	(if_else(	ah03g	==3  ,0,	ah03g))),
  ah03h=as.factor(	(if_else(	ah03h	==3  ,0,	ah03h))),
  cvo06_1=as.factor(	(if_else(	cvo06_1	==1 ,1,	0))),
  cvo07_1=as.factor(	(if_else(	cvo07_1	==1 | cvo07_1==2 ,1,	0))),
  cvo05_1=as.factor(	(if_else(	cvo05_1	==1 | cvo05_1	==2 ,1,	0))),
  cvo04= as.factor( (if_else(	cvo04	==1 ,1,	0))),
  cv08_1= as.factor( (if_else(	cv08_1	==1 | cv08_1==2 ,1,	0))),
  cv16= as.factor( (if_else(	cv16	==1 ,1,	0))),
  cv07= as.factor( (if_else(cv07>2,3,cv07))))



write.csv(hhi, file ="data/seswave_3.csv" )

