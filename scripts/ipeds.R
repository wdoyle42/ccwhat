################################################################################
##
## <PROJ> National Afforability Report: 50 States
## <FILE> ipeds.r
## <AUTH> Will Doyle and Benjamin Skinner
## <INIT> 2015-05-27
## <REV> 2021-04-08
################################################################################

## PURPOSE

## The purpose of this file is automate the process of:
##
## (1) downloading appropriate IPEDS survey data files
## (2) subsetting full datasets to desired variables
## (3) combining across datasets and years
## (4) output in tidy dataset

## CODE

## Code modified from <ipeds_combine.r>:

## https://gist.github.com/btskinner/f42c87507169d0ba773c

##Libraries 
library(tidyverse) 
library(noncensus)
library(here)

## load functions
source('functions.r')

rddir<-"../data/raw/"
addir<-"../data/cleaned/"
## =============================================================================
## BUILD DATASETS 
## =============================================================================

years<-2019

## IPEDS institutional characteristics (using HD files)

filenames<-paste0('HD',2019,'.zip')
var <- c('unitid','instnm','city','stabbr','control','sector','carnegie', 'ccipug','c15basic')
hd_df <- build.dataset.ipeds(filenames=filenames, datadir = rddir, vars = var,years=years)

## IPEDS enrollments (using EFIA files)

filenames <-paste0('EFIA',2019,'.zip')
var <- c('unitid','fteug')
efia_df <- build.dataset.ipeds(filenames=filenames, datadir = rddir, vars= var ,years=2019)


## Degrees awarded
filenames<-'C2019_C.zip'
var<-c('unitid','awlevelc','cstotlt')
comp_df<-build.dataset.ipeds(filenames=filenames, datadir = rddir, vars= var ,years=2019)
  
# ## AWlevel codes
#   3	Associate's degree
# 5	Bachelor's degree
# 7	Master's degree
# 9	Doctor's degree
# 10	Postbaccalaureate or Post-master's certificate
# 1	Award of less than 1 academic year
# 2	Award of at least 1 but less than 4 academic years
  
## =============================================================================
## MERGE DATASETS
## =============================================================================

pattern <- '*\\_df\\b'; byvar <- c('unitid', 'year')
inst <- merge.ipeds(pattern = pattern, byvar = byvar)

## =============================================================================
## Structure
## =============================================================================

## required library
library(tidyverse)

## merge in statadd full state name for state abbreviation
sl <- read.csv(paste0(mddir, 'statename.csv'))
inst <- merge(inst, sl, by = 'stabbr', all.x = TRUE)




## =============================================================================
## Some misc cleanup
## =============================================================================

##Drop if no undergrads

inst<-inst[inst$fteug>0,]

## drop cc of the airforce
inst <- inst[inst$unitid != 100636, ]

## Drop military academies

mil.ids<-c(128328,
           130624,
           197027,
           197036,
           164155,
           164155
)

inst<-filter(inst,!(unitid%in%mil.ids))


## REPORT CATEGORIES -----------------------------------------------------------
##
## 1 = Public 2 year (associate's dominant)
## 2 = Public Non-Doctoral (all other 4-years)
## 3= Public Doctoral (includes extensive and intensive) 4-year
## 4 = Private Non-Doctoral (as above)
## 5 = Private Doctoral (as above)
## -----------------------------------------------------------------------------

## Initialize grouping
inst$group <- NA

## All public four years
inst$group[inst$sector==1] <- 2

#Reassign public doctoral
inst$group[inst$group==2 & inst$carnegie %in% c(15:16)]<-3

## All private four years
inst$group[inst$sector==2]<-4

## Reassign private doctoral              

inst$group[inst$group==4 & inst$carnegie %in% c(15:16)]<-5

## Public 2 years

inst$group[inst$sector==4]<-1

## Reassign any asscoiate dominant to 2 years from 4 years

inst$group[inst$group==2 & inst$ccipug==2]<-1
inst$group[inst$group==2& inst$cc15basic==14]<-1

## Fix UM augusta

inst$group[inst$unitid==161217]<-2

## =============================================================================
## CLEAN
## =============================================================================

## drop if group is NA since it cannot be used
inst <- inst[!is.na(inst$group),]

## resort dataframe; order variables; reset rownames
inst <- inst[order(inst$unitid, inst$year, inst$group,inst$faminccat),]
inst<-inst[c("unitid","instnm","stabbr","statename","year","group","fteug","faminccat","netprice")]

##Drop rownames
rownames(inst) <- NULL

## =============================================================================
## OUTPUT FINAL DATASET AS .CSV
## =============================================================================

write.csv(inst, file = paste0(addir, 'institutions.csv'), row.names = FALSE)

## =============================================================================
## END
################################################################################

