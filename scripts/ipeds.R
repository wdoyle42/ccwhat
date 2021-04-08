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
var <- c('unitid','instnm','city','stabbr','control','sector','carnegie', 'ccipug','c15basic','obereg')
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
## Add full state names
## =============================================================================


## =============================================================================
## Some misc cleanup
## =============================================================================

inst<-inst%>%
  filter(fteug>0,   ##Drop if no undergrads
         sector!=0,  ## drop admin units 
         obereg!=0, ## drop military academies
         unitid != 100636 ) ## drop cc of the airforce
  

##Drop rownames
rownames(inst) <- NULL

## Select just publics

inst<-inst%>%
  filter(control==1)

## =============================================================================
## OUTPUT FINAL DATASET AS .CSV
## =============================================================================

write.csv(inst, file = paste0(addir, 'institutions.csv'), row.names = FALSE)

## =============================================================================
## END
################################################################################

