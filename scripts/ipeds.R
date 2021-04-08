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
library(here)

## load functions
source('functions.r')

## data dirs
#cddir <- '../../data/acs/'
rddir <- './'
mddir <- '../../data/misc/'
addir <- '../../data/analysis/'

## =============================================================================
## BUILD DATASETS 
## =============================================================================

years<-2008:2016

## IPEDS institutional characteristics (using HD files)

filenames<-paste0('HD',2008:2016,'.zip')
var <- c('unitid','instnm','city','stabbr','control','sector','carnegie', 'ccipug','c15basic')
attr.data <- build.dataset.ipeds(filenames=filenames, datadir = rddir, vars = var,years=years)

## IPEDS enrollments (using EFIA files)

filenames <-paste0('EFIA',2009:2017,'.zip')
var <- c('unitid','fteug')
enroll.data <- build.dataset.ipeds(filenames=filenames, datadir = rddir, vars= var ,years=years)

## IPEDS student netprice (using SFA files)

filenames<-paste0(c("SFA0809",
             "SFA0910",
             "SFA1011",
             "SFA1112",
             "SFA1213",
             "SFA1314",
             "SFA1415",
             "SFA1516",
             "SFA1617"),
                  ".zip"
                  )
var <- c('unitid','npis412','npis422','npis432','npis442','npis452',
         'npt412','npt422','npt432','npt442','npt452')

netprice.data <- build.dataset.ipeds(filenames=filenames, datadir = rddir,
                                     vars = var,years=years)


## =============================================================================
## MERGE DATASETS
## =============================================================================

pattern <- '*\\.data\\b'; byvar <- c('unitid', 'year')
inst <- merge.ipeds(pattern = pattern, byvar = byvar)

## your file
#inst_names<-mydf$inst_names

## Filter this dataset to just be the ones you're interested in
#inst<-inst%>%filter(instnm%in%inst_names)

## =============================================================================
## MAKE TIDY
## =============================================================================

## required library
library(tidyverse)

## vars that stay as is
id <- c(unitid,year)
#,'instnm','city','stabbr','control','sector','carnegie','fteug','ccipug','c15basic')

## vars that need reshaping
measure <- c('npis412','npis422','npis432','npis442','npis452',
             'npt412','npt422','npt432','npt442','npt452')

## gather
inst %>%select(unitid,year,instnm)%>%
  tidyr::gather(key= c("unitid","year","instnm"),
         value  = c("npis412","npis422")) %>%
  as_tibble()->inst2

## convert melted variable to character
inst$variable <- as.character(inst$variable)

## change column names to something more useful
names(inst)[names(inst) == 'variable'] <- 'faminccat'
names(inst)[names(inst) == 'value'] <- 'netprice'

## drop if control == -3 (since we cannot use it)
inst <- inst[!(inst$control == -3),]

## drop row when faminccat == npis if private; npt if public
inst <- inst[!(grepl('npis', inst$faminccat) & inst$control != 1),]
inst <- inst[!(grepl('npt', inst$faminccat) & inst$control == 1),]
    
## change npis/npt categories to useful values
levels <- c('< 30k','30k to 48k','48k to 75k','75k to 110k','> 110k')

inst$faminccat[inst$faminccat == 'npis412'] <- levels[1]
inst$faminccat[inst$faminccat == 'npis422'] <- levels[2]
inst$faminccat[inst$faminccat == 'npis432'] <- levels[3]
inst$faminccat[inst$faminccat == 'npis442'] <- levels[4]
inst$faminccat[inst$faminccat == 'npis452'] <- levels[5]

inst$faminccat[inst$faminccat == 'npt412'] <- levels[1]
inst$faminccat[inst$faminccat == 'npt422'] <- levels[2]
inst$faminccat[inst$faminccat == 'npt432'] <- levels[3]
inst$faminccat[inst$faminccat == 'npt442'] <- levels[4]
inst$faminccat[inst$faminccat == 'npt452'] <- levels[5]

## convert back to factor
inst$faminccat <- factor(inst$faminccat, levels = levels)

## =============================================================================
## ADD VARS
## =============================================================================

## merge in statadd full state name for state abbreviation
sl <- read.csv(paste0(mddir, 'statename.csv'))
inst <- merge(inst, sl, by = 'stabbr', all.x = TRUE)

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

## =============================================================================
## OUTPUT FINAL DATASET AS .CSV
## =============================================================================

write.csv(inst, file = paste0(addir, 'institutions.csv'), row.names = FALSE)

## =============================================================================
## END
################################################################################

