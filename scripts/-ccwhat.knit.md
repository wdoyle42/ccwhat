---
title: "What is a Commuity College?"
author: "Will Doyle"
date: "4/8/2021"
output: github_document
---


```r
library(tidyverse)
```

```
## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.0 ──
```

```
## ✓ ggplot2 3.3.3     ✓ purrr   0.3.4
## ✓ tibble  3.1.0     ✓ dplyr   1.0.4
## ✓ tidyr   1.1.2     ✓ stringr 1.4.0
## ✓ readr   1.4.0     ✓ forcats 0.5.1
```

```
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
library(here)
```

```
## here() starts at /Users/doylewr/ccwhat
```



```r
inst<-read_csv(here("data/cleaned/institutions.csv"))
```

```
## 
## ── Column specification ────────────────────────────────────────────────────────
## cols(
##   .default = col_double(),
##   instnm = col_character(),
##   city = col_character(),
##   stabbr = col_character(),
##   name = col_character(),
##   region = col_character(),
##   division = col_character(),
##   capital = col_character()
## )
## ℹ Use `spec()` for the full column specifications.
```


## What Is a Community College?

In an insightful  (New America piece)[https://www.newamerica.org/education-policy/edcentral/a-different-approach-to-free-community-college/], Kevin Carey points out that a free community college plan depends crucially on the definition of community college.  Carey is right to point out is that we don't have a single definition of a community college. Many states don't have any colleges called "community colleges", and some states don't have any institutions that fulfill the classic missions of community colleges. 

Carey's piece includes a good breakdown of the ratio of associate's degrees to bachelor's degrees awarded by state. I was curious what percent and number of students in each state might be counted as "community college" students using different possible definitions of "community college."




```r
inst<-inst%>%
  ## create four different definitions of what a "cc is"
  ## IPEDS:sector, already defined
  ## Has CC in name
  mutate(`CC in Name`=ifelse(
    str_detect(instnm,"Community College") & control==1,1,0))%>%
  ## Carnegie Classification
  mutate(`Carnegie Associates Inst`=
           ifelse(c18ipug%in%c(1:3,5) &control==1,1,0)) %>%
  ## Less  than .5 of degrees are bachelors 
  mutate(all_degrees=Bachelors  +
         Masters +
         PhD +  
         `Cert> 1`+
         Postbac+ 
         Associates+
         `Cert<1`  )%>%
  mutate(prop_bach=Bachelors/all_degrees)%>%
  mutate(`Non-Bachelors`=ifelse(prop_bach<.5&control==1,1,0))%>%
  rename(State=name)
```



```r
## IPEDS Dataset
inst_ipeds<-inst%>%
  group_by(State)%>%
  mutate(`FTE in State`=sum(fteug))%>%
  group_by(State,sector)%>%
  summarize(`FTE in IPEDS Sector`=sum(fteug),
            `Total FTE`=mean(`FTE in State`))%>%
  mutate(`Percent in IPEDS Sector`=(`FTE in IPEDS Sector`/`Total FTE`)*100)%>%
  filter(sector==4)%>%
  select(State,`FTE in IPEDS Sector`,`Percent in IPEDS Sector`)
```

```
## `summarise()` has grouped output by 'State'. You can override using the `.groups` argument.
```


```r
## Carnegie
inst_carnegie<-inst%>%
  group_by(State)%>%
  mutate(`FTE in State`=sum(fteug))%>%
  group_by(State,`Carnegie Associates Inst`)%>%
  summarize(`FTE in Carnegie Associates`=sum(fteug),
            `Total FTE`=mean(`FTE in State`))%>%
  mutate(`Percent in Carnegie Associates`=
           (`FTE in Carnegie Associates`/`Total FTE`)*100)%>%
  filter(`Carnegie Associates Inst`==1)%>%
  select(State,`FTE in Carnegie Associates`,`Percent in Carnegie Associates`)
```

```
## `summarise()` has grouped output by 'State'. You can override using the `.groups` argument.
```



```r
## CC in Name
inst_cc_name<-inst%>%
  group_by(State)%>%
  mutate(`FTE in State`=sum(fteug))%>%
  group_by(State,`CC in Name`)%>%
  summarize(`FTE in Named CCs`=sum(fteug),
            `Total FTE`=mean(`FTE in State`))%>%
  mutate(`Percent in Named CCs`=
           (`FTE in Named CCs`/`Total FTE`)*100)%>%
  filter(`CC in Name`==1)%>%
  select(State,`FTE in Named CCs`,`Percent in Named CCs`)
```

```
## `summarise()` has grouped output by 'State'. You can override using the `.groups` argument.
```



```r
##  Non-Bachelors
inst_not_bach<-inst%>%
  group_by(State)%>%
  mutate(`FTE in State`=sum(fteug))%>%
  group_by(State,`Non-Bachelors`)%>%
  summarize(`FTE in Primarily Non-Bachelors`=sum(fteug),
            `Total FTE`=mean(`FTE in State`))%>%
  mutate(`Percent in Primarily Non-Bachelors`=
           (`FTE in Primarily Non-Bachelors`/`Total FTE`)*100)%>%
  filter(`Non-Bachelors`==1)%>%
  select(State,
         `FTE in Primarily Non-Bachelors`,
         `Percent in Primarily Non-Bachelors`,
         `Total FTE`)
```

```
## `summarise()` has grouped output by 'State'. You can override using the `.groups` argument.
```


```r
df<-inst_ipeds%>%
  full_join(inst_carnegie,by="State")%>%
  full_join(inst_cc_name,by="State")%>%
  full_join(inst_not_bach)%>%
  mutate(
    across(everything(), ~replace_na(.x, 0))
  )
```

```
## Joining, by = "State"
```



## IPEDS definitions

Colleges and universities report data about themselves to the federal government through the Integrated Postsecondary Education Data System (IPEDS). IPEDS classifies institutions that award even 1 bachelor's degree per year as 4-year institutions. This misses a large number of institutions that either are community colleges or fulfill the roles of community colleges in their state. Here's the percent of students in each state that attend "community colleges" if the definition is public two year institutions that don't award any bachelor's degrees:

## Percent of Students Enrolled in Public Two-Year Colleges, by States

```r
df%>%
  ggplot(aes(x=fct_reorder(State,`Percent in IPEDS Sector`),
                           y=`Percent in IPEDS Sector`))+
  theme_minimal()+
  geom_point(color="blue",alpha=.5)+
  coord_flip()+
  xlab("State")+
  ylab("Percent in  Sector")
```

![](-ccwhat_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

This matchesup with this (table)[https://nces.ed.gov/programs/digest/d19/tables/dt19_304.80.asp] published by NCES in 2020. There are some surprises here: Florida, a national leader in the development of Community  Colleges, is reported as having about three percent of its enrolllment in public two year colleges. Delaware and Alaska do not haveany students enrolled in institutions classified as public two-year institutions.  

## Carnegie  Classification-- Instructional Program: Associates Institutions

The Carnegie Classification includes a definition of istructional programs that doesn't have a "Community College" definition but does include Associates Institutions of different types. The graph below shows the percent of students enrolled in all types of Associates institutions compared with the percent enrolled in public two-year colleges as defined by NCES.


```r
df%>%
  ggplot(aes(x=fct_reorder(State,`Percent in IPEDS Sector`),
                           y=`Percent in IPEDS Sector`))+
  theme_minimal()+
  geom_point(color="blue",alpha=.5)+
  geom_point(data=df,
             aes(x=fct_reorder(State,`Percent in IPEDS Sector`),
                           y=`Percent in Carnegie Associates`),color="red")+
  coord_flip()+
  xlab("State")
```

![](-ccwhat_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

Using the Carnegie definition there are considerably higher enrollments in Associates institutions as opposed to public two-year institutions. The biggest gains would come in the biggest states: Florida adds 280,000 students and California adds 144,000 students, but much smaller Washington adds 120,000 students. Overall, using the Carnegie Associates definition to designate an institution a "community college"  adds 916,000 students as opposed to using public two-year college.  



```r
df%>%
  mutate(diff=`FTE in IPEDS Sector`- `FTE in Carnegie Associates`)%>%
  arrange(diff)%>%
  select(State, diff)%>%
  print(n=50)
```

```
## # A tibble: 50 x 2
## # Groups:   State [50]
##    State             diff
##    <chr>            <dbl>
##  1 Florida        -278972
##  2 California     -144032
##  3 Washington     -121432
##  4 Texas           -95458
##  5 Ohio            -46814
##  6 Colorado        -43590
##  7 New York        -33964
##  8 Georgia         -33093
##  9 Nevada          -31134
## 10 Michigan        -24029
## 11 Utah            -10306
## 12 Delaware         -8886
## 13 Wisconsin        -8535
## 14 Indiana          -8367
## 15 South Carolina   -7473
## 16 Pennsylvania     -5236
## 17 Idaho            -4021
## 18 North Dakota     -2882
## 19 West Virginia    -2471
## 20 Hawaii           -1615
## 21 New Mexico       -1377
## 22 Vermont          -1295
## 23 South Dakota     -1078
## 24 Arizona           -627
## 25 Montana            -24
## 26 Arkansas             0
## 27 Illinois             0
## 28 Iowa                 0
## 29 Kansas               0
## 30 Kentucky             0
## 31 Maine                0
## 32 Maryland             0
## 33 Massachusetts        0
## 34 Mississippi          0
## 35 New Hampshire        0
## 36 New Jersey           0
## 37 North Carolina       0
## 38 Oregon               0
## 39 Rhode Island         0
## 40 Virginia             0
## 41 Wyoming              0
## 42 Alaska               0
## 43 Connecticut        112
## 44 Missouri           466
## 45 Nebraska           496
## 46 Alabama            542
## 47 Minnesota         1165
## 48 Oklahoma          2835
## 49 Louisiana         4753
## 50 Tennessee        11584
```

```r
df%>%ungroup()%>%
  mutate(diff=`FTE in IPEDS Sector`- `FTE in Carnegie Associates`)%>%
  filter(diff<0)%>%
  summarize(sum(diff))
```

```
## # A tibble: 1 x 1
##   `sum(diff)`
##         <dbl>
## 1     -916711
```


## Institutions with Community College in the Name

The simplest way to define an institution as a community college may be to use its name. The graph below compares enrollment in public two-year institutions with enrollment in institutions that include "community college" in their name. 

```r
df%>%
  ggplot(aes(x=fct_reorder(State,`Percent in IPEDS Sector`),
                           y=`Percent in IPEDS Sector`))+
  theme_minimal()+
  geom_point(color="blue",alpha=.5)+
  geom_point(data=df,
             aes(x=fct_reorder(State,`Percent in IPEDS Sector`),
                           y=`Percent in Named CCs`),color="orange")+
  coord_flip()+
  xlab("State")+
  ylab("Percent in Sector")
```

![](-ccwhat_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->
Most states have considerably more 



```r
df%>%
  mutate(diff=`FTE in IPEDS Sector`- `Percent in Named CCs`)%>%
  arrange(diff)%>%
  select(State, diff)%>%
  print(n=50)
```

```
## # A tibble: 50 x 2
## # Groups:   State [50]
##    State               diff
##    <chr>              <dbl>
##  1 Delaware          -21.3 
##  2 Nevada             -7.43
##  3 Alaska              0   
##  4 Vermont          2644.  
##  5 North Dakota     4701.  
##  6 Montana          5243.  
##  7 South Dakota     6035   
##  8 New Hampshire    9005.  
##  9 Rhode Island     9318.  
## 10 Maine            9751.  
## 11 West Virginia   10046.  
## 12 Idaho           10812   
## 13 Wyoming         11670.  
## 14 Hawaii          13172.  
## 15 Colorado        13802.  
## 16 Utah            17563.  
## 17 Washington      23837.  
## 18 Nebraska        25996.  
## 19 Connecticut     28487.  
## 20 Arkansas        30860.  
## 21 New Mexico      37715.  
## 22 Florida         38481.  
## 23 Oklahoma        42651.  
## 24 Louisiana       43081.  
## 25 Kentucky        44368.  
## 26 Wisconsin       48372   
## 27 South Carolina  50099.  
## 28 Kansas          51471.  
## 29 Massachusetts   52539.  
## 30 Indiana         55514.  
## 31 Iowa            57267.  
## 32 Missouri        57411.  
## 33 Oregon          57846.  
## 34 Alabama         59670.  
## 35 Mississippi     60869.  
## 36 Tennessee       68455.  
## 37 Maryland        70798.  
## 38 Minnesota       76375.  
## 39 Pennsylvania    80020.  
## 40 Georgia         80161   
## 41 Ohio            85896.  
## 42 Michigan        89405.  
## 43 New Jersey      99889.  
## 44 Virginia       100114.  
## 45 Arizona        107921.  
## 46 North Carolina 160554.  
## 47 New York       187442.  
## 48 Illinois       192113.  
## 49 Texas          381211.  
## 50 California     769780.
```

```r
df%>%ungroup()%>%
  mutate(diff=`FTE in IPEDS Sector`- `Percent in Named CCs`)%>%
  filter(diff>0)%>%
  summarize(sum(diff,na.rm=TRUE))
```

```
## # A tibble: 1 x 1
##   `sum(diff, na.rm = TRUE)`
##                       <dbl>
## 1                  3540429.
```




```r
df%>%
  ggplot(aes(x=fct_reorder(State,`Percent in IPEDS Sector`),
                           y=`Percent in IPEDS Sector`))+
  theme_minimal()+
  geom_point(color="blue",alpha=.5)+
  geom_point(data=df,
             aes(x=fct_reorder(State,`Percent in IPEDS Sector`),
                           y=`Percent in Primarily Non-Bachelors`),color="orange")+
  coord_flip()+
  xlab("State")+
  ylab("Percent  in Sector")
```

![](-ccwhat_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->




