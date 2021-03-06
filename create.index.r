#install.packages("BBmisc")
#library(BBmisc)

# rm(list = ls())
# 
# library(scales)
# library(stats)
# library(Hmisc)

# read data (to save running last function and reading in larger files)
# EDIT: don't need to read this is as creating in by using the 'all.variables.LA <- get.all.GB.data.by.LA()' command in run.functions.R
# all.variables.LA <-  read.csv("S:\\R&A\\Research Projects\\304 - BGET Advice need analysis and mapping\\07 Data\\variable.output.csv", stringsAsFactors = FALSE)

# EDIT: Added weights for index creation as parameters to the function so you can control these from run.functions.r 
# rather than always changing the scripts here
create.GB.index <- function(covid.economic.vulnerability.weight = 2,
                            indebtedness.weight = 2,
                            efg.wgt = 1,
                            prs.wgt = 1,
                            lone.p.wgt = 1,
                            ethnicity.wgt = 1,
                            low.inc.child.wgt = 1,
                            ppm.wgt = 1) {

## rescale data, add together (double weigth covid vulnerability & indebtedness)
variables.with.index <<- all.variables.LA %>%
  mutate(economic.vulnerability.scaled = rescale(Population.weighted.economic.vulnerability.score),
         indebted.scaled = rescale(X..over.indebted),
         EFG.scaled = rescale(prop.EFG),
         PRS.scaled = rescale(prop.private.rented),
         lone.parent.scaled = rescale(prop.lone.parent),   
         minority.ethnic.scaled = rescale(prop.minority.ethnic),
         children.low.income.scaled = rescale(Prop.children.in.low.income.households),
         PPM.scaled = rescale(prop.PPM)) %>%
  mutate(need.index = (economic.vulnerability.scaled * covid.economic.vulnerability.weight +
                         indebted.scaled * indebtedness.weight +
                         EFG.scaled * efg.wgt + 
                         PRS.scaled * prs.wgt +         
                         lone.parent.scaled * lone.p.wgt +
                         minority.ethnic.scaled * ethnicity.wgt +
                         children.low.income.scaled * low.inc.child.wgt + 
                         PPM.scaled * ppm.wgt)) %>%
  mutate(need.decile = ntile(need.index, 10)) %>%
  select(LAD19CD, # keep lsoa code for mapping - need to join to shape files and better to join by code than name (might vary between files).
         LAD19NM,
         Region,
         need.decile,
         need.index,
         economic.vulnerability.scaled,
         indebted.scaled,
         EFG.scaled,
         PRS.scaled,
         lone.parent.scaled,
         minority.ethnic.scaled,
         children.low.income.scaled,
         PPM.scaled
         )
  

}


create.filtered.GB.index <- function(){


## alternative method (index's pre-covd need (based  seperate created index) then only filters top quintile of covid economic vulnerability
# - hugely different as London Boroughs get filtered out (most 3/4 quintile for covid vulnerability but highest debt levels)

variables.with.filtered.index <<- all.variables.LA %>%
  mutate(economic.vulnerability.scaled = rescale(Population.weighted.economic.vulnerability.score),
         indebted.scaled = rescale(X..over.indebted),
         EFG.scaled = rescale(prop.EFG),
         PRS.scaled = rescale(prop.private.rented),
         lone.parent.scaled = rescale(prop.lone.parent),   
         minority.ethnic.scaled = rescale(prop.minority.ethnic),
         children.low.income.scaled = rescale(Prop.children.in.low.income.households),
         PPM.scaled = rescale(prop.PPM)) %>%
  mutate(FP.risk =  EFG.scaled + PRS.scaled + PPM.scaled,
         covid.risk.factors = lone.parent.scaled + 
           minority.ethnic.scaled +
           children.low.income.scaled) %>%
  mutate(FP.risk.scaled = rescale(FP.risk),
         covid.risk.factors.scaled = rescale(covid.risk.factors)) %>%
  mutate(need.index.2 = (FP.risk.scaled +
                           indebted.scaled +
                           covid.risk.factors)) %>% 
  mutate(economic.vulnerability.quintile = ntile(economic.vulnerability.scaled, 5)) %>% 
  filter(economic.vulnerability.quintile == 5) %>%
  select(LAD19NM,
         Region,
         need.index.2,
         indebted.scaled,
         FP.risk.scaled,
         covid.risk.factors.scaled)


}


  

