#clear everything
rm(list = ls())

#load packages
library(tidyverse)
library(dplyr)
library(scales)
library(Hmisc)
library(openxlsx)

#----------------------------------------------------------------------------------------------------------------------

# set up

# run this to create lsoa.lup file for the first time (i.e. set create.lsoa.lookup = TRUE), otherwise it will be ignore
create.lsoa.lookup = FALSE
if(create.lsoa.lookup) {
  source("set-up.R")
}

#create outputs folder (to save csv files etc.)
if(!dir.exists("outputs")) {
  dir.create("outputs")
}

#run scripts which create all required functions
source("get.and.tidy.data.r")
source("create.index.r")

#----------------------------------------------------------------------------------------------------------------------

## get the data from different sources, get at LA and join (LA data a combo of 17 and 19 LA)
## note there is still a problem with Glasgow City (a group_by kills it and I have no idea why, can come back to this with fresh eyes later)
all.variables.LA <- get.all.GB.data.by.LA()

#----------------------------------------------------------------------------------------------------------------------

### MAKE INDEX OPTION 1: create index by standardizing and summarizing variables (worst LA's in London and North West) *favourite method*
### use weights to determine whether fields included in the index calculation or not, and to what extend (0 means not included)
create.GB.index(covid.economic.vulnerability.weight = 2,
                indebtedness.weight = 2,
                efg.wgt = 1,
                prs.wgt = 0, # strong correlation with indebtedness (see below)
                lone.p.wgt = 0.5, # bit of a correlation with indebtedness so turned down to 0.5
                ethnicity.wgt = 0, # strong correlation with indebtedness
                low.inc.child.wgt = 0, # strong correlation with indebtedness
                ppm.wgt = 1)

# Not a strong correlation between these two
plot(variables.with.index$economic.vulnerability.scaled,
     variables.with.index$indebted.scaled)
cor(variables.with.index$economic.vulnerability.scaled,
    variables.with.index$indebted.scaled)

# suggest PRS is amplifying indebtedness so maybe remove from index
plot(variables.with.index$indebted.scaled,
     variables.with.index$PRS.scaled)
cor(variables.with.index$indebted.scaled,
    variables.with.index$PRS.scaled)

# lone parent v indebtedness
plot(variables.with.index$indebted.scaled,
    variables.with.index$lone.parent.scaled)
cor(variables.with.index$indebted.scaled,
    variables.with.index$lone.parent.scaled)

# keep both of these though no correlation with each other or with main two fields
plot(variables.with.index$EFG.scaled,
    variables.with.index$PPM.scaled)
cor(variables.with.index$EFG.scaled,
    variables.with.index$PPM.scaled)

# for example:
plot(variables.with.index$indebted.scaled,
     variables.with.index$PPM.scaled)
cor(variables.with.index$indebted.scaled,
    variables.with.index$PPM.scaled)


# save results
write.csv(variables.with.index,
          "outputs/la-covid-index.csv",
          row.names = FALSE)

#----------------------------------------------------------------------------------------------------------------------

## MAKE INDEX OPTION 2: create index by standardizing and summarizing variables for pre-covid vulernabilities
## then filtering on top 20% of covid economically vulnerable
# *this filters out most of London, that has very high debt & other vulnerabilities but 3/4 quintile for covid vulnerability
create.filtered.GB.index()
