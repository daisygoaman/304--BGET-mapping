rm(list = ls())

library(tidyverse)
library(dplyr)
library(scales)
library(Hmisc)

## get the data from different sources, get at LA and join (LA data a combo of 17 and 19 LA)
## note there is still a problem with Glasgow City (a group_by kills it and I have no idea why, can come back to this with fresh eyes later)

get.all.GB.data.by.LA()

### create index by standardizing and summarizing variables (worst LA's in London and North West) *favourite method*

create.GB.index()


## creat index by standardizing and summarizing variables for pre-covd vulernabilities
## then filtering on top 20% of covid economically vulnerable
# *this filters out most of London, that has very high debt & other vulnerabilities but 3/4 quintile for covid vulnerability
create.filtered.GB.index()
