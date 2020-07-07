rm(list = ls())

library(dplyr)
library(tidyverse)

# get GB data and PPM

get.all.GB.data.by.LA <- function(){


GBsocialindicators <- read.csv("S:\\R&A\\Research Projects\\304 - BGET Advice need analysis and mapping\\07 Data\\GB Social Indicators\\GB-lsoa-data-numbers.csv", stringsAsFactors = FALSE)

# select variables

GBsocialindicators <- GBsocialindicators %>%
  select(lsoa.code,
         lsoa.name,
         population,
         households.n,
         #efg.combined.n,
         cannot.speak.english.well.or.at.all.n,
         children.in.low.income.hholds.n,
         low.or.no.quals.n,
         private.rented.n,
         lone.parent.n,
         minority.ethnicity.n)


#replace EFG.n with one with Scotland

efg <- read.csv("S:\\R&A\\Research Projects\\304 - BGET Advice need analysis and mapping\\07 Data\\GB Social Indicators\\efg.n.csv", stringsAsFactors = FALSE) 

GBsocialindicators <- left_join(GBsocialindicators, efg, by ="lsoa.code")

## replace NAs with zeros 

## COME BACK TO THIS - What is going with Scottish EPC data?!

GBsocialindicators$efg.n[is.na(GBsocialindicators$efg.n)] <- 0


#get lookup

#lookup1117 <- read.csv("S:\\R&A\\Research Projects\\304 - BGET Advice need analysis and mapping\\07 Data\\Output_Area_to_LSOA_to_MSOA_to_Local_Authority_District__December_2017__Lookup_with_Area_Classifications_in_Great_Britain (2).csv", stringsAsFactors = FALSE)
#lookup1117 <- lookup1117 %>%
 # select ("LSOA11CD",
 #         "LSOA11NM",
 #         "LAD17CD",
 #         "LAD17NM",
 #         "RGN11NM") %>%
#  mutate(lsoa.code = LSOA11CD)

ONSPD <- read.csv("ONSPD_NOV_2019_UK.csv", stringsAsFactors = FALSE)
lookup <- ONSPD%>%
  select(lsoa11,
         oslaua) %>%
  distinct() %>%
  rename(lsoa.code = lsoa11,
         LAD19CD = oslaua)
  
  


#lookup1119 <- read.csv("S:\\R&A\\Team Resources\\DATASETS\\ONS lookups\\ONSPD\\ONSPD_NOV_2019_UK.zip\\Data\\ONSPD_NOV_2019_UK.csv", stringsAsFactors = FALSE)
 #lookup <- lookup1119 %>%
 #  select ("LSOA11CD",
  #         "LSOA11NM",
  #         "LAD19CD",
   #        "LAD19NM") %>%
  # mutate(lsoa.code = LSOA11CD) 
  # subset(!duplicated(lsoa.code))
 

GBsocialindicatorswithLA <- left_join(GBsocialindicators, lookup, by = "lsoa.code")


# get at LA

GBindicatorsLA <- GBsocialindicatorswithLA %>%
  group_by(LAD19CD) %>%
  summarise(LA.pop = sum(population),
            LA.households = sum(households.n),
            LA.EFG = sum(efg.n),
            #   LA.cannot.speak.english.well.or.at.all = sum(cannot.speak.english.well.or.at.all.n),
            LA.children.in.low.income.hholds = sum(children.in.low.income.hholds.n),
            #  LA.low.or.no.quals = sum(low.or.no.quals.n),
            LA.private.rented =sum(private.rented.n),
            LA.lone.parent.n = sum(lone.parent.n),
            LA.minority.ethnicity = sum(minority.ethnicity.n))

## Glasgow check: GB data is blank?! S12000046 contains household, EFG - not GB


# get PPM and add PPM.n

PPM <- read.csv("S:\\R&A\\Research Projects\\304 - BGET Advice need analysis and mapping\\07 Data\\PrePaymentMeter\\Local-authority-prepayment-electricity-statistics-2017 (1).csv", stringsAsFactors = FALSE)
PPM <- PPM %>%
  mutate(LAD19NM = Local.Authority,
         LAD19CD = LA.Code,
         PPM.n = Meters) %>%
  mutate(PPM.n = as.numeric(PPM.n)) %>%
  select(LAD19NM,
         LAD19CD,
         PPM.n)   

## *Glasgow check - PPM - countains Glasgow City S12000046


#  as.numeric(PPM$PPM.n)


GBindicatorsLA <- left_join(GBindicatorsLA, PPM, by = "LAD19CD")


# get n data at %
GBindicatorsLA <- GBindicatorsLA %>%
  mutate(prop.EFG = LA.EFG / LA.households, 
         Prop.children.in.low.income.households = LA.children.in.low.income.hholds / LA.households ,
         prop.private.rented = LA.private.rented / LA.households,
         prop.lone.parent = LA.lone.parent.n / LA.households,
         prop.PPM = PPM.n / LA.households,
         prop.minority.ethnic = LA.minority.ethnicity / LA.pop) %>%
  select(LAD19CD,
       #  Region,
         prop.EFG,
         prop.private.rented,
         prop.lone.parent,
         prop.minority.ethnic,
         prop.PPM,
         Prop.children.in.low.income.households)


## get redcross data
  
  redcrosscovid <- read.csv("S:\\R&A\\Research Projects\\304 - BGET Advice need analysis and mapping\\07 Data\\BritishRedCrossSociety\\BritishRedCrossSocietyCovid19Vulnerability.csv", stringsAsFactors = FALSE)
  
  # select variables and rename LA variable
  
  redcrosscovid <- redcrosscovid %>%
    rename(LAD19CD = Code,
           LAD19NM = Name) %>%
    select(LAD19CD,
         #  LAD19NM,
           Population.weighted.economic.vulnerability.score)
  
  #get extra LA names and codes (2019 data from new unitary authorities applied to non-unitary authorities from 2017)
  
 # extra.LAs <- read.csv("S:\\R&A\\Research Projects\\304 - BGET Advice need analysis and mapping\\07 Data\\extra.2017.LAs.csv", stringsAsFactors = FALSE)
  
  
 # redcrosscovid <- rbind(redcrosscovid, extra.LAs)
  

## get MAPS data

MAPSindebtedness <- read.csv("S:\\R&A\\Research Projects\\304 - BGET Advice need analysis and mapping\\07 Data\\Indebtedness\\MAPS-indebtedness.csv", stringsAsFactors = FALSE)


MAPSindebtedness <- MAPSindebtedness %>%
  rename( LAD19CD = Area.ID,
          LAD19NM = Lower.Tier.Authority) %>%
  select(LAD19CD,
         X..over.indebted,
         Region,
         LAD19NM)

indebted2019LAs <- read.csv("S:\\R&A\\Research Projects\\304 - BGET Advice need analysis and mapping\\07 Data\\Indebtedness\\19LAs-MAPS-indebtedness.csv", stringsAsFactors = FALSE) %>%
  mutate(LAD19CD = LADCD) %>%
   select(LAD19CD,
        X..over.indebted,
        Region)

MAPSindebtedness <- rbind(MAPSindebtedness, indebted2019LAs)


#PPM <- read.csv("S:\\R&A\\Research Projects\\304 - BGET Advice need analysis and mapping\\07 Data\\PrePaymentMeter\\Local-authority-prepayment-electricity-statistics-2017 (1).csv", stringsAsFactors = FALSE)
 #PPM <- PPM %>%
#   mutate(LAD17NM = Local.Authority,
 #         LAD17CD = LA.Code,
#          PPM.n = Meters) %>%
#   select(LAD17NM,
  #        LAD17CD,
  #        PPM.n,
   #       Region)


  all.variables.LA <- left_join(redcrosscovid, MAPSindebtedness, by = "LAD19CD")
  
  all.variables.LA <- left_join(all.variables.LA, GBindicatorsLA, by = "LAD19CD") %>%
    select( LAD19CD,
            LAD19NM,
           Region,
Population.weighted.economic.vulnerability.score,                
            X..over.indebted,                                
             prop.EFG,                                       
           prop.private.rented,
           prop.lone.parent,                            
           prop.minority.ethnic,
           Prop.children.in.low.income.households,
            prop.PPM)

  
# remove N.Ireland
  
  all.variables.LA <<- all.variables.LA %>%
    filter(!str_detect(LAD19CD, "^N")) 
  
  
write.csv(all.variables.LA, "S:\\R&A\\Research Projects\\304 - BGET Advice need analysis and mapping\\07 Data\\variable.output.csv", row.names = TRUE)


return(all.variables.LA)
  
}




