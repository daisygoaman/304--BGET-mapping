# rm(list = ls())
# 
# library(dplyr)
# library(tidyverse)

# get GB data and PPM

get.all.GB.data.by.LA <- function(){


  GBsocialindicators <- read.csv("S:\\R&A\\Research Projects\\304 - BGET Advice need analysis and mapping\\07 Data\\GB Social Indicators\\GB-lsoa-data-numbers - July 2020.csv",
                                 stringsAsFactors = FALSE)
  
  # select variables
  # TOBY NOTES:
  #        - 1. have replaced the GB file above with a newer one which includes EPC data for Scotland
  #        - 2. have added a filter here to only use GB data
  
  GB <- c("E", "S", "W")
  
  GBsocialindicators <- GBsocialindicators %>%
    filter(substring(lsoa.code, 1, 1) %in% GB) %>% 
    transmute(lsoa.code,
              lsoa.name,
              population,
              households.n,
              dwellings.n = round(total.properties.beis),
              efg.n,
              cannot.speak.english.well.or.at.all.n,
              children.in.low.income.hholds.n,
              low.or.no.quals.n,
              private.rented.n,
              lone.parent.n,
              minority.ethnicity.n)
  
  
  # THIS CAN NOW BE IGNORED:
  # replace EFG.n with one with Scotland
  
  # DONE: find Scotland EPC data / work out why it is blank for EFG - should NAs be zero?
  
  # efg <- read.csv("S:\\R&A\\Research Projects\\304 - BGET Advice need analysis and mapping\\07 Data\\GB Social Indicators\\efg.n.csv", stringsAsFactors = FALSE) 
  
  # GBsocialindicators <- left_join(GBsocialindicators, efg, by ="lsoa.code")
  
  ## replace NAs with zeros 
  ## COME BACK TO THIS - What is going with Scottish EPC data?!
  test.epcs <- function(data) {
    data %>% 
      group_by(country = substring(lsoa.code, 1, 1),
               null.value = is.na(efg.n)) %>% 
      summarise(n = n()) %>% 
      spread(key = null.value,
             value = n,
             fill = 0)
  }
  
  #test.epcs(efg)
  test.epcs(GBsocialindicators)
  
  # 185 Scotland LSOAs with NAs in epc data - not sure why.
  
  # can ignore this as NAs removed from sum calculation using the 'na.rm = TRUE' parameter (this defaults to FALSE if not specified)
  # GBsocialindicators$efg.n[is.na(GBsocialindicators$efg.n)] <- 0
  
  
  #get lookup
  
  #lookup1117 <- read.csv("S:\\R&A\\Research Projects\\304 - BGET Advice need analysis and mapping\\07 Data\\Output_Area_to_LSOA_to_MSOA_to_Local_Authority_District__December_2017__Lookup_with_Area_Classifications_in_Great_Britain (2).csv", stringsAsFactors = FALSE)
  #lookup1117 <- lookup1117 %>%
   # select ("LSOA11CD",
   #         "LSOA11NM",
   #         "LAD17CD",
   #         "LAD17NM",
   #         "RGN11NM") %>%
  #  mutate(lsoa.code = LSOA11CD)
  
  # ONSPD <- read.csv("ONSPD_NOV_2019_UK.csv", stringsAsFactors = FALSE)
  # lookup <- ONSPD%>%
  #   select(lsoa11,
  #          oslaua) %>%
  #   distinct() %>%
  #   rename(lsoa.code = lsoa11,
  #          LAD19CD = oslaua)
    
  
  #lookup1119 <- read.csv("S:\\R&A\\Team Resources\\DATASETS\\ONS lookups\\ONSPD\\ONSPD_NOV_2019_UK.zip\\Data\\ONSPD_NOV_2019_UK.csv", stringsAsFactors = FALSE)
   #lookup <- lookup1119 %>%
   #  select ("LSOA11CD",
    #         "LSOA11NM",
    #         "LAD19CD",
     #        "LAD19NM") %>%
    # mutate(lsoa.code = LSOA11CD) 
    # subset(!duplicated(lsoa.code))
   
  # get lsoa to LA lookup (which includes LA names)
  lookup <- read.csv("data/lsoa-to-la-lup.csv",
                     stringsAsFactors = FALSE)  
  
  # join to main table (i.e. add LA code and name)
  GBsocialindicatorswithLA <- left_join(GBsocialindicators, lookup, by = "lsoa.code")
  
  # get at LA
  # TODO: toby to check why lose glasgow data here.
  #       DONE: using the new lookup table seems to have sorted this. Also i notice that the Glasgow code is S12000049, 
  #             but below says S12000046 so maybe this has also changed recently...?
  # NAs removed from sum calculation using the 'na.rm = TRUE' parameter (this defaults to FALSE if not specified)
  GBindicatorsLA <- GBsocialindicatorswithLA %>%
    group_by(LAD19CD,
             LAD19NM) %>%
    summarise(LA.pop = sum(population),
              LA.households = sum(households.n, na.rm = TRUE),
              LA.EFG = sum(efg.n, na.rm = TRUE),
              #   LA.cannot.speak.english.well.or.at.all = sum(cannot.speak.english.well.or.at.all.n),
              LA.children.in.low.income.hholds = sum(children.in.low.income.hholds.n, na.rm = TRUE),
              #  LA.low.or.no.quals = sum(low.or.no.quals.n),
              LA.private.rented =sum(private.rented.n, na.rm = TRUE),
              LA.lone.parent.n = sum(lone.parent.n, na.rm = TRUE),
              LA.minority.ethnicity = sum(minority.ethnicity.n, na.rm = TRUE)) %>% 
    ungroup()
  
  ## Glasgow check: GB data is blank?! S12000046 contains household, EFG - not GB
  
  

  # ADD PPM DATA ------------------------------------------------------------
  # need to get PPM data at LSOA level and join to new LA codes and PPM data 
  # at LA level contains old LA boundaries and codes (its from 2017)
  
  # get PPM and add PPM.n
  ppm.lsoa <- read.xlsx("data/LSOA-prepayment-electricity-2017.xlsx",
                        sheet = "LSOA Dom Elec 2017",
                        startRow = 2)
  
  ppm.lsoa <- ppm.lsoa %>% 
    select(lsoa.code = `Lower.Layer.Super.Output.Area.(LSOA).Code`,
           ppm.n = Total.meters)
  
  ppm.lsoa.with.la <- left_join(lookup, ppm.lsoa, by = "lsoa.code")
  
  ppm.la <- ppm.lsoa.with.la %>% 
    group_by(LAD19CD) %>% 
    summarise(PPM.n = sum(ppm.n, na.rm = TRUE)) %>% 
    ungroup()
  
  ## *Glasgow check - PPM - countains Glasgow City ** S12000049 **
  
  #  as.numeric(PPM$PPM.n)
  
  GBindicatorsLA <- left_join(GBindicatorsLA, ppm.la, by = "LAD19CD")
  
  
  # get n data at %
  GBindicatorsLA <- GBindicatorsLA %>%
    mutate(prop.EFG = LA.EFG / LA.households, 
           Prop.children.in.low.income.households = LA.children.in.low.income.hholds / LA.households ,
           prop.private.rented = LA.private.rented / LA.households,
           prop.lone.parent = LA.lone.parent.n / LA.households,
           prop.PPM = PPM.n / LA.households,
           prop.minority.ethnic = LA.minority.ethnicity / LA.pop) %>%
    select(LAD19CD,
           LAD19NM,
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
           #LAD19NM
           )
  
  indebted2019LAs <- read.csv("S:\\R&A\\Research Projects\\304 - BGET Advice need analysis and mapping\\07 Data\\Indebtedness\\19LAs-MAPS-indebtedness.csv", stringsAsFactors = FALSE) %>%
    mutate(LAD19CD = LADCD) %>%
     select(LAD19CD,
          X..over.indebted,
          Region,
          #LAD19NM = LDNM
          )
  
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
      select(LAD19CD,
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
  all.variables.LA <- all.variables.LA %>%
       filter(!str_detect(LAD19CD, "^N")) 
  
  # write.csv(all.variables.LA, "S:\\R&A\\Research Projects\\304 - BGET Advice need analysis and mapping\\07 Data\\variable.output.csv", row.names = TRUE)
  
  return(all.variables.LA)
  
}




