# This script only needs to run if recreating the lsoa.to.la.lup file. This file has been saved in the repository anyway, so this shouldn't need to be
# but if it is, note location of the ONSPD - it will need to be saved in "data"...

ONSPD <- read.csv("data/ONSPD_NOV_2019_UK.csv", stringsAsFactors = FALSE)

#make a lookup - however some LSOAs straddle multiple LAs (only about 30, so need to select LA that most of the LSOA falls into)
lookup <- ONSPD %>%
  group_by(lsoa11,
           oslaua) %>%
  summarise(n = n()) %>% 
  ungroup() 

# find example of a duplicate lsoa
lookup.eg <- lookup %>% 
  filter(lsoa11 == "S01010117")

lookup <- lookup %>% 
  group_by(lsoa11) %>% 
  top_n(n = 1,
        wt = n) %>% 
  ungroup()

#check for duplicates
dups <- lookup %>% 
  group_by(lsoa11) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  filter(n > 1)

if(nrow(dups) > 0) {
  print("Duplicate LSOAs in LA lookup")
}

# finalise lookup table
lookup <- lookup %>% 
  select(lsoa.code = lsoa11,
         LAD19CD = oslaua)

# read in names and add to lookup
la.names <- read.csv("data/LA_UA names and codes UK as at 12_19.csv",
                     stringsAsFactors = FALSE)

#column headings read in in weird format so change
colnames(la.names) <- c("LAD19CD", "LAD19NM", "WelshName")
la.names <- la.names %>% 
  select(LAD19CD,
         LAD19NM)

lookup <- left_join(lookup, la.names, by = "LAD19CD")

write.csv(lookup,
          "outputs/lsoa-to-la-lup.csv",
          row.names = FALSE)
