#  Data wrangling the the california datasets   May 2021

# Packages
library(tidyverse)
library(tibble)
# 1. import dataset
str(Longcg)

# 1.. Convert dates into dates
Longcg$Date <- as.Date(Longcg$Date,
                     format = "%d/%m/%Y")
# SpudW$`Deployed Date` <- as.Date(SpudW$`Deployed Date`,
#                         format = "%d/%m/%Y")
dlong <- Longcg %>%   # convert data into long format producing a replicates column
  gather(key = "Replicate", value = "Cover", `Plate A1`, `Plate A2`, `Plate A3`, `Plate A4`,`Plate B1`,`Plate B2`,`Plate B3`,`Plate B4`)
# 1.1 check number of unique dates
d %>% 
  summarise(uniquedates = n_distinct(`Deployed Date`))

# 1.2 change nas into zeros
dlong$Cover[is.na(dlong$Cover)] <- 0
# data.long <- data.long %>%
#   separate(Replicate, c("Block", "Rep"), sep=" ")

# 2. remove replicate species as there were duplictes on plates, this totals them
dlong2 <- dlong %>% group_by(Date, Species.new, Replicate) %>% 
  summarise(Tcover= sum(Cover))
# 3. make dats frame wide
dwide <- pivot_wider(dlong2,  names_from = Species.new, values_from = Tcover)
# 4. change nas into zeros again
dwide[is.na(dwide)] <- 0

# 5. selecting unique rows
dselect <- dlong %>% filter(Replicate == "Plate A1" & Species.new=="Ciona sp.")  # this depends on the dataset requires checking
## jump to 6.1 if the unique values match
#dselect <- dselect[-20,]

# 6. finding missing date  
unique(d$Date)
#spud
# date <- dlong %>%  filter(`Recovered Date` >= as.Date("2008-04-09") &  `Recovered Date` <=  as.Date("2008-04-15"))# Spud
# date <-  date[c(1:7),] # select just one row
# date4 <- dlong %>%  filter(`Recovered Date` >= as.Date("2008-04-17") &  `Recovered Date` <=  as.Date("2008-04-23")) 
# date4 <-  date4[c(1:7),] # select just one row
# date5 <- dlong %>%  filter(`Recovered Date` >= as.Date("2008-04-25") &  `Recovered Date` <=  as.Date("2008-05-02"))
# date5 <-  date5[c(1:5),] # select just one row
# date2 <- dlong %>%  filter(`Recovered Date`=="2005-01-12") # this is the missing row for Spud 
# date2 <-  date2[1,] # select just one row
# date3 <- dlong %>%  filter(`Recovered Date`=="2010-06-17") # this is the missing row for Spud  
# date3 <-  date3[1,] # select just one row
# date6 <- dlong %>%  filter(`Recovered Date`=="2010-10-30") # this is the missing row for Spud  
# date6 <-  date6[1,] # select just one row
# #masons
# date <- dlong %>%  filter(`Recovered Date` >= as.Date("2002-09-27") &  `Recovered Date` <=  as.Date("2002-10-20"))# Masons
# date <-  date[c(1:8,10:14,16:23),] # select just one row
# date2 <- dlong %>%  filter(Date=="2008-06-05") # this is the missing row for Masons
# date2 <-  date2[1,] # select just one row
# date3 <- dlong %>%  filter(`Recovered Date`=="2005-06-05") # this is the missing row for Masons  
# date3 <-  date3[1,] # select just one row
# #coast
# date <- dlong %>%  filter(`Recovered Date`=="2002-01-10") # this is the missing row for CoastGuard
# date <-  date[1,] # select just one row
# date <- dlong %>%  filter(Date=="2006-01-19") # this is the missing row for Long MM
# date <-  date[1,] # select just one row
# date1 <- dlong %>%  filter(Date=="2006-02-17") # this is the missing row for Long MM
# date1 <-  date1[1,] # select just one row
date <- dlong %>%  filter(Date >= as.Date("2006-01-19") &  Date <=  as.Date("2006-07-13"))# Limg cg
date <-  date[c(1,17,69),] # select just 2 row
date1 <- dlong %>%  filter(Date=="2006-07-14") # this is the missing row for Long MM
date1 <-  date1[1,] # select just one row
date2 <- dlong %>%  filter(Date=="2006-10-08") # this is the missing row for Long MM
date2 <-  date2[1,] # select just one row

#"2006-01-19" "2006-02-17"

# 6.1 bind the data to get the unique variables to reattach 
data <- rbind(dselect,date) 
# data <- rbind(dselect,date,date2,date3,date4,date5,date6) # bind the data to get to get the unique variables to reattach 
# data <- rbind(dselect,date2)
# data <- data[,-c(4:6,8:10,12:20)] # remove the varibles that are unwanted
# data <- dselect[-c(4:10,12:20)]# remove the varibles that are unwanted
data <- data[-c(2:5,7:15)]# remove the varibles that are unwanted
#data <- dselect[-c(4:8,10:18)]# remove the varibles that are unwanted
#data <- dselect[-c(4:10,12:20)]# remove the varibles that are unwanted
# 7. join datasets horizontally based on a column name  
dwide <- dwide %>% 
  inner_join(data, by = "Date") #join datasets horizontally based on a column name  
# 8. creating a column name for the site
#dwide <-  mutate(dwide, Site = "Coast") #applies the same value to all observations
#dwide <-  mutate(dwide, Site = "Mason") #applies the same value to all observations
dwide <-  mutate(dwide, Site = "Spud") #applies the same value to all observations
# 8. creating a column name for the Season
#dwide <-  mutate(dwide, Season = "Autumn") #applies the same value to all observations
#dwide <-  mutate(dwide, Season = "Spring") #applies the same value to all observations
#dwide <-  mutate(dwide, Season = "Summer") #applies the same value to all observations
#dwide <-  mutate(dwide, Season = "Winter") #applies the same value to all observations

# 8.1 adding a column for effort
Mason_long <-  mutate(Mason_long, Num.panels = "8") #applies the same value to all observations

# 9. setting this data  aside
SpudAutW <- dwide
SpudSW <- dwide
SpudSpW <- dwide
SpudWW <- dwide
LongMasons <- dwide
LongCoast <- dwide
LongSpud <- dwide

LongMasons$`Lichenopora sp.`
# 10. adding missing column to wide data
CoastWW <- add_column(CoastWW, `Obelia dichotoma` = 0, .after = "Mytilus sp.")
CoastWW <- add_column(CoastWW, `Spirobranchus sp.` = 0, .after = "Schizoporella unicornis")
CoastWW <- add_column(CoastWW, `Metridium senile` = 0, .after = "Watersipora subtorquata")
SpudSW <- add_column(SpudSW, `Molgula manhattensis` = 0, .after = "Metridium senile")
LongSpud <- add_column(LongSpud, `Styela montereyensis` = 0, .after = "Styela truncata")
LongSpud <- add_column(LongSpud, `Cryptosula pallasiana` = 0, .after = "Styela montereyensis")

Mason_Summer_wide <- add_column(Mason_Summer_wide, `Spirobranchus sp.` = 0, .after = "Watersipora subtorquata")
Mason_2w_wide <- add_column(Mason_2w_wide, `Styela truncata` = 0, .after = "Metridium senile")
Spud_Summer_wide <- add_column(Spud_Summer_wide, `Ritterella pulchra` = 0, .after = "Styela truncata")
Spud_Summer_wide <- add_column(Spud_Summer_wide, `Styela clava` = 0, .after = "Ritterella pulchra")
Spud_Summer_wide <- add_column(Spud_Summer_wide, `Molgula manhattensis` = 0, .after = "Styela clava")

Mason_seasonal <- add_column(Mason_seasonal, `Styela montereyensis` = 0, .after = "Molgula manhattensis")
Mason_seasonal <- add_column(Mason_seasonal, `Cryptosula pallasiana` = 0, .after = "Molgula manhattensis")

Coast_Autumn_wide <- add_column(Coast_Autumn_wide, Recorder = "NA", .after = "Duration")
Coast_Autumn <- add_column(Coast_Autumn, Site = "Coast", .after = "scaled")
Spud_Autumn_wide <- add_column(Spud_Autumn_wide, Num.panels = 8, .after = "Season")
Masons_Summer_wide <- add_column(Masons_Summer_wide, Season = "Summer", .after = "Site")
LongCoast$`Molgula Manhattensis`
#moving a column  relocate()
Coast_winter_wide <- Coast_winter_wide %>% relocate(`Obelia dichotoma`, .after = `Mytilus sp.`) 

Spud_Summer_wide <- Spud_Autumn_wide %>% relocate(`Metridium senile`, .after = `Spirobranchus sp.`) 
Masons_Summer_wide <- Masons_Summer_wide %>% relocate(`Styela truncata`, .after = `Metridium senile`)
Spud_Autumn_wide <- Spud_Autumn_wide %>% relocate(`Spirobranchus sp.`, .after = `Watersipora subtorquata`) 

Masons_Summer_wide <- Masons_Summer_wide %>% relocate(`Ritterella pulchra`, .after = `Styela truncata`) 
Masons_Summer_wide <- Masons_Summer_wide %>% relocate(`Styela clava`, .after = `Ritterella pulchra`) 
Masons_Summer_wide <- Masons_Summer_wide %>% relocate(`Molgula manhattensis`, .after = `Styela clava`) 
Masons_Summer_wide <- Masons_Summer_wide %>%  relocate(`Styela montereyensis`, .after = `Molgula manhattensis`)
Spud_Summer_wide <- Spud_Summer_wide %>%  relocate(Num.panels, .after = Season)

d2 <- d2 %>%  relocate(`Mytilus sp.`, .after = `Didemnum vexillum`)
d2 <- d2 %>%  relocate(`Styela clava`, .after = `Watersipora subtorquata`)

d2$`Didemnum vexillum`
# add missing columns
Coast_spring_wide <-  mutate(Coast_spring_wide, Season = "Spring") %>% relocate(Season, .after = Site) #applies the same value to all observations

LongSpud <- LongSpud %>% relocate(`Molgula manhattensis`, .after = `Metridium senile`) 
CoastWW <- CoastWW %>% relocate(`Styela montereyensis`, .after = `Styela truncata`) 
LongSpud <- LongSpud %>% relocate(`Styela truncata`, .after = `Molgula manhattensis`) 
SpudAutW <- SpudAutW %>% relocate(`Styela truncata`, .after = `Molgula manhattensis`) 
LongSpud <- LongSpud %>% relocate(Bare, .after = `Balanus crenatus`) 
LongSpud <- LongSpud %>% relocate(`Metridium senile`, .after = `Watersipora subtorquata`) 
LongSpud <- LongSpud %>% relocate(`Mytilus sp.`, .after = `Lichenopora sp.`) 
MasonSpW <- MasonSpW %>% relocate(`Riterella Pulchra`, .after = `Styela truncata`)

Coast_Long_wide <- Coast_Long_wide %>% relocate(`Mytilus sp.`, .after = Bare) 
Coast_Long_wide <- Coast_Long_wide %>% relocate(`Molgula manhattensis`, .after = `Mytilus sp.`) 
Coast_Long_wide <- Coast_Long_wide %>% relocate(`Metridium senile`, .after = `Molgula manhattensis`) 
Coast_Long_wide <- Coast_Long_wide %>% relocate(`Styela truncata`, .after = `Metridium senile`) 
Coast_Long_wide <- Coast_Long_wide %>% relocate(`Ritterella pulchra`, .after = `Styela truncata`) 
Coast_Long_wide <- Coast_Long_wide %>% relocate(`Styela clava`, .after = `Ritterella pulchra`) 
Coast_Long_wide <- Coast_Long_wide %>% relocate(`Spirobranchus sp.`, .after = `Styela clava`) 

Masons_Summer_wide <- Masons_Summer_wide %>% relocate(`Watersipora subtorquata`, .after = `Schizoporella unicornis`)
Masons_Summer_wide$`Schizoporella unicornis`
LongMasons <- LongMasons %>% relocate(Site, .after = Date.Num) 

MasonSpW$`Metridium senile`
# rename column
Mason_Long_wide <- Mason_Long_wide %>% rename(`Ritterella pulchra` = `Riterella Pulchra`)
Spud_Spring_wide$`Riterella Pulchra`
Coast_Long_wide <- Coast_Long_wide %>% rename(`Molgula manhattensis` = `Molgula Manhattensis`) 

Coast_spring_wide <- Coast_spring_wide %>% rename(Date.Num = `Date Num`)
# rename level
levels(df$column1)[levels(df$column1)=="A"] <- "B"
library("plyr")
d <- levels(SeasonAll$`Deployed Date`)[levels(SeasonAll$`Deployed Date`)==as.Date("2008-09-05")] <- as.Date("2008-05-09")
SeasonAll %>% filter(Season =="Spring") %>% rename(as.Date("2008-05-09")  = as.Date("2008-09-05"))
SeasonAll$`Deployed Date` <- as.factor(SeasonAll$`Deployed Date`)
d$`Deployed Date`
# adding up 2 columns
LongMasons <- LongMasons %>% mutate(Molgula = `Molgula manhattensis`+ `Mogula manhattensis`)
LongMasons <- LongMasons[,-c(23,26)]
LongCoast <- LongCoast %>% rename(`Molgula manhattensis` = `Molgula Manhattensis`)
# remove level from a factor
taxadf <- alldata[-which(alldata$Phylum2 == "Comm"),]

# 11. combindatasets vertically
CoastSeasons <- rbind(CoastAutW,CoastSpW,CoastSW,CoastWW)
MasonSeasons <- rbind(MasonAutW,MasonSpW,MasonSW,MasonWW)
SpudSeasons <- rbind(SpudAutW,SpudSpW,SpudSW,SpudWW)

Seasonal <- rbind(CoastSeasons,MasonSeasons,SpudSeasons)
Longterm <- rbind(LongCoast,LongMasons,LongSpud)

CoastSW[is.na(CoastSW)] <- 0
# EXPORT
LongSpud <- as.data.frame(LongSpud)
write.csv(Mason_long, "Mason_long.csv", row.names = FALSE)

# 1. import dataset
str(Spud)

## second round of wrangling
# Final all long ####
# 1.. Convert dates into dates
SeasonAll$Date <- as.Date(SeasonAll$Date,
                        format = "%d/%m/%Y")
SeasonAll$`Recovered Date` <- as.Date(SeasonAll$`Recovered Date`,
                        format = "%d/%m/%Y")
SeasonAll$`Deployed Date` <- as.Date(SeasonAll$`Deployed Date`,
                                  format = "%d/%m/%Y")
dlong <- Spud %>%   # convert data into long format producing a replicates column
  gather(key = "Replicate", value = "Cover", `Plate A1`, `Plate A2`, `Plate A3`, `Plate A4`,`Plate B1`,`Plate B2`,`Plate B3`,`Plate B4`)
# remove unwanted columns
dlong <- dlong[,-c(8:10)]

#. creating a column name for the site
dlong <-  mutate(dlong, Site = "Spud") #applies the same value to all observations
dlong <-  mutate(dlong, Site = "Coast") #applies the same value to all observations
dlong <-  mutate(dlong, Site = "Mason") #applies the same value to all observations

#. creating a column name for the Season
# dlong <-  mutate(dlong, Season = "Autumn") #applies the same value to all observations
# dlong <-  mutate(dlong, Season = "Spring") #applies the same value to all observations
# dlong <-  mutate(dlong, Season = "Summer") #applies the same value to all observations
# dlong <-  mutate(dlong, Season = "Winter") #applies the same value to all observations
# rename data
Spud <- dlong

colnames(SeasonCoastSpring)
recruitAll <- rbind(Mason, Coast,Spud)
# EXPORT
recruitAll <- as.data.frame(recruitAll)
write.csv(recruitAll, "recruitAll.csv", row.names = FALSE)

# scale the species cover ####
# scaled based on the maximum abundance
LongAll[is.na(LongAll)] <- 0
table(LongAll$Species.new)
df <- LongAll %>% group_by(Species.new)%>% summarize(max(Cover))#getthe max each species occured
print(as_tibble(df), n = 24)

SeasonAll$Species.new <- recode_factor(SeasonAll$Species.new, 'Riterella Pulchra' = "Ritterella pulchra")
Mason_long$Species.new <- recode_factor(Mason_long$Species.new, 'Mogula manhattensis' = "Molgula manhattensis")

# do this for long format
recruitAll <- recruitAll %>% mutate(scaled = case_when(Species.new == "Ascidia ceratodes" ~ Cover/194, Species.new == "Balanus crenatus" ~ Cover/1400,
                                             Species.new == "Botrylloides diegensis" ~ Cover/55, Species.new == "Botrylloides violaceous" ~ Cover/206,
                                             Species.new == "Botryllus schlosseri" ~ Cover/132, Species.new == "Bowerbankia gracilis" ~ Cover/192,  
                                             Species.new == "Bugula californica" ~ Cover/71,Species.new == "Bugula neritina" ~ Cover/80, 
                                             Species.new == "Ciona sp." ~ Cover/64,Species.new == "Didemnum vexillum" ~ Cover/172,
                                             Species.new == "Diplosoma listerianum" ~ Cover/1248, Species.new == "Distaplia occidentalis" ~ Cover/2598,
                                             Species.new == "Lichenopora sp." ~ Cover/90, Species.new == "Metridium senile" ~ Cover/30, 
                                             Species.new == "Mytilus sp." ~ Cover/30,Species.new == "Obelia dichotoma" ~ Cover/86,
                                             Species.new == "Schizoporella unicornis" ~ Cover/53,Species.new == "Spirobranchus sp." ~ Cover/730,
                                             Species.new == "Watersipora subtorquata" ~ Cover/124, Species.new == "Cryptosula pallasiana" ~ Cover/2,
                                             Species.new == "Molgula manhattensis" ~ Cover/9,Species.new == "Ritterella pulchra" ~ Cover/0,
                                             Species.new == "Styela clava" ~ Cover/0, Species.new == "Styela montereyensis" ~ Cover/0,
                                             Species.new == "Styela truncata" ~ Cover/0, Species.new == "Bare" ~ Cover/0))



