# adding all the columns you need when the data is still seperate
# version 2 should have the errors removed so if you find one go back to that version and correct it 
library(tidyverse)

# 1. recruitment data
# input data

d <- cgsp # name it d
# format date
d$`Recovered Date` <- as.Date(d$`Recovered Date`,
                       format = "%d/%m/%Y")
d$`Deployed Date` <- as.Date(d$`Deployed Date`,
                        format = "%d/%m/%Y")
d$Date <- as.Date(d$Date,
                       format = "%d/%m/%Y")
# d[is.na(d)] <- 0

# go to remove species names to remove aux data #
# then come back here and continue to make the long data =set

##  add the extra columns
d <- d %>% mutate(feedgroup = case_when(Species.new == "Ascidia ceratodes" ~ "Filter feed", Species.new == "Balanus crenatus" ~ "Filter feed",
                                        Species.new == "Botrylloides diegensis" ~ "Filter feed", Species.new == "Botrylloides violaceous" ~ "Filter feed",
                                        Species.new == "Botryllus schlosseri" ~ "Filter feed", Species.new == "Bowerbankia gracilis" ~ "Filter feed",  
                                        Species.new == "Bugula californica" ~ "Filter feed",Species.new == "Bugula neritina" ~ "Filter feed", 
                                        Species.new == "Ciona sp." ~ "Filter feed",Species.new == "Didemnum vexillum" ~ "Filter feed",
                                        Species.new == "Diplosoma listerianum" ~ "Filter feed", Species.new == "Distaplia occidentalis" ~ "Filter feed",
                                        Species.new == "Lichenopora sp." ~ "Filter feed", Species.new == "Metridium senile" ~ "Predator", 
                                        Species.new == "Mytilus sp." ~ "Filter feed",Species.new == "Obelia dichotoma" ~ "Filter feed",
                                        Species.new == "Schizoporella unicornis" ~ "Filter feed",Species.new == "Spirobranchus sp." ~ "Filter feed",
                                        Species.new == "Watersipora subtorquata" ~ "Filter feed",Species.new == "Cryptosula pallasiana" ~ "Filter feed",
                                        Species.new == "Molgula manhattensis" ~ "Filter feed",Species.new == "Ritterella pulchra" ~ "Filter feed",
                                        Species.new == "Styela clava" ~ "Filter feed", Species.new == "Styela montereyensis" ~ "Filter feed",
                                        Species.new == "Styela truncata" ~ "Filter feed", Species.new == "Bare" ~ "Bare Space")) %>% 
  mutate(taxa = case_when(Species.new == "Ascidia ceratodes" ~ "Tunicata", Species.new == "Balanus crenatus" ~ "Crustacea",
                                   Species.new == "Botrylloides diegensis" ~ "Tunicata", Species.new == "Botrylloides violaceous" ~ "Tunicata",
                                   Species.new == "Botryllus schlosseri" ~ "Tunicata", Species.new == "Bowerbankia gracilis" ~ "Bryozoa",  
                                   Species.new == "Bugula californica" ~ "Bryozoa",Species.new == "Bugula neritina" ~ "Bryozoa", 
                                   Species.new == "Ciona sp." ~ "Tunicata",Species.new == "Didemnum vexillum" ~ "Tunicata",
                                   Species.new == "Diplosoma listerianum" ~ "Tunicata", Species.new == "Distaplia occidentalis" ~ "Tunicata",
                                   Species.new == "Lichenopora sp." ~ "Bryozoa", Species.new == "Metridium senile" ~ "Anemone", 
                                   Species.new == "Mytilus sp." ~ "Mollusca",Species.new == "Obelia dichotoma" ~ "Hydrozoa",
                                   Species.new == "Schizoporella unicornis" ~ "Bryozoa",Species.new == "Spirobranchus sp." ~ "Annelida",
                                   Species.new == "Watersipora subtorquata" ~ "Bryozoa",Species.new == "Cryptosula pallasiana" ~ "Bryozoa",
                                   Species.new == "Molgula manhattensis" ~ "Tunicata",Species.new == "Ritterella pulchra" ~ "Tunicata",
                                   Species.new == "Styela clava" ~ "Tunicata", Species.new == "Styela montereyensis" ~ "Tunicata",
                                   Species.new == "Styela truncata" ~ "Tunicata", Species.new == "Bare" ~ "Bare Space")) %>% 
  mutate(name = case_when(Species.new == "Ascidia ceratodes" ~ "Ascidian", Species.new == "Balanus crenatus" ~ "Barnacle",
                                   Species.new == "Botrylloides diegensis" ~ "Ascidian", Species.new == "Botrylloides violaceous" ~ "Ascidian",
                                   Species.new == "Botryllus schlosseri" ~ "Ascidian", Species.new == "Bowerbankia gracilis" ~ "Bryozoan",  
                                   Species.new == "Bugula californica" ~ "Bryozoan",Species.new == "Bugula neritina" ~ "Bryozoan", 
                                   Species.new == "Ciona sp." ~ "Ascidian",Species.new == "Didemnum vexillum" ~ "Ascidian",
                                   Species.new == "Diplosoma listerianum" ~ "Ascidian", Species.new == "Distaplia occidentalis" ~ "Ascidian",
                                   Species.new == "Lichenopora sp." ~ "Bryozoan", Species.new == "Metridium senile" ~ "Anemone", 
                                   Species.new == "Mytilus sp." ~ "Mussel",Species.new == "Obelia dichotoma" ~ "Hydroid",
                                   Species.new == "Schizoporella unicornis" ~ "Bryozoan",Species.new == "Spirobranchus sp." ~ "Tubeworm",
                                   Species.new == "Watersipora subtorquata" ~ "Bryozoan", Species.new == "Cryptosula pallasiana" ~ "Bryozoan",
                                   Species.new == "Molgula manhattensis" ~ "Ascidian",Species.new == "Ritterella pulchra" ~ "Ascidian",
                                   Species.new == "Styela clava" ~ "Ascidian", Species.new == "Styela montereyensis" ~ "Ascidian",
                                   Species.new == "Styela truncata" ~ "Ascidian", Species.new == "Bare" ~ "Bare Space")) %>% 
  mutate(form = case_when(Species.new == "Ascidia ceratodes" ~ "Solitary", Species.new == "Balanus crenatus" ~ "Solitary",
                                   Species.new == "Botrylloides diegensis" ~ "Colonial", Species.new == "Botrylloides violaceous" ~ "Colonial",
                                   Species.new == "Botryllus schlosseri" ~ "Colonial", Species.new == "Bowerbankia gracilis" ~ "Colonial",  
                                   Species.new == "Bugula californica" ~ "Colonial",Species.new == "Bugula neritina" ~ "Colonial", 
                                   Species.new == "Ciona sp." ~ "Solitary",Species.new == "Didemnum vexillum" ~ "Colonial",
                                   Species.new == "Diplosoma listerianum" ~ "Ascidian", Species.new == "Distaplia occidentalis" ~ "Colonial",
                                   Species.new == "Lichenopora sp." ~ "Colonial", Species.new == "Metridium senile" ~ "Anemone", 
                                   Species.new == "Mytilus sp." ~ "Solitary",Species.new == "Obelia dichotoma" ~ "Colonial",
                                   Species.new == "Schizoporella unicornis" ~ "Colonial",Species.new == "Spirobranchus sp." ~ "Solitary",
                                   Species.new == "Watersipora subtorquata" ~ "Colonial", Species.new == "Cryptosula pallasiana" ~ "Colonial",
                                   Species.new == "Molgula manhattensis" ~ "Solitary",Species.new == "Ritterella pulchra" ~ "Colonial",
                                   Species.new == "Styela clava" ~ "Solitary", Species.new == "Styela montereyensis" ~ "Solitary",
                                   Species.new == "Styela truncata" ~ "Solitary", Species.new == "Bare" ~ "Bare Space")) %>% 
  mutate(structure = case_when(Species.new == "Ascidia ceratodes" ~ "Erect", Species.new == "Balanus crenatus" ~ "Erect",
                                        Species.new == "Botrylloides diegensis" ~ "Encrusting", Species.new == "Botrylloides violaceous" ~ "Encrusting",
                                        Species.new == "Botryllus schlosseri" ~ "Encrusting", Species.new == "Bowerbankia gracilis" ~ "Erect",  
                                        Species.new == "Bugula californica" ~ "Erect",Species.new == "Bugula neritina" ~ "Erect", 
                                        Species.new == "Ciona sp." ~ "Erect",Species.new == "Didemnum vexillum" ~ "Encrusting",
                                        Species.new == "Diplosoma listerianum" ~ "Encrusting", Species.new == "Distaplia occidentalis" ~ "Encrusting",
                                        Species.new == "Lichenopora sp." ~ "Encrusting", Species.new == "Metridium senile" ~ "Erect", 
                                        Species.new == "Mytilus sp." ~ "Erect",Species.new == "Obelia dichotoma" ~ "Erect",
                                        Species.new == "Schizoporella unicornis" ~ "Encrusting",Species.new == "Spirobranchus sp." ~ "Encrusting",
                                        Species.new == "Watersipora subtorquata" ~ "Encrusting",Species.new == "Cryptosula pallasiana" ~ "Encrusting",
                                        Species.new == "Molgula manhattensis" ~ "Erect",Species.new == "Ritterella pulchra" ~ "Encrusting",
                                        Species.new == "Styela clava" ~ "Erect", Species.new == "Styela montereyensis" ~ "Erect",
                                        Species.new == "Styela truncata" ~ "Erect",Species.new == "Bare" ~ "Bare Space")) %>% 
  mutate(status = case_when(Species.new == "Ascidia ceratodes" ~ "Native", Species.new == "Balanus crenatus" ~ "Native",
                                     Species.new == "Botrylloides diegensis" ~ "Native", Species.new == "Botrylloides violaceous" ~ "Non-native",
                                     Species.new == "Botryllus schlosseri" ~ "Non-native", Species.new == "Bowerbankia gracilis" ~ "Non-native",  
                                     Species.new == "Bugula californica" ~ "Native",Species.new == "Bugula neritina" ~ "Non-native", 
                                     Species.new == "Ciona sp." ~ "Non-native",Species.new == "Didemnum vexillum" ~ "Non-native",
                                     Species.new == "Diplosoma listerianum" ~ "Cryptogenic", Species.new == "Distaplia occidentalis" ~ "Native",
                                     Species.new == "Lichenopora sp." ~ "Native", Species.new == "Metridium senile" ~ "Native", 
                                     Species.new == "Mytilus sp." ~ "Non-native",Species.new == "Obelia dichotoma" ~ "Native",
                                     Species.new == "Schizoporella unicornis" ~ "Non-native",Species.new == "Spirobranchus sp." ~ "Unknown",
                                     Species.new == "Watersipora subtorquata" ~ "Non-native",Species.new == "Cryptosula pallasiana" ~ "Non-native",
                                     Species.new == "Molgula manhattensis" ~ "Non-native",Species.new == "Ritterella pulchra" ~ "Native",
                                     Species.new == "Styela clava" ~ "Non-native", Species.new == "Styela montereyensis" ~ "Native",
                                     Species.new == "Styela truncata" ~ "Native",Species.new == "Bare" ~ "Bare Space"))



# make this a long dataset
dlong <- d %>%   # convert data into long format producing a replicates column
  gather(key = "Replicate", value = "Cover", `Plate A1`, `Plate A2`, `Plate A3`, `Plate A4`,`Plate B1`,`Plate B2`,`Plate B3`,`Plate B4`)
# remove some columns
dlong <- dlong[,-c(7:8)]
#dlong <- dlong[,-c(7:10)]
# dlong[is.na(dlong)] <- 0
dlong <- dlong[!is.na(dlong$Cover), ]

# add the scaled data
# Recruit %>% group_by(Species.new)%>% summarize(max(Cover)) %>%  print(n=26)#getthe max each species occured
# 
# #recruit data
# Recruit <- Recruit %>% mutate(scaled = case_when(Species.new == "Ascidia ceratodes" ~ Cover/194, Species.new == "Balanus crenatus" ~ Cover/1400,
#                                      Species.new == "Botrylloides diegensis" ~ Cover/206, Species.new == "Botrylloides violaceous" ~ Cover/68,
#                                      Species.new == "Botryllus schlosseri" ~ Cover/95, Species.new == "Bowerbankia gracilis" ~ Cover/896,
#                                      Species.new == "Bugula californica" ~ Cover/109,Species.new == "Bugula neritina" ~ Cover/80,
#                                      Species.new == "Ciona sp." ~ Cover/132,Species.new == "Didemnum vexillum" ~ Cover/147,
#                                      Species.new == "Diplosoma listerianum" ~ Cover/1248, Species.new == "Distaplia occidentalis" ~ Cover/2598,
#                                      Species.new == "Lichenopora sp." ~ Cover/31, Species.new == "Metridium senile" ~ Cover/2,
#                                      Species.new == "Mytilus sp." ~ Cover/30,Species.new == "Obelia dichotoma" ~ Cover/65,
#                                      Species.new == "Schizoporella unicornis" ~ Cover/86,Species.new == "Spirobranchus sp." ~ Cover/730,
#                                      Species.new == "Watersipora subtorquata" ~ Cover/100, Species.new == "Cryptosula pallasiana" ~ Cover/80,
#                                      Species.new == "Molgula manhattensis" ~ Cover/9
#                                    ))
                                  

# Longterm %>% group_by(Species.new)%>% summarize(max(Cover)) %>%  print(n=26)#getthe max each species occured
# print(as_tibble(df), n = 26)
# # # long data
Longterm <- Longterm %>% mutate(scaled = case_when(Species.new == "Ascidia ceratodes" ~ Cover/105, Species.new == "Balanus crenatus" ~ Cover/105,
                                             Species.new == "Botrylloides diegensis" ~ Cover/30, Species.new == "Botrylloides violaceous" ~ Cover/90,
                                             Species.new == "Botryllus schlosseri" ~ Cover/40, Species.new == "Bowerbankia gracilis" ~ Cover/105,
                                             Species.new == "Bugula californica" ~ Cover/93,Species.new == "Bugula neritina" ~ Cover/95,
                                             Species.new == "Ciona sp." ~ Cover/50,Species.new == "Didemnum vexillum" ~ Cover/100,
                                             Species.new == "Diplosoma listerianum" ~ Cover/105, Species.new == "Distaplia occidentalis" ~ Cover/92,
                                             Species.new == "Lichenopora sp." ~ Cover/92, Species.new == "Metridium senile" ~ Cover/75,
                                             Species.new == "Mytilus sp." ~ Cover/40,Species.new == "Obelia dichotoma" ~ Cover/83,
                                             Species.new == "Schizoporella unicornis" ~ Cover/93,Species.new == "Spirobranchus sp." ~ Cover/15,
                                             Species.new == "Watersipora subtorquata" ~ Cover/95, Species.new == "Cryptosula pallasiana" ~ Cover/0,
                                             Species.new == "Molgula manhattensis" ~ Cover/14,Species.new == "Ritterella pulchra" ~ Cover/20,
                                             Species.new == "Styela clava" ~ Cover/15, Species.new == "Styela montereyensis" ~ Cover/0,
                                             Species.new == "Styela truncata" ~ Cover/10, Species.new == "Bare" ~ Cover/205))
# scale the species cover ####
# scaled based on the maximum abundance

# SeasonAll[is.na(SeasonAll)] <- 0
table(dlong$Species.new)
df <- Longterm %>% group_by(Species.new)%>% summarize(max(Cover))#getthe max each species occured
print(as_tibble(df), n = 26)
# # seasonal data
dlong <- dlong %>% mutate(scaled = case_when(Species.new == "Ascidia ceratodes" ~ Cover/100, Species.new == "Balanus crenatus" ~ Cover/100,
                                             Species.new == "Botrylloides diegensis" ~ Cover/85, Species.new == "Botrylloides violaceous" ~ Cover/100,
                                             Species.new == "Botryllus schlosseri" ~ Cover/45, Species.new == "Bowerbankia gracilis" ~ Cover/105,
                                             Species.new == "Bugula californica" ~ Cover/1205,Species.new == "Bugula neritina" ~ Cover/95,
                                             Species.new == "Ciona sp." ~ Cover/80,Species.new == "Didemnum vexillum" ~ Cover/100,
                                             Species.new == "Diplosoma listerianum" ~ Cover/100, Species.new == "Distaplia occidentalis" ~ Cover/100,
                                             Species.new == "Lichenopora sp." ~ Cover/90, Species.new == "Metridium senile" ~ Cover/30,
                                             Species.new == "Mytilus sp." ~ Cover/70,Species.new == "Obelia dichotoma" ~ Cover/100,
                                             Species.new == "Schizoporella unicornis" ~ Cover/505,Species.new == "Spirobranchus sp." ~ Cover/6,
                                             Species.new == "Watersipora subtorquata" ~ Cover/320, Species.new == "Cryptosula pallasiana" ~ Cover/0,
                                             Species.new == "Molgula manhattensis" ~ Cover/88,Species.new == "Ritterella pulchra" ~ Cover/15,
                                             Species.new == "Styela clava" ~ Cover/4, Species.new == "Styela montereyensis" ~ Cover/6,
                                             Species.new == "Styela truncata" ~ Cover/12, Species.new == "Bare" ~ Cover/100))
# 

# add time sample based on deployed and recovered dates
# cg_long_dates <- dlong %>%
#   arrange(`Deployed Date`) %>%
#   distinct(`Recovered Date`,`Deployed Date`) %>%
#   mutate(time_sample = row_number(`Deployed Date`))# iding row numbers
# # long data
# sp_long_dates <- d %>%
#   arrange(Date) %>% 
#   distinct(Date) %>% 
#   mutate(time_sample = row_number(Date))# iding row numbers
# seasonal data
cg_long_dates <- dlong %>%
  arrange(Date) %>%
  distinct(Date) %>%
  mutate(time_sample = row_number(Date))# iding row numbers

#  export ####
# setwd("~/Documents/1. PhD/1. Research/3. Calif/CalifData/Dates_data")
# spSp_long_dates <- as.data.frame(spSp_long_dates)
# write.csv(spSp_long_dates, "spSp_long_dates.csv", row.names = FALSE)

# dlong <- dlong %>% # add time_sample to dataset
#   inner_join(cg_long_dates, by = c("Recovered Date", "Deployed Date"))
d <- d %>% # add time_sample to dataset
  inner_join(cg_long_dates, by = "Date")

#add site name
dlong <-  mutate(dlong, Site = "Coast") #applies the same value to all observations
#. creating a column name for the Season
dlong <-  mutate(dlong, Season = "Autumn") #applies the same value to all observations
dlong <-  mutate(dlong, Season = "Spring") #applies the same value to all observations
dlong <-  mutate(dlong, Season = "Summer") #applies the same value to all observations
# dlong <-  mutate(dlong, Season = "Winter") #applies the same value to all observations
# rename data
Mason_Summer <- dlong
setwd("~/Documents/1. PhD/1. Research/3. Calif/CalifData/Version long/recruit")
Mason_Summer <- as.data.frame(Mason_Summer)
write.csv(Mason_Summer, "Mason_Summer.csv", row.names = FALSE)

d <- Mason_Summer
## make the dataset wide
dlong1 <- d %>%   # convert data into long format producing a replicates column
  gather(key = "Replicate", value = "Cover", `Plate A1`, `Plate A2`, `Plate A3`, `Plate A4`,`Plate B1`,`Plate B2`,`Plate B3`,`Plate B4`)
# remove some columns
dlong1 <- dlong1[!is.na(dlong1$Cover), ]
dlong1 <- dlong1[,-c(7:8)]
#dlong <- dlong[,-c(4:5)]
# 1.1 check number of unique dates
# d %>% 
#   summarise(uniquedates = n_distinct(`Deployed Date`))
d %>%
  summarise(uniquedates = n_distinct(`Recovered Date`,`Deployed Date`))
d %>%
  summarise(uniquedates = n_distinct(Date))
# 1.2 change nas into zeros
# dlong$Cover[is.na(dlong$Cover)] <- 0
# 2. remove replicate species as there were duplictes on plates, this totals them
dlong2 <- dlong1 %>% group_by(Date, Species.new, Replicate) %>% 
  summarise(Tcover= sum(Cover))
# 3. make dats frame wide
dwide <- pivot_wider(dlong2,  names_from = Species.new, values_from = Tcover)
# 4. change nas into zeros again
dwide[is.na(dwide)] <- 0
# 5. selecting unique rows
dselect <- dlong1[!duplicated(dlong1$time_sample), ]# remove duplicated lines
dselect <- dselect[,-c(4,8:14,16:17)]
dselect <- dselect %>% arrange(time_sample)
dselect <- Mason_Summer[!duplicated(Mason_Summer$time_sample), ]# remove duplicated lines

dselect <- Mason_2w_wide[!duplicated(Mason_2w_wide$time_sample), ]# remove duplicated lines

dselect <- dselect %>% arrange(time_sample)
# dselect <- dlong %>%  #select(`Recovered Date`, `Deployed Date`, Duration,Collector,Recorder,`# Plates Sampled`, Date.Num, time_sample) %>% 
#   group_by(`Recovered Date`, `Deployed Date`) %>% 
#   arrange(`Recovered Date`) %>% 
#   distinct(`Recovered Date`,`Deployed Date`)
  
dselect <- dlong %>% filter(Replicate == "Plate A1" & Species.new=="Ciona sp.")  # this depends on the dataset requires checking
dselect <- dlong[!duplicated(dlong$time_sample), ]# remove duplicated lines
dselect <- Coast_2w_wide[,c(25:32)] 

dselect <- dselect[!duplicated(dselect$time_sample), ]# remove duplicated lines
dselect <- Mason_2w_wide[,c(2,25:31)] #%>% arrange(time_sample) %>% distinct(time_sample)

## jump to 6.1 if the unique values match
#dselect <- dselect[-20,]
# 6. finding missing date
unique(d$`Recovered Date`)
# missing data found throogh dates and time sample   
# cg recruit  - 
date1 <- dlong %>%  filter(`Recovered Date`=="2005-01-12") # 
date1 <-  date1[1,] # select just one row
# sp recruit  - 
date1 <- dlong %>%  filter(`Recovered Date`=="2003-09-13") # 
date1 <-  date1[1,] # select just one row
date2 <- dlong %>%  filter(`Recovered Date`=="2004-08-16") #
date2 <-  date2[1,] # select just one row
date3 <- dlong %>%  filter(`Recovered Date`=="2005-01-12") #
date3 <-  date3[1,] # select just one row
# cg - long 
date1 <- dlong %>%  filter(Date=="2006-10-08") # 
date1 <-  date1[1,] # select just one row
# 6.1 bind the data to get the unique variables to reattach 
data <- rbind(dselect,date1,date2,date3) 
data <-  rbind(dselect,date1)
# remove unwanted variables
#data <- data[-c(4,9,11:12)]# dont want species and cover
data <- data[-c(2,5:11,13:14)]# dont want species and cover
dselect <- dselect[-c(2,5:11,13:14)]# dont want species and cover

dselect <- dselect[-c(4,8:14,16:18)]# dont want species and cover

# 7. join datasets horizontally based on a column name  
dwide <- dwide %>% 
  inner_join(dselect, by = "Date") #join datasets horizontally based on a column name  
# 8. creating a column name for the site
dwide <-  mutate(dwide, Site = "Spud") #applies the same value to all observations
#. creating a column name for the Season
dwide <-  mutate(dwide, Season = "Autumn") #applies the same value to all observations
# dlong <-  mutate(dlong, Season = "Spring") #applies the same value to all observations
#dwide <-  mutate(dwide, Season = "Summer") #applies the same value to all observations
# dlong <-  mutate(dlong, Season = "Winter") #applies the same value to all observations
# 9. setting this data  aside
Spud_Autumn_wide <- dwide
# Mason_Long_wide <- dwide
# Spud_Long_wide <- dwide

# export
setwd("~/Documents/1. PhD/1. Research/3. Calif/CalifData/Version wide/recruit")
Spud_seasonal_wide <- as.data.frame(Spud_seasonal_wide)
write.csv(Spud_seasonal_wide,"Spud_seasonal_wide.csv", row.names = FALSE)


# combine data ####
Coast_long <- Coast_long[,-c(4:5)]
Coast_2w_wide <- Coast_2w_wide[,-23]
Mason_2w_wide <- Mason_2w_wide[,-1]
Spud_2w_wide <- Spud_2w_wide[,-1]

# 10. adding missing column to wide data
Spud_2w_wide <- add_column(Spud_2w_wide, `Molgula manhattensis` = 0, .after = "Metridium senile")
Coast_2w_wide <- add_column(Coast_2w_wide, `Molgula manhattensis` = 0, .after = "Metridium senile")

#11. attach the datsets
Recruit_all <- rbind(Coast_2w,Mason_2w,Spud_2w)

recruitWide <- rbind(Spud_2w_wide,Coast_2w_wide,Mason_2w_wide)
LongAll <- rbind(Coast_long,Mason_long,Spud_long)
Recruit <- rbind(Coast_2w,Mason_2w,Spud_2w)
Spud_seasonal_wide <- rbind(Spud_Autumn_wide, Spud_Spring_wide,Spud_Summer_wide, Spud_Winter_wide)
Spud_seasonal <- rbind(Spud_Autumn, Spud_Spring,Spud_Summer, Spud_Winter)
All_longterm_wide <- rbind(Coast_Long_wide,Mason_Long_wide,Spud_Long_wide)
All_longterm <- rbind(Coast_long, Mason_long, Spud_long)
All_seasonal_wide <- rbind(Coast_seasonal_wide,Mason_seasonal,Spud_seasonal_wide)
Spud_seasonal_wide <- rbind(Spud_Spring_wide, Spud_Summer_wide, Spud_Autumn_wide, Spud_Winter_wide)
Mason_seasonal <- rbind(Mason_Autumn_wide, Mason_Winter_wide, Mason_Spring_wide, Masons_Summer_wide)
#  export ####
LongAll <- as.data.frame(LongAll)
write.csv(LongAll, "LongAll.csv", row.names = FALSE)
write.csv(Coast_long, "Coast_long.csv", row.names = FALSE)
write.csv(Mason_long, "Mason_long.csv", row.names = FALSE)
write.csv(Spud_long, "Spud_long.csv", row.names = FALSE)

setwd("~/Documents/1. PhD/1. Research/3. Calif/CalifData/Version wide/Seasons/Spud")

Recruit <- as.data.frame(Recruit)
write.csv(Recruit,"Recruit.csv", row.names = FALSE)



