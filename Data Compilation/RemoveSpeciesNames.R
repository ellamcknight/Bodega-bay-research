######    # Editing species names -  Marc's fouling community 

# Create a vector to replace species names that are  wrong and another with corresponding ones
spp = c("Schizoporella", "Mytilus ", "mytilus ", "Ascidian #2", "Ciona savigniyi", "Metridium ", " Metridium senile","Metrdium ", "Mytilus gallo/tros.", "Didemnum ", "Didemnum     ","Balanus crenatus ", "2 yellow dots and black eyespot", "Ascidia ceretodes", "Ascidian 2", "B. gracilis", "Balanus crenautus", "Botryllid, clear with white and black specks (diegensis?)", "Botryllids [Botrylloides + SUM(B. violaceous + B. diegensis + Botryllus schlosseri + unknown)]", "Botrylloides", "Botrylloides diegensis", "Botrylloides sp.", "Botrylloides spp.", "Botrylloides violaceous", "Botryllus schlosseri", "Botryllus sp.", "Botryllus sp. ", "Botryllus spp.", "Bowerbankia gracilis", "Bugula californica", "Bugula neritina", "Celleporella hyalina", "Chaliluna losanoffi", "Ciona intestinalis", "Ciona savignyi", "corinactus", "Cryptosula", "Didemnum", "Didemnum albidum", "Didemnum lahillei", "Diplosoma", "Diplosoma albidum", "Diplosoma listerianum", "Distaplia ", "Distaplia occidentalis", "Entodesma navicula", "Halichondria", "Halichondria sp", "Haliclona", "Haliclona permolis", "hydroides", "Hydroides", "Leucoselenia spp.", "Lichenopora", "listed above as B. diag", "Metrdium", "Metridium", "metridium senile", "Metridium senile", "Metridium senile senile", "mogula like", "Mogula mahattensis", "Mogula Manhattensis", "Mogula manhattensis", "Mogula manhattensis (?)", "Molgula manhattensis (?)", "Molgula manhattensis", "Mytilis gallo/tros.", "mytilus", "Mytilus", "Mytilus gallo/tros.", "Mytilus gallo/tros", "Mytilus galloprovincialis", "Obelia", "Obelia longissima", "Riterella", "Riterella pulchra", "Schizoporella ", "Schizoporella unicornis", "spiorbid", "spirobid", "Spirobid", "spirorbid", "Spirorbid", "Spirorbis", "sponge", "Sponge", "Styela clava", "Styela Clava", "Styela montereyensis", "Styela truncata", "Styela tunicata", "tall purple sponge", "Watersipora subtorquata", "White Sponge", "White sponge", "yellow mat sponge", "Yellow mat sponge", "Yellow sponge")
fam = c("Schizoporella unicornis", "Mytilus sp.", "Mytilus sp.", "Ascidia ceratodes", "Ciona sp.", "Metridium senile", "Metridium senile", "Metridium senile", "Mytilus sp.", "Didemnum vexillum", "Didemnum vexillum", "Balanus crenatus", "Didemnum vexillum", "Ascidia ceratodes", "Ascidia ceratodes", "Bowerbankia gracilis", "Balanus crenatus", "Botrylloides diegensis", "Botrylloides violaceous", "Botrylloides diegensis", "Botrylloides diegensis", "Botrylloides diegensis", "Botrylloides violaceous", "Botrylloides violaceous", "Botryllus schlosseri", "Botryllus schlosseri", "Botryllus schlosseri", "Botryllus schlosseri", "Bowerbankia gracilis", "Bugula californica", "Bugula neritina", "Celleporella hyalina", "Sponge", "Ciona sp.", "Ciona sp.", "Corynactis californica", "Cryptosula pallasiana", "Didemnum vexillum", "Didemnum vexillum", "Didemnum vexillum", "Diplosoma listerianum", "Diplosoma listerianum", "Diplosoma listerianum", "Distaplia occidentalis", "Distaplia occidentalis", "Entodesma navicula", "Sponge", "Sponge", "Sponge", "Sponge", "Hydroides sp", "Hydroides sp", "Sponge", "Lichenopora sp.", "Botrylloides diegensis", "Metridium senile", "Metridium senile", "Metridium senile", "Metridium senile", "Metridium senile", "Molgula manhattensis", "Molgula manhattensis", "Molgula manhattensis", "Mogula manhattensis", "Mogula manhattensis", "Mogula manhattensis", "Molgula manhattensis", "Mytilus sp.", "Mytilus sp.", "Mytilus sp.", "Mytilus sp.", "Mytilus sp.", "Mytilus sp.", "Obelia dichotoma", "Obelia dichotoma", "Ritterella pulchra", "Ritterella pulchra", "Schizoporella unicornis", "Schizoporella unicornis", "Spirobranchus sp.", "Spirobranchus sp.", "Spirobranchus sp.", "Spirobranchus sp.", "Spirobranchus sp.", "Spirobranchus sp.", "Sponge", "Sponge", "Styela clava", "Styela clava", "Styela montereyensis", "Styela truncata", "Styela truncata", "Sponge", "Watersipora subtorquata", "Sponge", "Sponge", "Sponge", "Sponge", "Sponge")

## 2 loops to create a new variable that refers to the changed names of the species
##  First create a new 'Species.new' varibale that will be the same as Species but with characters
d$Species.new = as.character(d$Species)
### the loop begins. 'for (i ...)' - reads from 1 to the end of file 'd'. 'for (j ...)' - reads 'spp'. 'if (...' - If the new 
#variable contains a species from the 'spp' list, then assign it to 'fam'. The 'i' or 'j' could be any letter
for( i in 1:nrow(d) ) {
  for( j in 1:length(spp) ) {
    if( d$Species.new[i] == spp[j] ) d$Species.new[i] = fam[j]
  }}

## This is a test to see if the loop has worked properly
i=1;j=6
if( d$Species.new[i] == spp[j] ) d$Species.new[i] = fam[j]

##  Make a list of the two variables for comparison
d[,c("Species","Species.new")]


# omit species you don't want
d = d[ !d$Species.new %in% c("calcareous tube ",  "Unknwn Brwn Alga", "Filamentous Red algae", "tube worm ", "Aurelia",
                             "Celleporella hyalina", "unkwn. Solitary tunicate", "tube building amphipods", "Entodesma navicula", 
                             "Hydroides sp", "corinactus", "Corynactis californica", "Unknown Botryllus ", "Clam ", "Diatoms (Obelia longissima)",
                             "Ulva sp. ", "Uknwn Recruit", "Yellow Mat Sponge", "Red algae", " tube worm", "Unkwn. Filamentous Red Algae", 
                             "Tonicella", "Mopalia", "Mopalia spp.", "Unknwn recruit", "Sirorbis", "Unkwn Brwn Alga (Laminarian?)",
                             "Nudibranch egg case", "aeolid nudibranch", "ALC", "algae", "Alpidum solidum", "anemone", "anemone (white)", 
                             "anthazoan", "Anthozoan", "anthozoan", "Archedistoma", "archidistoma", "Archidistoma", "Ascidian blob", 
                             "Ascidiella aspersa (?)", "Aurelia ", "Avicularia",  "Bare/diatoms only", "Bivalve", "bivalve", 
                             "black residue", "Branching red algae", "brown ball", "brown sponge", "Bry (?)", "Bubble Algae", "byssal thread", 
                             "Cal. tube worm", "Cal. Tube worm", "calcareous tube worm", "Calcareous Tube worm", "Calcareous tube worm", 
                             "calcareous tube", "Calcareous tube", "calcareous tube worm", "calcarous tube worm", "Calcarous tube worm", 
                             "caprellids", "chiton", "Chiton", "CKK", "clam", "Clam", "clear with yellow orange blob", "clear/black blob", 
                             "clear/transparent solitary", "Cyclostome", "Dead watersipora", "Dendronotus", "Diatom", "diatoms", "Diatoms", 
                             "Diatoms (Plate)", "Diatoms (plate)", "diatoms (plate)", "diatoms (Obelia longissima)", "Dorid nudibranch", 
                             "E. ritteri", "egg case", "Egg case", "egg cases", "egg case (nudibranch)", "egg case (spiral)", "egg case, curly", 
                             "Egg string", "Eggs", "eggs", "empty tube", "Enteromorpha ", "filamentous algae", "filamentous green algae", 
                             "Filamentous green algae", "Filamentous Green Algae", "Filamentous Red", "filamentous red", "filamentous red alga", 
                             "Filamentous red alga", "Filamentous Red Alga", "filamentous red algae", "Filamentous red algae", 
                             "filamentous red thing", "flash/unknown", "flatworm", "Flatworm", "Flat worm", "golden flying saucer", 
                             "golden trapezoids", "green algae", "green algae (small)", "green flatworm", "Hermassinda", "HYD", "JMA", 
                             "Juv. Chiton", "Kamptozoa?", "Kamptozoa", "KCS", "Laminaria", "Leucoselenia", "limpet", "Limpet", "little white blob", 
                             "littorina", "Littorina", "mogula? Looks like spicules but not a sponge, white, infection on plate", "Mopalia", "Mopalia sp.", "mud tubes", 
                             "Nudibranch", "nudibrach egg case", "nudibrach eggs", "Nudibranch eggs", "nudibranch eggs", "Nudibranch egg case", 
                             "Nudibranch (Hermassinda)", "orange stripes of pigment spots", "orange blob", "orange \"stripes of pigment spots", "orange blob (young Didemnum?)", "orange circle with dip in middle", "orange clear blob with white speckles", "orange clear blob with white speckles (Aplidium spp?)", 
                             "orange clear blob with white speckles very small, Aplidium spp?", "orange sponge", "orange/white blob", "parchment worm", 
                             "peach ball", "Peach colored thing with nubs", "pink flatworm", "pink/orange unkwn ascidian, small in size", "Prionitis", "protozoa (blue)", 
                             "pseudoeros?", "purple blob (photo taken)", "red alga sp.", "red algae", "Red Algae", "Red algae (blade)", "red filamentous alga", 
                             "red flat squish thing", "Red Paper-like alga", "Sabellid", "same as unknown above, butg with heavier pigment on siphon", "sea urchin (baby)", 
                             "small clear eggs", "small unknown Ascidia 1", "small unknown Ascidia 2", "small white ascidian", "small white chiton", "Small yellow ascidian", 
                             "small yellow ascidian", "small yellow egg mass (sheet)", "squat, tall but small Botryllid, listed above as B. diag", 
                              "Stolonate", "SYA", "transparent circular tunic with ball inside", "tube-building amphipod", "tube-building amphipods", "tube dwelling annelid", "Tube worm", "tube worm", "Tubeworm", "tubeworm", 
                             "two clear siphons, two orange spots with black streaks at base, some white speckles", "two yellow spots", "Uknwn. Solitary tunicate", "Ulva", "ulva", "Ulva sp.", "Ulva spp.", 
                             "unid. 5-zooid animal", "Unidentifiable recruits", "unidentified hydroid", "unk 1", "unk ascidia", "unk ascidian", "unk bryozoan", "unk bryozoan ", "unk floppy, no specks", "unk hydroid", 
                             "unk hydroid (sertularia?)", "unk speckled B.", "unk y ascidian (Diplosoma listerianum?)", "unk. 1", "unk. #1", "Unk. 1", "Unk. #1", "Unk. 4", "Unk. #4", "unk. 4", "unk. #4", 
                             "unk. White tentacles", "unkn hydroid (Sertularia spp.?)", "unkn red stalk (Disptaplia shaped)", "Unknown", "unknown", "Unknown (bowl with tentacles)", "unknown (floppy)", 
                             "unknown (smooth yellow sphere)", "unknown * pic", "unknown 2", "unknown #2","unknown #3?", "unknown ascidian", "unknown ascidian (blue specked tunic)", "unknown bivalve", 
                             "Unknown blob w/ flashing sparlkles", "unknown botrylloides", "Unknown Botryllus", "unknown bryozoan", "unknown Bugula like bryozoan", "unknown II", "Unknown purple ascidian", 
                             "Unknown Recruit", "Unknown recruit", "unknown recruit", "unknown red", "unknown squished", "unknown yellow", "unknown, clear with yellow faint brown blob at base, distinctive white spots, concentrated in a ring around siphon, possibly Distaplia occidentalis but older?", 
                             "unknown, clear with yellow faint brown blob at base, distinctive white spots, concentrated in a ring around siphon, possibly distaplia occidentalis but older?", 
                             "unknown, starry pattern, took pic", "unknown, tiny pink opaque with few white speckles, fits in grooves on plate", "Unknwn Brwn Alga ", "Unknwn Brwn Alga (Laminarian?)", 
                             "unknwn chiton", "unknwn polyp", "unknwn recruit", "unknwn rust/wht col. Tunicate", "unknwn. Solitary tunicate", "Unknwn. Solitary tunicate", "Unknwn. Tube Worm", "Unkwn chiton", 
                             "Unkwn. Chiton", "Unkwn Limpet", "unkwn polyp", "Unkwn recruit", "very small orange blob", "white (diplo or didemn?)", "White blob", "white blob", "white blob surrounded by transparent diamond shapes", 
                             "white coiled eggs (nudibranch eggs?)", "white milky solitary", "white tentacles", "WMA", "worm tube (mud)", "WSOYD", "WTWYI", "wwty1", "WWTYI", "yellow (diplo or didemn?)", 
                             "yellow asc. (probably D. vex)", "yellow ascidian (dista?)", "yellow ascidians", "yellow blob", "yellow blob with black eyespots (young Didemnum?)", 
                             "yellow bryozoan like Schizoporella unicornis", "yellow bryoszoan like Schizoporella unicornis", "yellow dot two eyespots", "yellow elongate blob with two eyespots (not mobile)", 
                             "yellow flatworm", "yellow flying saucer", "yellowy blob", "byssal threads", "Enteromorpha", "Chaliluna losanoffi", "Halichondria sp", "Halichondria", "Haliclona permolis", 
                             "Haliclona", "Leucoselenia spp.", "sponge", "Sponge", "tall purple sponge","unid. \"5-zooid\" animal", "White Sponge", "White sponge", "yellow mat sponge", "Yellow mat sponge", "Yellow sponge", "Cal. Tube worm ", 
                             "White Sponge ", "Halichondria sp.", "Ulva sp."), ]

d$Species.new <-  as.factor(d$Species.new)
levels(d$Species.new)
levels(d$Species.new)[levels(d$Species.new)=="bare"] <- "Bare"
d$Species.new <- recode_factor(d$Species.new, 'Mogula manhattensis' = "Molgula manhattensis")
d$Species.new <- recode_factor(d$Species.new, 'Riterella Pulchra' = "Ritterella pulchra")

# now go back to Cali_pivot_mutate.r

library(tidyverse)
# Create a variable in data with a conditional statment 'if variable is == "no", is variable is not != "yes"
#coast <- d %>% mutate(feedgroup = case_when(Species.new == "Metridium senile" ~ "Predator", Species.new != "Metridium senile" ~ "Filter feed"))

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
                                           Species.new == "Styela truncata" ~ "Filter feed", Species.new == "Bare" ~ "Bare Space"))


#adding taxa
d <- d %>% mutate(taxa = case_when(Species.new == "Ascidia ceratodes" ~ "Tunicata", Species.new == "Balanus crenatus" ~ "Crustacea",
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
                                           Species.new == "Styela truncata" ~ "Tunicata", Species.new == "Bare" ~ "Bare Space"))
#ading name
d <- d %>% mutate(name = case_when(Species.new == "Ascidia ceratodes" ~ "Ascidian", Species.new == "Balanus crenatus" ~ "Barnacle",
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
                                           Species.new == "Styela truncata" ~ "Ascidian", Species.new == "Bare" ~ "Bare Space"))
#ading form
d <- d %>% mutate(form = case_when(Species.new == "Ascidia ceratodes" ~ "Solitary", Species.new == "Balanus crenatus" ~ "Solitary",
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
                                           Species.new == "Styela truncata" ~ "Solitary", Species.new == "Bare" ~ "Bare Space"))

#ading structure
d <- d %>% mutate(structure = case_when(Species.new == "Ascidia ceratodes" ~ "Erect", Species.new == "Balanus crenatus" ~ "Erect",
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
                                           Species.new == "Styela truncata" ~ "Erect",Species.new == "Bare" ~ "Bare Space"))
#ading status
d <- d %>% mutate(status = case_when(Species.new == "Ascidia ceratodes" ~ "Native", Species.new == "Balanus crenatus" ~ "Native",
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
#check levels of species
d$Species.new <-  as.factor(d$Species.new)
levels(d$Species.new)
# Rename by name: change "bare" to "Bare"
levels(coast$Species.new)[levels(coast$Species.new)=="bare"] <- "Bare"


# export
write.csv(coast,"Spud Point Winter V3.csv")






