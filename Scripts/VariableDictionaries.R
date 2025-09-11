# Dataset dictionary ------------------------------------------------------
# all the files have weird names so I need to define them

# where did the data come from
source_dictionary <- list(
  "Clemente_Data" = c("Annett_Kangaroo", "Annett_Possum", "Clemente_Echidna", "Clemente_Impala", "DiCicco_Perentie", "Sparkes_Koala", "Galea_Cat")
)

# name of the data file
dataset_dictionary <- list(
  "Wanja_Fox" = "wanja_fox.csv",
  "Nuijten_BewickSwans" = "Biotelemetry of Bewick's swans-acceleration.csv",
  "Khaewphakdee_FishingCat" = "Fishing cat (Prionairulus viverrinus) study in Khao Sam Roi Yot Thailand-acc.csv",
  "Schweitzer_WoodStork" = "NC Wood Stork Tracking-acceleration.csv",
  "Schloesing_FruitBat" = "Hammer-headed fruit bats (Hypsignathus monstrosus) in the Republic of Congo-acc.csv",
  "Dickinson_Goat" = "Goat10hz_processed.csv",
  "Dickinson_Ibex" = "Ibex10hz_processed.csv",
  "Chimienti_Razorbills" = "RAZO.txt",
  "Chimienti_Guillemots" = "COGU.txt",
  "Dunford_Cat" = "Dunford_et_al._Cats_calibrated_data.csv",
  "Isbell_OliveBaboon" = "Leopards vervets and baboons in Laikipia Kenya_part-acc.csv",
  "Kayes_Coatis" = "Coatis on BCI Panama (data from Powell et al.)-acceleration.csv",
  "Kays_Toucan" = "Toucan movement and seed dispersal, Gamboa, Panama (data from Kays et al. 2011)-acc.csv",
  "Rautiainen_Reindeer" = "acceleration.csv"
)

# sampling frequency
frequency_dictionary <- list(
  "Wanja_Fox" = 33.3,
  "Nuijten_BewickSwans" = 20, # bursts of 2 sec
  "Schweitzer_WoodStork" = 10.5,
  "Schloesing_FruitBat" = 6.7, # burst during 15s every 2 min
  "Dickinson_Goat" = 10,
  "Dickinson_Ibex" = 10,
  "Chimienti_Razorbills" = 25,
  "Chimienti_Guillemots" = 25,
  "Dunford_Cat" = 40,
  "Isbell_OliveBaboon" = 10.5, # bursts 3s/min
  "Rautiainen_Reindeer" = 10, # 8g
  "Seriyes_Bobcat" = 10, # 5-min intervals when the animal was moving, and at 3-h intervals when the animal was at rest
  "Acacio_Stork" = 1, # 9 consecutive GPS and acceleration fixes at 1 Hz every 20 min during daylight
  "Minasandra_Hyena" = 25,
  "Annett_Kangaroo" = 50,
  "Clemente_Echidna" = 100,
  "Kayes_Coatis" = 10.5,
  "Khaewphakdee_FishingCat" = 10,
  "Galea_Cat" = 50,
  "DiCicco_Perentie" = 50,
  "Annett_Possum" = 50
)

# "Kays_Toucan" # 50 s every 3 min

