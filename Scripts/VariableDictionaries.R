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
  "Rautiainen_Reindeer" = "acceleration.csv",
  "Mauny_Goat" = "Mauny_Goat_reformatted.csv",
  "Studd_Squirrel" = "Studd_Squirrel_reformatted.csv",
  "Ladds_Seal" = "Ladds_Seal_reformatted.csv",
  "Smit_Cat" = "Smit_Cat_reformatted.csv"
)


dataset_variables <- fread("Dataset_Variables.csv")
frequency_dictionary <- dataset_variables$Frequency

mass_dictionary <- dataset_variables$Log_Mass

