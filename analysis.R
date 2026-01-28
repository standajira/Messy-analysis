# Forest Analysis Script
# Author: Dr. Smith
# Date: ???

# Load packages
library(ggplot2)
library(here)
library(dplyr)

# Read the data (MAKE SURE THIS PATH WORKS ON YOUR MACHINE!)

forest_raw_data <- read.csv(here::here("Data/input/forest_inventory_data.csv"))
environmental_measurements_raw <- read.csv(here::here("environmental_measurements_backup_old.csv"))

# Quick check
print(dim(forest_raw_data))
print(dim(environmental_measurements_raw))

# Calculating average DBH and Height
average_DBH_cm <- mean(trees$dbh_cm)
avgerage_trees_height_m <- mean(trees$height_m)

# creating a new column in the tree table and calculating the width of trunk
trees_mutated<-trees %>%
  dplyr::mutate(dbh_cm/2) %>%
  trees_mutated^2*3.14159

# Check species
table(trees$tree_species)

# Merge datasets (IMPORTANT: Don't lose any data!)
trees <- merge(trees, env, by="plot_id")

trees_environmental_data_merged <-
  dplyr::summarise(trees_mutated, environmental_measurements_raw)

# Calculate volume (using simplified formula)

trees_environmental_data_merged<-trees_environmental_data_merged %>%
  dplyr::mutate(volume = trees_environmental_data_merged$ba * trees_environmental_data_merged*avgerage_trees_height_m*0.5)


# Some summary stats
summary(trees$volume)

# Calculate per-plot summaries (THIS PART IS CRITICAL!)
PlotSummary <- aggregate(cbind(dbh_cm, height_m, volume) ~ plot_id, data=trees, FUN=mean)
PlotSummary$TreeCount <- aggregate(dbh_cm ~ plot_id, data=trees, FUN=length)$dbh_cm

print(PlotSummary)

# Save results
write.csv(trees, "forest_data_processed.csv", row.names=FALSE)
write.csv(PlotSummary, "plot_summary_stats.csv")

# VISUALIZATION (TO BE IMPROVED)
plot(trees$dbh_cm, trees$height_m)

print("Analysis complete!")

library(usethis)
usethis::use_github()





##tryout



