#####################################################################################################################
# Sound Speed Profile Generator - Read in cnv File Exports from Seabird, extract Depth and Sound Speed Data  
# and generate a .CSV with records at each event number
#
# July 24, 2019
#
#####################################################################################################################


#-------------------------------------------------------------------------------
# LOAD PACKAGES
#-------------------------------------------------------------------------------

# check for any required packages that aren't installed and install them
required.packages <- c("dplyr", "stringr", "ggplot2")
uninstalled.packages <- required.packages[!(required.packages %in% installed.packages()[, "Package"])]
# install any packages that are required and not currently installed
if(length(uninstalled.packages)) install.packages(uninstalled.packages)

# require all necessary packages
lapply(required.packages, require, character.only = TRUE)

# Set working directory, and list all excel files created from Ruskin Exports
setwd("D:/_explorer/Documents")

# clear memory
rm(list=ls())

# output dir for plots 
output_dir <- "D:/_explorer/Output"

# set file ext to ".xlsx"
file_extension <- as.character(".cnv") 

# get list of files in wd matching file extension
files <- list.files(pattern = file_extension)

# Read in all files as seperate data tables
dfs <- lapply(files, FUN = function(file){
  # read in files as dataframes, skip 159 lines(header information)
  df <- read.table(file, skip = 159)
  # retain selected columns
  df <- df[, c(1, 2)]
  # name columns for clarity
  colnames(df) <- c("Depth", "SpeedOfSound")
  # convert columns to numbers (may be read in as factors)
  df$Depth <- as.numeric(df$Depth)
  df$SpeedOfSound <- as.numeric(df$SpeedOfSound)
  # drop records with depth <= 0
  df <- df[df$Depth > 0, ]
  # get file name and add to df as ID column
  df <- mutate(df, ID = str_sub(file, end = -nchar(file_extension) - 1))
  # add column for depth as integer (rounded)
  df <- mutate(df, DepthInt = round(Depth, 0))
  # remove duplicate records based on integer depth value
  df <- df[!duplicated(df$DepthInt), ]
})

# combine dfs into single long df
long_df <- bind_rows(dfs)

# aggregate dataframe based on DepthInt value, taking the mean Speed of sound value where duplicate depths exist between data sources
agg_df <- aggregate(long_df[, c("SpeedOfSound", "DepthInt")], by = list(long_df$DepthInt), FUN = mean)
agg_df$ID <- "Mean"

# create master dataframe 
master_df <- bind_rows(list(long_df, agg_df))

# Plot sound speed for each dive
# plot_name is a character vector for the name of the plot (without extension)
plot_fxn <- function(df, plot_name){
  df %>%
    ggplot(aes(x = DepthInt, y = SpeedOfSound, color = ID)) +
    scale_color_brewer(palette = "Spectral") + 
    geom_line() +
    theme_bw() +
    ylab("Speed of Sound") + 
    xlab("Depth (metres)") + 
  ggsave(file.path(output_dir, paste0(plot_name, ".pdf")), width = 11, height = 8.5)
}

# run plot function
SOS_plot <- plot_fxn(master_df, "depth_speed_of_sound_relationship")

# extport mean as csv with DepthInt column
write.csv(x = master_df[master_df$ID == "Mean", c("DepthInt", "SpeedOfSound")], file = file.path(output_dir, paste0("depth_speed_of_sound_relationship", ".csv")), row.names = FALSE)


