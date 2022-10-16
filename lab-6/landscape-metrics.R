
# Load Packages -----------------------------------------------------------

library(raster)
library(gdal)
library(terra)
library(landscapemetrics)
library(tidyverse)
library(RColorBrewer)


# Load Data ---------------------------------------------------------------

# clipped raster files from Leif
#daneco_2016 <- terra::rast("lab-6/data/DaneCoNLCD2016.tif")
#daneco_2001 <- terra::rast("lab-6/data/DaneCoNLCD2001.tif")

# read data in using the raster package
daneco_2001_orig <- raster::raster("lab-6/data/DaneCoNLCD2001.tif")
daneco_2016_orig <- raster::raster("lab-6/data/DaneCoNLCD2016.tif")

daneco_2001_orig
daneco_2016_orig

# Initial Plots -----------------------------------------------------------

plot(daneco_2001_orig)
plot(daneco_2016_orig)

# Reclassify --------------------------------------------------------------

# https://www.mrlc.gov/data/legends/national-land-cover-database-class-legend-and-description

# Simplify land cover types into water/snow/ice (1), developed/barren (2), vegetation (3), cropland (4)
reclass <- c(11,1,
             12,1,
             21,2,
             22,2,
             23,2,
             24,2,
             31,2,
             41,3,
             42,3,
             43,3,
             51,3,
             52,3,
             71,3,
             72,3,
             73,3,
             74,3,
             81,4,
             82,4,
             90,3,
             95,3
          )
reclass_mat <- matrix(reclass, ncol = 2, byrow = TRUE)

# terra option = terra::classify(daneco_2001_orig,reclass_mat)
daneco_2001 <- raster::reclassify(daneco_2001_orig, reclass_mat)
daneco_2016 <- raster::reclassify(daneco_2016_orig, reclass_mat)

# Landscape Metrics - Given -----------------------------------------------

# https://wisc.pb.unizin.org/consgis/chapter/lesson-2/

# package documentation
# https://cran.r-project.org/web/packages/landscapemetrics/landscapemetrics.pdf
# Levels: l (landscape), c (class), p (patch)

# Check that the data is projected
# Files from Leif use the Albers Conical Equal Area projection 
check_landscape(daneco_2001)
check_landscape(daneco_2016)

## Total number of patches
tot_patches <- data.frame(lsm_l_np(daneco_2001), year = "2001")
tot_patches <- rbind(tot_patches, data.frame(lsm_l_np(daneco_2016), year = "2016"))

## Number of patches per land class
patches_per_class <- data.frame(lsm_c_np(daneco_2001), year = "2001")
patches_per_class <- rbind(patches_per_class, data.frame(lsm_c_np(daneco_2016), year = "2016"))

## Total edge
# Units = meters
tot_edge <- data.frame(lsm_l_te(daneco_2001), year = "2001")
tot_edge <- rbind(tot_edge, data.frame(lsm_l_te(daneco_2016), year = "2016"))

## Patch areas
# units = hectares
patch_area <- data.frame(lsm_p_area(daneco_2001), year = "2001")
patch_area <- rbind(patch_area, data.frame(lsm_p_area(daneco_2016), year = "2016"))
mean_patch_area <- patch_area %>% 
  group_by(year) %>% 
  summarise(avg=mean(value))

## Proportion of like adjacency
# PLADJ is an ’Aggregation metric’. It calculates the frequency how often patches of different classes
# i (focal class) and k are next to each other, and following is a measure of class aggregation. The
# adjacencies are counted using the double-count method.

prop_like_adj <- data.frame(lsm_l_pladj(daneco_2001), year = "2001")
prop_like_adj <- rbind(prop_like_adj, data.frame(lsm_l_pladj(daneco_2016), year = "2016"))

# Landscape Metrics - New -------------------------------------------------

## Patch Cohesion Index
# COHESION is an ’Aggregation metric’. It characterises the connectedness of patches belonging to
# class i. It can be used to asses if patches of the same class are located aggregated or rather isolated
# and thereby COHESION gives information about the configuration of the landscape.
# Range from 0 to 100 (0 = isolated)
# Unit is %s

# rook's case (4 nearest neighbors)
class_cohesion <- data.frame(lsm_c_cohesion(daneco_2001, directions = 4), year = "2001") 
class_cohesion <- rbind(class_cohesion, data.frame(lsm_c_cohesion(daneco_2016, directions = 4), year = "2016"))

## Simpson's Evenness Index
# SIEI is a 'Diversity metric'. The metric is widely used in biodiversity and ecology.
# It is the ratio between the actual Simpson's diversity index and the theoretical maximum
# Simpson's diversity index.
# Ranges from 0 to 1
# Equals SIEI = 0 when only one patch is present and approaches SIEI = 1 when the number
# of class types increases while the proportions are equally distributed

siei <- data.frame(lsm_l_siei(daneco_2001, directions = 4), year = "2001")
siei <- rbind(siei, data.frame(lsm_l_siei(daneco_2016, directions = 4), year = "2016"))

## Shannon's Evenness Index
# SHEI is a 'Diversity metric'. It is the ratio between the actual
# Shannon's diversity index and and the theoretical maximum of the Shannon diversity index.
# It can be understood as a measure of dominance.
# Ranges from 0 to 1
# Equals SHEI = 0 when only one patch present and equals SHEI = 1 when the proportion of
# classes is completely equally distributed

shei <- data.frame(lsm_l_shei(daneco_2001), year = "2001")
shei <- rbind(shei, data.frame(lsm_l_shei(daneco_2016), year = "2016"))


# Visualizations ----------------------------------------------------------

## Compare 2001 and 2016

# Color palette
redylbu <- brewer.pal(name = "RdYlBu", n = 9)
colors <- c(redylbu[2], redylbu[4])
outline <- redylbu[1]

# Custom theme
my_theme <- theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(face = "italic", color = "gray"))

# Total number of patches
ggplot(tot_patches, aes(x = year, y = value, fill = year)) + 
  geom_bar(stat = "identity", color = outline) +
  geom_label(label = tot_patches$value, fill = "white") +
  scale_fill_manual(values = colors) +
  labs(x = "Year",
       y = "Number of Patches",
       title = "Total Number of Patches in Dane County",
       subtitle = "Average patch area in 2001: 21.8 hectares\nAverage patch area in 2016: 20.6 hectares") +
  guides(fill = "none") +
  my_theme
  
# Number of patches per land class
ggplot(patches_per_class, aes(x = factor(class), y = value, fill = year)) + 
  geom_bar(stat = "identity", position = "dodge", color = outline) +
  scale_x_discrete(labels = c("1" = "Water", "2" = "Developed", "3" = "Vegetation", "4" = "Cropland")) +
  scale_fill_manual(values = colors, name = "Year") +
  labs(x = "Land Cover Class",
       y = "Number of Patches",
       title = "Number of Patches Per Land Cover Class in Dane County") +
  my_theme

# Total edge
ggplot(tot_edge, aes(x = year, y = value/1000, fill = year)) + 
  geom_bar(stat = "identity", color = outline) +
  geom_label(label = round(tot_edge$value/1000,2), fill = "white") +
  scale_fill_manual(values = colors) +
  labs(x = "Year",
       y = "Edge Length (km)",
       title = "Total Edge Length of Patches in Dane County") +
  guides(fill = "none") +
  my_theme

# Proportion of like adjacency
ggplot(prop_like_adj, aes(x = year, y = value, fill = year)) + 
  geom_bar(stat = "identity", color = outline) +
  geom_label(label = round(prop_like_adj$value, 2), fill = "white") +
  scale_fill_manual(values = colors) +
  labs(x = "Year",
       y = "Proportion of Like Adjacency (%)",
       title = "Proportion of Like Adjacency for Patches in Dane County") +
  guides(fill = "none") +
  my_theme

# Patch cohesion index
ggplot(class_cohesion, aes(x = factor(class), y = value, fill = year)) + 
  geom_bar(stat = "identity", position = "dodge", color = outline) +
  scale_x_discrete(labels = c("1" = "Water", "2" = "Developed", "3" = "Vegetation", "4" = "Cropland")) +
  scale_fill_manual(values = colors, name = "Year") +
  labs(x = "Land Cover Class",
       y = "Class Cohesion Index (%)",
       title = "Class Cohesion Indices for Patches in Dane County") +
  guides(fill = "none") +
  my_theme


# SEIE
ggplot(siei, aes(x = year, y = value, fill = year)) + 
  geom_bar(stat = "identity", color = outline) +
  geom_label(label = round(siei$value, 2), fill = "white") +
  scale_fill_manual(values = colors) +
  labs(x = "Year",
       y = "Simpson's Evenness Index",
       title = "Simpson's Evenness Index for Dane County") +
  guides(fill = "none") +
  my_theme

# SHEI
ggplot(shei, aes(x = year, y = value, fill = year)) + 
  geom_bar(stat = "identity", color = outline) +
  geom_label(label = round(shei$value, 2), fill = "white") +
  scale_fill_manual(values = colors) +
  labs(x = "Year",
       y = "Shannon's Evenness Index",
       title = "Shannon's Evenness Index for Dane County") +
  guides(fill = "none") +
  my_theme

