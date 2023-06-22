

# https://data.cimmyt.org/dataset.xhtml?persistentId=hdl:11529/10842
# maize and legumes 

#################################################################################
# Source: Gardian
# Project name: Monitoring and evaluation of the effects over time of conservation 
# agriculture practices on crop yield, soil quality, weeds, pests and diseases
# Link: http://gardian.bigdata.cgiar.org/dataset.php?id=331#!/
## Description: 
# This data set is from a long-term (2010-2016) trial set in sandy soils. 
# The study seeks to monitor and evaluate the effects over time of conservation 
# agriculture (CA) practices on crop yield, soil quality, weeds, pests and diseases. 
# The trial was set as a randomised complete block design with the following treatments: 
# T1: Check plot (CP); traditional farmers practice using the mouldboard plough, 
# maize as a sole crop, no residue retention, stubbles incorporated T2: Direct seeding
# with animal drawn seeder (DSM), maize as a sole crop, residue retention (at a rate of 
# 2.5-3 t ha-1 in the first year, thereafter all crop residues retained) 
# T3: Basin (BAM), maize as a sole crop, residue retention T4: Jab planter (JPM), 
# maize as a sole crop, residue retention T5: Direct seeding with animal drawn seeder 
# (DSMB), biochar incorporated, maize as a sole crop, residue retention T6: Direct 
# seeding with animal drawn seeder (DSMP), maize-pigeon pea (Cajanus cajan) intercropping,
# residue retention T7: Crop rotation A1 (A1M): direct seeding with animal drawn seeder, 
# maize-groundnut rotation (Phase 1), residue retention; Maize- Groundnut T8: Crop rotation A2(A2G): 
# direct seeding with animal drawn seeder, maize-groundnuts rotation (Phase 2), 
# residue retention; Groundnuts- Maize T9: Crop rotation B1 (B1M): direct seeding 
# with animal drawn seeder, maize-sunflower rotation (Phase 1), residue retention; 
# Maize- Sunflower T10: Crop rotation B2 (B2S): 
# direct seeding with animal drawn seeder, maize-sunflower rotation (Phase 2), 
# residue retention; Sunflower- Maize

## Extra material: Protocol LT Trial at DTC 2015.doc and 58103.pdf
#################################################################################
setwd('C:/Users/camila/Google Drive/AfSoifFert/raw_data')
library(openxlsx)
library(raster)

df <- read.xlsx('Gardian/Maize/4_Gardian_Maize/Domboshawa_2010_2016.xlsx',sheet='All Maize yields Domboshawa')[,-c(4,6,9,11)]
colnames(df) <- c('year','country','town_village','plot_no','treatment_code','crop_type','replication',
                  'yield_kg_ha')
# Keep just the treatment with maize sole and no rotation
df <- df[df$treatment_code=='CP'|df$treatment_code=='DSM'|df$treatment_code=='BAM'|df$treatment_code=='JPM'|
           df$treatment_code=='DSMB',]
df$yield_kg_ha <- round(df$yield_kg_ha)
df$latitude <- -17.62
df$longitude <- 31.17
df$soil_type <- 'Gleyic luvisols'
df$kg_N_ha <- 14+68
df$kg_P_ha <- 12.2
df$kg_K_ha <- 11.6
df$clay <- 23
df$pH <- 5.1

df$comments <- 'data collected from paper'
df$nut_response_eval <-'NPK'
df$start_year <- df$year
df$end_year <- NA
df$start_month <- NA
df$end_month <- NA

sm <- as.numeric(summary(df$yield_kg_ha)[4])
df <- df[df$yield_kg_ha>sm,]

############
# Format
############
df$treatment_code <-'Nutrient_response'
df$institute <- 'CIMMYT'
df$block <- NA
df$rotation_system <- NA
df$crop_variety <- NA
df$density <- NA
df$ReadMe_file <- 'ReadMe_Gardian.xlsx'
df$source <- 'Gardian-,Maize'
df$link <- 'http://gardian.bigdata.cgiar.org/dataset.php?id=331#!/'
df$folder_name <- 'Gardian/Maize/4_Gardian_maize/'
df$file_name <- 'Domboshawa_2010_2016.xlsx'
df$extra_file <- '58103.pdf'
df$r_script <- '1_prepare_data/Gardian/Maize/4_Gardian_maize/4_Gardian_maize.R'
df$fertilizer_type <- 'Compound-D'
df$other_nutrients <- NA
df$kg_otherNutrient_ha <- NA
df$district <- NA

df$depth_soil_sample <- NA
df$sand <- NA
df$sand_unit <- NA
df$clay_unit <- NA
df$avail_P <- NA
df$avail_P_unit <- NA
df$soc <- NA
df$soc_unit <- NA
df$trial_type <- 'on_station'

# Reorder
df_4Gardian_maize <- df[,c('trial_type','year','start_month','start_year','end_month','end_year','country',
                           'district','town_village','longitude','latitude','treatment_code','block','replication',
                           'plot_no','rotation_system','crop_type','crop_variety','fertilizer_type',
                           'other_nutrients','nut_response_eval','kg_N_ha','kg_P_ha','kg_K_ha','kg_otherNutrient_ha',
                           'yield_kg_ha','density','soil_type', 'depth_soil_sample',
                           'pH','sand','sand_unit','clay','clay_unit','soc','soc_unit','avail_P','avail_P_unit',
                           'comments','institute','source','link',
                           'folder_name','file_name','r_script','extra_file')]

write.csv(df_4Gardian_maize,'../results/prepare_data/Gardian/Maize/individual/4_Gardian_Maize.csv')

# Map points
# zwe <- getData('GADM',country='ZWE',level=0)
# crdref <- CRS('+proj=longlat +datum=WGS84')
# pts <- SpatialPoints(df[,c('longitude','latitude')], proj4string=crdref)
# df_sp <- SpatialPointsDataFrame(pts, data=df)
# plot(zwe)
# points(df_sp$longitude,df_sp$latitude,pch=20)
