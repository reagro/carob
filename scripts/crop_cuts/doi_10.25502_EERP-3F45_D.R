
# R script for "carob"

## ISSUES
# ....



carob_script <- function(path) {
   " 
Description:
Increasing organic matter/carbon contents of soils is one option from a basket of strategies being proposed to offset climate change inducing greenhouse gas (GHG) emissions, under the auspices of the Paris-COP 4 per mille initiative. One of the complimentary practices to sequester carbon in soils on decadal timescales is amending it with biochar, a carbon rich byproduct of biomass gasification. In sub-Saharan Africa (SSA) there is widespread and close interplay of agrarian based economies and the use of biomass for fuel, which makes that the co-benefits of biochar production for agriculture and energy supply are explicitly different from the rest of the world. To date, the quantities of residues available from staple crops for biochar production, and their potential for carbon sequestration in farming systems of SSA have not been comprehensively investigated.
Herein we assessed the productivity and usage of biomass waste from: maize, sorghum, rice, millet and groundnut crops; specifically quantifying straw, shanks, chaff and shells, based on measurements from multiple farmer fields and census/surveys in eastern Uganda

"
   uri <- "doi:10.25502/EERP-3F45/D"
   dataset_id <- carobiner::simple_uri(uri)
   group <- "crop_cuts"
   ## dataset level data 
   dset <- data.frame(
      dataset_id = dataset_id, 
      group=group, 
      project=NA, 
      uri=uri, 
      data_citation="Roobroeck, D., Rebbeca, H.-N., John-Baptist, T., & Jackson, M. (2019). IITA-ADC biochar study Uganda on Groundnut [dataset]. International Institute of Tropical Agriculture (IITA).
      https://doi.org/10.25502/EERP-3F45/D", 
      publication= NA, 
      data_institutions = "IITA", 
      carob_contributor="Cedric Ngakou", 
      carob_date="2023-11-21", 
      data_type="survey"
      
   )
   
   ## download and read data 
   ff  <- carobiner::get_data(uri, path, group)
   js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=0)
   dset$license <- carobiner::get_license(js)
  dset$title <- carobiner::get_title(js)
	dset$authors <- carobiner::get_authors(js)
	dset$description <- carobiner::get_description(js)
   
   bn <- basename(ff)
   ## process file(s)
   
   r <- read.csv(ff[bn=="Groundnut_biomass_sampling.csv"])
   d<- r[,c("Season","Quadrant_ID","Density_plant_m_2","Yield_grain_dry_ton_ha_1","Yield_straw_dry_ton_ha_1")]
   colnames(d)<- c("season","trial_id","plant_density","yield","dmy_residue")
   
   d$dmy_storage <- d$yield * 1000 # in kg/ha 
   
   d$yield <- d$yield * 1000 * 1.18 #fresh weight in kg/ha 
   d$dmy_residue <- d$dmy_residue * 1000 
   
   d$plant_density <- d$plant_density * 10000 # in plant/ha
   
   #add columns
   d$crop<- "groundnut"
   d$dataset_id <- dataset_id
   d$country <- "Uganda"
   ##CN :I used the reverse function on GPS coordinate to obtain the location knowing long and lat coordinate.
   d$location<- "Nakasongola"
   d$on_farm <- FALSE
   d$is_survey <- TRUE
   d$irrigated <- FALSE
   d$inoculated <- FALSE
   d$yield_part <- "grain" 
   ## add long and lat
   d$longitude <- as.numeric(js$result$coverage_y)
   d$latitude <- as.numeric(js$result$coverage_x)
   
   d$planting_date <- c("2016-02", "2017-02")[d$season]
   d$harvest_date <- c("2016-06", "2017-06")[d$season]	
   d$season <- c("Feb-June 2016", "Feb-June 2017")[d$season]
   
   
   # all scripts must end like this
   carobiner::write_files(dset, d, path=path)
   
}


