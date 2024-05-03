# R script for "carob"

carob_script <- function(path) {

"Increasing organic matter/carbon contents of soils is one option from a basket of strategies being proposed to offset climate change inducing greenhouse gas (GHG) emissions, under the auspices of the Paris-COP 4 per mille initiative. One of the complimentary practices to sequester carbon in soils on decadal timescales is amending it with biochar, a carbon rich byproduct of biomass gasification. In sub-Saharan Africa (SSA) there is widespread and close interplay of agrarian based economies and the use of biomass for fuel, which makes that the co-benefits of biochar production for agriculture and energy supply are explicitly different from the rest of the world. To date, the quantities of residues available from staple crops for biochar production, and their potential for carbon sequestration in farming systems of SSA have not been comprehensively investigated. Herein we assessed the productivity and usage of biomass waste from: maize, sorghum, rice, millet and groundnut crops; specifically quantifying straw, shanks, chaff and shells, based on measurements from multiple farmer fields and census/surveys in eastern Uganda"


   uri <- "doi:10.25502/EDK6-AC73/D"
   group <- "crop_cuts"
   ff <- carobiner::get_data(uri, path, group)
  
   dset <- data.frame(
		carobiner::read_metadata(uri, path, group, major=2, minor=0),
		project=NA, 
		publication= NA, 
		data_institutions = "IITA", 
		carob_contributor="Cedric Ngakou", 
		carob_date="2023-11-21", 
		data_type="survey"
   )
   
   r <- read.csv(ff[basename(ff)=="Millet_biomass_sampling.csv"])
   d<- r[,c("Season","Quadrant_ID","Density_plant_m_2","Yield_grain_dry_ton_ha_1","Yield_straw_dry_ton_ha_1")]
   colnames(d)<- c("season","trial_id","plant_density","yield","dmy_residue")
   
   d$dmy_storage <- d$yield * 1000 # in kg/ha 
   
   d$yield <- d$yield * 1000 * 1.18 #fresh weight in kg/ha  
   d$dmy_residue <- d$dmy_residue * 1000 
   d$plant_density <- d$plant_density * 10000 # in plant/ha
   
   #add columns
   ## the data says "millet". Given that this is in Uganda, 
   ## presumably this refers to finger millet 
   d$crop<- "finger millet"
   
   d$country <- "Uganda"
   ##CN :I used the reverse function on GPS coordinate to obtain the location knowing long and lat coordinate.
   d$location<- "Nakasongola"
   d$on_farm <- FALSE
   d$is_survey <- TRUE
   d$irrigated <- FALSE
   d$inoculated <- FALSE
   d$yield_part <- "grain" 
   ## add long and lat
#   d$longitude <- as.numeric(js$result$coverage_y)
#   d$latitude <- as.numeric(js$result$coverage_x)
   
   d$planting_date <- c("2016-02", "2017-02")[d$season]
   d$harvest_date <- c("2016-06", "2017-06")[d$season]	
   
   carobiner::write_files(dset, d, path=path)
}


