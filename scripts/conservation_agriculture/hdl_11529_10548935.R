# R script for "carob"

## ISSUES
# 1. There are NA's in yield because I introduced a yield column in barley which 
# was absent.The column was introduced for binding purposes hence there are NA's
# in yield.


carob_script <- function(path) {

"
Six seed treatments were tested for 2 growing cycles (summer of 2021 and 2022) in
a field experiment with maize (Zea mays L.) and barley (Hordeum vulgare L.) under
conservation agriculture in the Mexican highlands, at CIMMYT’s experiment station
of El Batán Texcoco, the State of Mexico, Mexico. The experiment was a randomized
complete block design with 3 replicates, with separate areas for maize and barley.
The six seed treatments included a negative control, a chemical seed treatment 
(different for maize and barley, depending on common practices in the area), 
Trichoderma, Metarhizium, a commercial mixture of plant growth promoting 
rhizobacteria, and a combination of Trichoderma and Metarhizium. Soil and root 
samples were taken at two and three sampling times during the 2021 crop cycle for
barley and maize, respectively. Yield and yield components were determined at the
end of the crop cycle in 2021 and 2022. The soil and root samples were used to measure
root growth (root biomass per core), root colonization with mycorrhizal fungi, root
infection with pathogens (Polymyxa, Pythium, Microdochium), soil microbial communities
in terms of biomarker fatty acids, and ecological guilds of soil nematodes 
(Bacterivores, fungivores, plant parasitic and predators). (2023-08-01)
"

#### Identifiers
	uri <- "hdl:11529/10548935"
	group <- "conservation_agriculture"

#### Download data 
	ff  <- carobiner::get_data(uri, path, group)

##### dataset level metadata 
	dset <- data.frame(
		carobiner::read_metadata(uri, path, group, major=1, minor=1),
		data_institutions = "CIMMYT",
		publication = NA,
		project = NA,
		data_type = "experiment",
		exp_treatment = "chemical seed treatment;Trichoderma;Metarhizium;commercialised mixture;Trichoderma and Metarhizium", 
		carob_contributor = "Blessing Dzuda",
		carob_date = "2024-05-21"
	)
	
##### PROCESS data records

# read data 

	f <- ff[basename(ff) == "DAT-BV234-Database-2021-2022-Micro and yield.xlsx"]
  r <- carobiner::read.excel(f, sheet = "MAIZE yield 2021")
  r2<- carobiner::read.excel(f, sheet = "MAIZE yield 2022")
  r3<- carobiner::read.excel(f, sheet = "BARLEY yied 2021")
  r4<- carobiner::read.excel(f, sheet = "BARLEY yield 2022")
## process file(s)

## select the variables of interest and assign them to the correct name
	d0 <- data.frame(
		    crop=r2$Crop,
		    rep=r2$Rep,
		    treatment=r2$Trt,
		    flowering_days=r$Tasseling,
		    maturity_days=r2$Maturity,
		    plant_height=r2$Height,
		    dmy_storage=r2$Yield_dry,
		    yield=r2$`Yield_12%H2O`,
		    grain_weight=r2$Thou,
		    spike_density=NA,
		    silking_days=r2$Silking,
		    plant_density=r2$`Plants/m²`,
		    trial_id="1",
		    planting_date="2021"
	)
	

	d1 <- data.frame(
	  crop=r$Crop,
	  rep=r$Rep,
	  treatment=r$Trt,
	  flowering_days=r$Tasseling,
	  maturity_days=r$Maturity,
	  plant_height=r$Height,
	  dmy_storage=r$Yield_dry,
	  yield=r$`Yield_12%H2O`,
	  grain_weight=r$Thou,
	  spike_density=NA,
	  silking_days=r$Silking,
	  plant_density=r$`Plants/m²`,
	  trial_id="2",
	  planting_date="2022"
)

	d2 <- data.frame(
	  crop=r3$Crop,
	  rep=r3$Rep,
	  treatment=r3$Trt,
	  flowering_days=r3$Flowering,
	  maturity_days=r3$Maturity,
	  plant_height=r3$Altura,
	  dmy_storage=r3$Yield_Dry,
	  yield=r3$`Yield_12%H2O`,
	  grain_weight=r3$Thou,
	  spike_density=r3$`Spikes/m2`,
	  silking_days=NA,
	  plant_density=NA,
	  trial_id="3",
	  planting_date="2021"
	  
)

	
	d3 <- data.frame(
	  crop=r4$Crop,
	  rep=r4$Rep,
	  treatment=r4$Trt,
	  flowering_days=r4$Flowering,
	  maturity_days=r4$Maturity,
	  plant_height=r4$Altura,
	  dmy_storage=r4$Yield_Dry,
	  yield=r4$`Yield_12%H2O`,
	  grain_weight=r4$Thou,
	  spike_density=r4$`Spikes/m2`,
	  silking_days=NA,
	  plant_density=NA,
	  trial_id="4",
	  planting_date="2022"
	  
)

	d <- rbind(d0,d1,d2,d3)
	
#### about the data #####
## (TRUE/FALSE)
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	d$yield_part <- "grain"

	d$country <- "Mexico"
	d$site <- "El Batán Experimental Station"
	d$adm1 <- "Stae of Mexico"
	d$adm2 <- "Texcoco"
	d$elevation <- "2257"
	d$longitude <- "-98.88056"
	d$latitude <- "19.517"
	
	d$crop <- gsub("Barley","barley", d$crop)
	d$crop <- gsub("Maize","maize", d$crop)
	
	d$rep <- as.integer(d$rep)
	d$treatment <- as.character(d$treatment)
	d$maturity_days <- as.numeric(d$maturity_days)
  d$plant_height <- as.numeric(d$plant_height)
  d$dmy_storage <- as.numeric(d$dmy_storage)
  d$yield <- as.numeric(d$yield)
  d$grain_weight <- as.numeric(d$grain_weight)
  d$silking_days <- as.numeric(d$silking_days)
  d$plant_density <- as.numeric(d$plant_density)
  d$elevation <- as.numeric(d$elevation)
  d$longitude <- as.numeric(d$longitude)
  d$latitude <- as.numeric(d$latitude)
  d$plant_density <- d$plant_density*10000
	
# all scripts must end like this
	carobiner::write_files(path, dset, d)
	
}


