# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"
The data represent 804 cropping events of maize planting in Córdoba - Colombia from 
2013 to 2016. Each cropping event has information on climate, soil, and agronomic 
management. The data was collected with the aim of determining the factors that 
affect the variation in the yield of maize. The dataset was created through a 
collaboration project among International Center for Tropical Agriculture CIAT,
the Colombian National Cereals, and Legumes Federation (FENALCE) and the Colombian 
Ministry of Agriculture and Rural Development (MADR).
"

#### Identifiers
	uri <- "doi:10.7910/DVN/LSDQ5C"
	group <- "conservation_agriculture"

#### Download data 
	ff  <- carobiner::get_data(uri, path, group)

##### dataset level metadata 
	dset <- data.frame(
		# change the major and minor versions if you see a warning
		carobiner::read_metadata(uri, path, group, major=1, minor=0),
		data_institutions = "International Center for Tropical Agriculture - CIAT",
		publication= NA, 
		project=NA,
		data_type= "experiment",
		carob_contributor= "Shumirai Manzvera",
		carob_date="2024-04-02"
	)
	
##### PROCESS data records

# read data 

	f <- ff[basename(ff) == "Dataset_Maize_Cordoba.csv"]
	r <- read.csv(f)
	# or  r <- carobiner::read.excel(f)

## process file(s)

## use a subset
	d <- data.frame(crop="maize",planting_date=r$Planting_Date, harvest_date=r$Harvest_Date,
	                variety=r$Cultivar,previous_crop=r$Former_Crop,variety_type=r$Cultivar_Type,
	                soil_texture=r$Soil_Texture,yield=r$Yield*1000,
	                N_fertilizer=r$Total_N*1000,P_fertilizer=r$Total_P*1000,K_fertilizer =r$Total_K*1000,
	                soil_pH=r$pH,trial_id="1")
	
	#Algodon=cotton,Frijol=kidney bean,Maiz=maize,Pastos=pasture,Yuca=cassava
	d$previous_crop<- carobiner::replace_values(d$previous_crop, c("Algodon", "Frijol", "Maiz","Pastos","Yuca"), c("cotton", "kidney bean", "maize","pasture","cassava"))			

	
#### about the data #####
## (TRUE/FALSE)
	d$on_farm <- TRUE


##### Location #####
## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
## you can use carobiner::fix_name()
	d$country <- "Colombia"
	d$site <- "Córdoba"

## each site must have corresponding longitude and latitude
## see carobiner::geocode
	d$longitude <- -75.57405
	d$latitude <- 8.049293


##### Time #####
## time can be year (four characters), year-month (7 characters) or date (10 characters).
## use 	as.character(as.Date()) for dates to assure the correct format.
	#d$planting_date <- as.character(as.Date(d$planting_date))
	#d$harvest_date  <- as.character(as.Date( d$harvest_date))

  d$planting_date<-	as.Date(d$planting_date, format = '%m/%d/%Y')
	d$harvest_date <-	as.Date(d$harvest_date, format = '%m/%d/%Y')
	
	d$planting_date <- as.character(as.Date(d$planting_date))
	d$harvest_date  <- as.character(as.Date( d$harvest_date))
	
	d$yield_part <- "grain"
	
# all scripts must end like this
	carobiner::write_files(path, dset, d)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# path <- _____
# carob_script(path)

