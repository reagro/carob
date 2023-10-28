# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"Description:

    [A field experiment was conducted in maize under medium-term conservation agriculture (CA) based maizewheat system at BISA-CIMMYT, Ladhowal, Punjab during kharif 2019 to assess the effect of CA+ practices (CA with sub-surface drip irrigation) with variable N doses on maize. The CA+ treatments were residue retained (WR) permanent bed (PB) with sub-surface drip fertigation (PB-SSD): without N (N0), 120 kg N/ha,150 kg N/ha applied in 4-equal (Eq) and differential splits (Df); CA alone treatment includ PB furrow irrigation with 120 kg N/ha (PBWRFurrow- N120); conventional tillage (CT) involved furrow irrigation with 120 kg N/ha (CTWOR-Furrow-N120) and other treatments were residue removed (WOR) PB: PBWOR-without N (N0), with 120 kg N/ha, and 150 kg N/ha applied in four Eq-splits and Df-splits. The findings of the present experiment showed that the numerical value of yield attributing characters were higher under CA+ plots as compared to CA alone (PBWR-Furrow-N120) and CT (CTWOR-Furrow-N120). Biological yield of maize was significantly higher in all CA+ plots as compared to CA alone and CT plots. Highest biological yield was recorded under PBWR-SSD-N150 Df (23.45 t/ha). Highest no. of cobs (72800/ha), no. of grains/cob (605) and cob length (22.61cm) along with dry matter resulted highest biological yield in PBWR-SSD-N150 plots. The grain N content remained statistically similar across all the N management plots, but in case of total N uptake, PBWR-SSD-N150 Df (CA+) plots dominated due to higher biomass. Besides, CA+ based PBWR-SSD-N120 (average of Df and Eq) registered 23-24% higher total N uptake than CA alone (PBWRFurrow- N120) and conventional (CTWOR-Furrow-N120) plots. Improved agronomic N use-efficiency was also recorded under CA+ plots as compared to CA alone (36.4 kg/kg N) and CT (36.7 kg/kg N) plots. (2021-02-12)]

"

	uri <- "hdl:11529/10548767"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "conservation_agriculture"
	## dataset level data 
	dset <- data.frame(
		dataset_id = dataset_id,
		group=group,
		project=NA,
		uri=uri,
		data_citation="Patra, Kiranmoy; Parihar, CM; Nayak, HS; Rana, Biswajit; Singh, VK; Krishnan, P.; Pandey, Renu; Mandal, B.N.; Rathi, N.; Meena, B.R.; Singh, L.K.; Sidhu, HS; Jat, ML, 2022, Crop performance and nitrogen use-efficiency in maize under conservation agriculture coupled with sub-surface drip fertigation, https://hdl.handle.net/11529/10548767, CIMMYT Research Data & Software Repository Network, V1",
		## if there is a paper, include the paper's doi here
		## also add a RIS file in references folder (with matching doi)
		publication= NA,
		data_institutions = "CIMMYT",
   		data_type="on-farm experiment", # or, e.g. "on-farm experiment", "survey", "compilation"
		carob_contributor="Hope Mazungunye",
		carob_date="2023-09-15"
	)

## download and read data 
path <- "C:/carob"
	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=3)
	dset$license <- carobiner::get_license(js)


	f <- "C:/carob/data/raw/maize_trials/hdl_11529_10548767/IJAS_TABLE_REPLICATED_DATA.xlsx"

	r <- readxl::read_excel(f, sheet= "Yield and Attributing Character")|> as.data.frame()
	r1 <- readxl::read_excel(f, sheet= "N Data (NUE, Uptake)")|> as.data.frame()

	
## process file(s)

## use a subset
	d <- r
 d1<- r1
	
#### about the data #####
## (TRUE/FALSE)

	d$dataset_id <- dataset_id
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$is_experiment <- TRUE
	d$irrigated <- FALSE
## the treatment code	
	d$treatment <- d$Treatment
	
	d1$dataset_id <- dataset_id
	d1$on_farm <- TRUE
	d1$is_survey <- FALSE
	d1$is_experiment <- TRUE
	d1$irrigated <- FALSE
	## the treatment code	
	d1$treatment <- d$Treatment

##### Location #####
## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
## you can use carobiner::fix_name()
	d$country <- "India" 
	d$site <- NA
	d$adm1 <- NA
	d$adm2 <- NA
	d$adm3 <- NA
	d$elevation <- 229
	
	d1$country <- "India" 
	d1$site <- NA
	d1$adm1 <- NA
	d1$adm2 <- NA
	d1$adm3 <- NA
	d1$elevation <- 229
## each site must have corresponding longitude and latitude
## see carobiner::geocode
	d$longitude <- 30.99
	d$latitude <- 75.44
	d1$longitude <- 30.99
	d1$latitude <- 75.44

##### Crop #####
## normalize variety names
## see carobiner::fix_name
	d$crop <- "maize"
	d1$crop <- "maize"
	d$variety <- NA
	d1$variety <- NA

##### Time #####
## time can be year (four characters), year-month (7 characters) or date (10 characters).
## use 	as.character(as.Date()) for dates to assure the correct format.
	d$planting_date <- NA
	d1$planting_date <- NA
	d$harvest_date  <- NA
	d1$harvest_date  <- NA

##### Fertilizers #####
## note that we use P and K, not P2O5 and K2O
## P <- P2O5 / 2.29
## K <- K2O / 1.2051
   d$P_fertilizer <- NA
   d$K_fertilizer <- NA
   d$N_fertilizer <- NA
   d$S_fertilizer <- NA
   d$lime <- NA
   
   d1$P_fertilizer <- NA
   d1$K_fertilizer <- NA
   d1$N_fertilizer <- NA
   d1$S_fertilizer <- NA
   d1$lime <- NA
## normalize names 
   d$fertlizer_type <- NA
   d$inoculated <- FALSE
   d$inoculant <- NA
     
     d1$fertlizer_type <- NA
   d1$inoculated <- FALSE
   d1$inoculant <- NA
   
##### in general, add comments to your script if computations are
##### based on information gleaned from metadata, a publication, 
##### or when they are not immediately obvious for other reasons

##### Yield #####
	d$biomass_total <- NA
   d1$biomass_total <- NA

	d$yield <-d$`Biological Yield (t/ha)`
	d1$yield <- NA
	#what plant part does yield refer to?
	d$yield_part <- "grain"
	
	d <- d[,c("Cobs ('000/ha)", "Cob Length (cm)", "Cob Girth (cm)", "No. of grain rows/cob", "No. of grains/row", "No. of grains/cob", "Biological Yield (t/ha)", "Harvest Index (%)", "dataset_id", "on_farm", "is_survey", "is_experiment", "country", "elevation", "longitude", "latitude", "crop", "yield", "yield_part")]
	d1 <- d1[,c("Grain N (%) at harvest", "Straw N (%) at Harvest", "Total uptake (kg/ha)", "N Harvest Index (%)", "Agronomic N Use Efficiency (%)", "dataset_id", "on_farm", "is_survey", "is_experiment", "treatment", "country", "elevation", "longitude", "latitude", "crop")]
# all scripts must end like this
	carobiner::write_files(dset, d, path=path)
	
}
