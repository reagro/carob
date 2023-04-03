# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"
	Description:

    Despite the recent release of several improved varieties of groundnut in Nigeria the productivities have not increase significantly due to lack of commensurate recommendation in agronomic practices.
    Two groundnut varieties were evaluated for their response to different plant density and phosphorus application in two locations in the Sudan Savanna zone of Nigeria in 2012 and 2013.
    The groundnut were planted at density of 44444, 66667, and 133333 hills ha-1 with average of two plants per hill.
    Phosphorus was applied at rate of 0 or 20 kg P ha-1 .
    P fertilizer application increased pod and haulm yields by 26% and 16% respectively in Minjibir.
    It increased pod and haulm yields by 62% and 27% respectively in Wudil.
    Pod and haulm yields, harvest index, revenue, profit and cost benefit ratio increased with increasing plant density.
    Samnut-24 produced pod yields that were significantly higher than Samnut-22 across treatments.
    Pod yields at density of 133,333 hills ha-1 was 31% higher than at 66667 and 40% than at 44,444 hills ha-1.
    Application of fertilizer increased profit by 22% and 49% in Minjibir and Wudil respectively.
    Planting at density of 133,333 hill ha-1 increased profit by 19% and 27% over 66,667 and 444444 hill ha-1 respectively in Minjibir, while it increase profit by 9% in Wudil.`
    Cultivation of Samnut-24 at high density with phosphorus application will make groundnut production a more profitable venture in Sudan Savanna zone of Wudil, Nigeria.

"

	uri <- "doi:10.21421/D2/5O93XX"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "fertilizer"
	## dataset level data 
	dset <- data.frame(
	   dataset_id = dataset_id,
	   group=group,
	   uri=uri,
	   publication="http://dx.doi.org/10.12692/ijb/9.1.291-302",
	   carob_contributor="Eduardo Garcia Bendito",
	   experiment_type="fertilizer",
	   has_weather=FALSE,
	   has_management=FALSE

	)

## download and read data 

	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=0)
	dset$license <- carobiner::get_license(js)


	f <- ff[basename(ff) == "Data file of Groundnut to plant density and phosphorous application in Wudil 2012-13.xlsx"]

	d <- data.frame(readxl::read_excel(f))
	
	# Start formatting dataset
	d$country <- "Nigeria"
	d$adm1 <- "Kano"
	d$adm2 <- "Wudil"
	d$adm3 <- "Wudil" #as per the reference
	d$trial_id <- paste0(dataset_id, '-', d$Location)
	d$latitude <- 11.793702
	d$longitude <- 8.838846
	# sown during the growing seasons of 2012 and 2013 no actual dates mentioned
	d$start_date <- ifelse(d$Year == "2012", "2012","2013") 
	d$end_date <- ifelse(d$Year == "2012", "2012","2013") 
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$rep <- as.integer(d$Replication.number)
	d$crop <- "groundnut"
	d$variety <- d$Variety
	d$yield <- d$PodKgHa
	d$residue_yield <- d$FdWtKgHa
	d$grain_weight <- d$Seed.weight
	d$fertilizer_type <- "SSP" # As indicated in the publication associated
	d$N_fertilizer <- 0
	d$P_fertilizer <- ifelse(d$Fertilizer == "F1", 0, 20)
	d$K_fertilizer <- 0
	d$flowering <- d$flw50
	d$plant_spacing <- d$Spacing
	d$row_spacing <- 75 # from the reference
	d$plant_density <- d$plant_density <- ifelse(d$plant_spacing == 30, 44444,
	                                             ifelse(d$plant_spacing == 20, 66667, 133333))# from the reference
	#soil properties from reference
	d$s1 <- paste(d$Year,d$adm2, sep = "_")
	ss <- data.frame(s1 = c( "2012_Wudil", "2013_Wudil"),
	                  soil_SOC = c(0.162 ,0.156 ),
	                 soil_sand = c(88.9 ,92.8),
	                 soil_clay = c(6.5 ,2.5),
	                 soil_silt = c(4.6 ,4.6),
	          soil_P_available = c(2.6 ,2.9),
	                  soil_pH  = c(5.10, 5.0),
	                  rain     = c(945.8, 907.4))
	
	d <- merge(d,ss, by ="s1", all.x = TRUE)
	
	
	# process file(s)
	d <- d[,c("trial_id","country","adm1","adm2","adm3","latitude","longitude","start_date","end_date","crop","variety","row_spacing","plant_spacing","flowering","plant_density","on_farm","is_survey","soil_pH","soil_SOC","soil_sand","soil_clay","soil_silt","soil_P_available","fertilizer_type","N_fertilizer","P_fertilizer","K_fertilizer","yield","residue_yield","grain_weight")]
	
	d$dataset_id <- dataset_id

# all scripts must end like this
	carobiner::write_files(dset, d, path, dataset_id, group)

}
