# R script for "carob"




carob_script <- function(path) {
  
  "Description
Title: Response of Groundnut to plant density and phosphorous application in the sudan savanna zone of Minjibir, Nigeria. 
Abstract: Despite the recent release of several improved varieties of groundnut in Nigeria the productivities have not increase significantly due to lack of commensurate recommendation in agronomic practices. Two groundnut varieties were evaluated for their response to different plant density and phosphorus application in two locations in the Sudan Savanna zone of Nigeria in 2012 and 2013. The groundnut were planted at density of 44444, 66667, and 133333 hills ha-1 with average of two plants per hill. Phosphorus was applied at rate of 0 or 20 kg P ha-1 . P fertilizer application increased pod and haulm yields by 26% and 16% respectively in Minjibir. It increased pod and haulm yields by 62% and 27% respectively in Wudil. Pod and haulm yields, harvest index, revenue, profit and cost benefit ratio increased with increasing plant density. Samnut-24 produced pod yields that were significantly higher than Samnut-22 across treatments. Pod yields at density of 133,333 hills ha-1 was 31% higher than at 66667 and 40% than at 44,444 hills ha-1. Application of fertilizer increased profit by 22% and 49% in Minjibir and Wudil respectively. Planting at density of 133,333 hill ha-1 increased profit by 19% and 27% over 66,667 and 444444 hill ha-1 respectively in Minjibir, while it increase profit by 9% in Wudil. Cultivation of Samnut-24 at high density with phosphorus application will make groundnut production a more profitable venture in Sudan Savanna zone of Minjibir, Nigeria."
  
  ## Process 

  uri <- "doi:10.21421/D2/01WXFG"
  dataset_id <- carobiner::simple_uri(uri)
  group <- "fertilizer"
  
  dset <- data.frame(
    dataset_id = dataset_id,
    group=group,
    uri=uri,
    publication= "http://dx.doi.org/10.12692/ijb/9.1.291-302",
    carob_contributor="Siyabusa Mkuhlani",
    experiment_type="fertilizer",
    has_weather=TRUE,
    has_management=TRUE
  )
  
  ## treatment level data 
  ff  <- carobiner::get_data(uri, path, group)
  
  ## read the json for version, license, terms of use  
  js <- carobiner::get_metadata(dataset_id, path, major=1, minor=0, group)
  dset$license <- carobiner::get_license(js) 
  
  f <- ff[basename(ff) == "Data file of Groundnut to plant density and phosphorous application in Minjibir 2012-13.xlsx"]
  d <- carobiner::read.excel(f)
  
 d$country <- "Nigeria"
 d$adm1 <- "Kano"
 d$adm2 <- d$Location
 d$adm3 <- "Wasai" #from the reference
 d$latitude <- 8.67
 d$longitude <- 12.15
 # sown during the growing seasons of 2012 and 2013 no actual dates mentioned
 d$start_date <- ifelse(d$Year == "2012", "2012","2013") 
 d$end_date <- ifelse(d$Year == "2012", "2012", "2013") 
 d$rep <- as.integer(d$`Replication number`)
 d$variety <- d$Variety
 d$P_fertilizer <- ifelse(d$Fertilizer == "F1",0,20)
 d$N_fertilizer <- 0
 d$K_fertilizer <- 0 
 d$yield <- d$PodKgHa
 d$residue_yield <- d$FdWtKgHa
 d$grain_weight <- d$seedgm
 d$crop <-"groundnut"
 d$dataset_id <- dataset_id
 d$trial_id <- paste0(dataset_id, d$adm2, sep = "_")
 d$on_farm <- TRUE
 d$is_survey <- FALSE	
 d$row_spacing <- 75
 d$flowering <- d$Flw50
 d$plant_spacing <- d$Spacing
 d$rep <- as.integer(d$rep)
 d$plant_density <- ifelse(d$plant_spacing == 30, 44444,
	                      ifelse(d$plant_spacing == 20, 66667, 133333)) #as per the reference
 d$fertilizer_type <- "SSP" #from the reference
 
 #soil properties from reference
 d$s1 <- paste(d$Year,d$adm2, sep = "_")
 ss <- data.frame(s1 = c("2012_Minjibir","2013_Minjibir"),
            soil_SOC = c(0.221 ,0.198 ),
           soil_sand = c(91.7 ,89.8),
           soil_clay = c(4.2,6.0),
           soil_silt = c(4.0,4.2),
    soil_P_available = c(3.1,3.6),
            soil_pH  = c(5.10, 5.0),
            rain     = c(994, 1054.3))

 d <- merge(d,ss, by ="s1", all.x = TRUE)
 
 
 d <- d[,c("trial_id","country","adm1","adm2","adm3","latitude","longitude","start_date","end_date","crop","variety","row_spacing","plant_spacing","rain","flowering","plant_density","on_farm","is_survey","soil_pH","soil_SOC","soil_sand","soil_clay","soil_silt","soil_P_available","fertilizer_type","N_fertilizer","P_fertilizer","K_fertilizer","yield","residue_yield","grain_weight")]
 
 d$dataset_id <- dataset_id
 
 # all scripts must end like this
 carobiner::write_files(dset, d, path, dataset_id, group)

}


