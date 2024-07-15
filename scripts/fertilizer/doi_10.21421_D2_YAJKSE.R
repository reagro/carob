# R script for "carob"

carob_script <- function(path) {
  
  "Description
Title: Response of Groundnut to Plant Density and Phosphorous application in the Sudan Savanna zone of Nigeria, Minjibir.  
Abstract: Despite the recent release of several improved varieties of groundnut in Nigeria the productivities have not increase significantly due to lack of commensurate recommendation in agronomic practices. Two groundnut varieties were evaluated for their response to different plant density and phosphorus application in two locations in the udan Savanna zone of Nigeria in 2012 and 2013. The groundnut were planted at density of 44444, 66667, and 133333 hills ha-1 with average of two plants per hill. Phosphorus was applied at rate of 0 or 20 kg P ha-1 . P fertilizer application increased pod and haulm yields by 26% and 16% respectively in Minjibir. It increased pod and haulm yields by 62% and 27% respectively in Wudil. Pod and haulm yields, harvest index, revenue, profit and cost benefit ratio increased with increasing plant density. Samnut-24 produced pod yields that were significantly higher than Samnut-22 across treatments. Pod yields at density of 133,333 hills ha-1 was 31% higher than at 66667 and 40% than at 44,444 hills ha-1 . Application of fertilizer increased profit by 22% and 49% in Minjibir respectively. Planting at density of 133,333 hill ha-1 increased profit by 19% and 27% over 66,667 and 444444 hill ha-1 respectively in Minjibir, while it increase profit by 9% in Wudil. Cultivation of Samnut-24 at high density with phosphorus application will make groundnut production a more profitable venture in Sudan Savanna zone of Nigeria."
  
  ## Process 
 
  uri <- "doi:10.21421/D2/YAJKSE"
  group <- "fertilizer"
	ff <- carobiner::get_data(uri, path, group)
  
	meta <- data.frame(
		carobiner::read_metadata(uri, path, major=1, minor=0, group),
		publication="doi:10.12692_ijb_9.1.291-302",
		carob_contributor="Siyabusa Mkuhlani",
		carob_date="2022-09-12",
		data_type="experiment",
		data_institute="ICRISAT",
		project=NA
	)
  
	## treatment level data 
	  
	## read the json for version, license, terms of use  

	f <- ff[basename(ff) == "Data file of Groundnut fertilizer plant density of combine Minjibir.xlsx"]
	d <- carobiner::read.excel(f)
	
	#  names(d)
	e <- d[,c(1,2,4,5,6,7,14,15)]
	#  names(e)
	colnames(e) <- c('planting_date','location','rep','variety_type','treatment','plant_density','yield','residue_yield')
	e$rep <- as.integer(e$rep)  
	
	e$planting_date <- as.character(e$planting_date)
	e$country <-  "Nigeria"
	e$crop <- "groundnut"
	
	e$trial_id <- paste0("gnut_dens_phosph_", e$location)
	e$adm1 <- "Kano"
	e$site <- "Minjibir"
	e$longitude <- 8.557
	e$latitude <- 11.980
	
	e$on_farm <- FALSE
	e$is_survey <- FALSE
	# EGB
	# SSP composition is N 0%; P 9%; K 0%
	e$P_fertilizer[e$treatment=='F1'] <- 0
	e$P_fertilizer[e$treatment=='F2'] <- 20
	e$N_fertilizer <- 0
	e$K_fertilizer <- 0
	#  names(e)

	e$plant_density <- 2 * c(44444, 66667, 133333)[e$plant_density/10]
	e$fertilizer_type <- "unknown"
	e$yield_part <- "pod"
	carobiner::write_files(meta, e, path=path)
}


	
