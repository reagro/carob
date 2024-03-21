# R script for "carob"

carob_script <- function(path) {
  
"The dataset contains the result of a series of experiments conducted in Morocco between 2014 and 2015, about land and water productivity.  The overall objective of this study is the sustainable increase of wheat yield in dry areas of WANA. The purpose is the development of options that improve the adaptation of wheat to high temperature and drought."

	uri <- "hdl:20.500.11766.1/FK2/XOSFYG"
	dataset_id <- carobiner::simple_uri(uri)

	group <- "wheat_trials"

	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=5, minor=0)

	dset <- data.frame(
		carobiner::extract_metadata(js, uri, group=group),
		project=NA,
		publication="doi:20.500.11766/4516",
		data_citation = "Karrou, Mohammed; Daoui, Khalid; Razouk, Rachid; Boutfirass, Mohamed; Bahri, Abdeljabar, 2015. Experimental results on land and water productivity in rainfed areas in Morocco. https://hdl.handle.net/20.500.11766.1/FK2/XOSFYG",
		data_institutions = "ICARDA",
		carob_contributor="Samar Attaher",
		carob_date="2023-03-15",
		data_type="experiment"
    )
  

	f <- ff[basename(ff) == "Grain_Yield.csv"] 	
	r <- read.csv(f, sep=";")
 
	d <- data.frame(
		rep=r$Bloc,
		# from quintal/ha to kg/ha
		yield=r$Yield * 100,
		country = "Morocco",
		adm1 = "Béni Mellal-Khénifra Region",
		adm2 = "Fquih Ben Salah Province",
		adm3 = "Afourar",
		site = "Afourer experiment station",
		latitude = 32.262,
		longitude = -6.535,
		crop = "durum wheat",
		yield_part = "grain",
		seed_amount = 160,
		rain = 378,
		P_fertilizer = 200,
		N_fertilizer = 60
	)

	r$Planting_Date <- trimws(r$Planting_Date)

	d$treatment <- tolower(apply(r[, c("Water_Regime", "Planting_Date")], 1, \(i) paste(i, collapse="_")))
	
	i <- match(r$Variety, paste0("V", 1:4))	
	d$variety <- c("Karim", "Louiza", "Nassira", "PM9")[i]

	d$planting_date[r$Planting_Date=="Early"] <- "2014-11-11"
	d$planting_date[r$Planting_Date=="Late"] <- "2014-12-24"

	d$irrigation_amount <- 0
	d$irrigation_number <- 0L

	i <- d$treatment == "irrigated_early"
	d$irrigation_amount[i] <- 160
	d$irrigation_number[i] <- 3L

	i <- d$treatment == "irrigated_late"
	d$irrigation_amount[i] <- 275
	d$irrigation_number[i] <- 5L

	d$dataset_id <- dataset_id
	d$trial_id <- "1"

	carobiner::write_files (dset, d, path=path)
 
}
