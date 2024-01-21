
carob_script <- function(path) {
  
  "Description:Data for soil samples collected from WKIEMP project and analyzed at ICRAF using infrared spectroscopy methods and wet chemistry at Crop Nutrition labs"
  
  uri <- "doi:10.7910/DVN/FPPDCG"
  dataset_id <- carobiner::simple_uri(uri)
  group <- "soil_samples"
  dset <- data.frame(
    dataset_id =dataset_id,
    group=group,
    project= NA,
    uri=uri,
    data_citation="Sila, Andrew; Keith Shepherd; Ganesh Pokharyl, 2019, 'Evaluating regression-kriging for mid-infrared spectroscopy prediction of soil properties in western Kenya', https://doi.org/10.7910/DVN/FPPDCG, Harvard Dataverse, DRAFT VERSION ]",
    ## if there is a paper, include the paper's doi here
    ## also add a RIS file in references folder (with matching doi)
    publication= NA,
    data_institutions = "CIAT",
    data_type="on-farm experiment", 
    carob_contributor="Andrew Sila", 
    carob_date="2023-09-28"
  )
  
  ## download data from the uri provided
  ff <- carobiner::get_data(uri, path, group)
  js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=0)
  dset$license <- carobiner::get_license(js)$name
  dset$title <- carobiner::get_title(js)
 
  ##RH why not?
  # No need to read the table with MIR data
  #f0 <- ff[basename(ff) == "Infrared MIR.csv"]
  #d0 <- data.frame(read.csv2(f0, sep = ","))
  
  # read the table with wetchem data
  f1 <- ff[basename(ff) == "Wet Chemistry.csv"]
  d1 <- data.frame(read.csv2(f1, sep = ","))
  
  # read the table with site characterisation details
  f2 <- ff[basename(ff) == "Wkiemp site chracteristics.csv"]
  d2 <- data.frame(read.csv2(f2, sep = ","))
  
	# drop columns nitrogen_acid, exna, exbas, ESR, ESP, CaMg
  rd <- which(colnames(d1) %in% c("Nitrogen.acid", "ExNa", 'ExBas', "ESR", "ESP", "CaMg"))
  
  d1 <- d1[,-rd]
  
	hd <- c('soil_N','soil_total_carbon', 'soil_SOC', 'soil_pH','soil_Al','soil_B','soil_Cu','soil_Fe','soil_Mn','soil_P_total', 'soil_S','soil_Zn', 'soil_Ca','soil_Mg','soil_K', 'soil_EC', 'soil_ExAc')
	
	colnames(d1) <- c('SSN', hd)
	
	# Select from d2 , records from cluster 1 and plot 1 which were analyzed for wet chemistry
	d2 <- subset(d2, Plot == '1')
	# merge d1 and d2 tables
	# d <- merge(d2,d1, by = 'SSN')
	# 
	# # Remove any -ve B values
	# 
	# bn <- which(d$soil_B < 0)
	# 
	# d <-  d[-bn,]
	# 
	# # Add other common soil variables normally tested
	# d$soil_total_carbon <- NULL
	# d$soil_clay <- NULL
	# d$soil_sand <- NULL
	# d$soil_silt <- NULL
	# d$soil_pH_KCl <- NULL
	# d$soil_pH_CaCl2 <- NULL
	# d$soil_P_available <- NULL
	# d$dataset_id <- dataset_id
	# d$trial_id <- NA
	# d$crop <- NA
	# d$yield_part <- NA
	# d$yield <- NA
	# 
	# # Drop id, original_id, year and district columns from d
	# k <- which(colnames(d) %in% c("id", "original_id", "year", "district"))
	# carobiner::write_files(dset, d[,-k], path=path)
	
	carobiner::write_files(dset, d1, path=path)
}



