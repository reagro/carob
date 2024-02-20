# R script for "carob"

carob_script <- function(path) {
  
  
  "Description:
  Performance trials (N=52) in two zones (West Shewa and Jimma) in Ethiopia. Trials comprise four nutrient management treatments, namely control withzero fertilizer ; and three fertilizer recommendations to achieve the same  target yield based on regional fertilizer recommendation, a Nutrient Expert      (IPNI software) based recommendation and a soil-test NE based recommendation. Trials were conducted on-farm with four plots per farm. Observations  include biomass and grain yields, as well as pre-sowing pH, nitrogen and phosphorus levels. Some N & K data are missing."

	uri <- "hdl:11529/11012"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "fertilizer"

	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=1)
	
	dset <- data.frame(
		carobiner::extract_metadata(js, uri, group),
		project="TAMASA",
		data_citation="T Balemi; M Kebede; T Abera; G Hailu; J Rurinda; G Gurumu, 2017, TAMASA Ethiopia. Performance trial dataset for validating maize nutrient management recommendations, 2016 season., https://hdl.handle.net/11529/11012, CIMMYT Research Data & Software Repository Network, V2",
		publication=NA,
		data_institutions = "CIMMYT",
		data_type="experiment", 
		carob_contributor="Njogu Mary",
		carob_date="2023-02-20"
	)
  
  
	f <- ff[basename(ff) == "TAMASA_ET_PT_2016F.xlsx"]
	r <- carobiner::read.excel(f, sheet = 5, fix=TRUE) 
  
  	d <- data.frame(country= "Ethiopia", 
			adm1= r$Region, adm2= r$Zone, adm3s= r$Districts,
			location= r$Location, elevation= r$Altitude, 
			treatments= r$Treatments, crop= "maize", 
			yield_part= "grain",  dataset_id = dataset_id,
			yield =r$Kernel.yield.kg.ha, soil_pH = r$pH)

				
	# d$pl_st <- r$Crop.Stand.count * ???
	# d$biomass_weight <- r$total.biomass.weight.kg * ??

	# fix spelling variants
	d$adm1 <- "Oromia"
	## d$soil_N_total <- TN.pct  unit?
	d$soil_P_available <- r$Av.P.ppm

	d$N_fertilizer <- ?
	d$P_fertilizer <- ?
	d$K_fertilizer <- ?
  
  # get lat and lon coordinates using carobiner geocode function
#  h <- carobiner::geocode (country=d$country, adm1=d$region, location =d$location)$df
	# geo <- data.frame(location=c(), 
	#			lon=c(),
	#			lat=c() )
	
	# d <- merge(d, geo)
  
	carobiner::write_files(dset, d, path=path)
}





 
  