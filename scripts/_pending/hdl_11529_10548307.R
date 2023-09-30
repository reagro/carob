# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"Description:
TAMASA Agronomy Panel Survey in Nigeria (2016) (2016)
"

	uri <- "hdl:11529/10548307"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "crop_cuts"
	## dataset level data 
	dset <- data.frame(
		dataset_id = dataset_id,
		group=group,
		project="TAMASA",
		uri=uri,
		data_citation="Masuki, Kenneth; Chamberlin, Jordan, 2019, Tamasa APS Tanzania 2016, https://hdl.handle.net/11529/10548307, CIMMYT Research Data & Software Repository Network, V1, UNF:6:ROFhHRpFl3nj0rn+rxjaIA== [fileUNF]",
		## if there is a paper, include the paper's doi here
		## also add a RIS file in references folder (with matching doi)
		publication= NA,
		data_institutions = "CIMMYT",
		data_type="survey",
		carob_contributor="Shumirai Manzvera" 
	)

## download and read data 
	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=1)
	dset$license <- carobiner::get_license(js)

	getdf <- function(r) {
		d <- data.frame(country=r$Country, site=r$Zone, adm1=r$Region,
			adm2=r$District, adm3=r$Ward, adm4=r$Village, 
			elevation=r$Altitude, longitude= r$Longitude, latitude=r$Latitude)


		d$HHID <- r$HHID
		d$hhid_2016 <- r$hhid_2016
		d$FarmID <- r$FarmID
		
		d$trial_id <- r$SSID
		d$yield <- r$`Grain yield (kg/ha@12.5%)`
		if (!is.null(r$`Plant stands`)) d$plant_density <- 400 * r$`Plant stands` 
		d$soil_pH <- r$pH 

# to do: check units!
		d$soil_Al <- r$Al
		d$soil_N <- r$N
		d$soil_Na <- r$Na
		d$soil_Fe <- r$Fe
		d$soil_K <- r$K
		d$soil_S <- r$S
		d$soil_B <- r$B
		d$soil_C <- r$C
		d$soil_Ca <- r$Ca
		d$soil_Mg <- r$Mg
		d$soil_Mn <- r$Mn
		d$soil_Zn <- r$Zn
		d$soil_sampling_depth_bottom <- r$Depth
		if (!is.null(r$Depth)) d$soil_sampling_depth_top <- 0
		d
	}	

	f1 <- ff[basename(ff) == "TAMASA_TZ_APS_CC_2016.xlsx"]
	r1 <- carobiner::read.excel(f1, "Data")
	d1 <- getdf(r1)
	
	f2 <- ff[basename(ff) == "TAMASA_TZ_APS_Soil_2016.xlsx"]
	r2 <- carobiner::read.excel(f2, "Data")
	d2 <- getdf(r2)
	
#	d12 <- merge(d1, d2, by...)
#	d12$date <- "2016"
	
	f3 <- ff[basename(ff) == "TZ_TAMASA_BYS_Yield_2015_22June17.xlsx"]
	r3 <- carobiner::read.excel(f3, "Yield Data")
	d3 <- getdf(r3)
	
	f4 <- ff[basename(ff) == "TZ_TAMASA_BYS_Yield_2015_22June17.xlsx"]
	r4 <- carobiner::read.excel(f4, "Soil Data")
	d4 <- getdf(r4)

#	d34 <- merge(d3, d4, by...)
#	d34$date <- "2015"

	d <- rbind(d12, d34)
	
	d$dataset_id <- dataset_id
	d$on_farm <- FALSE
	d$is_survey <- TRUE
	d$is_experiment <- FALSE
	d$irrigated <- FALSE
	d$yield_part <- "grain" 
	d$crop <- "maize"

	
# all scripts must end like this
	carobiner::write_files(dset, d, path=path)
}

## now test your function in a clean R environment 
# path <- _____
# carob_script(path)

