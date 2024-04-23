# R script for "carob"

# ISSUES
# not useful without coordinates
# fertilizer amount is specified
#the fertilizer is given as "amount of Inorganic fertilizer" it however does do not give the  specifics
#it is not specified if the units are referring to Nitrogen or phosphorus or pottasium 


carob_script <- function(path) {

"This dataset was obtained from maize Crop cut survey conducted in 2015 by EIAR and CIMMYT. Replicated crop cuts of 16m2 in farmers fields along with addition data on nutrient use and variety, and soil sample (0-20, 20-50 cm). Note that not all soil samples have been analysed yet.

"
	uri <- "hdl:11529/11016"
	group <- "crop_cuts"

	ff <- carobiner::get_data(uri, path, group)

	dset <- data.frame(
		carobiner::read_metadata(uri, path, group, major=1, minor=1),
		#data_citation="Balemi T. and Kebede M., Tufa T., and Gurumu G. 2017. TAMASA Ethiopia. Yield, soil and agronomy data from farmers’ maize fields collected by EIAR, 2015 season.  International Maize and Wheat Improvement Centre (CIMMYT), Ethiopia.",
		data_institutions = "CIMMYT",
		publication= NA,
		project=NA,
		data_type= "survey",
		carob_contributor= "Shumirai Manzvera",
		carob_date="2024-03-19"
	)
	

	f <- ff[basename(ff) == "TAMASA_ET_CC_2015F.xlsx"]
	r <- carobiner::read.excel(f,sheet= "Revised_data")

	d <- data.frame(
		country = "Ethiopia",
		crop = "maize",
		variety=r$`Name of variety`, 
	    soil_C=r$`Carbon (%)`,
		soil_pH=r$pH,
		soil_Al=r$`Al (mg/kg)`,
		soil_Ca=r$`Ca  (mg/kg)` ,
	    soil_S=r$`S  (mg/kg)`,
		soil_Mn=r$`Mn  (mg/kg)`,
		soil_P_total=r$`P  (mg/kg)`,
		soil_Zn=r$`Zn  (mg/kg)`,
	    soil_K=r$`K  (mg/kg)`,
		soil_Na=r$`Na  (mg/kg)`, 
		soil_Fe=r$`Fe  (mg/kg)`,
	    soil_sample_top= 0,
	    soil_sample_bottom= 20,
## corrected coordinates
	latitude=6.58,
  longitude=42.12,
		trial_id="1",
		yield_part="grain",
		on_farm = TRUE,
		is_survey = TRUE,
		irrigated <- FALSE
	)

	qdr <- r[, grep("Quadrant\\(.)-Grain yield kg/ha", colnames(r))]
	qdr[qdr == "."] <- NA
	d$yield <- rowMeans(sapply(qdr, as.numeric), na.rm = TRUE)

	d$harvest_date <- "2015"
	
	carobiner::write_files(dset, d, path=path)
}


