# R script for "carob"


carob_script <- function(path) {
    "Nutrient Omission Trials (NOTs) from three states in Nigeria. Six treatments, yield, soil and ear leaf, stover and grain nutrient contents (2016)"
  uri <- "hdl:11529/10548238" 
	group <- "soil_samples"
	ff  <- carobiner::get_data(uri, path, group)
  
meta <- data.frame(
		carobiner::read_metadata(uri, path, group, major=2, minor=1),
		data_institute = "CIMMYT",
		#publication= "doi:10.3390/su10051610",
		project="NG 2015 2016 NOTs soil leaf yield database",
		data_type= "experiment",
		#treatment_vars = "NPK_fertilizer;Urea_fertilizer;DAP_fertilizer;NPK_fertilizer;SSP_fertilizer",
		carob_contributor= "Andrew Sila",
		carob_date="2024-07-29"
	)
  
	f1 <- ff[basename(ff) == "TAMASA_NOTs_Database_Nigeria_2015_2016.xlsx"]
	r <- carobiner::read.excel(f1, sheet = "Raw_Data_2015")
	
	# Remove last six rows which contains summary statistics computed in excel
	z <- nrow(r)
	y <- z-6
	r <- r[1:y,]
	
	colnames(r) <-  r[1,]
	# Remove the first row with colnames now 
	r <- r[-1,]
  # Remove row with NA
  r <- r[-which(is.na(r$`N (%)`)== TRUE),]
	
  	# Get required columns with data	
	d <- data.frame(
		country = r$Country,
		location = r$State,
		longitude = as.numeric(r$`GPS Coordinate Longitude`),
		latitude = as.numeric(r$`GPS Coordinate Latitude`),
		trial_id = r$`Experiment site code = EXPsi`,
		soil_SOC = as.numeric(r$`OC (%)`),
		soil_pH = as.numeric(r$`pH (Water)  1:1`),
		soil_N = as.numeric(r$`N (%)`)*10000,
		soil_P_Mehlich = as.numeric(r$`MehP (ppm)`), 
		soil_sand = as.numeric(r$`Sand (%)`),
		soil_clay  = as.numeric(r$`Clay (%)`),
		soil_silt = as.numeric(r$`Silt (%)`),
		soil_Ca = as.numeric(r$`Ca (cmol/kg)`) * 401,
		soil_Mg = as.numeric(r$`Mg (cmol/kg)`) * 243,
		soil_K = as.numeric(r$`K (cmol/kg)`)* 391,
		soil_Na = as.numeric(r$`Na (cmol/kg)`) * 230,
		soil_ex_acidity = as.numeric(r$`Exch. Acidity (cmol/kg)`),
		soil_ECEC = as.numeric(r$`ECEC (cmol/kg)`),
		soil_Zn = as.numeric(r$`Zn (ppm)`),
		soil_Cu = as.numeric(r$`Cu (ppm)`),
		soil_Mn = as.numeric(r$`Mn (ppm)`),
		soil_Fe = as.numeric(r$`Fe (ppm)`),
		#soil_S = r$`S (ppm)`,
		soil_bd = as.numeric(r$`BD (g/cm3)`)
	)
	
	carobiner::write_files(path, meta, d)
}


