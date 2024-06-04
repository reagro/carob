# R script for "carob"


carob_script <- function(path) {

"Nutrient Ommission Trials (NOTs) from three states in Nigeria. Six treatments, yield, soil and ear leaf nutrient contents."

	uri <- "hdl:11529/10548245"
	group <- "fertilizer"

	ff  <- carobiner::get_data(uri, path, group)

	dset <- data.frame(
		carobiner::read_metadata(uri, path, group, major=2, minor=0),
		data_institute = "CIMMYT",
		publication = NA,
		project = "TAMASA",
		data_type = "experiment",
		treatment_vars ="N_fertilizer;P_fertilizer;K_fertilizer;Ca_fertilizer;Zn_fertilizer;B_fertilizer", 
		carob_contributor = "Shumirai Manzvera",
		carob_date = "2024-04-06"
	)
	
	f <- ff[basename(ff) == "NG-2015_NOTs_soil_leaf_yield_database_PC.xlsx"]
	r <- carobiner::read.excel.hdr(f, sheet = "Raw_Data",skip = 1, hdr = 1)
	
	d <- data.frame(
		crop="maize",
		country=r$Country,
		adm1=r$State.Province,
		adm2=r$LGA.District,
		adm3=r$Community.village,
		latitude=r$GPS.Coordinate.Latitude,
		longitude=r$GPS.Coordinate.Longitude,
		elevation=r$GPS.Coordinate.Altitude,
		trial_id = r$Experimental.site.code.EXPsit,
		treatment=r$Treatment,
		yield = r$Grain.yield.kg.ha,
		variety = r$Maize.variety.type,
		planting_date = as.character(as.Date(r$Planting.date.PLNdat,format = '%d/%m/%Y')),
		harvest_date=r$Harvest.date.HDATE,
	 
		soil_SOC = r$OC.pct, 
		soil_sand= r$Sand.pct, 
		soil_silt = r$Silt.pct, 
		soil_clay = r$Clay.pct, 
		soil_ex_acidity = r$Exch.Acidity.cmol.kg, 
		soil_ECEC = r$ECEC.cmol.kg,
	 
	
		soil_P_Mehlich = r$MehP.ppm, 
		soil_K = r$K.cmol.kg, 
		soil_Na = r$Na.cmol.kg, 
		soil_Zn = r$Zn.ppm, 
		soil_Cu = r$Cu.ppm,
		soil_Mn = r$Mn.ppm, 
		soil_Fe = r$Fe.ppm, 
		soil_Ca = r$Ca.cmol.kg, 
		soil_Mg = r$Mg.cmol.kg,
	
		leaf_N = r$Ear_leaf.analysis_pct.N,
		leaf_P = r$pct.P,
		leaf_K = r$pct.K,
		leaf_Ca = r$pct.Ca,
		leaf_Mg = r$pct.Mg,
		leaf_Zn = r$ppm.Zn,
 
		previous_crop= tolower(r$Crops.grown.in.the.past.two.years)
	)

	d <- d[d$country == "Nigeria",]

	d$previous_crop <- gsub(" |  ", "", d$previous_crop)
	d$previous_crop <- gsub(",|and|&", ";", d$previous_crop)
	d$previous_crop <- gsub("g/nut|gnut", "groundnut", d$previous_crop)
	d$previous_crop <- gsub("101", NA, d$previous_crop)
	
#HD<-	carobiner::fix_name(d$harvest_date )
#HD<- gsub("1900-03-24", "2015-03-24", HD)
#d$harvest_date<- HD

	i <- grep("^4", d$harvest_date)
	d$harvest_date[i] <- as.character(as.Date(as.numeric(d$harvest_date[i]), origin = "1900-01-01"))
	i <- grep("/2015$", d$harvest_date)
	d$harvest_date[i] <- as.character(as.Date(d$harvest_date[i], format = "%d/%m/%Y"))


	d$on_farm <- TRUE
	d$yield_part <- "grain"
	
	carobiner::write_files(path, dset, d)
}

