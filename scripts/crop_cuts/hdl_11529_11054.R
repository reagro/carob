# R script for "carob"

carob_script <- function(path) {

"Maize crop cut data from farmer's field collected at Odisha plateau ecology."

	uri <- "hdl:11529/11054"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "crop_cuts"
	ff <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=1)
	dset <- data.frame(
		carobiner::extract_metadata(js, uri, group),
		project="CSISA",
		publication= NA,
		data_institutions = "CIMMYT",
		data_type="survey",
		carob_contributor="Robert Hijmans",
		carob_date="2024-01-27"
	)


	f <- ff[basename(ff) == "CSISA_OD_Maize_Kharif_CropCut_2016_FinalDV.csv"]
	r <- read.csv(f)

#	fm <- ff[basename(ff) == "CSISA_OD_Maize_Kharif_CropCut_2016_DV_MetaSheet.csv"]
#	mm <- read.csv(fm)
#	m <- as.list(mm[,2])
#   names(m) <- mm[,1]
	
	d <- data.frame(
		dataset_id=dataset_id,
		crop="maize", yield_part="grain", season=r$SEASON, 
		country = "India", adm1="Odisha", adm2 = r$DIST, 
		longitude = r$LON, latitude=r$LAT,
		variety = r$VAR, 
		planting_method = r$MSOW,
		herbicide_product=tolower(r$HRBCDE), 
		herbicide_amount=r$HRBCDE_AMT,
		herbicide_timing = r$HRBCDE_DAS, 
		plant_density = 10000 * r$PLNT_NO / 15 ,
		yield = r$GYLD_15 * 1000
	)

	d$planting_date = as.character(as.Date(carobiner::eng_months_to_nr(r$DSOW), "%d-%m-%y"))

	d$herbicide_used <- d$herbicide_product != "none" 
	d$herbicide_timing[d$herbicide_product == "none"] <- NA 
	d$herbicide_amount[d$herbicide_product == "none"] <- NA 

	fert <- data.frame(
		dap = r$DAP,
		urea = r$UR_1 + r$UR_2,
		mop = r$MoP_Basal + r$MoP_1
	) / 0.4047
	
	d$P_fertilizer <- fert$dap * .201
	d$K_fertilizer <- fert$mop * .498
	d$N_fertilizer <- fert$dap * .180 + fert$urea * 0.46
	d$fertilizer_type <- c("DAP", "KCl", "urea")

## to do
#	d$fertilizer_timing <- c("DAP", "KCl", "urea")
# $ UR_1_DAS             : chr "Urea first top dressing application in days after sowing"
# $ UR_2_DAS             : chr "Urea second top dressing application in days after sowing"
# $ MoP_Basal            : chr "MoP Basal dose in KG per ACRE"
# $ MoP_1_DAS            : chr "MoP first top dressing application in days after sowing"

	carobiner::write_files(dset, d, path=path)

}

