# R script for "carob"

carob_script <- function(path) {

"The International Bread Wheat Screening Nursery (IBWSN) is designed to rapidly assess a large number of advanced generation (F3-F7) lines of spring bread wheat under Mega-environment 1 (ME1) which represents diversity for a wide range of latitudes, climates, daylengths, fertility conditions, water management, and (most importantly) disease conditions. The distribution of these nurseries is deliberately biased toward the major spring wheat regions of the world where the diseases of wheat are of high incidence. It is distributed to 180 locations and contains 300-450 entries. (2020)"

	uri <- "hdl:11529/10548590"
	group <- "wheat_trials"

	ff <- carobiner::get_data(uri, path, group)

	dset <- data.frame(
		carobiner::read_metadata(uri, path, group, major=4, minor=0),
		project="International Bread Wheat Screening Nursery",	   
		publication=NA,
		data_institutions = "CIMMYT",
		carob_contributor="Robert Hijmans",
		carob_date="2023-10-02",
		data_type="on-station experiment",
		exp_treatments = "variety_code;location"
 	)

	proc_wheat <- carobiner::get_function("proc_wheat", path, group)
	d <- proc_wheat(ff)
	carobiner::write_files(path, dset, d)
}

