# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"The historic crossing block (HCB) is a collection of all the parents of spring wheat pre-breeding crossing block that include landraces, synthetics, synthetic derivatives, elite lines, high biomass lines, lines with rust resistance, and some varieties. Every year, to incorporate novel alleles into elite back grounds several strategic crosses-source× sink, landrace × elite, synthetic × elite-are made based on the physiological breeding approach (Reynolds et al. 2017b). The HCB is comprised of three panels: germplasm used for the crosses for yield potential (CB-YP), drought (CB-D), and heat stress (CB-H). The number of lines in each panel increased from 128 to 311 in CB-YP, from 123 to 219 in CB-D, and from 80 to 195 in CB-H during 2012-2017. A total of 119,144 SNP markers were identified on the germplasm. After quality control (removing markers with missing data > 30%, and minor allele frequency < 5%), 13135, 11717, and 14441 polymorphic SNPs for CB-YP, CB-D, and CB-H, respectively were used for further analysis.

    

"

	uri <- "hdl:11529/10548292"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "wheat_trials"
	## dataset level data 
	dset <- data.frame(
		dataset_id = dataset_id,
		group=group,
		project=NA,
		uri=uri,
		data_citation="Sukumaran, Sivakumar, 2020, Physiological pre-breeding crossing block dataset, https://hdl.handle.net/11529/10548509, CIMMYT Research Data & Software Repository Network, V1, UNF:6:zXWmVpPwNsYZNjlM1Pd6sQ== [fileUNF]",
		## if there is a paper, include the paper's doi here
		## also add a RIS file in references folder (with matching doi)
		publication=NA,
		data_institutions = "CIMMYT",

   	data_type="experiment", 
		carob_contributor="Shumirai Manzvera",
		# date of first submission to carob
		carob_date="2024-02-08"

	)

## download and read data 

	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=2)
	dset$license <- carobiner::get_license(js)
	dset$title <- carobiner::get_title(js)
	dset$authors <- carobiner::get_authors(js)
	dset$description <- carobiner::get_description(js)

	proc_wheat <- carobiner::get_function("proc_wheat", path, group)
	d <- proc_wheat(ff)
	d$dataset_id <- dataset_id
	
	# all scripts must end like this
	carobiner::write_files(dset, d, path=path)
}
	