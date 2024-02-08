# R script for "carob"

## ISSUES
# 1. Max heat yield in the original data is about 15 000kg per ha and carob
#    cannot capture it, yield too high.


carob_script <- function(path) {

"Description:

    [International Durum Yield Nurseries are replicated yield trials designed
    to measure the yield potential and adaptation of superior CIMMYT-bred spring
    durum wheat germplasm that have been developed from tests conducted under
    irrigation and induced stressed cropping conditions in northwest Mexico.
    These materials have been subjected to numerous diseases (leaf, stem and
    yellow rust; Septoria tritici blotch) and varied growing environments.
    It is distributed to 70 locations, and contains 50 entries. (2020)]

"

	uri <- "hdl:11529/10548289"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "wheat_trials"
	## dataset level data 
	dset <- data.frame(
		dataset_id = dataset_id,
		group=group,
		project="Global_Wheat_Program",
		uri=uri,
		data_citation="Global Wheat Program; IWIN Collaborators; Ammar, Karim; Payne, Thomas, 2020, 37th International Durum Yield Nursery, https://hdl.handle.net/11529/10548289, CIMMYT Research Data & Software Repository Network, V1",
		publication=NA,
		data_institutions = "CIMMYT",
   		data_type="experiment", 
		carob_contributor="Blessing Dzuda",
		carob_date="2024-02-06"
	)

## download and read data 

	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=3)
	dset$license <- carobiner::get_license(js)
	dset$title <- carobiner::get_title(js)
	dset$authors <- carobiner::get_authors(js)
	dset$description <- carobiner::get_description(js)
	
#### about the data #####
## (TRUE/FALSE)
	
	proc_wheat <- carobiner::get_function("proc_wheat", path, group)
	d <- proc_wheat(ff)
	d$dataset_id <- dataset_id
	d$previous_crop<-carobiner::replace_values(d$previous_crop,"maize-wheat","maize")
	d$previous_crop<-carobiner::replace_values(d$previous_crop,"rape seat","rapeseed")
	#Assigning NA on the previous crop value "intercroping" because intercrop is not specified on the crop used
	d$previous_crop<-carobiner::replace_values(d$previous_crop,"intercroping",NA)
	
	d$yield<-round(d$yield)
	d$yield<- as.numeric(d$yield)
	
# all scripts must end like this
	carobiner::write_files(dset, d, path=path)
}

