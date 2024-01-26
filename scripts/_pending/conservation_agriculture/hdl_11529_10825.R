# R script for "carob"

## ISSUES
# ....

# TO DO: Fertilizers

carob_script <- function(path) {

"Description: On-farm demonstration plots were set in Zambia to demonstrate the effects of conservation agriculture (CA) technologies as compared to the traditional farmers practice (ploughing with a mouldboard plough). The CA treatments included basins (BA), ripping (RI) and direct seeding with a direct seeder (DS) and direct seeding with a jab planter (JP). Also superimposed to the treatments are rotations and intercropping of maize with a grain legume (either soyabean or cowpea) and these are compared with continuous maize planting. The study is carried out in various communities of Zambia. Thus, the data set presents yields for maize and the legumes from these sites over 9 seasons (2006-2015). (2016-12-08)

    

"

	uri <- "hdl:11529/10825"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "conservation_agriculture"
	## dataset level data 
	dset <- data.frame(
		dataset_id = dataset_id,
		group=group,
		project=NA,
		uri=uri,
		data_citation="Thierfelder, Christian, 2016, 'Facilitating the widespread adoption of conservation agriculture in maize-based systems in Zambia', https://hdl.handle.net/11529/10825, CIMMYT Research Data & Software Repository Network, V3",
		## if there is a paper, include the paper's doi here
		## also add a RIS file in references folder (with matching doi)
		publication= NA,
		data_institutions = "CIMYTT",
   		data_type="on-farm experiment",
		carob_contributor="Mitchelle Njukuya"  ,
		carob_date="2023-08-23"
	)

## download and read data 
	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=3, minor=1)
	dset$license <- carobiner::get_license(js)
  dset$title <- carobiner::get_title(js)
	dset$authors <- carobiner::get_authors(js)
	dset$description <- carobiner::get_description(js)


	f <- ff[basename(ff) == "Summary Zambia On-farm Demonstration 2006-2015.xls"]
	r1 <- carobiner::read.excel(f, sheet = "Zambia all sites all maize", fix_names=TRUE)
	r2 <- carobiner::read.excel(f, sheet = "Zambia all legume all years", fix_names=TRUE)
	
############################### Zambia all sites all maize#####################
	d1 <- data.frame(adm2=r1[,3], location=r1[,4], treatment=r1$Tmnt, 
			yield=r1$Grain.yield.kg.ha, residue_yield=r1$Stalk.yield.kg.ha, 
			rep=r1$Rep, trial_id=r1$site.rep, crop=tolower(r1$Crop.grown), 
			plant_density=r1$Final.stand.pl.ha, harvest_date=as.character(r1[,2]))
	# one missing value
	d1$crop[is.na(d1$crop)] <- "maize"
	
##### Fertilizers #####
## note that we use P and K, not P2O5 and K2O
## P <- P2O5 / 2.29
## K <- K2O / 1.2051
   
   
   
   

###########################Zambia all legume all years#############################################

	d2 <- data.frame(adm2=r2[,3], location=r2[,4], treatment=r2$Tmnt, 
			yield=r2$Grain.yield.kg.ha, residue_yield=r2$Stalk.yield.kg.ha, 
			trial_id=r2$Site.rep, crop=tolower(r2$Crop.grown), 
			plant_density=r2$Final.stand.pl.ha, harvest_date=as.character(r2[,2]))


##### Fertilizers #####


	d <- carobiner::bindr(d1, d2)

	geo <- data.frame(
		adm2 = c("Chibombo", "Chipata", "Chipata", "Chipata", "Chipata", "Kabwe", "Kabwe", "Katete", "Katete", "Lundazi", "Lundazi", "Monze"), 
		location = c("Chibombo", "Chanje", "Kapara", "Kayowozi", "Mtaya", "Waya", "Waya Camp", "Kafumbwe", "Kawalala", "Hoya", "Vuu", "Malende"), 
		longitude = c(28.0889, 32.8515, 32.517, 32.588, 32.05, 27.9833, 27.9833, 32.065, 31.808, 33.154, 33.0356, 27.606), 
		latitude = c(-14.6554, -13.3812, -13.519, -13.692, -14.3067, -14.3667, -14.3667, -14.063, -14.1996, -12.27, -12.267, -16.013))

	d$location[d$location == "Waya camp"] <- "Waya Camp"	
	d <- merge(d, geo, by=c("adm2", "location"), all.x=TRUE)
	

	d$dataset_id <- dataset_id
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	d$country <- "Zambia"
	d$yield_part <- "grain" 
	d$inoculated <- FALSE

	d$crop <- gsub("cowpeas", "cowpea", d$crop)
	d$planting_date <- as.character(NA)
	d$rep <- as.integer(d$rep)
	d$trial_id <- as.character(d$trial_id)

	d <- d[!is.na(d$yield), ]

# all scripts must end like this
	carobiner::write_files(dset, d, path=path)
}
