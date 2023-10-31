## ISSUES
##P_fertilizer data is in integers meaning it maybe a code for certain amount. This needs to be checked.
##Yield and biomass data is recorded as per area but its not clear what area we are working with.We assumed area is 200 m2 but this needs to be re-looked at once we get more feedback from Joost. 

carob_script <- function(path) {

"Description
Title:  N2Africa farm monitoring - Malawi, 2010 - 2011   
Abstract: 

N2Africa is to contribute to increasing biological nitrogen fixation and 
productivity of grain legumes among African smallholder farmers which will 
contribute to enhancing soil fertility, improving household nutrition and 
increasing income levels of smallholder farmers. As a vision of success, 
N2Africa will build sustainable, long-term partnerships to enable African 
smallholder farmers to benefit from symbiotic N2-fixation by grain legumes 
through effective production technologies including inoculants and fertilizers 
adapted to local settings. A strong national expertise in grain legume 
production and N2-fixation research and development will be the legacy of the 
project.

The project is implemented in five core countries (Ghana, Nigeria, Tanzania, 
Uganda and Ethiopia) and six other countries (DR Congo, Malawi, Rwanda, 
Mozambique, Kenya & Zimbabwe) as tier one countries.
"
	## Process 
	uri <- "doi:10.25502/8YQ1-DM57/D"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "fertilizer"
	dset <- data.frame(
		dataset_id = dataset_id,
		group=group,
		uri=uri,
		data_citation="Vanlauwe, B., Adjei-Nsiah, S., Woldemeskel, E., Ebanyat, P., 
		Baijukya, F., Sanginga, J.-M., Woomer, P., Chikowo, R., Phiphira, L., Kamai,
		N., Ampadu-Boakye, T., Ronner, E., Kanampiu, F., Giller, K., Baars, E., & 
		Heerwaarden, J. van. (2020). N2Africa farm monitoring - Malawi, 2010 - 2011 
		[Data set]. International Institute of Tropical Agriculture (IITA). 
		https://doi.org/10.25502/8YQ1-DM57/D ",
		publication=NA,
		carob_contributor="Siyabusa Mkuhlani",
		carob_date="2023-07-27",
		data_type="survey",
		data_institutions="IITA",
		project="N2Africa"
	)

	## treatment level data 
	ff  <- carobiner::get_data(uri, path, group)

	## read the json for version, license, terms of use  
	js <- carobiner::get_metadata(dataset_id, path, major=1, minor=0, group)
	dset$license <- carobiner::get_license(js)[[1]]

	b <- ff[basename(ff) == "c_use_of_package_2.csv"] 
	# e <- read_csv("data/raw/fertilizer/doi_10.25502_8YQ1-DM57_D/c_use_of_package_2.csv")
	b <- read.csv(b)
  b <- b[,c(3,4,5,6,9,12,13)]
  names(b)
  names(b) <- c('farm_id','rep','crop','variety','inoculated','fertilizer_type','P_fertilizer')
	names(b)
	
	# k <- read_csv("data/raw/fertilizer/doi_10.25502_8YQ1-DM57_D/d_cropping_calendar.csv")
	k <- ff[basename(ff) == "d_cropping_calendar.csv"] 
	k <- read.csv(k)
	k <- as.data.frame(k[,c(3,4,5,6,22,23,24)])
	p <- k[,2]
	q <- k[,3]
	r <- k[,4]
	s <- k[,5]
	t <- k[,6]
	u <- k[,7]

	k$planting_date <- as.character(as.Date(paste(r,q,p,sep='-')))
	k$harvest_date <- as.character(as.Date(paste(u,t,s,sep='-'),format='%Y-%m-%d'))
	
	names(k)
	k <- k[,c(1,8,9)]
  names(k) <- c('farm_id',"planting_date", "harvest_date")

	n <- ff[basename(ff) == "e_harvest.csv"] 
	n <- read.csv(n)
	n <- n[,c(3,4,7,8)]
	names(n)
	names(n) <- c('farm_id',"rep", "biomass_total", "yield")
	
	d <- Reduce(function(...) merge(..., all=T), list(b,k,n))
	names(d)

	#transfer columns
	d$country <- "Malawi"
	d$longitude <- 34.30153
	d$latitude <- -13.25431
	d$dataset_id <- carobiner::simple_uri(uri)
	d$trial_id <- 'N2Africa_farm_monitoring_Mal_2010_11'
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$N_fertilizer <- 0
	d$K_fertilizer <- 0
	d$dataset_id <- dataset_id
	d$yield_part <- 'grain'
	
	names(d)
	d$rep <- as.integer(d$rep)
	d$yield <- as.numeric(d$yield)
	d$biomass_total <- as.numeric(d$biomass_total)
	
	#Extrapolate crop yield values. #Needs revision
	d$yield <- (10000/200*(d$yield))
	d$biomass_total <- (10000/200*(d$biomass_total))
	
	#Remove the 'farmer_id' we have been using for merging the data sets.
	d <- d[,-c(1)]
	
  #remove rows with NAs in key variables e.g. crops.
	d <- subset(d,d$crop>0)
	
	#Rename objects in a column
	d$crop[d$crop == "bush bean"] <- "common bean"
  d[['variety']][d[['variety']]=='not sure'] <- NA
  d[['fertilizer_type']][d[['fertilizer_type']]=='TSP'] <- 'TSP'
  d[['fertilizer_type']][d[['fertilizer_type']]=='tsp'] <- 'TSP'
  d[['fertilizer_type']][d[['fertilizer_type']]=='tsP'] <- 'TSP'
  
  d$inoculated <- ifelse(d$inoculated=='N',FALSE,TRUE)

	carobiner::write_files(dset, d, path=path)
}




