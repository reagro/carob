# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"Description:

    [copy the abstract from the repo]

"

	uri <- "hdl:11529/10829"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "conservation_agriculture"
	## dataset level data 
	dset <- data.frame(
		dataset_id = dataset_id,
		group=group,
		project=NA,
		uri=uri,
		data_citation='Thierfelder, Christian, 2016, "Options available for the management of drought-tolerant maize varieties and conservation agriculture practices in Malawi", https://hdl.handle.net/11529/10829, CIMMYT Research Data & Software Repository Network, V1',
		## if there is a paper, include the paper's doi here
		## also add a RIS file in references folder (with matching doi)
		publication=NA,
		data_institutions = "CIMMYT",
   		data_type="on-farm experiment",
		carob_contributor="Mitchelle Njukuya",
		carob_date="2023-06-21"
	)

## download and read data 

	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=1)
	dset$license <- carobiner::get_license(js)


	f <- ff[basename(ff) == "Summary files Malawi 2005-15..xlsx"]

	
	r <- readxl::read_excel(f,sheet="Maize working data") |> as.data.frame()
	r1 <- readxl::read_excel(f,sheet="Legume yields") |> as.data.frame()
	r2 <- readxl::read_excel(f,sheet="Intercropped legume yields") |> as.data.frame()

	
## process file(s)

## use a subset
	d <- r
	d$country <- "Malawi"
	d$adm1 <- d$District
	d$adm2 <- d$Village
	d$intercrop <- NA
	d$intercrop[d$`sole/intercrop`==1 & d$treatment %in% c("CA +Maize/Pp", "CA+maize/Pp", "Maizepp")] <- "pigeon pea"
	d$intercrop[d$`sole/intercrop`==1 & d$treatment %in% c("CA +Maize/mucuna")] <- "velvet bean"
	d$intercrop[d$`sole/intercrop`==1 & d$treatment %in% c("CA+Maize/Cp")] <- "cowpea"
	d$yield <- d$`Grain yield (kg/ha)`
	d$crop <- d$`crop grown`
	d$dataset_id <- dataset_id
	d$on_farm <- TRUE 
	d$is_survey <- FALSE
	d$is_experiment <- TRUE 
	d$irrigated <- FALSE
	
	d <- d[,c("dataset_id","country","adm1","adm2","treatment","variety","crop","intercrop","on_farm","irrigated","is_survey","yield")]
	
	#merging with latidude and longitude for districts
	d <- merge(d, districts$df, by.x = c("country" , "adm1"), by.y = c("country", "location"))
	dd <- merge(d, districts$df, by.x = c("country" , "adm2"), by.y = c("country", "location"))
	
	districts <- carobiner::geocode(country= "Malawi",location = unique(d[is.na(d$lat) & is.na(d$lon), "adm1"]))
	d <- merge(d, districts$df, by.x = c("country" , "adm1"), by.y = c("country", "location"))
	
	#Looping
	for (row in 1:nrow(districts$df)) {
	  distr <- districts$df[row,]
	  d[d$country == distr$country & d$adm1 == distr$location & is.na(dd$lon), "lon"] <- distr$lon
	  d[d$country == distr$country & d$adm1 == distr$location & is.na(dd$lat), "lat"] <- distr$lat
	}
		dd$lon[dd$adm1=="Mchinga"] <- 35.6026
	dd$lat[dd$adm1=="Mchinga"] <- -14.9027
	d1 <- r1
	d1$country <- "Malawi"
	d1$site <- d1$Site
	d1$treatment <- d1$Tmnt.
	d1$crop <- d1$`Crop grown`
	d1$plant_density <- d1$`Final stand (pl/ha)`
	d1$biomass_leaves <- d1$`Above ground biomass yield (kg/ha)`
	d1$biomass_total <- d1$`total Biomass yield (kg/ha)`
	d1$yield <- d1$`Grain yield (kg/ha)`
	d1$dataset_id <- dataset_id
	d1$on_farm <- TRUE 
	d1$is_survey <- FALSE
	d1$is_experiment <- TRUE 
	d1$irrigated <- FALSE
	d1 <- d1[,c("country","dataset_id" ,"site","crop","treatment","biomass_leaves", "biomass_total","plant_density","yield","irrigated","on_farm","is_experiment")]
	d1$lon[d1$site=="Mwansambo"] <- 34.1178 
	d1$lat[d1$site=="Mwansambo"] <- -13.3100
	d1$lon[d1$site=="Kaluluma"] <- 33.5190
	d1$lat[d1$site=="Kaluluma"] <- -12.5818
	d1$lon[d1$site=="Songani"] <- 35.4459
	d1$lat[d1$site=="Songani"] <- -15.3026
	d1$lon[d1$site=="Linga"] <- 33.7654
	d1$lat[d1$site=="Linga"] <- -14.0060
	d1$lon[d1$site=="Malula"] <- 
		# Villages
	village <- carobiner::geocode(country= "Malawi",location = unique(d$adm2))
	dd <- merge(d, village$df, by.x = c("country" , "adm2"), by.y = c("country", "location"))
	
	#merging with latitude and longitude for districts
	districts <- carobiner::geocode(country= "Malawi",location = unique(dd[is.na(dd$lat) & is.na(dd$lon), "adm1"]))
	dd <- merge(dd, districts$df, by.x = c("country" , "adm1"), by.y = c("country", "location"))
	
	#Looping
	for (row in 1:nrow(districts$df)) {
	  distr <- districts$df[row,]
	  dd[dd$country == distr$country & dd$adm1 == distr$location & is.na(dd$lon), "lon"] <- distr$lon
	  dd[dd$country == distr$country & dd$adm1 == distr$location & is.na(dd$lat), "lat"] <- distr$lat
	}
	
	dd$lon[dd$adm1=="Mchinga"] <- 35.6026
	dd$lat[dd$adm1=="Mchinga"] <- -14.9027 
	
	#joining tables
	d4 <- carobiner::bindr(d, d1)
	
	d2 <- r2
	d2$country <- "Malawi"
	d2$site <- d2$`Site name`
	d2$treatment <- d2$Tmnt.
	d2$crop <- d2$`Crop grown`
	d2$plant_density <- d2$`Final stand (pl/ha)`
	d2$biomass_leaves <- d2$`Above ground biomass yield (kg/ha)`
	d2$biomass_total <- d2$`total Biomass yield (kg/ha)`
	d2$yield <- d2$`Grain yield (kg/ha)`
	d2$dataset_id <- dataset_id
	d2$on_farm <- TRUE 
	d2$is_survey <- FALSE
	d2$is_experiment <- TRUE 
	d2$irrigated <- FALSE
	d2 <- d2[,c("country","dataset_id" ,"site","crop","treatment","biomass_leaves", "biomass_total","plant_density","yield","irrigated","on_farm","is_experiment")]
	
	#joining tables
	d5 <- carobiner::bindr(d, d1, d2)
	
	
#### about the data #####
## (TRUE/FALSE)

	d$dataset_id <- dataset_id
	d$on_farm <- TRUE 
	d$is_survey <- FALSE
	d$is_experiment <- TRUE 
	d$irrigated <- FALSE
## the treatment code	
	d$treatment <- d$Tmnt
	

##### Location #####
## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
## you can use carobiner::fix_name()
 
	
# all scripts must end like this
	carobiner::write_files(dset, d5, path=path)
}

## now test your function in a clean R environment 
# path <- _____
 carob_script(path)

