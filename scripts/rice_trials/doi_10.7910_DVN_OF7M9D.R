# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {
  
"
Description:
In 2010, the Africa-wide Rice Breeding Task Force was launched by AfricaRice involving National Agricultural Research System (NARS) from about 30 countries. The objectives of the network are to evaluate the stability of traits incorporated in breeding processes and to identify varieties best fit to growth conditions in target regions and to markets. The Task Force also accumulates data on performance of new elite lines, thereby facilitating varietal release procedures. Furthermore, by exposing breeders from NARS and farmers to these elite lines during the testing phase, dissemination will be facilitated. The activities conducted by the Task Force consists of a series of consecutive trials. Promising breeding lines developed by AfricaRice or by national and international partners, such as IRRI, CIAT and the NARS are nominated for evaluation in one or several rice cultivation environments: rainfed lowland, irrigated lowland, rainfed upland, high elevation and mangrove. All nominated lines should be fixed and accompanied by supporting data on traits incorporated during the breeding process and with information on yield performance. These characteristics are checked at AfricaRice before incorporation into the network. The first phase (MET, Multi-Environment Testing) consists of an initial evaluation of about 100 lines selected from the nominated lines. Each national partner evaluates these lines at sites in their country. Such sites may be at an experimental station under optimal management to evaluate yield potential, or may be ‘hot spots’ to check the performance of the nominations in a stressed growth environment. Trials are replicated three times and include at least a common and a local check. The second phase (PET,Participatory Evaluation Trial) serves to evaluate and confirm the performance of the selected lines. These lines are cultivated using the same experimental design with 3 replications. An important feature of PET is that farmer and other stakeholders such as miller and traders are invited to participate in varietal selection and their opinion on the performance of all entries (i.e. participatory varietal selection, PVS) collected. Based on the data collected, observations by the breeders and the opinion of stakeholder groups, NARS partners select up to 10 lines. Further, NARS evaluated these lines in at least three sites per country and during one or more growing seasons, depending on varietal release requirements. All stakeholders are again invited to get acquainted with the new lines and voice their opinion to help select lines for further advancement. Among the 10 lines, farmers are invited to select three lines and cultivate these in their own fields, together with a common check and their own variety.

"
  
  uri <- "doi:10.7910/DVN/OF7M9D"
  dataset_id <- carobiner::simple_uri(uri)
  group <- "rice_trials"
  ## dataset level data 
  dset <- data.frame(
    dataset_id = dataset_id,
    group=group,
	project=NA,
    uri=uri,
	data_citation="Fofana, Mamadou, 2017. 2016 Africa-wide Breeding Task Force Trials for Rainfed Upland - MET. https://doi.org/10.7910/DVN/OF7M9D, Harvard Dataverse, V1",
    publication=NA,
	data_institutions="AfricaRice",
    carob_contributor="Eduardo Garcia Bendito",
    carob_date="2022-01-21",
    data_type="experiment"
  )
  
  ## download and read data 
  
  ff  <- carobiner::get_data(uri, path, group)
  js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=0)
  dset$license <- carobiner::get_license(js)[[1]]
  dset$title <- carobiner::get_title(js)
  
  d <- list()
  ## Process all country files in a loop, since all have similar structure. Then append them together
  xlfiles <- grep("\\.xlsx$", ff, value=TRUE)
  for (i in 1:length(xlfiles)) {
	dd <- data.frame(readxl::read_excel(ff[i]))
    colnames(dd) <- tolower(colnames(dd))
    dd$variety_code <- dd$genotype
	# Burkina Faso and Mali miss the grain weight data
    dd$grain_weight <- ifelse("gw1000" %in% colnames(dd), dd$gw1000, NA) 
    d[[i]] <- dd[,c("country", "site", "season", "variety_code", "yield", "grain_weight")]
  }

	d <- do.call(rbind, d)
    d$country <- gsub("Cote d'Ivoire", "Côte d'Ivoire", d$country)
	d$yield <- d$yield * 1000

    d$dataset_id <- dataset_id
    d$trial_id <- paste0(dataset_id, '-', dd$country)
    
	## RH: these are not trial start and end dates 
	#d$planting_date <- js$data$latestVersion$metadataBlocks$citation$fields$value[[15]]$timePeriodCoveredStart[[4]]
    #d$harvest_date <- js$data$latestVersion$metadataBlocks$citation$fields$value[[15]]$timePeriodCoveredEnd[[4]]
	d$planting_date <- d$harvest_date <- "2016"

    # Coordinates extracted using Geonames.org
	# RH: Africa Rice (CdI) and Bordo from Google
	# RH: Bordo = Bordo ENAE in Kankan, Guinea
	xy <- data.frame(country=c("Burkina Faso", "Benin", "Côte d'Ivoire", "Mali", "Nigeria", "Guinea"), 
		  longitude=c(-4.339967, 2.4239, -5.10362, -5.65644, 6.48478, -9.30609),
		  latitude =c(11.082302, 10.3079, 7.88761, 11.38856, 9.48267, 10.38971))

	d <- merge(d, xy, by="country", all.x=TRUE)

    # Rainfed Upland (RU) farming systems
    d$crop <- "rice"
	d$yield_part <- "grain"
    d$irrigated <- FALSE 
    d$is_survey <- FALSE
    d$on_farm <- TRUE
 
  # all scripts must end like this
	carobiner::write_files(dset, d, path=path)

}
