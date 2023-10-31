# R script for "carob"

# ## ISSUES 

#RH: perhaps we should capture more managment variables 


# ....


carob_script <- function(path) {
  
  "Description:
Farmer participatory on-farm trials with CA technologies comparing with farmers’ practices (CT), were conducted in several fields in each community. Likewise, farmer-participatory alternative cropping systems trials were conducted comparing to existing systems and to find out suitable and more profitable cropping systems, prioritized to increase visibility and to avoid implementation and management problems that emerge when utilizing small plots with significant edge effects. Most trials were replicated in several fields within each community and were farmer-managed with backstopping from project staff and NARES partners. Project partners and staff coordinated monitoring and data acquisition. Where possible, collaborating farmers were selected by the community, and the project worked with existing farmer groups, with groups of both men and women farmers.
  "
  
  uri <- "hdl:11529/10547997"
  dataset_id <- carobiner::simple_uri(uri)
  group <- "conservation_agriculture"
  ## dataset level data 
  dset <- data.frame(
    dataset_id = dataset_id,
    group=group,
    project="Rabi (winter) crops-all nodes-Alternative cropping systems trial-Sunsari-Nepal",
    uri=uri,
    data_citation= "Gathala, Mahesh K. (CIMMYT) - ORCID: 0000-0001-8282-2953, Tiwari, Thakur P. (CIMMYT), Islam, Saiful (CIMMYT) - ORCID: 0000-0002-6482-5031, Shrestha, Renuka (Nepal Agricultural Research Council), Shrestha, H.K. (Nepal Agricultural Research Council), Manandhar, S. (Nepal Agricultural Research Council) - ORCID: 0000-0002-6353-3539, Shrestha, Shukra Raj (Nepal Agricultural Research Council).",
    publication= NA,
    data_institutions = "CIMMYT",
    data_type="on-farm experiment",
    carob_contributor="Fredy chimire",
    carob_date="2023-10-31"
  )
  
  ## download and read data 
  
	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=3)
 # dset$license <- "not specified" #carobiner::get_license(js)
	dset$license <- carobiner::get_license(js)
    
	get_data <- function(f) {
		r1 <- carobiner::read.excel.hdr(f, sheet ="4- Stand counts & Phenology", skip=4, hdr=2)
		r2 <- carobiner::read.excel.hdr(f, sheet ="14 - Grain Harvest ", skip=4, hdr=2)
		r3 <- carobiner::read.excel.hdr(f, sheet ="6 - Fertilizer amounts ", skip=4, hdr=2)

		nms <- c("Site.No", "Tmnt", "Grain.yield.t.ha", "TGW.g", "Biomass.t.ha", "Straw.yield.t.ha")
		r2 <- r2[, nms]
		nms <- c("Site.No", "Tmnt", "N.kg.ha", "P2O5.kg.ha", "K2O.kg.ha", "Gypsum.kg.ha", "ZnSO4.kg.ha", "Boric.acid.kg.ha", grep("Product.used", names(r3), value=TRUE))
		r3 <- r3[, nms]

		r <- merge(r1, r2, by=c("Site.No", "Tmnt"))
		merge(r, r3, by=c("Site.No", "Tmnt"))
	}

	f <- ff[basename(ff) == "Maize-Rabi 2016-17-ACS-Bhokraha-Sunsari.xlsx"]
	r <- get_data(f)	
	
  #### about the data #####
  
	d <- data.frame(trial_id = as.character(r$Trial.Code), season = r$Season,
			crop=tolower(r$Crop), variety= r$Variety, 
			treatment = r$Tmnt, 
			yield = r$Grain.yield.t.ha * 1000,  
			biomass_total = r$Biomass.t.ha * 1000,
			residue_yield = r$Straw.yield.t.ha * 1000,
			N_fertilizer = r$N.kg.ha,
			P_fertilizer = r$P2O5.kg.ha / 2.29,
			K_fertilizer = r$K2O.kg.ha / 1.2051,
			B_fertilizer = r$Boric.acid.kg.ha * 0.1748,
			planting_date = as.character(as.Date(r$Date.of.seeding.dd.mm.yy)),
			harvest_date = as.character(as.Date(r$Datw.of.harvest.dd.mm.yy)),			
			flowering_date = as.character(as.Date(r$Date.of.50.anthesis.dd.mm.yy)),
			site = paste("site ", r$Site.No),
			row_spacing =r$Row.spacing.cm,
			country= "Nepal",
			dataset_id=dataset_id,
			S_fertilizer= 0,
			Zn_fertilizer= 0,
			lime =0, gypsum =0
		)
			
  
  # for first dataset
	d$yield_part <- "grain"
	d$irrigated <- TRUE
	d$adm2 <- "Sunsari" # district provided in the excel
	d$location <- "Bhokraha" # community provided in the excel
	d$crop <- "maize"
	d$latitude <- 26.5920003 # https://www.gps-coordinates.net/
	d$longitude <- 87.1025479
  	d$fertilizer_type <- apply(r[, grep("Product.used", names(r), value=TRUE)], 1, 
		function(i) {
			i <- gsub("Urea.*", "urea", i[!is.na(i)])
			i <- gsub("MOP", "KCl", i[!is.na(i)])
			paste(unique(i[!is.na(i)]), collapse="; ")
		})

	carobiner::write_files(dset, d, path=path)
}
