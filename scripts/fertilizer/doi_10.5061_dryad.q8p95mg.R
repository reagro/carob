

carob_script <- function(path) {

"
Title: Bean yield and economic response to fertilizer in eastern and southern Africa

Description: Bean (Phaseolus vulgaris L.) is important in sub-Saharan Africa for human dietary protein. Low yields are attributed to biotic and abiotic constraints including inadequate nutrient availability. Research was conducted to determine nutrient response functions for bean production areas of Kenya, Mozambique, Rwanda, Tanzania, and Zambia.
"

	uri <- "doi.org/10.5061/dryad.q8p95mg"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "fertilizer"
  
  ## data set level data0
	dset <- data.frame(
		dataset_id = dataset_id, 
		group=group, 
		uri=uri, 
		publication= "https://doi.org/10.1007/s10705-018-9915-9", 
		project="Optimizing Fertilizer Use in Africa", 
		data_citation = "Kaizzi, K. C. et al. (2018), Data from: Bean yield and economic response to fertilizer in eastern and southern Africa, Dryad, Dataset, https://doi.org/10.5061/dryad.q8p95mg", 
		data_institutions = "University of Nebraska - Lincoln", 
		carob_contributor="Rachel Mukami", 
		data_type="on_farm & on_station")
	
	## download and read data
		
	ff <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=0)
	dset$license <- carobiner::get_license(js)
	#fpdf <- carobiner::get_more_data(url, dataset_id, path, group)
	
	# reading the datasets
	f <- ff[basename(ff) == "ESA Bean Nutrient Response Dataset.xlsx"]
	
### Kenya
	r <- readxl::read_xlsx(f, sheet = 1)
	r$Yr[d0$Yr == "2015b"] <- "2015"

	d0 <- data.frame(
		country = "Kenya",
		adm1 = NA,
		adm2 = NA,
		location = carobiner::fix_name(r$S, "title"),
		planting_date = r$Yr,
		trial_id = paste0("KE_", r$S),
		variety = NA, # GLP2 bean variety from publication??
		N_fertilizer = r$Nrate,
		P_fertilizer = r$Prate,
		K_fertilizer = r$Krate,
		treatment = r$Trtcode, 
		rep = r$Rep,
		yield = r$GrainYld
	)
	# why?
	#d0[d0$trial_id == "KE_Kisii"] <- "KE_Migori" 
	
### Mozambique
	r <- readxl::read_xlsx(f, sheet = 2)
	d1 <- data.frame(
		country = "Mozambique",
		adm1 = NA,
		adm2 = r$District,
		location=NA,
		planting_date = r$Y,
		trial_id = paste0("MZ_", r$District),
		variety = r$v,
		N_fertilizer = r$n,
		P_fertilizer = r$p,
		K_fertilizer = r$k,
		treatment = paste0(r$n, "N", r$p, "P", r$k, "K", "v", r$v, "d", r$Diagnostic),
		rep = r$Rep,
		yield = r$GrainYld
	)


### Rwanda
	r <- readxl::read_xlsx(f, sheet = 3)
	r$Prov[r$Tr %in% c("E_Ngoma_Sake_15A", "E_Busegerwa_Mareba_15A")] <- "E"

	d2 <- data.frame(
		country="Rwanda",
		adm1 = ifelse(r$Prov == "N", "Amajyaruguru", 
				ifelse(r$Prov == "E", "Iburasirazuba", "Amajyepfo")),
		adm2 = NA,
		location=NA,
		planting_date = ifelse(grepl("14", r$Tr), 2014, 2015),
		trial_id = paste0("RW_", r$Tr),
		variety = ifelse(r$BT == "CL", "MAC44", "RWR2245"),
		N_fertilizer = r$N,
		P_fertilizer = r$P,
		K_fertilizer = r$K,
		treatment = r$Treatment,
		rep = r$Rep,
		yield = r$GrainYld
	)

### Tanzania

	r <- readxl::read_xlsx(f, sheet = 4) 
	r$S[r$S %in% c("Kar", "Kar2")] <- "Karangai"
	r$Diagnostic[is.na(r$Diagnostic)] <- 0
	
	d3 <- data.frame(
		country = "Tanzania",
		adm1 = NA,
		adm2 = NA,
		location = r$S,
		planting_date = r$Yr,
		trial_id = paste0("TZ_", r$S, r$Yr),
		variety = NA,
		N_fertilizer = r$n,
		P_fertilizer = r$p,
		K_fertilizer = r$k,
		treatment = paste0(r$n, "N", r$p, "P", r$k, "K", "diag", r$Diagnostic),
		rep = r$Rep,
		yield = r$GrainYld
	)
	
### Zambia
	r <- readxl::read_xlsx(f, sheet = 5)
	r$Diagnostic[is.na(r$Diagnostic)] <- 0
	
	d4 <- data.frame(
		country = "Zambia",
		adm1 = NA,
		adm2 = NA,
		location = r$S,
		planting_date = r$Yr,
		trial_id = paste0("ZM_", r$S),
		variety = NA,
		N_fertilizer = r$N,
		P_fertilizer = r$P,
		K_fertilizer = r$K,
		treatment = paste0(r$N, "N", r$P, "P", r$K, "K", "diag", r$Diagnostic),
		rep = r$R,
		yield = r$GrainYld
	)

	d4$location[d4$location == "Mt Makulu"] <- "Mt. Makulu"


	#merging all countries datasets
## why merge? That is a bad mistake
##	z <- Reduce(function(...) merge(..., all=T), list(d, d1, d2, d3, d4))

	z <- rbind(d1, d2, d3, d4)
	
	z$rep <- as.integer(z$rep)
	z$crop = "common bean" 
	z$dataset_id <- dataset_id
	z$inoculated <- FALSE
	z$yield_part <- "grain"
	z$irrigated <- FALSE
	z$variety_type <- ifelse(z$variety %in% c("RWR2245", "GLP2"), "bush bean", 
						ifelse(z$variety == "MAC44", "climbing bean", NA))
	z$yield <- z$yield*1000 # converting to kg/ha from Mg/ha


	#z <- z[, c("dataset_id", "trial_id", "country", "adm1", "adm2", "location", "latitude", "longitude", "elevation", "rep", "treatment", "crop", "variety", "variety_type", "inoculated", "irrigated", "yield", "yield_part", "N_fertilizer", "P_fertilizer", "K_fertilizer", "soil_type", "soil_pH",  "soil_SOC", "soil_P_total", "soil_K", "soil_Mg")]


#	z <- merge(z, d5, by = "trial_id", all.x = TRUE)	
#	z <- merge(z, d6, by = "trial_id", all.x = TRUE)	
#	z <- merge(z, d6, by = "trial_id", all.x = TRUE)	
	
#	z$latitude <- gsub("[^-0-9.]", "", z$latitude)
#	z$latitude <- as.numeric(z$latitude)
#	z$longitude <- as.numeric(z$longitude)
#	z$elevation <- as.numeric(z$elevation)
#	z$soil_pH <- as.numeric(z$soil_pH)
#	z$soil_SOC <- as.numeric(z$soil_SOC)
#	z$soil_SOC[z$soil_SOC > 20] <- NA
#	z$soil_P_total <- as.numeric(z$soil_P_total)
#	z$soil_K <- as.numeric(z$soil_K)
#	z$soil_Mg <- as.numeric(z$soil_Mg)
	
#	z$latitude <- ifelse(z$trial_id == "E_Busegerwa_Mushikiri_14B", -2.18893865, 
#					ifelse(z$trial_id == "ENGOMAUFITUB15b", -2.1663637, 
#					ifelse(z$trial_id == "E_Busegerwa_Musenyi_14B", 	-2.17889725, z$latitude)))
	
#	z$longitude <- ifelse (z$trial_id == "E_Busegerwa_Mushikiri_14B", 30.68517191848185, 
#					ifelse(z$trial_id == "ENGOMAUFITUB15b", 30.5391524, 
#					ifelse(z$trial_id == "E_Busegerwa_Musenyi_14B", 30.02089217993953, z$longitude)))

	z$planting_date <- as.character(z$planting_date)
	# all scripts must end like this
	carobiner::write_files(dset, z, path=path)
}



process_pdf_doi_10.5061_dryad.q8p95mg <- function() {

	purl <- "https://xxxx"
	path <- "c:/"
	did <- "doi_10.5061_dryad.q8p95mg"
	fpdf <- carobiner::get_more_data(purl, did, path, group="fertilizer")
	
	extract_from_pdf <- function(f, pages) {
		p <- package_name::extract_areas(f, pages = pages)
		as.data.frame(p)
	}

	# Publication Table 1 Site-year, FAO dominant soil type, growth habit (GH) of climbing (CL) and bush (BB) type, coordinates, mean site-year yield, and response to a diagnostic treatment (Diag) for bean nutrient response trials conducted in Rwanda during 2014–2015""
#	d5 <- as.data.frame(extract_areas("C:/Users/User/Downloads/s10705-018-9915-9.pdf", pages = 3))

	d5 <- extract_from_pdf(fpdf, pages = 3)
	names(d5) <- c("Site-year_soil type", "GH", "Lat (WGS84°)a", "Long (WGS84°)", "Elev (m)", "Yield (Mg ha−1", "Diag (Mg ha−1)c")
	
	d5$`Site-year_soil type`[1] <- gsub("Nyagat", "Nyagat_", d5$`Site-year_soil type`[1])
	d5$`Site-year_soil type`[1] <- gsub("b", "", d5$`Site-year_soil type`[1])
	d5$soil_type <- sapply(d5$`Site-year_soil type`, function(x) strsplit(x, "_")[[1]][4])
	d5$trial_id <- paste0(sapply(d5$`Site-year_soil type`, function(x) strsplit(x, "_")[[1]][1]), "_", 
						sapply(d5$`Site-year_soil type`, function(x) strsplit(x, "_")[[1]][2]), "_", 
						sapply(d5$`Site-year_soil type`, function(x) strsplit(x, "_")[[1]][3]))
	d5 <- d5[, c("trial_id", "Lat (WGS84°)a", "Long (WGS84°)", "Elev (m)", "soil_type")]
	names(d5)[2] <- "latitude"
	names(d5)[3] <- "longitude"
	names(d5)[4] <- "elevation"

	# Publication Table 2 Mean trial yield, location, FAO dominant soil type, year, mean yield (Mg ha−1), latitude, longitude, elevation (m) and variety information for bean nutrient response trials conducted in Kenya, Tanzania, Zambia and Mozambique

	
#	d6 <- as.data.frame(extract_areas("C:/Users/User/Downloads/s10705-018-9915-9.pdf", pages = 4))
	d6 <- extract_from_pdf(fpdf, pages = 4)

	names(d6) <- c("Site_soil type", "Year", "Lat", "Long", "Elev", "Variety", "Yield")
	d6$`Site_soil type`<- gsub("-", "_", d6$`Site_soil type`)
	d6$`Site_soil type`<- gsub(" ", "", d6$`Site_soil type`)
	d6$soil_type <- sapply(d6$`Site_soil type`, function(x) strsplit(x, "_")[[1]][3])
	d6$trial_id <- paste0(sapply(d6$`Site_soil type`, function(x) strsplit(x, "_")[[1]][1]), "_", sapply(d6$`Site_soil type`, function(x) strsplit(x, "_")[[1]][2]))
	d6$trial_id[d6$trial_id == "TZ_Selian" & d6$Year == 2014] <- "TZ_Selian14"
	d6$trial_id[d6$trial_id == "TZ_Selian" & d6$Year == 2015] <- "TZ_Selian15"
	d6$trial_id[d6$trial_id == "TZ_Uyole" ] <- "TZ_Uyole15"
	d6 <- d6[, c("trial_id", "Year", "Lat", "Long", "Elev", "Variety", "soil_type")]
	names(d6) <- carobiner::replace_values(names(d6), 
				c("Year", "Lat", "Long", "Elev", "Variety"), 
				c("season", "latitude", "longitude", "elevation", "variety"))
	d6 <- unique(d6)
	
	# Publication Table 3 Soil test information of the 0 to 20 cm depth for bean-nutrient response trials conducted in eastern and southern Africa
	#d7 <- as.data.frame(extract_areas("C:/Users/User/Downloads/s10705-018-9915-9.pdf", pages = 5))

	d7 <- extract_from_pdf(fpdf, pages = 5)
	names(d7) <- c("Site-year", "TC", "pH", "SOC(g kg-1)", "P(mg kg-1)", "K(mg kg-1)", "Mg(mg kg-1)", "S(mg kg-1)", "Zn(mg kg-1)", "B(mg kg-1)")
	d7$`Site-year`[d7$`Site-year` == "RW_ENyagatKat14B"] <- "RW_ENyagat_Kat14B"
	d7 <- d7[, c("Site-year", "pH", "SOC(g kg-1)" , "P(mg kg-1)", "K(mg kg-1)", "Mg(mg kg-1)")]
	names(d7) <- carobiner::replace_values(names(d7), 
		c("Site-year", "pH", "SOC(g kg-1)" , "P(mg kg-1)", "K(mg kg-1)", "Mg(mg kg-1)"), 
		c("trial_id", "soil_pH", "soil_SOC", "soil_P_total", "soil_K", "soil_Mg"))


	list(d5=dput(d5), d6=dput(d6), d7=dput(d7))
}
