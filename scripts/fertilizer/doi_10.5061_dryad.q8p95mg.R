

carob_script <- function(path) {

"
Title: Bean yield and economic response to fertilizer in eastern and southern Africa

Description: Bean (Phaseolus vulgaris L.) is important in sub-Saharan Africa for human dietary protein. Low yields are attributed to biotic and abiotic constraints including inadequate nutrient availability. Research was conducted to determine nutrient response functions for bean production areas of Kenya, Mozambique, Rwanda, Tanzania, and Zambia.
"

	uri <- "doi:10.5061/dryad.q8p95mg"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "fertilizer"

  ## data set level data0
	dset <- data.frame(
		dataset_id = dataset_id,
		group=group,
		uri=uri,
		publication= "doi:10.1007/s10705-018-9915-9",
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
	ff <- carobiner::get_data(uri, path, group)
	f <- ff[basename(ff) == "ESA Bean Nutrient Response Dataset.xlsx"]

### Kenya
	r0 <- readxl::read_xlsx(f, sheet = 1)
	r0$Yr[r0$Yr == "2015b"] <- "2015"

	d0 <- data.frame(
		country = "Kenya",
		adm1 = NA,
		adm2 = NA,
		location = carobiner::fix_name(r0$S, "title"),
		planting_date = r0$Yr,
		trial_id = paste0("KE_", r0$S),
		variety = NA, # GLP2 bean variety from publication??
		N_fertilizer = r0$Nrate,
		P_fertilizer = r0$Prate,
		K_fertilizer = r0$Krate,
		treatment = r0$Trtcode,
		rep = r0$Rep,
		yield = r0$GrainYld
	)
	# why?
	#d0[d0$trial_id == "KE_Kisii"] <- "KE_Migori"

### Mozambique
	r1 <- readxl::read_xlsx(f, sheet = 2)
	d1 <- data.frame(
		country = "Mozambique",
		adm1 = NA,
		adm2 = r1$District,
		location=NA,
		planting_date = r1$Y,
		trial_id = paste0("MZ_", r1$District),
		variety = r1$v,
		N_fertilizer = r1$n,
		P_fertilizer = r1$p,
		K_fertilizer = r1$k,
		treatment = paste0(r1$n, "N", r1$p, "P", r1$k, "K", "v", r1$v, "d", r1$Diagnostic),
		rep = r1$Rep,
		yield = r1$GrainYld
	)


### Rwanda
	r2 <- readxl::read_xlsx(f, sheet = 3)
	r2$Prov[r2$Tr %in% c("E_Ngoma_Sake_15A", "E_Busegerwa_Mareba_15A")] <- "E"

	d2 <- data.frame(
		country="Rwanda",
		adm1 = ifelse(r2$Prov == "N", "Amajyaruguru",
				ifelse(r2$Prov == "E", "Iburasirazuba", "Amajyepfo")),
		adm2 = NA,
		location=NA,
		planting_date = ifelse(grepl("14", r2$Tr), 2014, 2015),
		trial_id = paste0("RW_", r2$Tr),
		variety = ifelse(r2$BT == "CL", "MAC44", "RWR2245"),
		N_fertilizer = r2$N,
		P_fertilizer = r2$P,
		K_fertilizer = r2$K,
		treatment = r2$Treatment,
		rep = r2$Rep,
		yield = r2$GrainYld
	)

### Tanzania

	r3 <- readxl::read_xlsx(f, sheet = 4)
	r3$S[r3$S %in% c("Kar", "Kar2")] <- "Karangai"
	r3$Diagnostic[is.na(r3$Diagnostic)] <- 0

	d3 <- data.frame(
		country = "Tanzania",
		adm1 = NA,
		adm2 = NA,
		location = r3$S,
		planting_date = r3$Yr,
		trial_id = paste0("TZ_", r3$S, r3$Yr),
		variety = NA,
		N_fertilizer = r3$n,
		P_fertilizer = r3$p,
		K_fertilizer = r3$k,
		treatment = paste0(r3$n, "N", r3$p, "P", r3$k, "K", "diag", r3$Diagnostic),
		rep = r3$Rep,
		yield = r3$GrainYld
	)

### Zambia
	r4 <- readxl::read_xlsx(f, sheet = 5)
	r4$Diagnostic[is.na(r4$Diagnostic)] <- 0

	d4 <- data.frame(
		country = "Zambia",
		adm1 = NA,
		adm2 = NA,
		location = r4$S,
		planting_date = r4$Yr,
		trial_id = paste0("ZM_", r4$S),
		variety = NA,
		N_fertilizer = r4$N,
		P_fertilizer = r4$P,
		K_fertilizer = r4$K,
		treatment = paste0(r4$N, "N", r4$P, "P", r4$K, "K", "diag", r4$Diagnostic),
		rep = r4$R,
		yield = r4$GrainYld
	)

	d4$location[d4$location == "Mt Makulu"] <- "Mt. Makulu"


	z <- rbind(d1, d2, d3, d4)
	z$rep <- as.integer(z$rep)
	z$crop <- "common bean"
	z$dataset_id <- dataset_id
	z$inoculated <- FALSE
	z$yield_part <- "grain"
	z$irrigated <- FALSE
	z$variety_type <- ifelse(z$variety %in% c("RWR2245", "GLP2"), "bush bean",
						ifelse(z$variety == "MAC44", "climbing bean", NA))
	# convert to kg/ha from Mg/ha
	z$yield <- z$yield*1000


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

#	z$latitude <- ifelse(z$trial_id == "E_Busegerwa_Mushikiri_14B", -2.1889,
#					ifelse(z$trial_id == "ENGOMAUFITUB15b", -2.1664,
#					ifelse(z$trial_id == "E_Busegerwa_Musenyi_14B", -2.1789, z$latitude)))

#	z$longitude <- ifelse (z$trial_id == "E_Busegerwa_Mushikiri_14B", 30.6852,
#					ifelse(z$trial_id == "ENGOMAUFITUB15b", 30.5392,
#					ifelse(z$trial_id == "E_Busegerwa_Musenyi_14B", 30.0209, z$longitude)))

	z$planting_date <- as.character(z$planting_date)
	# all scripts must end like this
	carobiner::write_files(path, dset, z)
}



# process_pdf_doi_10.5061_dryad.q8p95mg <- function() {
# 
# 	purl <- "https://xxxx"
# 	path <- "c:/"
# 	did <- "doi_10.5061_dryad.q8p95mg"
# 	fpdf <- carobiner::get_more_data(purl, did, path, group="fertilizer")
# 
# 	extract_from_pdf <- function(f, pages) {
# 		p <- package_name::extract_areas(f, pages = pages)
# 		as.data.frame(p)
# 	}
# 
# 	# Publication Table 1 Site-year, FAO dominant soil type, growth habit (GH) of climbing (CL) and bush (BB) type, coordinates, mean site-year yield, and response to a diagnostic treatment (Diag) for bean nutrient response trials conducted in Rwanda during 2014–2015""
# #	d5 <- as.data.frame(extract_areas("C:/Users/User/Downloads/s10705-018-9915-9.pdf", pages = 3))
# 
# 	d5 <- extract_from_pdf(fpdf, pages = 3)
# 	names(d5) <- c("Site-year_soil type", "GH", "Lat (WGS84°)a", "Long (WGS84°)", "Elev (m)", "Yield (Mg ha−1", "Diag (Mg ha−1)c")
# 
# 	d5$`Site-year_soil type`[1] <- gsub("Nyagat", "Nyagat_", d5$`Site-year_soil type`[1])
# 	d5$`Site-year_soil type`[1] <- gsub("b", "", d5$`Site-year_soil type`[1])
# 	d5$soil_type <- sapply(d5$`Site-year_soil type`, function(x) strsplit(x, "_")[[1]][4])
# 	d5$trial_id <- paste0(sapply(d5$`Site-year_soil type`, function(x) strsplit(x, "_")[[1]][1]), "_",
# 						sapply(d5$`Site-year_soil type`, function(x) strsplit(x, "_")[[1]][2]), "_",
# 						sapply(d5$`Site-year_soil type`, function(x) strsplit(x, "_")[[1]][3]))
# 	d5 <- d5[, c("trial_id", "Lat (WGS84°)a", "Long (WGS84°)", "Elev (m)", "soil_type")]
# 	names(d5)[2] <- "latitude"
# 	names(d5)[3] <- "longitude"
# 	names(d5)[4] <- "elevation"
# 
# 	# Publication Table 2 Mean trial yield, location, FAO dominant soil type, year, mean yield (Mg ha−1), latitude, longitude, elevation (m) and variety information for bean nutrient response trials conducted in Kenya, Tanzania, Zambia and Mozambique
# 
# 
# #	d6 <- as.data.frame(extract_areas("C:/Users/User/Downloads/s10705-018-9915-9.pdf", pages = 4))
# 	d6 <- extract_from_pdf(fpdf, pages = 4)
# 
# 	names(d6) <- c("Site_soil type", "Year", "Lat", "Long", "Elev", "Variety", "Yield")
# 	d6$`Site_soil type`<- gsub("-", "_", d6$`Site_soil type`)
# 	d6$`Site_soil type`<- gsub(" ", "", d6$`Site_soil type`)
# 	d6$soil_type <- sapply(d6$`Site_soil type`, function(x) strsplit(x, "_")[[1]][3])
# 	d6$trial_id <- paste0(sapply(d6$`Site_soil type`, function(x) strsplit(x, "_")[[1]][1]), "_", sapply(d6$`Site_soil type`, function(x) strsplit(x, "_")[[1]][2]))
# 	d6$trial_id[d6$trial_id == "TZ_Selian" & d6$Year == 2014] <- "TZ_Selian14"
# 	d6$trial_id[d6$trial_id == "TZ_Selian" & d6$Year == 2015] <- "TZ_Selian15"
# 	d6$trial_id[d6$trial_id == "TZ_Uyole" ] <- "TZ_Uyole15"
# 	d6 <- d6[, c("trial_id", "Year", "Lat", "Long", "Elev", "Variety", "soil_type")]
# 	names(d6) <- carobiner::replace_values(names(d6),
# 				c("Year", "Lat", "Long", "Elev", "Variety"),
# 				c("season", "latitude", "longitude", "elevation", "variety"))
# 	d6 <- unique(d6)
# 
# 	# Publication Table 3 Soil test information of the 0 to 20 cm depth for bean-nutrient response trials conducted in eastern and southern Africa
# 	#d7 <- as.data.frame(extract_areas("C:/Users/User/Downloads/s10705-018-9915-9.pdf", pages = 5))
# 
# 	d7 <- extract_from_pdf(fpdf, pages = 5)
# 	names(d7) <- c("Site-year", "TC", "pH", "SOC(g kg-1)", "P(mg kg-1)", "K(mg kg-1)", "Mg(mg kg-1)", "S(mg kg-1)", "Zn(mg kg-1)", "B(mg kg-1)")
# 	d7$`Site-year`[d7$`Site-year` == "RW_ENyagatKat14B"] <- "RW_ENyagat_Kat14B"
# 	d7 <- d7[, c("Site-year", "pH", "SOC(g kg-1)" , "P(mg kg-1)", "K(mg kg-1)", "Mg(mg kg-1)")]
# 	names(d7) <- carobiner::replace_values(names(d7),
# 		c("Site-year", "pH", "SOC(g kg-1)" , "P(mg kg-1)", "K(mg kg-1)", "Mg(mg kg-1)"),
# 		c("trial_id", "soil_pH", "soil_SOC", "soil_P_total", "soil_K", "soil_Mg"))
# 
# 
# 	list(d5=dput(d5), d6=dput(d6), d7=dput(d7))
# }
