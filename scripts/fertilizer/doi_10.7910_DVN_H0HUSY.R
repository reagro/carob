

carob_script <- function(path){
  
"Title: Impact of NPK fertilization on upland rice yield, Nicaragua
  
Description: This dataset contains information of experiments carried out upland rice in two regions of Nicaragua (Caribbean and Pacific Region), as well as a compilation of soils data from different regions in Nicaragua collected during 2019 in seed banks of rice and beans. The experiments were designed to explore the effects of N, P and K in the yield of upland rice. The experiments were carried out on farmer’s field during the 2019 production cycle, the dataset contains yield and aerial biomass of the experiments.
" 
	uri <- "doi:10.7910/DVN/H0HUSY"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "fertilizer"
  
  ## dataset level data 
	dset <- data.frame(
		dataset_id = dataset_id,
		group=group,
		uri=uri,
		publication= NA,
		data_citation = "Siles, Pablo; Tellez, Orlando; Peng, Yuan-Ching; Zeledón, Yasser, 2020, Impact of NPK fertilization on upland rice yield, Nicaragua, doi:10.7910/DVN/H0HUSY",
		data_institutions = "CIAT",
		carob_contributor="Jean-Martial Johnson",
		carob_date="2022-12-09",
		data_type="experiment",
		project=NA
    )
  
	ff <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=2)
	dset$license <- carobiner::get_license(js) 
	dset$title <- carobiner::get_title(js)
	dset$authors <- carobiner::get_authors(js)
	dset$description <- carobiner::get_description(js)
	

	dfun <- function(r) {
		data.frame(
			adm1 = r$Departamento,
			adm2 = r$Municipio,
			adm3 = r$Comunidad,
	#		location = r1$Localidad,
			treatment = as.character(r$ttos),
			N_fertilizer = r$N,
			P_fertilizer = r$P,
			K_fertilizer = r$K,
			yield = as.numeric(r$rto_grano_kgha),
			dmy_total= as.numeric(r$rto_biom_kgha),
			trial_id = r$Localidad,
			rep = as.integer(r$rep)
		)
	}

	# processing Rice Data - Caribbean.tab
	f1 <- ff[basename(ff) == "03. Rice Data - Caribbean.xlsx"]
	r1 <- data.frame(readxl::read_xlsx(f1))
	d1 <- dfun(r1)
	d1$adm1 <- "Región Autónoma de la Costa Caribe Sur"
	d1$longitude <- ifelse(d1$adm3 == "Montivideo", -84.609,
					ifelse(d1$adm3 == "El Panchon", -83.863,
					ifelse(d1$adm3 == "La Tortuga", -84.469, -84.312)))
	d1$latitude <- ifelse(d1$adm3 == "Montivideo", 11.808,
					ifelse(d1$adm3 == "El Panchon", 12.323,
					ifelse(d1$adm3 == "La Tortuga", 11.999, 12.170)))
	d1$trial_id <- "Caribbean"
	
	
	# processing 04. Rice Data - Pacific.tab
	f2 <- ff[basename(ff) == "04. Rice Data - Pacific.xlsx"]
	r2 <- data.frame(readxl::read_xlsx(f2))
	d2 <- dfun(r2)
	d2$longitude <- ifelse(d2$adm3 == "Rio chiquito", -86.909579,
					ifelse(d2$adm3 == "El Ensayo", -87.170,
					ifelse(d2$adm3 == "El Tololar", -86.832, -85.764)))
	d2$latitude <- ifelse(d2$adm3 == "Rio chiquito", 12.318,
					ifelse(d2$adm3 == "El Ensayo", 12.587,
					ifelse(d2$adm3 == "El Tololar", 12.486, 13.080)))
	
	
	# processing 02. Soils Data.xlsx
	f3 <- ff[basename(ff) == "02. Soils Data.xlsx"]
	r3 <- data.frame(readxl::read_xlsx(f3))
	d3 <- data.frame(
		adm1 = r3$Departamento,
		adm2 = r3$Municipio,
		adm3 = r3$Comunidad,
		soil_pH = as.numeric(r3$pH),
		soil_SOC = as.numeric(r3$MO)/1.724, # This is a coefficient to convert SOC into OM
		soil_sand = as.numeric(r3$Arena),
		soil_clay = as.numeric(r3$Arcilla),
		soil_N = as.numeric(r3$N),
		soil_P_available = as.numeric(r3$P),
		soil_K = as.numeric(r3$K) * 39 * 10
	)

	d3$adm2[grep("Kuka hill",d1$Municipio)] <- 'Kukrahill'

	d <- carobiner::bindr(d1, d2)
	d <- merge(x=d, y=d3, by = c("adm1", "adm2", "adm3"), all.x = TRUE)

	d$dataset_id <- dataset_id
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$yield_part <- "grain"
	d$country <- "Nicaragua"
	d$crop <- "rice"
	d$planting_date = "2019"
	
	carobiner::write_files(dset, d, path=path)
}

