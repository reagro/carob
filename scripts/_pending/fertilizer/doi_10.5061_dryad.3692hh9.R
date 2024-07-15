# R script for "carob" 	

carob_script <- function(path) {
   
"Wheat (Triticum æstivum L.) is an important East Africa highland crop but yields are low. Information is scarce for optimization of fertilizer use. Research was conducted to determine yield response functions for N, P and K, and to diagnose Mg–S–Zn–B deficiencies. The average grain yield increase in Rwanda due to N application was 1.5 Mg ha−1 with a mean economically optimal rate (EOR) of 68 kg ha−1 N. In Kenya and Tanzania, yield was increased by 29% with EOR N for two SY but unaffected by N rate for four other SY which on average had 50% of the soil organic C (SOC) as the N-responsive SY. Yield was increased, on average, with application of P and K by 0.47 and 0.23 Mg ha−1, respectively, at EOR in Rwanda but effects were inconsistent for other SY where soil test K was higher than in Rwanda. Application of Mg–S–Zn–B resulted in 0.46 Mg ha−1 more yield in Rwanda but did not affect yield at other SY where the average soil test values for these nutrients was 35% higher than in Rwanda. If the financially constrained farmer opts to apply the affordable fertilizer to twice as much land at 50% EOR compared with 100% EOR, the mean yield increase is reduced by 27% but production and PCR are increased by 43 and 72%, respectively. Nutrient effects were relatively consistent and positive in Rwanda, but less and less inconsistent elsewhere with generally less SOC, more K–Mg–S–Zn–B availability, and often lower yields."
 
 
	uri <- "doi:10.5061/dryad.3692hh9"
	group <- "fertilizer" 
   
	ff  <- carobiner::get_data(uri, path, group)
   
	meta <- data.frame(
		carobiner::read_metadata(uri, path, group, major=1, minor=4), 
		data_institute = "UNL", 
		publication = "abc", 
		project = NA, 
		data_type = "experiment", 
		treatment_vars = "N_fertilizer;P_fertilizer;P_fertilizer;K_fertilizer;S_fertilizer;Mg_fertilizer;Zn_fertilizer;B_fertilizer", 
		carob_contributor = "Robert Hijmans", 
		carob_date = "2024-06-29"
	)
   
	f <- ff[basename(ff)=="EA wheat database.xlsx"]
   
	r1 <- carobiner::read.excel(f, sheet = "Rwanda")
	r2 <- carobiner::read.excel(f, sheet = "Kenya", fix_name=TRUE)
	r3 <- carobiner::read.excel(f, sheet = "Tanzania")

	r0 <- carobiner::read.excel(f, sheet = "Readme", skip=29, fix=TRUE)
	i <- which(is.na(r0[,1]))
	geo <- r0[1:(i[1]-1) , ]
	soil <- r0[(i[3]-1):(nrow(r0)-1), ]
	names(soil) <- soil[1,]
	soil <- soil[-(1:2), ]
	names(soil) <- 	c("Site_year", "soil_texture", "pH", "SOC", "soil_P", "soil_K", "soil_Ca", "soil_Mg", "soil_S", "soil_Zn", "soil_B")
	i <- soil$Site_year == "RW_Rusarabuye14b,15b"
	soil$Site_year[i] = "RW_Rusarabuye14b"
	soil <- rbind(soil, soil[i, ])
	soil$Site_year[nrow(soil)] = "RW_Rusarabuye15b"
	i <- soil$Site_year == "RW_Tare14a,15a,b"
	soil$Site_year[i] = "RW_Tare14a"
	soil <- rbind(soil, soil[i, ])
	soil$Site_year[nrow(soil)] = "RW_Tare15a"
	soil <- rbind(soil, soil[i, ])
	soil$Site_year[nrow(soil)] = "RW_Tare15b"
	geosoil <- merge(geo, soil, by=1)
	
	
	d1 <- data.frame(
		country= "Rwanda",
		trial_id= r1$SiteYr,
		planting_date = gsub("b", "",  r1$y),
		treatment= r1$Treatment,
		N_fertilizer = r1$N,
		P_fertilizer = r1$P,
		K_fertilizer = r1$K,
		S_fertilizer = r1$S,
		Mg_fertilizer = r1$Mg,
		Zn_fertilizer = r1$Zn,
		B_fertilizer = r1$B,
		yield = r1$GY * 1000,
		rep = r1$r
	)
	d1$planting_date[grep("15", d1$trial_id)] <- "2015"
	i <- which(d1$K_fertilizer == "d")
	
	d2 <- data.frame(
		country= "Kenya",
		trial_id= r2$Tt,
		treatment= r2$Trtcode ,
		N_fertilizer = r2$N,
		P_fertilizer = r2$P,
		K_fertilizer = r2$K,
		yield = r2$GY * 1000,
		rep = r2$R
	)
	d2$planting_date <- "2014"
	d2$planting_date[grep("15", d2$trial_id)] <- "2015"

	d3 <- data.frame(
		country= "Tanzania",
		trial_id= r3$SY,
		location = r3$S,
		planting_date = r3$Y,
		N_fertilizer = r3$N,
		P_fertilizer = r3$P,
		K_fertilizer = r3$K,
		yield = r3$`GraYldHa (kg)`,
		residue_yield = r3$`StoYldHa (kg)`,
		rep = r3$Rep
	)
	
	d <- carobiner::bindr(d1, d2, d3)

	d$rep <- as.integer(d$rep)
	d$on_farm= NA
    d$irrigated= NA
    d$is_survey= FALSE
	d$crop <- "wheat"
	
	carobiner::write_files(path, meta, d)
}


