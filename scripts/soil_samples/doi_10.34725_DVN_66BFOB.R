# R script for "carob"


carob_script <- function(path) {
  
"This dataset contains a subset of the samples collected during the AfSIS Phase I project and was a collaborative effort between World Agroforestry (ICRAF) and Rothamsted Research. The soil samples were retrieved from ICRAF Soil Archive: https://worldagroforestry.org/output/icraf-soil-archive-physical-archive-systematically-collected-soil-samples and subject to wet chemical analysis at Rothamsted Research in the UK under a Global Challenges Research Fund project, 'BBS/OS/GC/000014B: Chemical and Biological Assessment of AfSIS soils funded through the UK Biotechnology and Biological Sciences Research Council. This dataset includes the Site, Cluster, Plot as well as the GPS coordinates and wet chemistry data from 2002 samples collected from 18 countries and 51 LDSF sites. The original data collection was part of the AfSIS Phase I project, funded by the Bill and Melinda Gates Foundation (BMGF) and took place between 2009-2013. ICRAF and CIAT contributed the Site, Cluster, Plot and GPS coordinates for the soil samples, ICRAF organized the sub-sampling of the soil samples from the ICRAF physical archive in Nairobi and Rothamsted analysed the soil samples in the UK in 2017 and 2018. Visit our websites here: https://worldagroforestry.org/landhealth and https://www.rothamsted.ac.uk/. The AfSIS Phase I project funded by the Bill and Melinda Gates Foundation (BMGF) from 2009-2013, aimed to provide a consistent baseline of soil information across sub-Saharan Africa (SSA). Led by CIAT-TSBF, partners included: ISRIC, CIESIN, The Earth Institute at Columbia University and World Agroforestry (ICRAF). ICRAF led the systematic assessments of soil health using the Land Degradation Surveillance Framework (LDSF), which was developed at ICRAF, http://landscapeportal.org/blog/2015/03/25/the-land-degradation-surveillance-framework-ldsf/. LDSF sites were randomized using spatial stratification based on Koeppen-Geiger Climate zones across 19 countries in SSA. In total 60 LDSF sites were sampled. Soil samples were collected using the LDSF at two depths, 0-20 cm (labelled Topsoil) and 20-50 cm (labelled Subsoil). In each LDSF site, approximately 320 standard soil samples were collected. All of these were also scanned using MIR Spectroscopy and are available on Dataverse here: Vågen, Tor-Gunnar;Winowiecki, Leigh Ann;Desta, Luseged;Tondoh, Ebagnerin Jérôme;Weullow, Elvis;Shepherd, Keith;Sila, Andrew, 2020, 'Mid-Infrared Spectra (MIRS) from ICRAF Soil and Plant Spectroscopy Laboratory: Africa Soil Information Service (AfSIS) Phase I 2009-2013', https://doi.org/10.34725/DVN/QXCWP1, World Agroforestry - Research Data Repository, V1."
	uri <- "doi:10.34725/DVN/66BFOB"
	group <- "soil_samples"
	ff  <- carobiner::get_data(uri, path, group)
  
	dset <- data.frame(
		carobiner::read_metadata(uri, path, group, major=1, minor=2),
		data_institute = "CIAT",
		#publication= "https://doi.org/10.5194/soil-2020-69",
		project="AfSIS",
		data_type= "survey",
		treatment_vars = "none",
		carob_contributor= "Andrew Sila",
		carob_date="2024-06-10"
	)
  
	f1 <- ff[basename(ff) == "African archived samples September 2018.csv"]
	r <- read.csv(f1)
	f2 <- ff[basename(ff) == "Variables_Description.xlsx"]
	r2 <- carobiner::read.excel(f2, TRUE)
	r2[1:15,1:4]

	r$soil_sample_top[r$Depth=="Topsoil"] <- 0
	r$soil_sample_top[r$Depth=="Subsoil"] <- 20
	r$soil_sample_sub[r$Depth=="Topsoil"] <- 20
	r$soil_sample_sub[r$Depth=="Subsoil"] <- 50
	
	# correct country names
	r$Country[r$Country == "SAfrica"] <- "South Africa"
	r$Country[r$Country == "Zimbambwe"] <- "Zimbabwe"
	r$Site[is.na(as.numeric(r$Latitude)) == TRUE]
	bt <- r[r$Country=="Botswana",]
	# Get geocodes for the two sites Prieska and Analavory
	#p1 <- carobiner::geocode (country="South Africa", adm1="Prieska", location="Prieska")
	#p2 <- carobiner::geocode (country="Madagascar", adm1="Analavory", location="Analavory")
	r$Latitude[is.na(as.numeric(r$Latitude)) == TRUE] <-  c(-29.6776,rep(-18.9736,125))
	r$Longitude[is.na(as.numeric(r$Longitude)) == TRUE] <-  c(22.7538,rep(46.9736,125))
	
	# Geocode for Shoshong with points falling on water
	#p3 <- carobiner::geocode (country="Botswana", adm1="Shoshong", location="Shoshong")
	r$Latitude[(r$Site) == "Shoshong"] <- -23.07762
	r$Longitude[(r$Site) == "Shoshong"] <- 26.5137
	# Replace -ve reported values with a small value equivalent to lowest detection limit
	r$eCEC[r$eCEC<=0] <- 0.0001
	r$Caex[r$Caex<=0] <- 0.0001
	r$Kex[r$Kex<=0] <-0.0001
	r$Mgex[r$Mgex<=0] <- 0.0001
	r$Naex[r$Naex<=0] <- 0.0001
	r$P[r$P<=0] <- 0.0001
	r$Percent_Org_C[r$Percent_Org_C<=0] <- 0.001
	
	


	d <- data.frame(
		country = r$Country,
		location = r$Site,
		longitude = r$Longitude,
		latitude = r$Latitude,
		trial_id = r$SSN,
		soil_sample_top = r$soil_sample_top,
	  soil_sample_bottom = r$soil_sample_sub,
		soil_pH = as.numeric(r$pH),
		soil_ECEC = as.numeric(r$eCEC),
		soil_ex_Ca = as.numeric(r$Caex),
		soil_ex_K = as.numeric(r$Kex),
		soil_ex_Mg = as.numeric(r$Mgex),
		soil_ex_Na = as.numeric(r$Naex),
		soil_P_total = as.numeric(r$P),
		soil_P_available = as.numeric(r$Olsen.P),
		soil_C = r$Percent_C,
		soil_SOC = r$Percent_Org_C,
		soil_N = r$Percent_N
	)

	# To be added
	#soil_extr_Al <- r$Am.Ox.Al
	#soil_extr_Fe <- r$Am.Ox.Fe
	#soil_extr_Mn <- r$AmOx.Mn
	#soil_extr_P <- r$Am.Ox.P
	#soil_PBI <- r$pbi
	#soil_Al_total = as.numeric(r$Al)
	#soil_Ca_total = as.numeric(r$Ca)
	#soil_Co_total = as.numeric(r$Co)
	#soil_Cu_total = as.numeric(r$Cu)
	#soil_Fe_total = as.numeric(r$Fe)
	#soil_K_total = as.numeric(r$K)
	#soil_Mg_total = as.numeric(r$Mg)
	#soil_Mn_total = as.numeric(r$Mn)
	#soil_Na_total = as.numeric(r$Na)
	#soil_Ni_total = as.numeric(r$Ni)
	#soil_Pb_total = as.numeric(r$Pb)
	#soil_S_total = as.numeric(r$S)
	#soil_Zn_total = as.numeric(r$Zn)
	#soil_As = as.numeric(r$As.75)
	#soil_Se = as.numeric(r$Se.78)
	#soil_Mo = as.numeric(r$Mo.95)
	#soil_Cd = as.numeric(r$Se.Cd114)
	carobiner::write_files(path, dset, d)
}


