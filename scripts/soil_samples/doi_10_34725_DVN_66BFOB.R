carob_script <- function(path) {  " Description:This dataset contains a subset of the samples collected during the AfSIS Phase I project and was a collaborative effort between World Agroforestry (ICRAF) and Rothamsted Research. The soil samples were retrieved from ICRAF Soil Archive: https://worldagroforestry.org/output/icraf-soil-archive-physical-archive-systematically-collected-soil-samples and subject to wet chemical analysis at Rothamsted Research in the UK under a Global Challenges Research Fund project, 'BBS/OS/GC/000014B: Chemical and Biological Assessment of AfSIS soils' funded through the UK Biotechnology and Biological Sciences Research Council. This dataset includes the Site, Cluster, Plot as well as the GPS coordinates and wet chemistry data from 2002 samples collected from 18 countries and 51 LDSF sites. The original data collection was part of the AfSIS Phase I project, funded by the Bill and Melinda Gates Foundation (BMGF) and took place between 2009-2013. ICRAF and CIAT contributed the Site, Cluster, Plot and GPS coordinates for the soil samples, ICRAF organized the sub-sampling of the soil samples from the ICRAF physical archive in Nairobi and Rothamsted analysed the soil samples in the UK in 2017 and 2018. Visit our websites here: https://worldagroforestry.org/landhealth and https://www.rothamsted.ac.uk/. The AfSIS Phase I project funded by the Bill and Melinda Gates Foundation (BMGF) from 2009-2013, aimed to provide a consistent baseline of soil information across sub-Saharan Africa (SSA). Led by CIAT-TSBF, partners included: ISRIC, CIESIN, The Earth Institute at Columbia University and World Agroforestry (ICRAF). ICRAF led the systematic assessments of soil health using the Land Degradation Surveillance Framework (LDSF), which was developed at ICRAF, http://landscapeportal.org/blog/2015/03/25/the-land-degradation-surveillance-framework-ldsf/. LDSF sites were randomized using spatial stratification based on Koeppen-Geiger Climate zones across 19 countries in SSA. In total 60 LDSF sites were sampled. Soil samples were collected using the LDSF at two depths, 0-20 cm (labelled Topsoil) and 20-50 cm (labelled Subsoil). In each LDSF site, approximately 320 standard soil samples were collected. All of these were also scanned using MIR Spectroscopy and are available on Dataverse here: Vågen, Tor-Gunnar;Winowiecki, Leigh Ann;Desta, Luseged;Tondoh, Ebagnerin Jérôme;Weullow, Elvis;Shepherd, Keith;Sila, Andrew, 2020,'Mid-Infrared Spectra (MIRS) from ICRAF Soil and Plant Spectroscopy Laboratory: Africa Soil Information Service (AfSIS) Phase I 2009-2013', https://doi.org/10.34725/DVN/QXCWP1, World Agroforestry - Research Data Repository, V1.	"  	uri <- "doi.org/10.34725/DVN/66BFOB"
	dataset_id <- carobiner::simple_uri(uri)	group <- "soil_samples"  	## data set level data 	dset <- data.frame(		dataset_id = dataset_id,		group=group,		project="AfSIS",		uri=uri,		#publication = "von Fromm, S. F., Hoyt, A. M., Acquah, G. E., Aynekulu, E., Berhe, A. A., Haefele, S. M., Lange, M., McGrath, S.P., Shepherd, K.D., Sila, A.M., Six, J., Towett, E. K., Trumbore, S. E., Vågen, T-G., Weullow, E., Winowiecki, L.A., Doetterl, S.. (2021). Continental-scale controls on soil organic carbon across sub-Saharan Africa. SOIL. doi: 10.5194/soil-2020-69 https://doi.org/10.5194/soil-2020-69",
	data_citation = "Vågen, Tor-Gunnar; Winowiecki, Leigh Ann; Desta, Luseged; Tondoh, Jerome; Weullow, Elvis; Shepherd, Keith; Sila, Andrew; Dunham, Sarah J.; Hernández-Allica, Javier; Carter, Joanna; McGrath, Steve P, 2021, 'Wet chemistry data for a subset of AfSIS: Phase I archived soil samples', https://doi.org/10.34725/DVN/66BFOB, World Agroforestry (ICRAF), V1, UNF:6:15bkgMxXho9IjXVQ0SI0Sw== [fileUNF]",		data_institutions = "ICRAF",		carob_contributor="Andrew Sila",
		carob_date = Sys.Date(),		data_type = "wet chemistry"    )    ## download and read data   	ff	<- carobiner::get_data(uri, path, group)	js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=2)	dset$license <- carobiner::get_license(js)		# read field, plant and plot data and tables	f0 <- ff[basename(ff) == "African archived samples September 2018.csv"]	d0 <- read.csv(f0)	# rename columns	colnames(d0) <- tolower(colnames(d0))	# A table with soil variable names and corresponding units. Reference it to populate terms table	f1 <- ff[basename(ff) == "Variables_Description.xlsx"]	d1 <- readxl::read_excel(f1,1)		# Drop res.id, cluster, plot columns from d0 table	to_drop <- c('res.id', 'cluster', 'plot')	to_drop <- which(colnames(d0) %in% to_drop)	d0 <- d0[,-c(to_drop)]		hd <- c('record_id','country','site','latitude','longitude','Depth', 'soil_pH', 'soil_N', 'soil_C_total','soil_inorgC_total', 'soil_SOC', 'soil_AmOx_Al', 'soil_AmOx_Fe','soil_AmOx_Mn','soil_AmOx_P','soil_olsen_P','soil_pbi','soil_Ex_Ca','soil_Ex_K', 'soil_Ex_Mg', 'soil_Ex_Na','soil_EC','soil_Al', 'soil_Ca','soil_Co','soil_Cr','soil_Cu','soil_Fe', 'soil_K','soil_Mg','soil_Mn','soil_Na','soil_Ni', 'soil_P_total', 'soil_Pb', 'soil_S', 'soil_Zn', 'soil_As_75', 'soil_Se_78', 'soil_Mo_95', 'soil_Cd_114')	colnames(d0) <- hd
	
	# Rename SAfrica and Zimbambwe country names as South Africa and Zimbabwe
	d0$country <- carobiner::replace_values(d0$country, c('SAfrica', 'Zimbambwe'),c('South Africa', 'Zimbabwe'))
	d0$dataset_id <- dataset_id
	d0$trial_id <- 'AfSIS_RRES'
	
	# Remove negative and zero values function
	zng <- function(x){
		as.numeric(ifelse(x<=0,NA, x))
	}
	
	d0$soil_N <- zng(d0$soil_N) *10000 #to convert mg/kg from %
	d0$soil_SOC <- zng(d0$soil_SOC)
	d0$soil_EC <- zng(d0$soil_EC)
	d0$soil_P_total <- zng(d0$soil_P_total)
	d0$soil_Pb <- zng(d0$soil_Pb)
	d0$soil_Cu <- zng(d0$soil_Cu)
	d0$soil_AmOx_P <- zng(d0$soil_AmOx_P)
	d0$soil_Ex_Ca <- zng(d0$soil_Ex_Ca)
	d0$soil_Ex_K <- zng(d0$soil_Ex_K)
	d0$soil_Ex_Mg <- zng(d0$soil_Ex_Mg)
	d0$soil_Ex_Na <- zng(d0$soil_Ex_Na)
	d0$soil_As_75 <- zng(d0$soil_As_75)
	d0$soil_Mo_95 <- zng(d0$soil_Mo_95) 
	d0$soil_Cd_114 <- zng(d0$soil_Cd_114)
	
	# Geocode for sites with lat/lon values missing
	z <- which(is.na(d0$latitude) == TRUE)
	h <- carobiner::geocode(country = d0$country[z], location=d0$site[z])
	d0$longitude[z] <- h$df$lon
	d0$latitude[z] <- h$df$lat
	
	# Rewrite depth as top_bottom
	d0$soil_sample_top <- ifelse(d0$Depth == "Topsoil", 0, 20)
	d0$soil_sample_bottom <- ifelse(d0$Depth == "Topsoil", 20, 50)
	
	# remove prefix from d0$record
	d0$record_id <- as.integer(substr(d0$record_id, 4,20))
	
	#drop Depth column
	depth <- which(colnames(d0)=='Depth')
	d0 <- d0[,-depth]

	carobiner::write_files(dset, d0, path=path)}