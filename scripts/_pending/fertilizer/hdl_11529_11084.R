# R script for "carob"

## ISSUES
#1. Unspecified districts and subdisticts


carob_script <- function(path) {

"Description:

Working with 64 farmers in eight production environments, we examined yield response to three genotypes, BG25 and BG27 (with salinity - and heat - tolerant traits) and BG21 (local check), across a gradient of sowing dates, grouped as ‘early’ (sown before 15 December) and ‘late’ (after 15 December), under 0, 100 and 133 and 0, 67 and 100 kg N ha-1 for early- and late-sowing groups, respectively. 

Across environments and genotypes, yield ranged from 2.11 to 4.77 t ha-1(mean: 3.9 t ha-1) under early-sowing, and from 0.83 to 4.27 t ha-1(mean: 2.74 t ha-1) under late-sowing. Wheat performance varied with environment (1.68 - 4.77 t ha-1 at 100 kg N ha-1across sowing groups); the lowest yields found where early sowing was delayed and soil salinity levels were elevated. Small but significant (P less than 0.001) yield differences (0.22 t ha-1) were found between 100 and 133 kg N ha-1 for the early-sowing group, though no difference was found between 67and 100 kg N ha-1 for late-sowing. Combining early- and late-sowing groups, significant environment x N rate and sowing-group x N rate interactions (both P less than 0.001) for 100 kg N ha-1 indicated the importance of site-and time-specific N management in these stress-prone environments.

Considering all cultivars and environments, ECa at sowing, flowering and grain filling negatively correlated with yield (r = - 0.50, - 0.59 and - 0.54, all P less than 0.001). Correlations with ground water depth at flowering and grain filling were negative and significant, but less pronounced in the context of farmer-managed irrigation scheduling. Despite putative stress-tolerance traits in two of the three entries, no genotypic yield differences were found under early-sowing, though s mall differences (less than 0.19 t ha-1) were observed with late sowing. Agronomic fertilizer-N efficiency (AE-N) was consistently higher for 100 than 133 and 67 than 100 kg N ha-1 for early- and late-sowing. The marginal economic value of N application followed similar trends, indicating that rates of at most 100 and 67 kg N ha-1 are favorable for sowing before or after December 15th.]

"

	uri <- "hdl:11529/11084"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "fertilizer"
	
	## dataset level data 
	dset <- data.frame(
		dataset_id = dataset_id,
		group=group,
		project=NA,
		uri=uri,
		data_citation="Timothy J. Krupnik; Zia Uddin Ahmed; Jagadish Timsina; Md. Shahjahan; A.S.M. Alanuzzaman Kurishi; Azahar A. Miah; B.M. Saidur Rahman; Mahesh K. Gathala; Andrew J. McDonald, 2017, 'Forgoing the fallow in Bangladesh’s stress-prone coastal deltaic environments: Effect of sowing date, nitrogen, and genotype on wheat yield in farmers’ fields.', https://hdl.handle.net/11529/11084, CIMMYT Research Data & Software Repository Network, V1",
		publication= "doi.org/10.1016/j.fcr.2014.09.019",
		data_institutions = "CIMMYT;IRRI;IFPRI",
		data_type="experiment", 
		carob_contributor="Mitchelle Njukuya",
		carob_date="2023-12-18"
	)

	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=2)
	dset$license <- "CIMMYT"
  dset$title <- carobiner::get_title(js)

	
	f <- ff[basename(ff) == "WheatGenoTrial.csv"]
	r <- read.csv(f, fileEncoding = "latin1")
	
	d <- r
	d <- carobiner::change_names(d,c("SEASON","Nitrogen_.kg.ha.","GENOTYPE","Harvest_Biomass_dry_weight_.g.")
	                             ,c("season","N_fertilizer","genotype","dym_biomass"))
	
	d$season<-d$SEASON
	d$N_fertilizer<-d$Nitrogen_.kg.ha.
	d$genotype<-d$GENOTYPE
	d$dmy_biomass<-d$Harvest_Biomass_dry_weight_.g.
	 
	d$dataset_id <- dataset_id
	d$on_farm <- TRUE
	d$is_experiment <- TRUE 
	d$irrigated <- TRUE
	d$country <- "Bangladesh"
	d$crop <- "wheat"
	
	#Names of 2 Districts were given as Satkhira and Khulna in publication,
	#there was however no information on which name is identified as DISTRICT 1 OR 2 in dataset
	#Names of 4 sub-districts were also given publication as Fultala(Khulna),Dumuria(Khulna),Sadar(Satkhira) and Kaliganj(Satkhira) 
	#no information on which of the 4 is sub-district 1 or 2 and so on  
	# d$longitude[d$site=="Fultala"] <- 92.1344
	# d$latitude[d$site=="Fultala"] <- 24.4292
	# d$longitude[d$site=="Dumuria"] <- 89.425
	# d$latitude[d$site=="Dumuria"] <- 22.8083
	# d$longitude[d$site=="Sadar"] <- 89.03
	# d$latitude[d$site=="Sadar"] <- 22.73
	# d$longitude[d$site=="Kaliganj"] <- 89.0900
	# d$latitude[d$site=="Kaliganj"] <- 22.48

  d <- d[d$DATE_OF_SOWING !=".",]
	d$planting_date <- as.character(as.Date(d$DATE_OF_SOWING,format="%m/%d/%y"))
	d <- d[d$Harvest_Date_.Month.Day.Year !=".",]
	d$harvest_date <- as.character(as.Date(d$Harvest_Date_.Month.Day.Year,format="%d/%m/%y"))
	d$fertilizer_type <- "urea"
  
	#fixing data types to extract soil_EC
  d$SOIL_EC_.DS.M._._SAMPLE_1 <- as.numeric(d$SOIL_EC_.DS.M._._SAMPLE_1)

  d$`SOIL_EC_.DS.M._._SAMPLE_2`[d$SOIL_EC_.DS.M._._SAMPLE_2 == "."] <- NA
  d$SOIL_EC_.DS.M._._SAMPLE_2[d$SOIL_EC_.DS.M._._SAMPLE_2 == ".2.13"] <- "2.13"
  d$SOIL_EC_.DS.M._._SAMPLE_2 <- as.numeric(d$SOIL_EC_.DS.M._._SAMPLE_2)
 
  d$SOIL_EC_.DS.M._._SAMPLE_3 <- as.numeric(d$SOIL_EC_.DS.M._._SAMPLE_3)
  
  d$SOIL_EC_.DS.M._._SAMPLE_4[d$SOIL_EC_.DS.M._._SAMPLE_4 == "."] <- NA
  d$SOIL_EC_.DS.M._._SAMPLE_4[d$SOIL_EC_.DS.M._._SAMPLE_4 == ".1.27"] <- "1.27"
  d$SOIL_EC_.DS.M._._SAMPLE_4 <- as.numeric(d$SOIL_EC_.DS.M._._SAMPLE_4)
  
  d$SOIL_EC_.DS.M._._SAMPLE_5[d$SOIL_EC_.DS.M._._SAMPLE_5 == "."] <- NA
  d$SOIL_EC_.DS.M._._SAMPLE_5[d$SOIL_EC_.DS.M._._SAMPLE_5 == "4..23"]<- "4.23"
  
  d$SOIL_EC_.DS.M._._SAMPLE_5[d$SOIL_EC_.DS.M._._SAMPLE_5 == "6,8"]<- "6.8"
  
  
  d$SOIL_EC_.DS.M._._SAMPLE_5 <- as.numeric(d$SOIL_EC_.DS.M._._SAMPLE_5)
  
  d$SOIL_EC_.DS.M._._SAMPLE_6[d$SOIL_EC_.DS.M._._SAMPLE_6 == "."] <- NA
  d$SOIL_EC_.DS.M._._SAMPLE_6 <- as.numeric(d$SOIL_EC_.DS.M._._SAMPLE_6)
  
  d$SOIL_EC_.DS.M._._SAMPLE_7[d$SOIL_EC_.DS.M._._SAMPLE_7 == "."] <- NA
  d$SOIL_EC_.DS.M._._SAMPLE_7[d$SOIL_EC_.DS.M._._SAMPLE_7 == "5.5."]<- "5.5"
  d$SOIL_EC_.DS.M._._SAMPLE_7 <- as.numeric(d$SOIL_EC_.DS.M._._SAMPLE_7)
  
  d$SOIL_EC_.DS.M._._SAMPLE_8[d$SOIL_EC_.DS.M._._SAMPLE_8 == "."] <- NA
  
  
  d$SOIL_EC_.DS.M._._SAMPLE_8 <- as.numeric(d$SOIL_EC_.DS.M._._SAMPLE_8)
  
  # means for soil_EC samples
  soil_ec <- d[,c("SOIL_EC_.DS.M._._SAMPLE_1","SOIL_EC_.DS.M._._SAMPLE_2","SOIL_EC_.DS.M._._SAMPLE_3","SOIL_EC_.DS.M._._SAMPLE_4","SOIL_EC_.DS.M._._SAMPLE_5",
         "SOIL_EC_.DS.M._._SAMPLE_6","SOIL_EC_.DS.M._._SAMPLE_7","SOIL_EC_.DS.M._._SAMPLE_8")]
  d[names(soil_ec)] <- lapply(d[names(soil_ec)], as.numeric)
  
   sample_means <- rowMeans(d[names(soil_ec)],na.rm = TRUE)
  d$soil_EC <- sample_means

  ### Yield #####
  d$grain_weight<-as.numeric(d$X100_grain_weight_after_oven_drying_.g.)*10
  d$dym_residue<-as.numeric(d$Straw_yield_moisture_adjusted_.T.ha.)*1000
	d$yield <- as.numeric(d$Grain_yield_moisture_adjusted_.T.ha.)*1000
	d$yield_part <- "grain"
	
	d<-d[,c("season","N_fertilizer","genotype","dmy_biomass","dataset_id","on_farm","is_experiment","irrigated","country","crop","planting_date","harvest_date","fertilizer_type","soil_EC","grain_weight","dym_residue","yield_part","yield")]
	
# all scripts must end like this
	carobiner::write_files(dset, d, path=path)
}

