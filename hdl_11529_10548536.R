# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"Description:

    [CIMMYT annually distributes improved germplasm developed by its researchers and partners in international nurseries trials and experiments. The High Temperature Wheat Yield Trial (HTWYT) is a replicated yield trial that contains spring bread wheat (Triticum aestivum) germplasm adapted to Mega-environment 1 (ME1) which represents high temperature areas. (2019)]

"

	uri <- "hdl:11529/10548536"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "wheat_trials"
	## dataset level data 
	dset <- data.frame(
		dataset_id = dataset_id,
		group=group,
		project="EiA",
		uri=uri,
		data_citation="Global Wheat Program; IWIN Collaborators; Singh, Ravi; Payne, Thomas, 2020, '18th High Temperature Wheat Yield Trial', https://hdl.handle.net/11529/10548536, CIMMYT Research Data & Software Repository Network, V3",
		## if there is a paper, include the paper's doi here
		## also add a RIS file in references folder (with matching doi)
		publication= "",
		data_institutions = "CIMMYT",
		# e.g. "on-farm experiment", "survey", "compilation"
   		data_type="experiment", 
		carob_contributor="Mitchelle Njukuya",
		# date of first submission to carob
		carob_date="2023-02-11",
		# name(s) of others who made significant improvements
		revised_by=""
	)

## download and read data 
  path <- "C:/Users/user/Documents/DataAnalysis/carob_IndiaDataset"
	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=3, minor=0)
	dset$license <- carobiner::get_license(js)


	f <- ff[basename(ff) == "18TH HTWYT_Loc_data.xlsx"]
  library("readxl")
	r <- read_excel(f)
	r <- readxl::read_excel(f) |> as.data.frame()

	
## process file(s)
	d<-r
#### about the data #####
## (TRUE/FALSE)

	d$dataset_id <- dataset_id
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$is_experiment <- TRUE
	d$irrigated <- FALSE
## the treatment code	
	d$treatment <- NA

##### Location #####
## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
## you can use carobiner::fix_name()
	d$country <- d$Country
	d$location <-d$`Loc. Description`
	d$adm1 <- NA
	d$adm2 <- NA
	d$adm3 <- NA
	d$elevation <- NA
## each site must have corresponding longitude and latitude
## see carobiner::geocode
	d$latitude<-d$Latitud
	d$longitude<-d$Longitude
	d$latitude[d$location=="MELKASSA"] <- 8.3833
	d$longitude [d$location=="MELKASSA"] <-39.3333
	d$latitude[d$location=="IRA-BAMBUI/UPPER FARM"] <- 6.0000
	d$longitude [d$location=="IRA-BAMBUI/UPPER FARM"] <-10.0000
	d$latitude[d$location=="SHANDAWEEL"] <- 26.6000
	d$longitude [d$location=="SHANDAWEEL"] <-31.6667
	d$latitude[d$location=="SIDS"] <- 29.0667
	d$longitude [d$location=="SIDS"] <-31.1000
	d$latitude[d$location=="KOMOMBO"] <- 23.1333
	d$longitude [d$location=="KOMOMBO"] <-32.7833
	d$latitude[d$location=="NEW VALLEY"] <- 25.4500
	d$longitude [d$location=="NEW VALLEY"] <-30.5333
	d$latitude[d$location=="ASSIUT"] <- 27.2833
	d$longitude [d$location=="ASSIUT"] <-31.5333
	d$latitude[d$location=="MATTANA RES. STN."] <- 25.6667
	d$longitude [d$location=="MATTANA RES. STN."] <-32.7000
	d$latitude[d$location=="DEHDADI FARM"] <- 36.6333
	d$longitude [d$location=="DEHDADI FARM"] <-66.9333
	d$latitude[d$location=="BOLLAN RESEARCH STATION"] <- 31.5667
	d$longitude [d$location=="BOLLAN RESEARCH STATION"] <-64.3667                                               	 
	d$latitude[d$location=="AKF FARM STATION"] <- 34.9500
	d$longitude [d$location=="AKF FARM STATION"] <-68.5167
	d$latitude[d$location=="DEZFOUL - SAFIABAD A. R.C. - SPII"] <- 32.2667
	d$longitude [d$location=="DEZFOUL - SAFIABAD A. R.C. - SPII"] <-48.4167
	d$latitude[d$location=="ZABOL - ZAHAK STN. - SPII"] <- 30.5167
	d$longitude [d$location=="ZABOL - ZAHAK STN. - SPII"] <-60.6833
	d$latitude[d$location=="GACHSARAN -AGRIC. RES. STN - DARI"] <- 30.3333
	d$longitude [d$location=="GACHSARAN -AGRIC. RES. STN - DARI"] <-50.0000
	d$latitude[d$location=="PARSABAD - MOGHAN ARS -SPII"] <- 39.0333
	d$longitude [d$location=="PARSABAD - MOGHAN ARS -SPII"] <-47.0833
	d$latitude[d$location=="GONBAD AGRIC. RES. STN. - DARI"] <- 37.2667
	d$longitude [d$location=="GONBAD AGRIC. RES. STN. - DARI"] <-55.2000
	d$latitude[d$location=="ADAPAZARI - SAKARYA MAIZE RES. INST."] <- 40.7833
	d$longitude [d$location=="ADAPAZARI - SAKARYA MAIZE RES. INST."] <-30.4167
	d$latitude[d$location=="IZMIR - AEGEAN REGIONAL A.R.I."] <- 38.5833
	d$longitude [d$location=="IZMIR - AEGEAN REGIONAL A.R.I."] <-27.0833
	d$latitude[d$location=="KAHRAMANMARAS - ARS"] <- 37.5833
	d$longitude [d$location=="KAHRAMANMARAS - ARS"] <-36.9167
	d$latitude[d$location=="BALCALI - SARICAM - ATLAS"] <- 37.0500
	d$longitude [d$location=="BALCALI - SARICAM - ATLAS"] <-35.3500
	d$latitude[d$location=="ANKARA UNIVERSTY HAYMANA"] <- 40.0000
	d$longitude [d$location=="ANKARA UNIVERSTY HAYMANA"] <-33.0000
	d$latitude[d$location=="GAP INTL AGRRES & TRAINING CTR"] <- 37.8833
	d$longitude [d$location=="GAP INTL AGRRES & TRAINING CTR"] <-40.2667
	d$latitude[d$location=="GAP INTL AGRRES & TRAINING CTR"] <- 37.8833
	d$longitude [d$location=="GAP INTL AGRRES & TRAINING CTR"] <-40.2667
	d$latitude[d$location=="JUNAGADH - WRS - JUNAGADH AGRIC UNIV"] <- 21.5000
	d$longitude [d$location=="JUNAGADH - WRS - JUNAGADH AGRIC UNIV"] <-70.4833
	d$latitude[d$location=="JUNAGADH - WRS - JUNAGADH AGRIC UNIV"] <- 21.5000
	d$longitude [d$location=="JUNAGADH - WRS - JUNAGADH AGRIC UNIV"] <-70.4833
	d$latitude[d$location=="HOSHANGABAD - JHKVV, WRS - POWARKHEDA"] <- 22.7333
	d$longitude [d$location=="HOSHANGABAD - JHKVV, WRS - POWARKHEDA"] <-77.7000
	d$latitude[d$location=="MPKV ARS - NIPHAD"] <- 20.1000
	d$longitude [d$location=="MPKV ARS - NIPHAD"] <-74.1000
	d$latitude[d$location=="LUDHIANA -PUNJAB AGRICULTURAL UNIVERSITY"] <- 30.9333
	d$longitude [d$location=="LUDHIANA -PUNJAB AGRICULTURAL UNIVERSITY"] <-75.8667
	d$latitude[d$location=="RAJASTHAN ARI - JAIPUR"] <- 26.9667
	d$longitude [d$location=="RAJASTHAN ARI - JAIPUR"] <-75.8000
	d$latitude[d$location=="INDORE - RS FARM - IARI"] <- 22.6167
	d$longitude [d$location=="INDORE - RS FARM - IARI"] <-75.8333
	d$latitude[d$location=="VARANASI - AGRIC FARM - BANARAS H. U."] <- 25.3000
	d$longitude [d$location=="VARANASI - AGRIC FARM - BANARAS H. U."] <-83.0500
	d$latitude[d$location=="PUSA-IARI"] <- 25.8667
	d$longitude [d$location=="PUSA-IARI"] <-85.8000
	d$latitude[d$location=="KARNAL - IIWBR"] <- 29.6667
	d$longitude [d$location=="KARNAL - IIWBR"] <-77.0333
	d$latitude[d$location=="HISSAR - DEPT. PLANT BREEDING - CCS HAU"] <- 29.1667
	d$longitude [d$location=="HISSAR - DEPT. PLANT BREEDING - CCS HAU"] <-75.7667
	d$latitude[d$location=="BILASPUR - EXPT STN - TCBCARS"] <- 22.1500
	d$longitude [d$location=="BILASPUR - EXPT STN - TCBCARS"] <-82.2000
	d$latitude[d$location=="PUNE - ARI FARM HOL- AGHARKAR R.I"] <- 18.0667
	d$longitude [d$location=="PUNE - ARI FARM HOL- AGHARKAR R.I"] <-74.3500
	d$latitude[d$location=="JABALPUR - LIVESTOCK FARM -JNKVV"] <- 23.1500
	d$longitude [d$location=="JABALPUR - LIVESTOCK FARM -JNKVV"] <-79.9667
	d$latitude[d$location=="SHRIRAM BIOSEEDS"] <- 30.9000
	d$longitude [d$location=="SHRIRAM BIOSEEDS"] <-75.8000
	d$latitude[d$location=="WELLINGTON -  REGIONAL STATION - IARI"] <- 11.3667
	d$longitude [d$location=="WELLINGTON -  REGIONAL STATION - IARI"] <-76.7833
	d$latitude[d$location=="GOKULWADI JALNA"] <- 19.8500
	d$longitude [d$location=="GOKULWADI JALNA"] <-75.8833
	d$latitude[d$location=="UPANTNAGAR -  P. AGRICULTURAL UNIVERSITY"] <- 29.0500
	d$longitude [d$location=="UPANTNAGAR -  P. AGRICULTURAL UNIVERSITY"] <-79.5167
	d$latitude[d$location=="UAS - MAS DHARWARD"] <- 15.4833
	d$longitude [d$location=="UAS - MAS DHARWARD"] <-74.9833
	d$latitude[d$location=="AURANGABAD - GANGAPUR - AJEET SEEDS LTD"] <- 19.8833
	d$longitude [d$location=="AURANGABAD - GANGAPUR - AJEET SEEDS LTD"] <-72.3833
	d$latitude[d$location=="LUDHIANA - LADHOWAL - BISA"] <- 30.9500
	d$longitude [d$location=="LUDHIANA - LADHOWAL - BISA"] <-75.8833
	d$latitude[d$location=="JABALPUR - LAKHANWADA - BISA"] <- 23.1667
	d$longitude [d$location=="JABALPUR - LAKHANWADA - BISA"] <-79.9833
	d$latitude[d$location=="SCL - MADHAVGARH"] <- 23.5667
	d$longitude [d$location=="SCL - MADHAVGARH"] <-72.9667
	d$latitude[d$location=="MES KUMARGANJ"] <- 26.7833 
	d$longitude [d$location=="MES KUMARGANJ"] <-82.1833 
	d$latitude[d$location=="PANKON  A.R.I. FARM"] <- 23.0333
	d$longitude [d$location=="PANKON  A.R.I. FARM"] <-95.4667
	d$latitude[d$location=="THEGON  RESEARCHS  FARM"] <- 18.6333
	d$longitude [d$location=="THEGON  RESEARCHS  FARM"] <-95.4167
	d$latitude[d$location=="LETPADAN  RESEARCHS  FARM"] <- 17.7833
	d$longitude [d$location=="LETPADAN  RESEARCHS  FARM"] <-95.7833
	d$latitude[d$location=="BHAIRAHWA - NWRP- NARC"] <- 27.5000
	d$longitude [d$location=="BHAIRAHWA - NWRP- NARC"] <-83.4500
	d$latitude[d$location=="QUETTA ARI SARIAB"] <- 30.2000
	d$longitude [d$location=="QUETTA ARI SARIAB"] <-67.0167
	d$latitude[d$location=="NIAB FAISALABAD"] <- 31.4167
	d$longitude [d$location=="NIAB FAISALABAD"] <-73.0833
	d$latitude[d$location=="QUAID-E-AWAM A.R.I."] <- 27.5333
	d$longitude [d$location=="QUAID-E-AWAM A.R.I."] <-68.2000
	d$latitude[d$location=="PFRS MARDAN"] <- 34.1833
	d$longitude [d$location=="PFRS MARDAN"] <-72.0333
	d$latitude[d$location=="NIFA EXP FARM"] <- 34.0167
	d$longitude [d$location=="NIFA EXP FARM"] <-71.7000
	d$latitude[d$location=="CIANO - HEAT"] <- 27.4000
	d$longitude [d$location=="CIANO - HEAT"] <-109.9333
	d$latitude[d$location=="CIANO-BED-5IR"] <- 27.4167
	d$longitude [d$location=="CIANO-BED-5IR"] <-109.9500
	d$latitude[d$location=="CIANO-FLAT-5IR"] <- 27.3667
	d$longitude [d$location=="CIANO-FLAT-5IR"] <-109.9167
	d$latitude[d$location=="CIANO-BED LATE SOWN"] <- 27.3500
	d$longitude [d$location=="CIANO-BED LATE SOWN"] <-109.9167
	d$latitude[d$location=="CENEB - FLAT DRIP"] <- 27.3833
	d$longitude [d$location=="CENEB - FLAT DRIP"] <-109.9333
	d$latitude[d$location=="CENEB - BED 2IR"] <- 27.4000
	d$longitude [d$location=="CENEB - BED 2IR"] <-109.9500
	d$latitude[d$location=="CRIA"] <- 25.4333
	d$longitude [d$location=="CRIA"] <-57.3167
	d$latitude[d$location=="PBS ALENTEJO"] <- 38.9000
	d$longitude [d$location=="PBS ALENTEJO"] <-7.1500
	
	##### Crop #####
	## normalize variety names
	## see carobiner::fix_name
	d$crop <- "wheat"
	d$variety <- NA 
	
	 	 
	f1 <- ff[basename(ff) == "18TH HTWYT_RawData.xlsx"]
	library("readxl")
	r1 <- read_excel(f1)
	
	d1<-r1
	#### about the data #####
	## (TRUE/FALSE)
	
	d1$dataset_id <- dataset_id
	d1$on_farm <- TRUE
	d1$is_survey <- FALSE
	d1$is_experiment <- TRUE
	d1$irrigated <- FALSE
	## the treatment code	
	d1$treatment<-d1$`Trial name`
	d1$country<-d1$Country
	d1$location<-d1$Loc_desc
	d1$year<-d1$Cycle
	d1$crop<-"wheat"
	d1$variety<-d1$Gen_name
	d1$yield<-d1$Value
	d1$yield_part<-"grain"
	
# all scripts must end like this
	carobiner::write_files(dset, d, path=path)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# path <- _____
# carob_script(path)

