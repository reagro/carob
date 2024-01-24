# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"Description:

    Conventional rice–wheat (RW) rotation in the Indo-Gangetic Plains (IGP) 
    of South Asia is tillage, water, energy, and capital intensive. 
    Coupled with these, crop residue burning contributes significantly 
    to greenhouse gas (GHG) emission and environmental pollution. 
    So, to evaluate the GHG mitigation potential of various climate-smart 
    agricultural practices (CSAPs), an on-farm research trial was conducted 
    during 2014–2017 in Karnal, India. Six management scenarios 
    (portfolios of practices), namely, Sc1—business as usual (BAU)/conventional 
    tillage (CT) without residue, Sc2—CT with residue, 
    Sc3—reduced tillage (RT)with residue + recommended dose of fertilizer (RDF), 
    —RT/zero tillage (ZT) with residue + RDF, Sc5—ZT with residue + RDF + GreenSeeker + Tensiometer, and Sc6—Sc5 + nutrient-expert tool, were included. The global warming potential (GWP) of the RW system under CSAPs (Sc4, Sc5, and Sc6) and the improved BAU (Sc2 and Sc3) were 33–40% and 4–26% lower than BAU (7653 kg CO2 eq./ha/year), respectively. This reflects that CSAPs have the potential to mitigate GWP by ~387 metric tons (Mt) CO2 eq./year from the 13.5 Mha RW system of South Asia. 
    Lower GWP under CSAPs resulted in 36–44% lower emission intensity (383 kg CO2 eq./Mg/year) compared to BAU (642 kg CO2 eq./Mg/year). Meanwhile, the N-factor productivity and eco-efficiency of the RW system under CSAPs were 32–57% and 70–105% higher than BAU, respectively, which reflects that CSAPs are more economically and environmentally sustainable than BAU. The wheat yield obtained under various CSAPs was 0.62 Mg/ha and 0.84 Mg/ha higher than BAU during normal and bad years (extreme weather events), respectively. Thus, it is evident that CSAPs can cope better with climatic extremes than BAU. Therefore, a portfolio of CSAPs should be promoted in RWbelts for more adaptation and climate change mitigation. (2021-12-14)

"

	uri <- "hdl:11529/10548753"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "conservation_agriculture"
	## dataset level data 
	dset <- data.frame(
		dataset_id = dataset_id,
		group=group,
		project=NA,
		uri=uri,
		data_citation="Effect of Climate-Smart Agriculture Practices on 
		Climate Change Adaptation, Greenhouse Gas Mitigation and Economic 
		Efficiency of Rice-Wheat System in India",
		publication= NA,
		data_institutions = "CIMMYT",
   	data_type="is_experiment", 
		carob_contributor="Shumirai Manzvera",
		# date of first submission to carob
		carob_date="2023-12-12",
		# name(s) of others who made significant improvements
		revised_by="NA"
	)

## download and read data 

	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=1)
	dset$license <- "CIMMYT"
	f <- ff[basename(ff) == "Agriculture_11-0-1269_Kakraliya et al 2021.xlsx"]
	d <- carobiner::read.excel(f,skip = 1)
	
## process file(s)


	d$dataset_id <- dataset_id
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$is_experiment <- TRUE
	d$irrigated <- TRUE
  d$treatment <- d$Scenario
	d$rep <- d$Replication

##### Location #####
## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
## you can use carobiner::fix_name()
	d$country <- "India"
	d$site <- "Karnal"
## each site must have corresponding longitude and latitude
## see carobiner::geocode
	d$longitude <- 76.990547
	d$latitude <- 29.685629

	d$crop <- "rice"
  d$grain_N <- d$`N uptake grain`
	d$P_fertilizer <- factor(d$Scenario, levels =  1:6, labels = c(58/2.29,58/2.29,60/2.29,60/2.29,60/2.29,39/2.29))
	d$K_fertilizer <- factor(d$Scenario, levels =  1:6, labels = c(0/1.2051,0/1.2051,60/1.2051,60/1.2051,60/1.2051,70/1.2051))
  d$N_fertilizer <- d$Nitrogen

##### Yield #####

	d$yield <- d$`rice grain yield`
	d$yield_part <- "grain" 
	d$dmy_stems <- d$`straw yield (t/ha)`
	
	d<-d[, c("dataset_id","on_farm","is_survey","is_experiment","irrigated","treatment","rep","country","site","longitude","latitude","crop","grain_N","P_fertilizer","K_fertilizer","N_fertilizer","yield","yield_part","dmy_stems")]
	d<-d[d$grain_N != "N uptake grain",]
	## wheat
	d2 <- readxl::read_excel(f,skip = 22) 
	d2$crop <- "Wheat"
	d2$treatment <- d2$Scenario
	d2$yield <- d2$`wheat grain yield`
	d2$yield_part <- "grain"
	d2$dmy_stems <- d2$`straw yield (t/ha)`
	d2$rep <- d2$Replication
	
	# fertiliser
	d2$P_fertilizer <- factor(d2$Scenario, levels =  1:6, labels = c(58/2.29,58/2.29,60/2.29,60/2.29,60/2.29,62/2.29))
	d2$N_fertilizer <- d2$`N uptake grain`
	d2$K_fertilizer <- factor(d2$Scenario, levels =  1:6, labels = c(0/1.2051,0/1.2051,60/1.2051,60/1.2051,60/1.2051,60/1.2051))
  d2$grain_N <- d2$`N uptake grain`
  
  d2$dataset_id <- dataset_id
  d2$on_farm <- TRUE
  d2$is_survey <- FALSE
  d2$is_experiment <- TRUE
  d2$irrigated <- TRUE
  d2$treatment <- d2$Scenario
  d2$rep <- d2$Replication
  d2$country <- "India"
  d2$site <- "Karnal"
  d2$longitude <- 76.990547
  d2$latitude <- 29.685629
  d2<- d2[, c("crop","treatment","yield","yield_part","dmy_stems","rep","P_fertilizer","N_fertilizer","K_fertilizer","grain_N","dataset_id","on_farm","is_survey","is_experiment","irrigated","country","site","longitude","latitude")]
  d2<-d2[d2$grain_N != "N uptake grain", ]
  d<-carobiner::bindr(d,d2)
# all scripts must end like this
	carobiner::write_files(dset, d, path=path)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# path <- _____
#carob_script(path)








