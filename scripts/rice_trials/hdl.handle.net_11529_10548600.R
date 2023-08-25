# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"Description:

    The database contains data about on-farm trials with transplanted rice were conducted during monsoon (‘Aman’) season in 2016 and 2017 and winter (‘Boro’) season in 2016 to 2017 in agroecological zones (AEZs) 11 and 12 of south-west Bangladesh with ten treatments - seven herbicide-based IWM options, one mechanical weed control-based option, and two checks – farmers’ current weed control practice and weed-free, to assess effects on weed control, grain yield, labor use, and profitability. (2021-07-09)]

"

	uri <- "doi:https://hdl.handle.net/11529/10548600"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "rice_trials"
	path <- ("C:/Users/user/Documents/MY WORKING DIRECTORY/carob")
	## dataset level data 
	dset <- data.frame(
		dataset_id = dataset_id,
		group=group,
		project= "Cereal Systems Initiative for South Asia (CSISA)",
		uri=uri,
		data_citation="Ahmed,Sharif; Kumar,Virender; Alam,Murshedul; Dewan,Mahbubur Rahman; Bhuiya,Khairul Alam; Saha,Abhijit; Miajy,Abu Abdullah; Singh,Sudhanshu; Timsina,Jagadish; Krupnik,Timothy J., 2021, Replication Data for: Integrated weed management in transplanted rice: Options for addressing labor constraints and improving farmers’ income in Bangladesh, https://hdl.handle.net/11529/10548600, CIMMYT Research Data & Software Repository Network, V1, UNF:6:HmO/JVvcmNC7pPO6MNVZJQ== [fileUNF]",
		## if there is a paper, include the paper's doi here
		## also add a RIS file in references folder (with matching doi)
		publication= "Replication Data for: Integrated weed management in transplanted rice: Options for addressing labor constraints and improving farmers’ income in Bangladesh",
		data_institutions = "CIMMYT, IFPRI & IRRI",
   		data_type="on-farm experiment",
		carob_contributor="Blessing Dzuda"  
	)

## download and read data 

	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=0)
	dset$license <- carobiner::get_license(js)
	
	f <- "C:/Users/user/Documents/MY WORKING DIRECTORY/carob/data/raw/rice_trialsdoi_hdl.handle.net_11529_10548600 "
 
	library(readxl)
	Aman_Yield_and_economics <- read_excel("data/raw/rice_trials/doi_hdl.handle.net_11529_10548600/Aman_Yield_and_economics.xlsx")
	Boro_yield_and_economics_1 <- read_excel("data/raw/rice_trials/doi_hdl.handle.net_11529_10548600/Boro_yield_and_economics-1.xlsx")
	

	
## process file(s)

## use a subset
	a <- Aman_Yield_and_economics
	a1 <- Boro_yield_and_economics_1

	
#### about the data #####
## (TRUE/FALSE)

	a$dataset_id <- dataset_id
	a1$dataset_id <- dataset_id
	a$on_farm <- TRUE
	a1$on_farm <- TRUE
	a$is_survey <- FALSE
	a1$is_survey <- FALSE
	a$is_experiment <- TRUE
	a1$is_experiment <- TRUE
	a$irrigated <- FALSE
	a1$irrigated <- FALSE
## the treatment code	
	a$treatment <- a$Treatment
	a1$treatment <- a1$Treatment
	a$rep <- a$Replication
	a1$rep <- a1$Replication
	a$site <- a$SITE
	a1$site <- a1$SITE
  a$harvest_date <- a$Season
  a1$harvest_date <- a1$Season
  a$yield <- a$`Grain yield (t/ha)`
  a1$yield <- a1$`Grain yield (t/ha)`
  
##### Location #####
## make sure that the names are normalized (proper capitalization, spelling, no additional white space).
## you can use carobiner::fix_name()
	a$country <- "Bangladesh" 
  a1$country <- "Bangladesh"
	d$site <- NA
	d$adm1 <- NA
	d$adm2 <- NA
	d$adm3 <- NA
	d$elevation <- NA
## each site must have corresponding longitude and latitude
## see carobiner::geocode
	a$longitude <- 89.2137
	a1$longitude <- 89.2137
	a$latitude <- 23.1697
	a1$latitude <- 23.1697

##### Crop #####
## normalize variety names
## see carobiner::fix_name
	a$crop <- "Rice"
	a1$crop <- "Rice"
	a$variety <- "BRRI dhan49"
	a1$variety <- " BRRI dhan28"



##### Fertilizers #####
#NO FERTILISERS WERE USED IN THE EXPERIMENT 
   
   
##### in general, add comments to your script if computations are
##### based on information gleaned from metadata, a publication, 
##### or when they are not immediately obvious for other reasons

##### Yield #####
	#what plant part does yield refer to?
	a$yield_part <- "grain"
	a1$yield_part <- "grain"
	
	a <- a[, c("country","site","longitude","latitude","treatment","rep","crop","harvest_date","variety","yield_part","yield")]
	a1 <- a1[, c("country","site","longitude","latitude","treatment","rep","crop","harvest_date","variety","yield_part","yield")]
	
# all scripts must end like this
	carobiner::write_files(dset, a,a1, path=path)
}

## now test your function in a clean R environment 
# path <- _____
# carob_script(path)

