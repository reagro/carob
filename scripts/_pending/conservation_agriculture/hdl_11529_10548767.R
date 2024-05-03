# R script for "carob"

## ISSUES
# ....
## review by Cedric Ngakou

carob_script <- function(path) {

"A field experiment was conducted in maize under medium-term conservation agriculture (CA) based maizewheat system at BISA-CIMMYT, Ladhowal, Punjab during kharif 2019 to assess the effect of CA+ practices (CA with sub-surface drip irrigation) with variable N doses on maize. The CA+ treatments were residue retained (WR) permanent bed (PB) with sub-surface drip fertigation (PB-SSD): without N (N0), 120 kg N/ha,150 kg N/ha applied in 4-equal (Eq) and differential splits (Df); CA alone treatment includ PB furrow irrigation with 120 kg N/ha (PBWRFurrow- N120); conventional tillage (CT) involved furrow irrigation with 120 kg N/ha (CTWOR-Furrow-N120) and other treatments were residue removed (WOR) PB: PBWOR-without N (N0), with 120 kg N/ha, and 150 kg N/ha applied in four Eq-splits and Df-splits. The findings of the present experiment showed that the numerical value of yield attributing characters were higher under CA+ plots as compared to CA alone (PBWR-Furrow-N120) and CT (CTWOR-Furrow-N120). Biological yield of maize was significantly higher in all CA+ plots as compared to CA alone and CT plots. Highest biological yield was recorded under PBWR-SSD-N150 Df (23.45 t/ha). Highest no. of cobs (72800/ha), no. of grains/cob (605) and cob length (22.61cm) along with dry matter resulted highest biological yield in PBWR-SSD-N150 plots. The grain N content remained statistically similar across all the N management plots, but in case of total N uptake, PBWR-SSD-N150 Df (CA+) plots dominated due to higher biomass. Besides, CA+ based PBWR-SSD-N120 (average of Df and Eq) registered 23-24% higher total N uptake than CA alone (PBWRFurrow- N120) and conventional (CTWOR-Furrow-N120) plots. Improved agronomic N use-efficiency was also recorded under CA+ plots as compared to CA alone (36.4 kg/kg N) and CT (36.7 kg/kg N) plots. (2021-02-12)"

	uri <- "hdl:11529/10548767"
	group <- "conservation_agriculture"
	ff <- carobiner::get_data(uri, path, group)

	dset <- data.frame(
		carobiner::read_metadata(uri, path, group, major=1, minor=3),
		project=NA,
		https://hdl.handle.net/11529/10548767, CIMMYT Research Data & Software Repository Network, V1",
		publication= NA,
		data_institutions = "CIMMYT",
		data_type="on-farm experiment", 
		carob_contributor="Hope Mazungunye",
		carob_date="2023-09-15"
	)

#path <- "C:/carob"### always set to the working direction 

	## ## CN 
	#It is no advance to read the data from local computer 
	#f <- "C:/carob/data/raw/maize_trials/hdl_11529_10548767/IJAS_TABLE_REPLICATED_DATA.xlsx"
  
	bn <- basename(ff)
	##CN
	### use carobiner::read.excel
	r<- carobiner::read.excel(ff[bn=="IJAS_Table_Replicated_Data.xlsx"],sheet= "Yield and Attributing Character")
	names(r) <- make.names(names(r))
	
	###CN
	## Variables in the "N Data (NUE, Uptake)" sheet are not required for carob. 
	#r1<- carobiner::read.excel(ff[bn=="IJAS_Table_Replicated_Data.xlsx"],sheet= "N Data (NUE, Uptake)")
	#names(r1) <- make.names(names(r1))
	
## process file(s)

## use a subset
	#d <- r
	#d1 <- r1
	d <- r[,c("Biological.Yield..t.ha.","Harvest.Index....")]
   d$treatment <- r$...1
	d<- d[-c(1),]
	colnames(d)<- c("yield","Harvest_index","treatment")
	d$Harvest_index<- NULL
	## add columns
	d$crop<- "maize"
	
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- TRUE
	d$country <- "India" 
	d$elevation <- 229
	d$planting_date <- "2019"
	##CN
	##add fertilizer
	### 0-60-30 ( N-P205-K20)
	d$P_fertilizer<- 60/2.29
	d$K_fertilizer<- 30/1.2051 
	d$N_fertilizer<- 0
	i1<- grepl("PBWR-SSD-N120 Eq",d$treatment) |grepl("PBWOR-SSD-N120 Df",d$treatment) |grepl("PBWR-SSD-N120 Df",d$treatment) |grepl("PBWOR-SSD-N120 Eq",d$treatment)|grepl("CTWOR-Furrow-N120",d$treatment) |grepl("PBWR-Furrow-N120",d$treatment)
	d$N_fertilizer[i1]<- 120
	
	i2<- grepl("PBWOR-SSD-N150 Eq",d$treatment) |grepl("PBWR-SSD-N150 Eq",d$treatment) |grepl("PBWOR-SSD-N150 Df",d$treatment) |grepl("PBWR-SSD-N150 Df",d$treatment)
	d$N_fertilizer[i2]<- 150
#### add lon and lat 
	d$longitude <- 79
	d$latitude <- 22
#### CN
	## the longitude and latitude correspond to the country. the site of the experiment is not specified. 
	
	d$yield_part <- "grain"
	##data type
	d$yield<-  as.numeric(d$yield)
	d$yield<- d$yield*1000 # convert in kg/ha
	carobiner::write_files(dset, d, path=path)
}
