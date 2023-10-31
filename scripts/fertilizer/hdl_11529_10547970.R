# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"Description:

    [Farmers’ participatory researchers managed long-term trails aimed to improve the productivity, profitability, and sustainability of smallholder agriculture in the EGP by activities carried out to address the objectives: 1. Understand farmer circumstances with respect to cropping systems, natural and economic resources base, livelihood strategies, and capacity to bear risk and undertake technological innovation. 2. Develop with farmers more productive and sustainable technologies that are resilient to climate risks and profitable for small holders. 3. Facilitate widespread adoption of sustainable, resilient, and more profitable farming systems. (2018-02-18)]

"

	uri <- "hdl:11529/10547970"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "fertilizer"
	## dataset level data 
	dset <- data.frame(
		dataset_id = dataset_id,
		group=group,
		project=NA,
		uri=uri,
		data_citation="Gathala, Mahesh K.; Tiwari, Thakur P.; Islam, Saiful; Chowdhury, Apurba K.; Bhattacharya, Prateek M.; Das, K.K.; Dhar, Tapamay; Pradhan, K.; Sinha, A.K.; Ghosh, Arunava; Mitra, B.; Chattopadhyay, C., 2018, '2.8-Rabi (winter) crops-all nodes-Long term trial (LT)-Malda-West Bengal', https://hdl.handle.net/11529/10547970, CIMMYT Research Data & Software Repository Network, V2",
		## if there is a paper, include the paper's doi here
		## also add a RIS file in references folder (with matching doi)
		publication= NA,
		data_institutions = "CIMMYT",
   		data_type="experiment", 
		carob_contributor="Mitchelle Njukuya",
		# date of first submission to carob
		carob_date="2023-10-10" 
	)

## download and read data 
	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=1)
	dset$license <- carobiner::get_license(js)

	f <- ff[basename(ff) == "Rabi Maize 2016-17-LT-all nodes-Malda.xlsx"]

# fix the bad spreadsheets
	filler <- function(x){
		e <- rle(x)
		empty <- which(is.na(e))
		x[empty] <- x[empty-1] 
		inverse.rle(e)
	}

	fix_ss <- function(x, hdr) {
		nms <- x[hdr,]
		if (is.na(nms[1,1])) nms[1,1] <- "" 
		for (i in 2:ncol(nms)) {
			if (is.na(nms[1,i])) nms[1,i] <- nms[1,i-1]
		}		
		nms <- apply(nms, 2, \(i) paste(i[!is.na(i)], collapse="_"))
		x <- x[-c(1:max(hdr)), ]
		names(x) <- nms
		x
	}


	pr <- carobiner::read.excel(f, sheet = "4- Stand counts & Phenology")
	pr <- fix_ss(pr, 2:3)

	fr <- carobiner::read.excel(f, sheet = "6 - Fertilizer amounts ")
	fr <- fix_ss(fr, 3:4)

	famnt <- c("N  (kg/ha)", "P2O5 (kg/ha)", "K2O (kg/ha)", "Gypsum (kg/ha)", "ZnSO4 (kg/ha)", "Boric acid (kg/ha)")
	famnt <- paste0("Fertilizer 5 Application_", famnt)
	prds <- grep("Product used", names(fr), value=TRUE)
	fr <- fr[, c("Site No.", "Tmnt", famnt, prds)]


	yr <- carobiner::read.excel(f, sheet = "14 - Grain Harvest ")
	yr <- fix_ss(yr, 3:4)
	nms <- c("Grain yield (t/ha)", "TGW (g)", "Biomass (t/ha)", "Straw yield (t/ha)", "HI")
	yr <- yr[, c("Site No.", "Tmnt", paste0("Calculation_", nms))]
	colnames(yr) <- gsub("Calculation_", "", colnames(yr))
	
	r <- merge(pr, fr, by=c("Site No.", "Tmnt"))
	r <- merge(r, yr, by=c("Site No.", "Tmnt"))
		

#################################################### 4- Stand counts & Phenology #############################################	
## do not do this and then subset
#	d <- r
# rather start a new data.frame
   d <- data.frame(
		trial_id = r$`Trial Code`, 
		treatment = r$Tmnt,
		crop=tolower(r$Crop), variety= r$Variety, 
		season=r$Season, 
		biomass_total = as.numeric(r$`Biomass (t/ha)`) * 1000,
		residue_yield = as.numeric(r$`Straw yield (t/ha)`) * 1000,
		yield = as.numeric(r$`Grain yield (t/ha)`) * 1000)

	d$planting_date <- as.character(as.Date(as.numeric(r$`Date of seeding (dd-mm-yy)`), origin = "1899-12-30"))
	d$harvest_date <- as.character(as.Date(as.numeric(r$`Datw of harvest (dd-mm-yy)`), origin = "1899-12-30"))

#	emergence= r$`100% emergence (DAS)`,
#	flowering=r$`50% anthesis (DAS)`
#	maturity=r$`80% physiological maturity (DAS)`,
#	harvest=r$`Harvesting (DAS)`

#### about the data #####
## (TRUE/FALSE)

	d$dataset_id <- dataset_id
	d$on_farm <- TRUE
	d$is_survey <- FALSE 
	d$irrigated <- TRUE
    d$inoculated <- FALSE
	d$yield_part <- "grain"

##### Location #####
	d$country <- "India"
	d$adm1 <- "West Bengal"
	d$adm2 <- "Malda"
	d$location <- r$Node
	d$longitude [d$location =="Mahadipur"] <- 88.1265
	d$latitude [d$location =="Mahadipur"] <- 24.8501 
	d$longitude [d$location =="Bidyanandapur"] <- 87.9903
	d$latitude [d$location =="Bidyanandapur"] <- 25.9517
	d$longitude [d$location =="Urgitola"] <- 88.1411
	d$latitude [d$location =="Urgitola"] <- 25.0108
	
##### Fertilizer ########
	pnms <- grep("Product used", names(fr), value=TRUE)
	frp <- cbind(fr[, c("Site No.", "Tmnt")], value=apply(fr[, pnms], 1, 
		function(i) paste(unique(i[!is.na(i)]), collapse="; "))
	)
	# the excel file has encoded 10:26:26 as "time"!
	frp$value <- gsub("0.43502314814814813", "NPK", frp$value)
	frp$value <- gsub("UREA", "urea", frp$value)
	frp$value <- gsub("Borax 21", "Borax", frp$value)

	d$fertilizer_type <- frp$value
	d$N_fertilizer <- as.numeric(r$`Fertilizer 5 Application_N  (kg/ha)`)
	d$P_fertilizer <- as.numeric(r$`Fertilizer 5 Application_P2O5 (kg/ha)`) / 2.29
	d$K_fertilizer <- as.numeric(r$`Fertilizer 5 Application_K2O (kg/ha)`) / 1.2051
	d$S_fertilizer <- 0
	d$Zn_fertilizer <- 0
	d$B_fertilizer <- as.numeric(r$`Fertilizer 5 Application_Boric acid (kg/ha)`) * 0.1748
	d$lime <- 0
	d$gypsum <- 0

# all scripts must end like this
	carobiner::write_files(dset, d, path=path)
}

