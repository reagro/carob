# R script for "carob"

# ## ISSUES 
#Fertiliser amount are given as ratios 
# ....


carob_script <- function(path) {
  
  "
  This study contains data originating from on-farm trials that were conducted to test and demonstrate the crop yield and economic benefits derived from manual and animal traction conservation agriculture (CA) systems on smallholder farms where the ridge and furrow tillage system is the traditional practice. The farm trials were conducted at six farms in Chipata, Lundazi, and Sinda districts of the eastern province of Zambia. At each site, the trials were replicated four times and had two general treatment sets:1) manual CA; and 2) animal traction CA.

  The manual CA system trial consisted of three treatments and these treatments were compared with conventional ridge and furrow practice at each farmer's field. The four treatments including control were:
  
  Conventional ridge and furrow with continuous sole maize (CRF);
  No-tillage / Dibblestick CA system with continuous sole maize;
  Dibblestick CA system with maize intercropped with cowpea;
  Dibblestick CA with maize rotated with legumes
  The animal traction CA system consisted of two treatments that were compared with a conventional ridge and furrow practice at each farmer's field. The three treatments including control were:
  
  Conventional ridge and furrow with continuous sole maize (CRF);
  Animal traction (AT) ripline seeding with continuous sole maize;
  Animal traction (AT) ripline seeding with maize rotated with legumes"
  
  uri <- "doi:10.7910/DVN/RSGLGB"
  group <- "conservation_agriculture"
  ff <- carobiner::get_data(uri, path, group)
 
  dset <- data.frame(
  	carobiner::read_metadata(uri, path, group, major=1, minor=2),
    project=NA,
    publication= "doi:10.1017/S1742170517000606",
    data_institute = "CIMMYT",
    data_type="experiment",
    carob_contributor="Fredy Chimire",
    carob_date="2024-1-16"
  )
  
  
	f <- ff[basename(ff) == "AR_ZAM_CIMMYT_CAmother_onfarm_2021.csv"]
  
  # Select sheet with revised data from the excel file 
	r <- read.csv(f)
  
	d <- data.frame(
		country= r$Country, harvest_date=r$Year,
		rep= r$Rep, crop= r$Cropgrown, treatment= r$Description,
		adm2=r$District, location=r$Camp,
		dmy_total = r$Biomassyield, yield = r$Grainyield
	)
  
  # for first dataset
  
  
  d$is_survey <- FALSE
  d$on_farm <- TRUE
  
  d$yield_part <- "grain"

	d$harvest_date <- as.character(d$harvest_date)
	d$crop <- tolower(d$crop)
  
  # https://www.mindat.org/feature-905632.html
	geo <- list("Chipata" = c(-14.017, 32.65),
              "Chiapata" = c(-14.017, 32.65),
              "Lundazi" = c(-12.5, 32.75),
              "Sinda" = c(-14.187, 32.012))
			  
    geo <- t(as.data.frame(geo))
	colnames(geo) <- c("latitude", "longitude")
  
	d <- merge(d, geo, by.x="adm2", by.y=0, all.x=TRUE)
	
   carobiner::write_files(dset, d, path=path)
}

