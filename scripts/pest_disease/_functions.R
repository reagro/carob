

proc_lb_trial <- function(f, dates) {

	r <- carobiner::read.excel(f, sheet = "Fieldbook") 

# excluding variables perhaps of interest such as 
# 'NTP', 'SAUDPC', 'NoMTWP', 'TTWP', 'MTWP', 'MTYNA',

	d <- data.frame(
		rep = as.integer(r$REP),
		yield = r$TTYNA * 1000,
		variety = r$INSTN,
		AUDPC = r$AUDPC / 100,
		rAUDPC = r$rAUDPC,
		trial_id = "1",
		crop = "potato",
		pathogen = "Phytophthora infestans",
		yield_part = "tubers",
		on_farm = TRUE,
		is_survey = FALSE,
		irrigated = FALSE
	)
	
	r$record_id <- as.integer(1:nrow(r))
	lbvars <- grep('^LB', colnames(r), value=TRUE)
	x <- reshape(r[, c("record_id", lbvars)], direction="long", varying =lbvars, v.names="severity", timevar="step")
	x$time <- dates[x$step]
	x$step <- x$id <- NULL
	
	list(d=d, tim=x)
}

proc_cip_lb_trial <- function(f) {
  
  r1 <- carobiner::read.excel(f, sheet = "Fieldbook")
  
  d <- data.frame(
    crop="potato",
    variety_code= r1$INSTN,
    rep= as.integer(r1$REP),
    AUDPC= r1$AUDPC/100,
    on_farm= FALSE,
    irrigated= NA,
    inoculated= FALSE,
    yield_part= "tubers",
    trial_id= "1"        
  )
  
  ## Add more variables
  m <- carobiner::read.excel(f, sheet = "Minimal")
  r2 <- data.frame(rbind(m$Value))
  names(r2) <- m$Factor
  n <- carobiner::read.excel(f, sheet = "Installation")
  r3 <- data.frame(rbind(n$Value))
  names(r3) <- n$Factor
  o <- carobiner::read.excel(f, sheet = "Crop_management")
  r4 <- data.frame(rbind(o[,"Date"]))
  names(r4) <- o[,"Intervention type"]
  
  d$trial_id <- r2$`Short name or Title`
  d$country <- r2$Country
  d$adm1 <- r2$Admin1
  d$adm2 <- r2$Admin2
  d$adm2 <- r2$Admin2
  d$adm3 <- r2$Admin3
  d$location <- r2$Locality
  d$latitude <- as.numeric(r2$Latitude)
  d$longitude <- as.numeric(r2$Longitude)
  d$elevation <- as.numeric(r2$Elevation)
  d$geo_from_source <- TRUE
  d$planting_date <- as.character(as.Date(r4$Planting, "%Y-%m-%d"))
  d$harvest_date <- as.character(as.Date(r4$Harvest, "%Y-%m-%d"))
  d$plant_spacing <- as.numeric(r3$`Distance between plants (m)`)*100 
  d$row_spacing <- as.numeric(r3$`Distance between rows (m)`)*100
  d$plot_area <- as.numeric(r3$`Plot size (m2)`)
  d$plant_density <- as.numeric(r3$`Number of plants planted per plot`)/(d$plot_area/10000)
  
  d$yield <- as.numeric(r1$TTWP)/(d$plot_area/10000) # to kg/ha
  
  ### disease scores during the season
  lbvars <- grep('^LB', colnames(r1), value=TRUE)
  dd <- r1[,lbvars]
  # EGB:
  # # Need to keep the trial_id in case there's more than 1 trial in the dataset
  dd$record_id <- paste(r2$`Short name or Title`, as.integer(1:nrow(dd)),sep = "-")
  dates <- as.character(r4[,grep('^Percentage', colnames(r4), value=FALSE)])
  x <- reshape(dd, direction="long", varying =lbvars, v.names="severity", timevar="step")
  x$time <- dates[x$step]
  x$step <- x$id <- NULL
  
  list(d=d, tim=x)
}
