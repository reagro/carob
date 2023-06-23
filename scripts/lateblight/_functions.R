

proc_breeding_trial <- function(f, dates, dataset_id) {

	d <- carobiner::read.excel(f, sheet = "Fieldbook") 

	d$record_id <- as.integer(1:nrow(d))
	lbvars <- grep('^LB', colnames(d), value=TRUE)
	
	x <- reshape(d[, c("record_id", lbvars)], direction="long", varying =lbvars, v.names="severity", timevar="step")
	x$time <- dates[x$step]
	x$step <- x$id <- NULL
	x$dataset_id <- dataset_id
	
	d[, lbvars] <- NULL	
	d <- carobiner::change_names(d, 
		c("REP", "INSTN", "TTYNA"),
		c("rep", "variety", "yield"))

	d$rep <- as.integer(d$rep)
	d$yield <- d$yield * 1000
	d$AUDPC <- d$AUDPC / 100
	
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE
## the treatment code	
	d$treatment <- "none"
	d$trial_id <- "1"
	d$crop <- "potato"
	d$pathogen <- "Phytophthora infestans"
	d$dataset_id <- dataset_id

# excluding variables perhaps of interest such as 
# 'NTP', 'SAUDPC', 'NoMTWP', 'TTWP', 'MTWP', 'MTYNA',
	d <- d[, c('dataset_id', 'record_id', 'rep', 'variety', 'AUDPC', 'rAUDPC',  'yield', 'on_farm', 'is_survey', 'irrigated', 'treatment', 'trial_id', 'crop', 'pathogen')] 
	
	list(d=d, tim=x)
}
