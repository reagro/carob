

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
