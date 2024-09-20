## there are more variables to be processed; depending on the dataset


process_cassava <- function(ff, location=NULL, adm1=NULL) {

	f <- grep("\\.csv$", ff, value=TRUE)
	r <- read.csv(f)
	fd <- dirname(f)
	fj <- file.path(fd, paste0(basename(fd), ".json"))
	m <- jsonlite::fromJSON(fj)$result
	
	
	d <- data.frame(
		longitude = as.numeric(m$coverage_y), #!!
		latitude = as.numeric(m$coverage_x),
		geo_from_source = TRUE,
		country = m$coverage_country,
		planting_date = r$plantingDate,
		harvest_date = r$harvestDate,
		year = r$studyYear,
		plot_length = as.numeric(r$plotLength),
		plot_width = as.numeric(r$plotWidth),
		location = carobiner::fix_name(r$locationName, "title"),
		variety = r$germplasmName,
		variety_alt = as.character(r$germplasmSynonyms),
		variety_code = as.character(r$germplasmDbId),
		rep = r$replicate,
		trial_id = r$studyName,
		on_farm = FALSE
	)
	if (!is.null(r$fresh.root.yield.CO_334.0000013)) {
		d$fwy_storage <- d$yield <- 1000 * r$fresh.root.yield.CO_334.0000013
	} else if (!is.null(r$fresh.storage.root.weight.per.plot.CO_334.0000012)) {
		d$fwy_storage <- d$yield <- 1000 * r$fresh.storage.root.weight.per.plot.CO_334.0000012 / (d$plot_length * d$plot_width)
	}
	if (!is.null(r$fresh.shoot.weight.measurement.in.kg.per.plot.CO_334.0000016)) {
		d$fwy_residue <- 1000 * r$fresh.shoot.weight.measurement.in.kg.per.plot.CO_334.0000016 / (d$plot_length * d$plot_width)	
	}
	d$dmy_storage <- r$dry.yield.CO_334.0000014
	if (!is.null(r$dry.matter.content.percentage.CO_334.0000092)) {
		yield_moisture <- 100 - r$dry.matter.content.percentage.CO_334.0000092
	}
	d$harvest_index <- r$harvest.index.variable.CO_334.0000015


	d$planting_date <- carobiner:::eng_months_to_nr(d$planting_date) |> as.Date() |> as.character()
	d$harvest_date <- carobiner:::eng_months_to_nr(d$harvest_date) |> as.Date() |> as.character()
	if (!is.null(location)) {
		d$location <- location
		d$adm1 <- adm1
		d$on_farm <- TRUE
	}


	vdis <- c("cassava.mosaic.disease.incidence.1.month.evaluation.CO_334.0000195", "cassava.mosaic.disease.incidence.3.month.evaluation.CO_334.0000196", "cassava.mosaic.disease.incidence.6.month.evaluation.CO_334.0000198", "cassava.mosaic.disease.severity.1.month.evaluation.CO_334.0000191", "cassava.mosaic.disease.severity.3.month.evaluation.CO_334.0000192", "cassava.mosaic.disease.severity.6.month.evaluation.CO_334.0000194", "cassava.mosaic.disease.severity.12.month.evaluation.CO_334.0000199", "cassava.bacterial.blight.severity.3.month.evaluation.CO_334.0000175", "cassava.bacterial.blight.severity.6.month.evaluation.CO_334.0000176", "cassava.bacterial.blight.incidence.6.month.evaluation.CO_334.0000179")

	i <- match(vdis, names(r))
	tmrecs <- NULL
	if (any(!is.na(i))) {
		vnms <- vdis[which(!is.na(i))]
		dd <- r[, vnms, drop=FALSE]
		d$record_id <- dd$record_id <- 1:nrow(d)
		DAP <- rep(NA, ncol(dd)-1)
		DAP[grepl("1.month", vnms)] <- 30
		DAP[grepl("3.month", vnms)] <- 91
		DAP[grepl("6.month", vnms)] <- 182
		DAP[grepl("11.month", vnms)] <- 365
		DAP <- as.integer(DAP)
		dates <- NULL
		if (!is.na(d$planting_date[1])) {
			dates <- as.character(as.Date(d$planting_date[1]) + DAP)
		}
		diseases <- rep("mosaic", length(vnms))
		diseases[grep("blight", vnms)] <- "bacterial blight"
		incidence <- severity <- NULL
		sev <- grepl("severity", names(dd))	
		if (any(sev)) {
			ddi <- dd[, c("record_id", names(dd)[sev])]
			severity <- reshape(ddi, direction="long", varying=vnms[sev], v.names="disease_severity", timevar="step")
			severity$time <- dates[sev][severity$step]
			severity$DAP <- DAP[sev][severity$step]
			severity$diseases <- diseases[sev]
			severity$disease_severity <- as.character(severity$disease_severity)
			severity$severity_scale <- as.character(NA) #perhaps "1-4?  
			severity <- na.omit(severity)
			if (nrow(severity) == 0) severity <- NULL
		}
		inc <- grepl("incidence", names(dd))	
		if (any(inc)) {
			ddi <- dd[, c("record_id", names(dd)[inc])]
			incidence <- reshape(ddi, direction="long", varying=vnms[inc], v.names="disease_incidence", timevar="step")
			incidence$time <- dates[inc][incidence$step]
			incidence$DAP <- DAP[inc][incidence$step]
			incidence$diseases <- diseases[inc]
			incidence$disease_incidence <- as.character(incidence$disease_incidence)
			incidence <- na.omit(incidence)
			if (nrow(incidence) == 0) incidence <- NULL
			
		}
		tmrecs <- carobiner::bindr(severity, incidence)
		if (!is.null(tmrecs)) {
			tmrecs$id <- tmrecs$step <- NULL      
		}
		d[vnms] <-  NULL
	}


	if (all(is.na(d$planting_date))) d$planting_date <- as.character(d$year)
	d$year <- NULL

	d$is_survey <- FALSE
	d$crop <- "cassava"
	d$yield_part <- "roots"
	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
	d$irrigated <- NA


	list(records=d, timerecs=tmrecs)
}


