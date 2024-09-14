# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {
    
    "Datasets from four experiments planted in Huancayo under normal conditions for potato crop and one experiment planted under high temperatures in order to determinate Parental value for yield in advanced clones of B3C3 population. In the field experiments, the randomized complete block design was used with 3 replications  and 40 plants."
    
    uri <- "doi:10.21223/RHSVIY"
    group <- "varieties"
    ff  <- carobiner::get_data(uri, path, group)
    meta <- data.frame(
        carobiner::read_metadata(uri, path, group, major=4, minor=0),
        data_institute = "CIP",
        publication = NA,
        project = NA,
        data_type = "experiment",
        treatment_vars = "variety",
        response_vars = "yield;yield_marketable", 
        carob_contributor = "Henry Juarez",
        carob_date = "2024-09-11",
        notes = NA
    )
    
    process <- function(filename){
        
        r <- carobiner::read.excel(filename, sheet="Fieldbook")               
        minimal <- carobiner::read.excel(filename, sheet="Minimal")
        m <- as.list(minimal$Value)
        names(m) <- minimal$Factor
        data.frame(
            rep = as.integer(r$REP),
            variety = r$INSTN,
            yield = as.numeric(r$TTYNA) * 1000,
            yield_marketable = as.numeric(r$MTYNA) * 1000,
            AUDPC = as.numeric(NA),
            country = m$Country,
            adm1 = m$Admin1,
            adm2 = m$Admin2,
            adm3 = m$Admin3,
            location = m$Locality,
            longitude = as.numeric(m$Longitude),
            latitude = as.numeric(m$Latitude),
            elevation = as.numeric(m$Elevation),
            planting_date = as.character(as.Date(m$Begin_date,format = "%d/%m/%Y")),
            harvest_date = as.character(as.Date(m$End_date,format = "%d/%m/%Y")),
            trial_id = gsub(".xlsx", "", basename(filename))
        )
    }
    
    f <- ff[grep("PTYield", basename(ff))]
    d <- lapply(f, process)
    d <- do.call(rbind, d)
    
    d$on_farm <- TRUE
    d$is_survey <- FALSE
    d$irrigated <- FALSE
    d$crop <- "potato"
    d$pathogen <- "Phytophthora infestans"
    d$yield_part <- "tubers"
    d$geo_from_source <- TRUE
    d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
    
    carobiner::write_files(path = path, metad = meta, records = d)
    
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# path <- _____
# carob_script(path)
