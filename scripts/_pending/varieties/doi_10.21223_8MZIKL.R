# R script for "carob"

## need to extract more variables. 
## use carobiner::read.excel.hdr
## use d <- data.frame(var = r$Var, ...) to assingn old to new variables  
## location is not correct. Need to be full names and IDSs can be created with reshape; and no need for bizarre reg expr.    
## do not use more than four decimals in coordinates if they are estimated

carob_script <- function(path) {
    
"The LBHT x LTVR population came from crossing the two populations developed at CIP: LBHT for late blight resistance and LTVR for virus resistance in order to exploit heterosis for tuber yield, in 2013 under quarantine greenhouses at La Molina. 7200  genotypes (45 families with 160 seeds each) were planted. At harvest 258 clones were selected. Since 2015 until 2019, these clones were tested for late blight and PVX, PVY virus resistance, heat and drought tolerance,  marketable tuber yield, dry matter and quality for industrial processing, The experiments were planted in sites where environmental conditions are favorable to have high pressure biotic and abiotic stresses that allow us to select clones with resistance and / or tolerance to these factors. Thirty-nine clones were selected for late blight resistance, heat tolerance, some clones have drought tolerance, resistance to virus PVX and or PVY an good quality for industrtial processing."
    
    uri <- "doi:10.21223/8MZIKL"
    group <- "varieties"
    ff  <- carobiner::get_data(uri, path, group)
    
    meta <- data.frame(
        carobiner::read_metadata(uri, path, group, major=1, minor=0),
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
    
    f <- ff[grep("Data.xls", basename(ff))]
    
    d <- carobiner::read.excel(f = f, sheet="Table")
    cols_names <- c("record_id","Clone","MTYNA SRA","MTYNA HYO 2017-2018","MTYNA OXA 2017-2018",
                    "MTYNA MAJ normal irrigation 2018-2019","MTYNA MAJ restricted irrigation 2018-2019",
                    "AUDPC","PVX Resistance","PVY Resistance","Heat Tolerance",
                    "Drought Tolerance","Dry matter","Chips color")
    d <- d[3:nrow(d),]
    colnames(d) <- cols_names
    variable_cols <-
        c("MTYNA SRA", "MTYNA HYO 2017-2018","MTYNA OXA 2017-2018",
          "MTYNA MAJ normal irrigation 2018-2019","MTYNA MAJ restricted irrigation 2018-2019")
    long_data <-
        reshape(
            d,
            direction = "long",
            idvar = "Clone",
            varying = list(variable_cols),
            timevar = "MTYNA",
            times = variable_cols
        )
    
    names(long_data)[9] <- "MTYNA Category"
    names(long_data)[10] <- "MTYNA"
    
    d = long_data[,c('Clone','AUDPC','MTYNA SRA')]
    d = carobiner::change_names(d, from=c("Clone","MTYNA SRA"), 
                                 to = c("variety","yield_marketable") )
    
    d$yield_marketable <- as.numeric(d$yield_marketable) * 1000
    d$AUDPC <- as.numeric(d$AUDPC) / 100
    
    d$on_farm <- TRUE
    d$is_survey <- FALSE
    d$irrigated <- FALSE	
    d$treatment <- "varieties"
    d$trial_id <- "1"
    d$crop <- "potato"
    d$pathogen <- "Phytophthora infestans"
    d$country <- 'Peru'
    d$yield_part <- "tubers"
    d$location <- sub(".*MTYNA\\s+([A-Za-z0-9]+(?:\\s+[A-Za-z]+)?)\\s*(?:irrigation|\\d+)?", "\\1", rownames(d))
    d$location <- tolower(sub("([A-Za-z]+)(?:-\\d+|\\s+\\d+).*", "\\1", d$location))

    location_coords <- data.frame(
        location = tolower(unique(d$location)),
        latitude = c(-12.00925833,-10.59535278,-10.59535278,-11.12858056,-12.00925833),
        longitude = c(-75.22366389, -75.38681667,-75.38681667, -75.35643056,-75.22366389),
        adm1 = c('Junin','Arequipa','Arequipa','Pasco','Junin'),
        adm2 = c('Huancayo','Majes','Majes','Oxapampa','San Ramon')
    )
    d <- merge(d, location_coords, by = "location", all.x = TRUE)
    d$geo_from_source = FALSE
    d$yield <- as.numeric(NA)
    d$planting_date <- "2017-05-05"
    d$harvest_date  <- "2017-11-17"
    d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
    d$treatment <- "Clones under different locations & irrigations systems"
    d$location <- d$adm2
    d <- unique(d)
    
    carobiner::write_files(path = path, metadata = meta, records = d)

}

