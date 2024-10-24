# R script for "carob"

## ISSUES
# ....


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
    
    r <- carobiner::read.excel(f = f, sheet="Table")
    cols_names <- c("record_id","Clone","MTYNA SRA","MTYNA HYO 2017-2018","MTYNA OXA 2017-2018",
                    "MTYNA MAJ normal irrigation 2018-2019","MTYNA MAJ restricted irrigation 2018-2019",
                    "AUDPC","PVX Resistance","PVY Resistance","Heat Tolerance",
                    "Drought Tolerance","Dry matter","Chips color")
    r <- r[3:nrow(r),]
    colnames(r) <- cols_names
    variable_cols <-
        c("MTYNA SRA", "MTYNA HYO 2017-2018","MTYNA OXA 2017-2018",
          "MTYNA MAJ normal irrigation 2018-2019","MTYNA MAJ restricted irrigation 2018-2019")
    long_data <-
        reshape(
            r,
            direction = "long",
            idvar = "Clone",
            varying = list(variable_cols),
            timevar = "MTYNA",
            times = variable_cols
        )
    
    names(long_data)[9] <- "MTYNA Category"
    names(long_data)[10] <- "MTYNA"
    
    location_mapping <- data.frame(
        MTYNA = c("MTYNA SRA", "MTYNA HYO 2017-2018", "MTYNA OXA 2017-2018",
                  "MTYNA MAJ normal irrigation 2018-2019", "MTYNA MAJ restricted irrigation 2018-2019"),
        location = c("San Ramon", "Huancayo", "Oxapampa", "Majes", "Majes")
    )
    
    
    long_data <- merge(long_data, location_mapping, by = "MTYNA", all.x = TRUE)
    
    
    r = long_data[,c('Clone','AUDPC','MTYNA SRA', 'location', "PVX Resistance","PVY Resistance","Heat Tolerance","Drought Tolerance")]
    
    d = data.frame(
        variety = r$Clone,
        AUDPC = r$AUDPC,
        yield_marketable = r$`MTYNA SRA`,
        pvx = r$`PVX Resistance`,
        pvy = r$`PVY Resistance`,
        heat = r$`Heat Tolerance`,
        drought = r$`Drought Tolerance`
    )
    
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
    d$location <- r$location

    location_coords <- data.frame(
        location = c("Huancayo","Majes","Majes","Oxapampa","San Ramon"),
        latitude = c(-12.0092,-10.5953,-10.5953,-11.1285,-12.0092),
        longitude = c(-75.2236, -75.3868,-75.3868, -75.3564,-75.2236),
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
    
    carobiner::write_files(path = path,
                           metadata = meta,
                           records = d)
    
    
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# path <- _____
# carob_script(path)
