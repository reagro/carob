# R script for "carob"

## ISSUES
# if there are remaining issues, please put these in "meta$notes"


carob_script <- function(path) {
    
    "
    Group B1, cycle B1C5 of Population B (fifth cycle of recombination of the pure native Andigena group B1), is the result of a new population improvement strategy in the absence of R genes started at CIP in 1990. The group B1 derives from the primitive cultivars of Solanum tuberosum ssp. andigena, known to be free of R-genes. \r\n\r\nThese clones were planted in a randomized complete block design (RCBD) with 2-4  replicates at Oxapampa,  located at 1810 masl in Pasco-Peru in the Eastern mountain ranges facing the Amazon. The trials were established at Oxapampa due to the high disease pressure of late blight in these areas from 2005 to 2006.
    "
    
    ## Identifiers
    uri <- "doi:10.21223/P3/VYCLVX"
    group <- "varieties"
    
    ## Download data 
    ff  <- carobiner::get_data(uri, path, group)
    
    ## metadata 
    meta <- data.frame(
        # change the major and minor versions if you see a warning
        carobiner::read_metadata(uri, path, group, major=1, minor=3),
        data_institute = "CIP",
        # if there is a paper, include the paper's doi here
        # also add a RIS file in references folder (with matching doi)
        publication = NA,
        project = NA,
        # data_type can be e.g. "on-farm experiment", "survey", "compilation"
        data_type = "experiment",
        # treatment_vars has semi-colon separated variable names that represent the
        # treatments if the data is from an experiment. E.g. "N_fertilizer;P_fertilizer;K_fertilizer"
        treatment_vars = "variety",
        # response variables of interest such as yield, fwy_residue, disease incidence, etc. Do not include variable that describe management for all treatments or other observations that were not related to the aim of the trial (e.g. the presence of a disease).
        response_vars = "treatment", 
        carob_contributor = "Henry Juarez",
        carob_date = Sys.Date() |> as.character(),
        notes = NA
    )
    
    ## read data 
    
    f <- ff[grep("OXAPMP", basename(ff))]
    
    # Read files
    
    trial_id_counter <- 1
    
    process = function(file, variables=c("REP","INSTN","AUDPC","rAUDPC","TTYNA","MTYNA",'NoMTWP','TTWP','MTWP')){
        
        # Reading Fieldbook
        
        dat = carobiner::read.excel(file, sheet="Fieldbook")
        
        dat = dat[, variables] 
        
        dat = carobiner::change_names(dat,
                                      from = c("REP","INSTN","TTYNA","MTYNA"),
                                      to = c("rep","variety","yield","yield_marketable"))
        
        tryCatch({
            dat$rep <- as.integer(as.numeric(dat$rep))
            dat$yield <- as.numeric(dat$yield) * 1000
            dat$yield_marketable <- as.numeric(dat$yield_marketable) * 1000
            dat$AUDPC <- as.numeric(dat$AUDPC) / 100
        },error = function(e){
            print(paste0("Error: ", e$message))
            Sys.sleep(1.5)
        })
        
        # Reading Minimal
        
        minimal = carobiner::read.excel(file, sheet="Minimal")
        
        minimal_list = as.list(minimal$Value)
        names(minimal_list) = minimal$Factor
        
        dat$country <- minimal_list$Country
        dat$adm1 <-minimal_list$Admin1
        dat$adm2 <- minimal_list$Admin2
        dat$adm3 <- minimal_list$Admin3
        dat$longitude <- as.numeric(minimal_list$Longitude)
        dat$latitude <- as.numeric(minimal_list$Latitude)
        dat$elevation <- as.numeric(minimal_list$Elevation)
        dat$planting_date <- minimal_list$`Begin date`
        dat$harvest_date <- minimal_list$`End date`
        dat$trial_id = as.character(trial_id_counter)
        
        trial_id_counter <<- trial_id_counter + 1
        
        
        return(dat)
        
    }
    
    data = lapply(f, process)
    
    data = do.call(rbind, data)
    
    data$on_farm <- TRUE
    data$is_survey <- FALSE
    data$irrigated <- FALSE
    data$treatment = data$variety
    data$crop <- "potato"
    data$pathogen <- "Phytophthora infestans"
    data$yield_part <- "tubers"
    data$geo_from_source = TRUE
    data$N_fertilizer = NA
    data$P_fertilizer = NA
    data$K_fertilizer = NA
    
    carobiner::write_files(path = path, metadata = meta, records = data)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# path <- _____
# carob_script(path)
