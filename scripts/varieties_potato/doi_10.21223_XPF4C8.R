# R script for "carob"

carob_script <- function(path) {
    
"During the 2019-2020 season, 10 potato clones selected were evaluated, previously selected for their high levels of resistance to late blight and their excellent quality for french fries tested under high and low-temperature conditions (important conditions for the content of reducing sugars such as glucose and fructose, which cause the dark color in frying) were used together with two control varieties Canchan and Única, widely adopted by farmers and final consumers. The experiment planted in Majes was under technical drip irrigation in two different periods of the year and the control of pests and diseases is being carried out in a timely and adequate manner. The experiments were planted using tuber seeds from in vitro (basic) plants, in 1 location in the south of Peru using the statistical design of randomized complete blocks with three replications of 150 plants each. The fertilization rate was 200-220-180 NPK per hectare, using as sources ammonium nitrate 33% N; di-ammonium phosphate 46% P2O5, 18% N; and potassium sulfate 50% K2O. Pest and disease control was carried out in a timely and adequate manner. In all experiments, late blight control was carried out on Canchan and Unica varieties planted as susceptible controls. Clone selection was planned to be carried out using the Participatory Varietal Selection (PVS) methodology, at flowering, harvest, and post-harvest stages. At harvest, the number and weight of marketable and unmarketable tubers per plot were recorded, then the tuber yield per hectare in t/ha was calculated, tuber samples were taken to determine the dry matter content using the hydrometer method and the dry weight/fresh weight, The tubers were also stored at room temperature (15-16oC) for frying after three months to see if they maintain their frying quality. The frying quality of the potato chips was evaluated based on the frying color, using the scale in grades from 1 to 5, developed by the Potato chip- \"Snack Food Association\" (www.sfa.org), the color grade of the selected clones should be 1 or 2. Three clones were selected as potential varieties with resistance to late blight, quality for french fries and/or baked. These clones were selected based on their high yield, good quality for frying, low content of reducing sugars, high content of dry matter, and information from the PVS methodology."
    
    uri <- "doi:10.21223/XPF4C8"
    group <- "varieties_potato"
    ff  <- carobiner::get_data(uri, path, group)
    
    meta <- carobiner::get_metadata(uri, path, group, major=1, minor=4,
        data_organization = "CIP",
        publication = NA,
        project = NA,
        data_type = "experiment",
        treatment_vars = "variety",
        response_vars = "yield;yield_marketable", 
        carob_contributor = "Henry Juarez",
        carob_date = "2024-09-13",
        notes = NA
    )
    
    f <- ff[grep("PTYield", basename(ff))]
    
    
    process = function(file_index_1,file_index_2){
        
        r <- carobiner::read.excel(f[file_index_1], sheet="Fieldbook")  
        installation <- carobiner::read.excel(f[file_index_2], sheet="Installation")
        
        n <- as.list(installation$Value)
        names(n) <- installation$Factor
        
        plot_size <- as.numeric(n$`Plot_size_(m2)`)
        plot_adj <- 10000 / plot_size
        
        df <- data.frame(
            rep = as.integer(r$REP),
            variety = r$INSTN,
            yield = as.numeric(r$TTWP) * plot_adj,
            yield_marketable = as.numeric(r$MTWP) * plot_adj,
            country = 'Peru',
            adm1 = 'Arequipa',
            adm2 = 'Caylloma',
            adm3 = 'Majes',
            longitude =  -72.2874,
            latitude = -16.3766,
            planting_date = "2020-08-20" ,
            harvest_date = "2021-01-20",
            plant_density = as.numeric(n$`Planting_density_(plants/Ha)`),
            row_spacing = as.numeric(n$`Distance_between_rows_(m)`) * 100,
            plant_spacing = as.numeric(n$`Distance_between_plants_(m)`) * 100,
            trial_id = gsub(".xls|.xlsx", "", basename(f[1]))
        )
        
        if (!is.null(r$AUDPC)) {
            df$AUDPC <- as.numeric(r$AUDPC) / 100
            df$pathogen <- "Phytophthora infestans"
        }
        if (!is.null(r$AUDPC)) {
            df$rAUDPC <- as.numeric(r$rAUDPC)
            df$pathogen <- "Phytophthora infestans"
        }
        
        df$on_farm <- TRUE
        df$is_survey <- FALSE
        df$irrigated <- FALSE
        df$treatment = "varieties"
        df$crop <- "potato"
        df$pathogen <- "Phytophthora infestans"
        df$yield_part <- "tubers"
        df$geo_from_source = FALSE
        df$N_fertilizer = 200
        df$P_fertilizer = 220
        df$K_fertilizer = 180
        
        return(df)
        
    }
    
    
    index_pairs <- list(c(1, 2), c(3, 4))
    
    d <- lapply(index_pairs, function(pair) {
        process(pair[1], pair[2])
    })
    d <- do.call(rbind, d)
    
    carobiner::write_files(path = path, metadata = meta, wide=d)

}
