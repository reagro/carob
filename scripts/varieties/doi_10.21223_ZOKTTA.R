# R script for "carob"

## need to extract more variables. 
## use carobiner::read.excel.hdr
## use d <- data.frame(var = r$Var, ...) to assingn old to new variables  
## use carobiner::read.excel.hdr
## location is not correct. Need to be full names and IDSs can be created with reshape; and no need for bizarre reg expr.  
## do not use more than four decimals in coordinates if they are estimated

carob_script <- function(path) {
  
"B3C3 Population is the resulted fromcrosses between the elite clones from the previous cycle of the same population made at 2011. Since 2012 until 2019 this population was evaluated  for late blight resistance.  51 clones were selected having high levels of late blight resistance in Oxapampa and good tuber yield in Huancayo, both better than in the control varieties. AUDPC values are in the range of 17.50 to 705.83, lower than the AUDPC values of the Yungay and Amarilis control varieties with 1586.67 and 1534.17 respectively. The tuber yield on average in Highlands was from 20.67 to 56.41 th-1 compared to the Yungay and Amarilis varieties with 26.29 and 35.66 th-1 respectively. Under high temperature conditions, the tuber yield in the heat tolerant clones was in the range of 16.32 to 33.71th-1, higher than the Desiree and Amarilis varieties with 13.39 and 5.24 th-1 respectively). Six clones were selected that have some potential for drought tolerance, but further trials are required to confirm these results. The dry matter content of the elite clones is in the range of 17.94 to 26.92% and 33 clones have good frying quality. Twenty-eight clones showed extreme resistance to PVX and 17 to PVY. Fifteen clones have a high parental value to be used as parents in a new selection cycle or in improvement programs in the regions or countries of Latin America, Africa and Asia. Fifteen clones show phenotypic stability for tuber yield.  The process of introduction of these clones to the genebank for international distribution is in progress."
  
  uri <- "doi:10.21223/ZOKTTA"
  group <- "varieties"
  ff  <- carobiner::get_data(uri, path, group)
  meta <- data.frame(
      carobiner::read_metadata(uri, path, group, major=1, minor=1),
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

# use 
#  d <- carobiner::read.excel(f = f, sheet="Table", skip=1)
# or, what I would do:
#  d <- carobiner::read.excel.hdr(f = f, sheet="Table", skip=1, hdr=1)
  
  cols_names <- c("record_id","Clone","MTYNA HYO 2015-2018","MTYNA SRA 2017","MTYNA OXA 2015-2018",
                  "MTYNA MAJ normal irrigation 2018-2019","MTYNA MAJ restricted irrigation 2018-2019",
                  "AUDPC","Scale","PVX Resistance","PVY Resistance","Heat Tolerance",
                  "Drought Tolerance","Dry matter","Chips color","Phenotipic Stability MTY",
                  "Phenotipic Stability LB resistance")
  
  d <- d[2:nrow(d),]
  colnames(d) <- cols_names
  variable_cols <-
      c("MTYNA HYO 2015-2018", "MTYNA SRA 2017", "MTYNA OXA 2015-2018",
        "MTYNA MAJ normal irrigation 2018-2019","MTYNA MAJ restricted irrigation 2018-2019")
  
  long_data <-reshape(
          d,
          direction = "long",
          idvar = "Clone",
          varying = list(variable_cols),
          timevar = "MTYNA",
          times = variable_cols
      )
  
  names(long_data)[12] <- "MTYNA Category"
  names(long_data)[13] <- "MTYNA"
  
  d <- long_data[,c("Clone","AUDPC","MTYNA HYO 2015-2018")]
  d <- carobiner::change_names(d, from=c("Clone","MTYNA HYO 2015-2018"), 
                               to = c("variety","yield_marketable"))
  
  d$AUDPC <- as.numeric(d$AUDPC) / 100
  d$yield_marketable <- as.numeric(d$yield_marketable) * 1000
  d$rep <- as.integer(NA)
  d$yield <- as.numeric(NA)
  d$on_farm <- TRUE
  d$is_survey <- FALSE
  d$irrigated <- FALSE
  d$trial_id <- "1"
  d$crop <- "potato"
  d$pathogen <- "Phytophthora infestans"
  d$country <- 'Peru'
  d$yield_part <- "tubers"
  d$location <- tolower(sub(".*MTYNA\\s+(.*?)\\s+(irrigation|\\d+).*", "\\1", rownames(d)))
  
  location_coords <- data.frame(
      location = tolower(unique(d$location)),
      latitude = c(-12.00925833,-10.59535278,-10.59535278,-11.12858056,-12.00925833),
      longitude = c(-75.22366389, -75.38681667,-75.38681667, -75.35643056,-75.22366389),
      adm1 = c('Junin','Arequipa','Arequipa','Pasco','Junin'),
      adm2 = c('Huancayo','Majes','Majes','Oxapampa','San Ramon')
  )
  d <- merge(d, location_coords, by = "location", all.x = TRUE)
  d$geo_from_source = FALSE
  d$planting_date <- "2017-05-04"
  d$harvest_date  <- "2017-11-17"
  d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <-  as.numeric(NA)
  d$treatment <- "Clones under different locations & irrigations systems"
  d$location <- d$adm2
  d <- unique(d)
  
  carobiner::write_files(path = path, metadata = meta,records = d)
  
}

