# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {
  
  "The B3C3 population came from crossing the elite clones of B3C2 population, in 2011 under quarantine greenhouses at La Molina. For this, 30,000 genotypes (60 families with 500 seeds each) were planted. At harvest, 21685 genotypes were selected (72%). These selected clones were planted in La Victoria, Huancayo, to increase the number of tubers and make selection for agronomic attributes, where 3123 clones (10.41% of the original population) were selected.\r\nThis dataset is part of the following selection where the selected clones were evaluated for three seasons (2012, 2013 and 2014), in Oxapampa where the environmental conditions (rain, relative humidity, temperature) are great to have a high pressure of late blight allowing us to select clones with resistance to this disease.\r\nIn 2012-2013 at Oxapampa, 3005 clones were planted in observation plots of 5 plants each with two replications. In this experiment, the resistance to late blight of the plants was recorded. And at harvest, the marketable and total tuber yield were measured. Finally, 507 clones were selected."
  
  uri <- "doi:10.21223/P3/OTYRIV"
  group <- "varieties"
  ff  <- carobiner::get_data(uri, path, group)
  
  meta <- data.frame(
    carobiner::read_metadata(uri, path, group, major=1, minor=3),
    data_institute = "CIP",
    publication = NA,
    project = NA,
    data_type = "experiment",
    treatment_vars = "variety",
    response_vars = "yield;yield_marketable", 
    carob_contributor = "Henry Juarez",
    carob_date = "2024-09-13",
    notes = NA
  )
  
  
  process <- carobiner::get_function("process_cip_lbvars", path, group)
  
  f <- ff[grep("PTLate", basename(ff))]
  d <- lapply(f, process, addvars=c("AUDPC","rAUDPC","TTWP","MTWP"))
  d <- do.call(rbind, d)
  
  carobiner::write_files(path = path,
                         metadata = meta,
                         records = d)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# path <- _____
# carob_script(path)
