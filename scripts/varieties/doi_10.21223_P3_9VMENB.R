# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {
  
    "B3C2 is the second cycle of recombination of advanced B3C1 potato clones. These clones have high levels of  horizontal resistance to late blight in absence of R-genes, and they also have high tuber yield. These clones  were planted in a randomized complete block design (RCBD) with 2-4 replicates at Oxapampa,  located at 1810 masl in Pasco-Peru in the Eastern mountain ranges facing the Amazon. \r\nThe trials were established at Oxapampa due to the high disease pressure of late blight in these areas from 1999 to 2005."
  
  uri <- "doi:10.21223/P3/9VMENB"
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
  
  f <- ff[grep("OXAPMP", basename(ff))]
  d <- lapply(f, process)
  d <- do.call(rbind, d) 
  
  d <- unique(d)
  
  carobiner::write_files(path = path,
                         metadata = meta,
                         records = d)
  
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# path <- _____
# carob_script(path)
