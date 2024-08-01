# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"
A field study was framed in rice crop under conservation agriculture (CA) based rice-wheat system at experimental farm of Borlaug Institute for South Asia (BISA)-CIMMYT, Ladhowal, Punjab, India during kharif 2019. In the present study, nine treatments were imposed out of which four are CA-based treatments (ZT-N0, ZT-N50, ZT-N75 and ZT-N100), four are CA coupled with subsurface drip fertigation (CA+) based treatments (SSD-N0,SSD-N50, SSD-N75 and SSD-N100) and puddled transplanted rice (PTR) treatment as farmer’s practice. The findings of the study showed that PTR treatment out yielded in terms of yield attributing characters and biological yield than other treatments. CA+ treatment (SSD-N100) resulted higher biological yield (2.8%) than CA-based treatments (ZT-N100). SSD-N100 dominated ZT-N100 and PTR treatment in terms of plant N content (both grain and straw), total N uptake and N harvest index. PTR treatment resulted 22-33% higher ANUE than ZT-N100 and SSD-N100 treatments. (2020-12-01)
"

## Identifiers
	uri <- "hdl:11529/10548773"
	group <- "agronomy"

## Download data 
	ff  <- carobiner::get_data(uri, path, group)

## metadata 
	meta <- data.frame(
		# change the major and minor versions if you see a warning
		carobiner::read_metadata(uri, path, group, major=1, minor=2),
		data_institute = "CIMMYT",
		# if there is a paper, include the paper's doi here
		# also add a RIS file in references folder (with matching doi)
		publication = "doi.org/10.56093/ijas.v91i4.112625",
		project = NA,
		# data_type can be e.g. "on-farm experiment", "survey", "compilation"
		data_type = "experiment",
		# treatment_vars has semi-colon separated variable names that represent the
		# treatments if the data is from an experiment. E.g. "N_fertilizer;P_fertilizer;K_fertilizer"
		treatment_vars = "treatment",
		# response variables of interest such as yield, residue_yield, disease incidence, etc. Do not include variable that describe management for all treatments or other observations that were not related to the aim of the trial (e.g. the presence of a disease).
		response_vars = "yield", 
		carob_contributor = "Shumirai Manzvera ",
		carob_date = "2024-08-01"
	)
	
## read data 

	f <- ff[basename(ff) == "CIMMYT Data.xlsx"]
  r <- carobiner::read.excel(f, sheet= "IJAS-Rana", skip=1 )

## select the variables of interest and assign them to the correct name
  #fixing dataframe
 
  r1 <- r[,c('Treatment', 'Biological yield (t/ha)', '...4', '...5')]
  r1 <- r[,c('Treatment', 'Biological yield (t/ha)', '...4', '...5')]
  names (r1) <- c('Treatment', 'R1', 'R2', 'R3')
  r1 <- r1[-1,]
  r1.1 <- data.table::melt(r1, id.vars = 'Treatment', variable.name = 'Rep', value.name = 'yield')
  
  r2 <- r[,c('Treatment', 'N grain content (%) at harvest', '...28', '...29')]
  names (r2) <- c('Treatment', 'R1', 'R2', 'R3')
  r2 <- r2[-1,]
  r2.1 <- data.table::melt(r2, id.vars = 'Treatment', variable.name = 'Rep', value.name = 'grain_N')
  
  r3 <- r[,c('Treatment', 'N straw content (%) at harvest', '...32', '...33')]
  names (r3) <- c('Treatment', 'R1', 'R2', 'R3')
  r3 <- r3[-1,]
  r3.1 <- data.table::melt(r3, id.vars = 'Treatment', variable.name = 'Rep', value.name = 'residue_N')
  
  
  d0 <- merge(r1.1, r2.1 , by = c("Treatment","Rep") ,all.x=TRUE)
  d <- merge(d0, r3.1 , by = c("Treatment","Rep") ,all.x=TRUE)
 
  d$planting_date <- "2019"
  d$crop <- "rice" 
	d$latitude <- 20.5937
	d$longitude <-78.9629
	d$yield <- as.numeric(d$yield )
	d$yield <- d$yield * 1000
	d$grain_N <- as.numeric(d$grain_N)
	d$residue_N <- as.numeric(d$residue_N)
	d$treatment <- d$Treatment
	d$rep <- as.integer(d$Rep)
	d$yield_part  <-  "grain"
	d$country  <-  "India"
	d$trial_id <- as.character(as.integer(as.factor("1")))
	
		
	
## about the data (TRUE/FALSE)
	d$on_farm <- TRUE
	d$irrigated <- TRUE


# all scripts must end like this
	carobiner::write_files(path, meta, d)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# path <- _____
# carob_script(path)

