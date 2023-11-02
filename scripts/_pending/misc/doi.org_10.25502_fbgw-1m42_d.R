# R script for "carob"

## ISSUES
# ....


carob_script <- function(path) {

"Description:
IITA-ADC biochar study Uganda on Sorghum
Increasing organic matter/carbon contents of soils is one option from a basket of strategies being proposed to offset climate change inducing greenhouse gas (GHG) emissions, under the auspices of the Paris-COP 4 per mille initiative. One of the complimentary practices to sequester carbon in soils on decadal timescales is amending it with biochar, a carbon rich byproduct of biomass gasification. In sub-Saharan Africa (SSA) there is widespread and close interplay of agrarian based economies and the use of biomass for fuel, which makes that the co-benefits of biochar production for agriculture and energy supply are explicitly different from the rest of the world. To date, the quantities of residues available from staple crops for biochar production, and their potential for carbon sequestration in farming systems of SSA have not been comprehensively investigated. Herein we assessed the productivity and usage of biomass waste from: maize, sorghum, rice, millet and groundnut crops; specifically quantifying straw, shanks, chaff and shells, based on measurements from multiple farmer fields and census/surveys in eastern Uganda. Moreover, allometric models, using grain productivity, plant height and density, as input variables, were tested. These models enable rapid and low-cost assessment of the potential availability of feedstocks at both site-specific, farmer field and/or regional scales. Ultimately we modelled the carbon balance in soils of major cropping systems receiving a ‘circular’ amendment of biochar from residues, and up-scaled this for basic scenario analysis. This interdisciplinary approach has wielded a framework for country-wide assessment of the biophysical potential of crop biomass wastes for soil C sequestration through scaling of biochar technologies, and to determine its co-benefits for agriculture and energy production. In doing so, we identified engineering synergies that could substantially contribute to a number of the Sustainable Development Goals.
"

	uri <- "doi:10.25502/FBGW-1M42/D"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "conservation_agriculture"
	## dataset level data 
	dset <- data.frame(
		dataset_id = dataset_id,
		group=group,
		project=NA,
		uri=uri,
		data_citation="Roobroeck, D., Rebbeca, H.-N., John-Baptist, T., & Jackson, M. (2019). IITA-ADC biochar study Uganda on Sorghum [dataset]. International Institute of Tropical Agriculture (IITA). https://doi.org/10.25502/FBGW-1M42/D",
		## if there is a paper, include the paper's doi here
		## also add a RIS file in references folder (with matching doi)
		publication=NA,
		data_institutions = "IITA",
   		data_type= "compilation",
		carob_contributor="Shumirai Manzvera"  
	)

## download and read data 

	ff  <- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=2, minor=1)
	dset$license <- carobiner::get_license(js)

	
## process file(s)

## use a subset
	f <- ff[basename(ff) == "Sorghum_biomass_sampling.csv"] 

	r <- read.csv(f)

	d <- data.frame(biomass_stems=r$Yield_straw_dry_ton_ha_1,
					yield=r$Yield_grain_dry_ton_ha_1,
					plant_height=r$Avg_Hgt_in_quadrant_m,
					dataset_id= dataset_id
				)


	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$is_experiment <- TRUE
	d$irrigated <- TRUE
	d$country <- "Uganda"
	 
	d$longitude <- 32.29028
	d$latitude <- 1.37333

	d$crop <- "sorghum"
	d$yield_part <- "grain"

	
# all scripts must end like this
	carobiner::write_files(dset, d, path=path)
}

