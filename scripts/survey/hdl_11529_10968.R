# R script for "carob"


carob_script <- function(path) {

"
The major objectives of this rice crop cuts and survey were to:
(i) identify major rice yield determinants in western terai districts of Nepal and

(ii) to identify major determinants that describe the variability in rice yield across farms thereby linking with farm characteristics and socio-economic gradients.

This study uses Remote Sensing (RS) data particularly the NDVI (Normalized Difference Vegetative Index) value extracted from LandSat satellite images in each of the six districts (Kanchanpur, Kailali, Bardyia, Banke, K apilbastu and Rupendhai). Since the resolution of LandSat satellite was better than MODIS; we used LandSat derived NDVI values to capture the variability in standing green biomass (a proxy for yield) so that sample selected will represent this variability in NDVI values. In each of the districts we saw a normal distribution of NDVI and samples were selected randomly by stratifying the NDVI values into four quartiles of bell curve, so that selected sample will represent proportionally the bell curve quartiles.

The study contains several optional modules questions and mandatory modules. The mandatory modules give the broader insights on input uses across farms while the optional module provides better insight on each input uses for example; fertilizer use details, irrigation dynamics, weed management and socio-economics gradients particularly based on income and expenditure of households. The optional module is accompanied by the crop cuts data which was done in three quadrants of each plot wit h 2*2=4 meter square area.

The sample size for this survey is 1052 households and the inputs use were asked for largest rice grown plots as farms may have multiple plots and inputs use might be different in different plots. Out of the total samples of 1052; a set of (~12%; 126 samples) households participated in optional modules to get the detail data on irrigation dynamics, weed management and socio-economics parameters.

(2017)"

	uri <- "hdl:11529/10968"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	dset <- data.frame(
		carobiner::read_metadata(uri, path, group, major=4, minor=3),
		project="CSISA",
		#data_citation="Gokul Paudel; Shashish Maharjan; David Guerena; Ashok Rai; Andrew James McDonald, 2017. Nepal Rice Crop Cut and Survey Data 2016. https://hdl.handle.net/11529/10968, CIMMYT Research Data & Software Repository Network, V4",
		publication= NA,
		data_institutions = "CIMMYT",
		data_type="survey",
		treatment_vars = "none",
		carob_contributor="Robert Hijmans",
		carob_date="2024-01-27"
	)


	return(TRUE)	

}

