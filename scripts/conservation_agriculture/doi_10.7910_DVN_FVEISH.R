
## needs info from Christian Thierfelder about meaning of 
## treatments and fertilizer 

carob_script <- function(path) {
	
"
The datasets contain field level trial data on low-input agriculture systems in Zambia

About the project
Project title: AfricaRISING- Sustainable Intensification of Maize-Legume-Livestock Integrated Farming Systems in East and Southern Africa

Project abstract

Sustainable intensification of mixed crop-livestock systems is a key pathway towards better food security, improved livelihoods, and a healthy environment. As part of the US government’s Feed the Future initiative to address hunger and food security issues in sub-Saharan Africa, the US Agency for International Development (USAID) is supporting three multi-stakeholder agricultural research projects to sustainably intensify key African farming systems. In East and Southern Africa, the project is being implemented in Tanzania and Malawi, and Zambia. In Tanzania, the project is being implemented in Babati and Kongwa districts in Manyara region of northern Tanzania and Kiteto district in Dodoma region, central Tanzania. The action sites were selected to acknowledge agro-ecological differences, allow appropriate targeting of technologies and strategies, and complement the development efforts of another USAID-supported program, the Tanzania Staples Value Chain (NAFAKA) project. In Malawi, the project is being implemented in Ntechu and Dedza districts in central Malawi where maize-based productions systems are dominant. Agroecological considerations guided the identification of research action sites. The pilot site for the study will be Eastern and Lusaka Provinces in Zambia.
"	
	uri <- "doi:10.7910/DVN/FVEISH"
	dataset_id <- carobiner::simple_uri(uri)
	group <- "fertilizer"
	## dataset level data 
	dset <- data.frame(
		dataset_id = dataset_id,
		group=group,
		project=NA,
		uri=uri,
		data_citation="Wasim Iftikar; Nabakishore Parida; Vivek Kumar; Narayan Chandra Banik; Amit Mishra, 2017, Odisha Rice Crop Cut Data 2013 - 2016, https://hdl.handle.net/11529/11111, CIMMYT Research Data & Software Repository Network, V2",
		publication= NA,
		data_institutions = "CIMMYT",
		data_type="survey", # or, e.g. "on-farm experiment", "survey", "compilation"
		carob_contributor="",
		carob_date="2023-09-25",
		revised_by=as.character(NA)
	)
	
	## download and read data 
	
	ff	<- carobiner::get_data(uri, path, group)
	js <- carobiner::get_metadata(dataset_id, path, group, major=1, minor=2)
	dset$license <- carobiner::get_license(js)
	dset$title <- carobiner::get_title(js)
	dset$authors <- carobiner::get_authors(js)
	dset$description <- carobiner::get_description(js)
	
	TRUE
}


