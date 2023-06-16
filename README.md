Exposure products for 1940-ADRD-Project
=====

*	Total exposure to all sources
    -	Species emissions
        -	Data: modeled emissions from CMIP6 (download these gridded emissions products)
	Species
•	BC
•	SO2
•	Primary PM2.5
•	NOx
•	Others?
	Exposure assessment
•	Gridded emissions spatially apportioned to counties (units: kg/s/m2)
•	Inverse-distance weighted emissions
o	Modeled historical concentrations from CMIP6
	Data: CMIP6, 11 different models
	Species: 
•	PM2.5
•	O3
	Exposure assessment: gridded concentrations aggregated to counties

*	Exposure to individual source categories
o	Power plants
	Data: EIA generators dataset
	Fuel types
•	Petroleum
•	Coal
•	Gas
•	Renewable 
	Exposure: 
•	gridded inverse distance weighted exposure (weighted by generator capacity)
•	DisperseR, weighted by generator capacity
o	Automobiles
	Data
•	Nationwide gasoline sales (your power point said statewide gasoline sales—which is it?)
•	Statewide vehicle registrations (again, is this correct?)
•	County population
•	Select cities’ road networks
	Exposure
•	Roadiness in select cities
•	County concentrations based on box model
o	New idea—what if we distribute emissions to PBL & windspeed grid, then estimate concentration in consistent grid instead of counties?
•	Roadiness-downscaled box model concentrations in select cities
o	Oil/gas wells
	Data: oil/gas well locations (what is the source of this data?)
	Categories
•	Production type
o	Brine
o	CBM
o	Disposal
o	Dry hole
o	Gas
o	Injection
o	Monitor
o	Oil
o	Oil & Gas
o	Other
o	Producer
o	Service 
o	Storage
o	Unknown
o	Water 
•	Well status
o	Abandoned
o	Active
o	Cancelled
o	Completed
o	Drilled
o	Inactive
o	Orphan
o	P & A
o	Permitted
o	SHUT – IN
o	TA
	Exposure
•	Inverse distance weighting (weighted by number of wells in each 4km grid cell)




