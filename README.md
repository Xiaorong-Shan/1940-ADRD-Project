Exposure products for 1940-ADRD-Project
=====

*	Total exposure to all sources
    -	Species emissions
        -	Data: modeled emissions from CMIP6 (download these gridded emissions products)
        -	Species
            -	BC
            -	SO2
            -	Primary PM2.5
            -	NOx
            -	Others?
        -	Exposure assessment
            -	Gridded emissions spatially apportioned to counties (units: kg/s/m2)
            -	Inverse-distance weighted emissions
    -	Modeled historical concentrations from CMIP6
        -	Data: CMIP6, 11 different models
        -	Species: 
            -	PM2.5
            -	O3
        -	**Exposure assessment: gridded concentrations aggregated to counties**

*	Exposure to individual source categories
    -	Power plants
        -	Data: EIA generators dataset
        -	Fuel types
            -	Petroleum
            -	Coal
            -	Gas
            -	Renewable 
        -	Exposure: 
           ** -	gridded inverse distance weighted exposure (weighted by generator capacity)
            -	DisperseR, weighted by generator capacity**
    -	Automobiles
        -	Data
            -	Nationwide gasoline sales (your power point said statewide gasoline sales—which is it?)
            -	Statewide vehicle registrations (again, is this correct?)
            -	County population
            -	Select cities’ road networks
        -	Exposure
        **    -	Roadiness in select cities
            -	County concentrations based on box model**
                -	New idea—what if we distribute emissions to PBL & windspeed grid, then estimate concentration in consistent grid instead of counties?
            -	**Roadiness-downscaled box model concentrations in select cities**
    -	Oil/gas wells
        -	Data: oil/gas well locations (what is the source of this data?)
        -	Categories
            -	Production type
                -	Brine
                -	CBM
                -	Disposal
                -	Dry hole
                -	Gas
                -	Injection
                -	Monitor
                -	Oil
                -	Oil & Gas
                -	Other
                -	Producer
                -	Service 
                -	Storage
                -	Unknown
                -	Water 
            -	Well status
                -	Abandoned
                -	Active
                -	Cancelled
                -	Completed
                -	Drilled
                -	Inactive
                -	Orphan
                -	P & A
                -	Permitted
                -	SHUT – IN
                -	TA
        -	Exposure
            -	**Inverse distance weighting (weighted by number of wells in each 4km grid cell)**




