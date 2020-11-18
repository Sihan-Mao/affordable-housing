# Amenity Accessibility Evaluation of Shared-Ownership Housing in Beijing

Beijing Housing Authority released a new affordable housing project called “Shared-Ownership Housing” (SOH) in April 2017 to meet the excessive housing demand. However, it has not been welcomed as expected and faced criticism on amenity accessibility. The study used the Spatial Access Model developed by Center of Spatial Data Science at the University of Chicago, to evaluate and compare access to schools, hospitals subway stations and air quality of Shared-Ownership Housing and commercial residential locations. The study found no significant evidence that Beijing Housing Authority had discriminatory site selection process when zoning for SOH. But compared to the more evenly distributed commercial residential locations, SOH locations are all far from the city center and are mostly located in neighborhoods with low access scores.

--- 

## Data Preparation 

1. Scraping land transaction records from www.3fang.com to identify Shared-Ownership Property units.

2. Scraping NASA MODIS MCD19A2 data and convert the MODIS AOD data into Geotiff for Air Quality Analysis.
    * Scrape HDF4 satellite data using [Python](../master/hdf_processing/hdf-scrape.py);
    * Convert HDF4 to GeoTIFF using Python and OSGeo4W [command shell](../master/hdf_processing/hdf-geotiff.py);
    * [Raster Processing using R](../master/hdf_processing/bj_aod_estimates.R).

3. Calculate access scores for each property location to nearby amenities.
    * The Spatial Access Model runs in Ubuntu Server with [IPython notebook](../master/hdf_processing/hdf-scrape.py).

## Analysis
RMarkdown: [soh_analysis.Rmd](../master/soh_analysis.Rmd)

Project Paper: [Amenity Accessibility Evaluation of Shared-Ownership Housing in Beijing](../master/location_analysis_soh_beijing_sihanmao.pdf)
