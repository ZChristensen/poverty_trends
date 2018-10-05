Data sources for the Development Data Hub

The main input is taken from data downloaded from the PovcalNet API. The code for the API can be found here https://github.com/ZChristensen/poverty_trends/blob/master/code/povcal_api.R
 
The raw PovcalNet data was re-written at https://github.com/ZChristensen/poverty_trends/blob/master/code/Povcal_data.R

The final additional code for these files is found at https://github.com/ZChristensen/poverty_trends/blob/master/code/generate_data_warehouse_files.R

Variable names ending in "Interp" have been linearly interpolated for years during which data is missing. PovcalNet data has only been scraped to include data during the "lined up" years: 1981, 1984, 1987, 1990, 1993, 1996, 1999, 2002, 2005, 2008, 2011, 2011, 2012, 2013, and 2015. 
