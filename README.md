README file for data associated with "Soil Carbon Dynamics in Soybean Cropland and Forests in Mato Grosso, Brazil" article DOI: 10.1002/2017JG004269.

There are 7 data files (.csv) and 5 R scripts associated with this project.  The data can be downloaded here: https://zenodo.org/record/1127667#.Wj0tQ1WnGpo . All of the code can be found at https://github.com/nagyrc/Tanguro_soil_carbon .   

The file Tanguro_14C_data.csv is the master datasheet.  Column A is the UC Irvine sample identification number, column B is a second identification number, column C is a third sample identification number, column D is the delta 13C value, column E is the standard error on the delta 13C value, column F is the fraction modern value, column G is the standard error on the fraction modern, column H is the radiocarbon value, column I is the standard error on the radiocarbon, column J is the radiocarbon age, column K is the standard error on the radiocarbon age, column L is the type of sample (bulk soil= BS, CO2= CO2, soil post-incubation =SPI), column M is the year the samples were collected (either 2009 or 2013), column N is the yield of C in the sample, column O is the % carbon of the sample, column P is the point where the samples were collected in the field (randomly assigned), column Q is the depth of the sample (0-10 cm = 10, 10-20 cm = 20, 40-50 cm = 50, 90-100 cm = 100, 190-200 cm =200), column R is the bulk density, column S is the % clay in the sample, column T is the land use (either forest or soybeans), column U is the year converted to soybean agriculture (either 2003, 2004, 2007, or 2008; samples with 2020 = forest), column V indicates whether or not soils were acidified (as a test for carbonates), column W indicates whether a sampling point was sampled across the whole soil profile to 2 m deep for radiocarbon, column X is the grams of carbon per gram of soil, column Y is the depth interval sampled (e.g., 0.1 = 10 cm), column Z is the carbon content in the interval sampled, column AA is the clay category (either high or low), column AB is the depth represented in the interval, column AC is the depth integrated in the profile in cm, column AD is the depth integrated in the profile in m, column AE is the carbon content of the profile in grams per square meter, column AF is the % of C4-derived carbon, column AG is the % of C3-derived carbon, column AI is the % of C3-derived carbon corrected, column AK is the C3 endmembers, column AL is the C4 endmembers, and column AM is the lab where the samples were analyzed.

The files Tanguro_14C_data_for_analyis_BS.csv and Tanguro_14C_data_for_analysis_CO2.csv are subsets of Tanguro_14C_data.csv (master datasheet).  The Tanguro_14C_data_for_analyis_BS.csv uses the following column headings: column A is a sample identification number, column B is the delta 13C value, column C is the standard error on the delta 13C value, column D is the fraction modern, column E is the standard error on the fraction modern, column F is the radiocarbon value, column G is the standard error on the radiocarbon value, column H is the radiocarbon age value, column I is the standard error on the radiocarbon age, column J is the type of sample (bulk soil= BS, CO2= CO2, soil post-incubation =SPI), column K is the year the samples were collected (either 2009 or 2013), column L is the yield of C in the sample, column M is the % carbon of the sample, column N is the point where the samples were collected in the field (randomly assigned), column O is the depth of the sample (0-10 cm = 10, 10-20 cm = 20, 40-50 cm = 50, 90-100 cm = 100, 190-200 cm =200), column P is the bulk density, column Q is the % clay in the sample, column R is the land use (either forest or soybeans), column S is the year converted to soybean agriculture (either 2003, 2004, 2007, or 2008; samples with 2020 = forest), column T indicates whether or not soils were acidified (as a test for carbonates), column U indicates whether a sampling point was sampled across the whole soil profile to 2 m deep for radiocarbon, column V is the carbon content of the interval in grams per square meter, column W is the clay category (either high or low), column X is the carbon content of the profile in grams per square meter, column Y is the % of C4-derived carbon, column Z is the % of C3-derived carbon, column AA is the % of C4-derived carbon depth corrected, column AB is the % of C3-derived carbon depth corrected, and column AC is the lab where the samples were analyzed.

The Tanguro_14C_data_for_analysis_CO2.csv uses the following column headings: Column A is the UC Irvine sample identification number, column B is a second identification number, column C is a third sample identification number, column D is the delta 13C value, column E is the standard error on the delta 13C value, column F is the fraction modern, column G is the standard error on the fraction modern, column H is the radiocarbon value, column I is the standard error on the radiocarbon value, column J is the radiocarbon age value, column K is the standard error on the radiocarbon age, column L is the type of sample (bulk soil= BS, CO2= CO2, soil post-incubation =SPI), column M is the year the samples were collected (either 2009 or 2013), column N is the yield of C in the sample, column O is the % carbon of the sample, column P is the point where the samples were collected in the field (randomly assigned), column Q is the depth of the sample (0-10 cm = 10, 10-20 cm = 20, 40-50 cm = 50, 90-100 cm = 100, 190-200 cm =200), column R is the bulk density, column S is the % clay in the sample, column T is the land use (either forest or soybeans), column U is the year converted to soybean agriculture (either 2003, 2004, 2007, or 2008; samples with 2020 = forest), column V indicates whether or not soils were acidified (as a test for carbonates), column W indicates whether a sampling point was sampled across the whole soil profile to 2 m deep for radiocarbon, column X is the % of C4-derived carbon, column Y is the % of C3-derived carbon, column Z is the % of C4-derived carbon depth corrected, column AA is the % of C3-derived carbon depth corrected.

The CO2_conce_data.csv contains the data for the amount of CO2 respired.  This file uses the following column headings: Column A is the CO2 sample #, column B is the date the sample was collected and measured, column C is the concentration, column D is the change in concentration (if applicable), column E is the point where the samples were collected in the field (randomly assigned), column F is the depth of the bulk soil sample (0-10 cm = 10, 10-20 cm = 20, 40-50 cm = 50, 90-100 cm = 100, 190-200 cm =200) collected in the field, column G is the date sample was run in the lab, column H is the land use (either soy or forest), column I is the age of soybean agriculture (fields were converted in either 2003, 2004, 2007, 2008, or remain forest), column J is the running concentration, column K is the jar volume in mL, column L is the jar volume in L, colummn M is the mass of soil in the incubation in grams, column N is the conversion to mg of carbon per mL of air, column O is the number of days that the sample had been incubating for, column P is the carbon evolved per mg of soil per day, can column Q is the cumulative amount of CO2 evolved per mg of soil per day.

The figS4_forest_model.csv contains the data from our two-pool model for tropical forest soils at our site and was used to make this supplemental figure (S4). This file uses the following column headings: Column A is the year, column B is the 14C of the atmosphere, column C is the 14C of bulk soil, column D is the 14C of CO2, column E is the 14C of pool 1, and column F is the 14C of pool 2.

The time_to_respire_C.csv contains simple calculations on how long it would take to respire all the C in a sample. This file uses the following column headings: Column A is the point where the samples were collected in the field (randomly assigned), column B is the depth of the bulk soil sample (0-10 cm = 10, 10-20 cm = 20, 40-50 cm = 50, 90-100 cm = 100, 190-200 cm =200) collected in the field, column C is the carbon evolved from the incubated sample in mg C per gram of soil per day, column D is the bulk soil % carbon, column E is the bulk soil mg carbon, column F is the time to respire all the carbon in the sample in days, column G is the timer to respire all of the carbon in the sample in years, and column H is the land use (either soy or forest). 

The weighted.csv contains the C-mass weighted radiocarbon and stable isotope values. This file uses the following column headings: Column A is the land use (either soy or forest), column B is the point where the samples were collected in the field (randomly assigned), column C is the depth of the bulk soil sample (0-10 cm = 10, 10-20 cm = 20, 40-50 cm = 50, 90-100 cm = 100, 190-200 cm =200) collected in the field, column D is the cumulative carbon storage in grams per square meter, column E is the C-mass weighted 13C value, column F is the C-mass weighted 14C value, column G is the adjusted cumulative carbon storage in grams per square meter, column H is the C-mass weighted 14C value of CO2, and column I is the bulk density in grams per cubic cm.



Questions?  Please contact Dr. R Chelsea Nagy (Earth Lab, University of Colorado, Boulder) at nagyrc@gmail.com.
