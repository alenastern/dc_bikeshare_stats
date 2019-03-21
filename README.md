# dc_bikeshare_stats

Instructions to run code: 
1.	Clone the complete repository into the local. 
2.	Open src/analysis/data_analysis_outline.R 
3.	Change the working directory to the folder in which the local copy of the github repo was copied. Comment out any working directory if any has been set before. 
4.	In order to run, the ACS data.R file needs to have a census API key. For privacy purposes we are not including the key we have used for the implementation, but a key can be easily retrieved from:  
a.	Register for a key here: https://api.census.gov/data/key_signup.html. After submitting your organization and email, you will receive an email from the US Census Bureau with your key. If you already have a key, they will re-send you your old key.
b.	Key  needs to be included in line 15 of ACS Data.R.
Run the entire data_analysis_outline.R code. This code incorporates calls to the other files on the folder and that will be executed upon called. 
The code is commented as to include explanations for each pre-processing and processing step performed. 

We have two sets of files: 
1.	Code for reshaping, cleaning and wrangling the data
●	get_data.R - 
■	Description: This is the main file to build our initial working dataframe. It calls get_data_bikeshare.R and ACS data.R to get data on ridership and demographics, and it performs a series of spatial joins to join this information by geography id and in turn  with business licenses. 
■	Outcome: The main outputted dataframe is  df.final, for which the unit of analysis is each GEOID-Date pair, and which contains: 
●	GEOID: Id of each block tract. 
●	Date identification: at the day level 
●	Number of incoming rides to that area for the specified time period
●	Number of business licences approved for that area for the specified time period. 
○	Number of business licences disaggregated by type. 
●	ACS data for the 2010-2015 period for the area specified. 
○	getdata_bikeshare.R - Refactored code from Rina Barber’s original code, returns bike rides until 2015. 
○	ACS data.R - Uses census API to get sociodemographic variables for the 2010-2015 period.
○	data_timelags.R - 
■	Description: This file takes as input the dataframe created by get_data.R script and creates new lagged features in order to be able to look at the effect of business licenses approved in the past for the ridership for a given month. 
■	Outcome: The main outputted dataframe is  df.no.dups, which is the cleaned up version of df.final which also includes lagged variables. 
2.	Code for performing exploratory analysis 
○	plots.R
■	Description: performs simple exploratory plots of our final dataframe. 
3.	Code for running analysis on the clean dataframe
●	data_analysis_outline.R - This file takes as an input the main data frame created by data_timelags.R. From this code, all of the other code files can are called and executed. The code performs: 
○	Data splitting: 
■	Data splitting into train, test and validation sets for our data. Since the data is formatted as a panel where our unit of analysis is each combination of GEOID-date, we don’t randomly split rows, but rather we split by GEOID blocks of observations. 
○	Feature selection:
■	Trains linear, lasso, ridge, elastic net, Poisson, grouped lasso linear, and grouped lasso Poisson models.
■	Getting metrics for performance and saving them into a comparison frame
○	Fits final model (with selected features) on testing data.
■	Trains linear and Poisson model
■	Conducts difference of means tests on relevant groups of coefficients 
●	clean_selective_inference.R 
○	Description: Performs selective inference with different values of lambda
○	Outcome: Gets different FixedLasso output objects, which are saved in an excel spreadsheet that also lives on our git repository. 
●	appendix4.R
○	Description: Using the results of the models performed on data_analysis_outline.R, this file creates the comparison table presented on Appendix 4, summarizing the non-negative betas obtained from specifications of grouped lasso models. 
