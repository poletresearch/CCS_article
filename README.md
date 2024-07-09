# Code and data for the article Kazlou T., Cherp A., Jewell J. Feasible deployment of carbon capture and storage and the requirements of climate targets
### Notes
* Each R file we release can run standalone, provided that the necessary R packages (listed within the file) are installed.
* When running any of the R files, working directory should be set to the directory containing source data files. See comments and data import statements (read.csv) within the code to see which source files are required for each piece of code.
* Tested with: macOS 14.4.1, R version 4.3.2 (2023-10-31)

## R code files
**st1_failure.R** - code used for calculating failure rates from historical CCS data (replication of Supplementary Table 1). Produces a CSV file of Supplementary Table 1 in the working directory provided that it is set to the directory containing source data files. 

**fig2_formative.R** – code used for constructing a feasibility space for CCS deployment in the formative phase (replication of Figure 2 – all panels). Produces a PNG file of Figure 2 in the working directory provided that it is set to the directory containing source data files. 

**fig3_acceleration.R** – code used for producing a feasibility space for CCS deployment in the acceleration phase (replication of Figure 3) and for calculating the number of scenarios within a given feasibility frontier. Produces a PNG file of Figure 3 in the working directory provided that it is set to the directory containing source data files.  

**market_size_calc.R** – code used for estimating the future potential market for CCS technologies based on scenario emission data (using a random IPCC scenario). Produces a PNG file of Supplementary Figure 4 in the working directory provided that it is set to the directory containing source data files.

**functions.R** – code used for fitting Gompertz and logistic growth models (originally developed in Cherp et al. 2021) to CCS deployment projections in the IPCC AR6 scenarios;

**curve_fitting.R** – code used for creating a summary of growth parameters after fitting Gompertz and logistic growth curves (originally developed in Cherp et al. 2021) to CCS deployment projections in the IPCC AR6 scenarios. Produces an output file "formatted_scenario_data_fit.csv" in the working directory (not in the "outputs folder"). 

**edfig4_stable.R** – code used for normalising growth curve parameters to the market size and generating a feasibility space for CCS deployment at the stable growth phase. Produces a PNG file of Extended Data Fig. 4 in the working directory provided that it is set to the directory containing source data files. 

**fig4_vetting2100.R** – code used for vetting IPCC AR6 1.5°C- and 2°C-compatible scenarios with feasibility constraints in the formative, acceleration, and stable growth phases. Produces a PNG file of Fig. 4 in the working directory provided that it is set to the directory containing source data files. 

## Data files
**CCS_Projects_database.csv** - database containing historical and planned CCS projects. Columns:

* Name - project name
* Status - project status
* Country - project country
* Region - IPCC R10 region (see Table S2 for definitions)
* Sector - project sector (see Table S3 for definitions)
* Subsector - project subsector (see Table S3 for definitions)
* Storage - project CO2 storage type
* ActualCapacity - current capacity of the project, in tons per day
* AnnouncedCapacity - announced (planned) capacity of the project, in tons per day
* ProjectStart - the year of project announcement
* ProposedProjectEnd - the year when project is planned to become operational
* ActualProjectEnd - the year when project was finished (either successfully or not)
* FacilityStart - (if project is successful) the year when project became operational
* FacilityEnd - the year when the operational phase of the project was suspended

**CCS_data.csv** – timeseries with scenario deployment data for CCS (incl. BECCS and DACCS). For 2000-2020, we add historical data.

**acceleration_frontier.csv** – dataset of scenarios within the feasibility frontier at the acceleration phase (see Figure 3). 

**formatted_scenario_data.csv** – timeseries with scenario deployment data for CCS (incl. BECCS and DACCS). For 2000-2020, we add historical data. Each timeseries is truncated at the maximum value to ensure a goodness of fit for growth models. This dataset is ready for growth curve fitting. Columns:

* Country - IPCC AR6 model and scenario name (separated with an underscore)
* temp - temperature group (IPCC Scenario Categories C1+C2=1.5°C, C3+C4=2°C, C5 = 2.5°C)
* Year - year
* Value - CCS capacity in a given year, in Mt/yr

**formatted_scenario_data_fit.csv** – a summary of growth curve parameters fitted to the timeseries of CCS deployment. This dataset can be otherwise obtained by running curve_fitting.R. Variable definition and other methodological details are described in the Methods section of the article.

**single_scenario.csv** – a single scenario from the IPCC AR6 Scenario Database hosted by IIASA used for calculating the size of the CCS market in market_size_calc.R

**IEA_electricity.csv** – this dataset from the International Energy Agency (IEA) contains source-wise time-series electricity generation data for all countries in the IEA database along with some regions & country-groups. Depending on the country, the data starts in 1960/71/90 and ends in 2021 (for IEA members and some other countries) or 2020 (for all other countries).

**CCS_market.csv** – time-series of potential CCS market size calculated from IPCC AR6 scenario emission data, in MtCO2, by decade for every scenario. 

**normalisation.csv** – dataset containing average stable growth parameters of CCS deployment in IPCC AR6 scenarios, obtained from fitting logistic and Gompertz growth curves (Methods). 
