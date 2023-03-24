# GTA_Analysis_Reports 
Report and analysis on Global Tuna Atlas data and workflow.

## Global Tuna Atlas Automatic Report and Analysis Repository ##

This repository contains code and data for automatically generating reports and performing analyses on data from the Global Tuna Atlas. The Global Tuna Atlas is a database of tuna populations and fisheries around the world, and contains a wealth of information that can be used to better understand and manage these important species.

The code in this repository is designed to automatically use data from the Global Tuna Atlas, clean and preprocess the data as needed, and generate reports and visualizations that can be used to explore and analyze the data. The repository includes scripts for both data preprocessing and report generation, and is designed to be run on a regular basis to ensure that the latest data is always available for analysis.
An renv documentation of the needed packages is provided. In case your version of R is different you may need to adjust avalaible versions of packages used.

## Requirements

- Rstudio
- Renv package
- Connection to a database filled with latest version of Global Tuna Atlas dataset. If you cannot access to the database, you can create your own one following the workflow in geoflow-tunaatlas.

## Usage

To use the code in this repository, follow these steps:

- Clone the repository to your local machine.
- Install the required packages using renv::restore()
- Run the different report for your own need. 

## Contributing
If you would like to contribute to this repository, please open an issue or submit a pull request with your changes.
