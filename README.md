# Bradford assay analysis
This script processes and analyses Bradford assay data per student. It cleans the dataset, builds BSA standard curves, calculates R² values and converts unknown samples into protein concentrations.

# Requirements for the dataset
To avoid errors, ensure the CSV meets these requirements:  Column 1 contains student codes: A, B, C, D, E, F, H

Columns 2 to 8 are the BSA standards in this order:  0.0 ; 0.1 ; 0.2 ; 0.4 ; 0.8 ; 1.5 ; 2.0

Columns 9 to 12 are the unknown samples in this order:  FL2.5 ; FL10 ; dTPR2.5 ; dTPR10

Absorbance values use commas as decimal markers (for example 0,243)

Keep the original column names provided by Canvas

Delete rows 1 to 9 before saving

No empty rows at the bottom

Save as CSV (Comma delimited), not semicolon CSV or UTF-16

# Preparing the data file
Download the file from Canvas

Place it in your working folder

Open it in Excel

Delete rows 1 to 9

Save a copy as CSV, for example: Groep4_Bradford.csv

# How to run the script
Set the working directory at the top of the script

Place the CSV file in the same folder

Run the script in R or RStudio

# Output
The script produces:  A cleaned dataset

BSA standard curves per student

R² values for each curve

Converted concentrations of unknown samples

A results table with all calculated values
Converted concentrations of unknown samples

A results table with all calculated values
