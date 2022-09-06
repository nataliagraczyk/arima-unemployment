# Forecasting unemployment rate using ARIMA model
The following analysis is part of a project for the Forecasting and Simulation classes prepared in June 2022 by Natalia Graczyk and Joanna Gawlik.

Title: *Forecasting the unemployment rate in Poland using the Box-Jenkins method*.

The project was selected as the 3rd best project in class out of 26 teams.

## Project Objective

The aim of this paper is to find the most appropriate forecasting model to analyse the unemployment rate in Poland using the Box-Jenkins methodology. Based on the selected model, the expected level of the unemployment rate in Poland in May-July 2022 will be projected.

To see the full report go here: [report](Forecasting_report.pdf)

## Data

To estimate the forecasting model, monthly data on the of the unemployment rate in Poland between January 2004 and April 2022 in Poland. The data were downloaded from the CSO website (2022). 

More information on the data itself can be found on the following website: [CSO-website](https://stat.gov.pl/obszary-tematyczne/rynek-pracy/bezrobocie-rejestrowane/)

## Methods

- In the first stage of the analysis, the series were seasonally adjusted using the procedure of Tramo-Seats. 
- Then, in order to determine the stationarity of the series, a Dickey-Fuller test was performed. 
- The next step was to determine the p and q parameters in the ARIMA model (p, d, q).
- Once the appropriate ARIMA model was selected, a forecast of the unemployment rate was made
