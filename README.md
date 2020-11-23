# Backtesting a Beta-based strategy

## Set up

The input data is a data frame with daily closing prices of stocks, requiring

1. file name is `Prices.rds` and saved in `data/` (or change the code in `analysis.R` and the Rmd)
2. dates as row names
3. each stock having its own column, with the ticker as the column name
4. an index based on which Beta is calculated

## Beta definition

Beta is defined here as the slope of the OLS (Ordinary Least Squares) regression line of the scatter-plot where on the x-axis are reported the daily returns of the index (benchmark) and on the y-axis are reported the daily returns of the stock for which the beta is calculated.

## Generate report

To set up the data for the R markdown, run the necessary code in `analysis.R` to generate save these objects in `data/`

* `beta_all.RDS`
* `return_all.RDS`
* `s_ratios_df.RDS`
* `strat_return.RDS`

Then knit the Rmd file to get an HTML report.
