# ## Intro {.tabset .tabset-fade}
# 
# ### The Task
# 
# The tasks is evaluating a strategy based on Beta.
# 
# The strategy is on the first work day in Jan 2015, calculate the **6-month** Beta of all stocks, and select **30 stocks** with the lowest Beta values, then allocate €1,000,000 **evenly** to the 30 stocks, and sell these stocks at the end of the **3rd month**. Using the available capital to invest in another 30 stocks selected in the same way (with lowest Beta for the 6-months prior). Repeat this on a quarterly basis until the end of 2019.
# 
# Do the same one more time except use the highest Beta values each time.
# 
# As highlighted in **bold**, these parameters can be explored to understand returns. This will be discussed in the last section "Generalisation".
# 
# ### The Approach
# 
# Generally data cleaning and sense checking are required, e.g. removing duplicates, correcting price shifting (e.g. stock dilution), missing values etc., but for simplicity I have assumes there are no devastating errors in the data and therefore skipped this step. Although missing values will be dealt with appropriately in certain steps.
# 
# Best practices include (all observed in the code):
#   
#   * using single purpose functions as much as appropriate
# * exposing adjustable parameters (e.g. above bold ones)
# * using vectorised, and if possible, parallel processing for heavy and long calculations
# 
# The high level steps are:
#   
#   1. Calculate the Beta of all stocks at all the starting points in one go
# 2. Select the stocks at each start
# 3. Calculate the daily return of the whole portfolio for each quarter
# 4. Scale each quarter by the return of the last quarter
# 5. Get the Sortino Ratio


# Load libraries
library(timeDate)                 # for getting Bank Holidays
library(tidyverse)                # for majority of the data manipulations
library(lubridate)                # date manipulations
library(purrr)                    # efficient iteration, part of Functional Programming
library(furrr)                    # the multi-core parallel version of purrr
library(plotly)                   # interactive data visualisation
library(DT)                       # flexible data table
library(PerformanceAnalytics)     # quant package, used here for getting Sortino Ratio
library(kableExtra)               # makes kable prettier

# Load functions
source('helpers.R')

# Import the data
raw_prices <- readRDS('data/Prices.rds')

prices <- raw_prices %>%
  as.data.frame() %>%             # Convert to data frame
  rownames_to_column('date') %>%  # Make date available as column
  mutate(date = as.Date(date))    # Convert date to "Date" format for date arithmetic

# Set parameters
initial_capital <- 1000000
start_year <- 2015 # start and end years of activity
end_year <- 2019
index_col <- 'Eurostoxx600' # the column name of the index
holding_months <- 3 # the number of months for holding the portfolio
portf_size <- 30 # the number of stocks to select each time
beta_history <- 6 # the number of months for calculating Beta
portf_types <- c('low', 'high') # the top (high) or bottom (low) Beta band
weights <- c('equal') # the stock weights for allocating the capital
# other options can be by Beta or by Sharpe Ratio
tickers <- colnames(raw_prices)[-1] # the available ticker names, for ease of access


# Stock selection
cycle_cal <- prices %>% # create a cycle calendar
  select(date) %>%
  getCycleCal(start_year, end_year, holding_months)

stocks <- expand_grid( # select the stocks for each year / quarter combination
  target_year = start_year:end_year,
  target_cycle = 1:(12 / holding_months)) %>%
  pmap(function(target_year, target_cycle) {
    selectStocks(
      prices, index_col, cycle_cal,
      target_year, target_cycle,
      holding_months, portf_size,
      beta_history, F)
  }) %>% bind_rows() # bind all elements of the list together by row


# Strategy returns
strat_return <- portf_types %>% # get the strategy returns for low and high Beta
  map(function(x) {
    getStratRet(
      prices, stocks, start_year, end_year,
      holding_months, beta_history,
      portf_size, x, weights)
  })


# Visualise quarterly returns
cycle_returns <- strat_return %>% # extract cycle returns from the above
  map(function(x) {
    x[['portf_cycle_return']] %>%
      mutate(
        holding_months = x[['holding_months']],
        beta_history = x[['beta_history']],
        portf_size = x[['portf_size']],
        portf_type = x[['portf_type']]
      )
  }) %>% bind_rows()

cycle_returns %>% # create a bar chart for quarterly returns
  mutate(
    year_cycle = paste0(t_year, '-', t_cycle), # create a single year / quarter column
    # apply the initial investment to price
    investment_value = cumul_return * initial_capital) %>%
  plot_ly(
    x = ~ year_cycle, y = ~ cumul_return, # x is year/cycle, y is the cumulative return
    color = ~ portf_type, type = 'bar') %>%
  layout(
    legend = list(orientation = 'h', x = 0.4, y = -0.2),
    title = "Quarterly Cumulative Returns",
    xaxis = list(title = 'Quarter', automargin = 'true'),
    yaxis = list(title = 'Return €'))


# Visualise daily returns
daily_returns <- strat_return %>% # same as the quarterly returns, except using daily
  map(function(x) {
    x[['portf_daily_return']] %>%
      mutate(
        holding_months = x[['holding_months']],
        beta_history = x[['beta_history']],
        portf_size = x[['portf_size']],
        portf_type = x[['portf_type']]
      )
  }) %>% bind_rows()

daily_returns %>%
  mutate(investment_value = return * initial_capital) %>%
  plot_ly(
    x = ~ date, y = ~ return,
    color = ~ portf_type,
    type = 'scatter', mode = 'lines') %>%
  layout(
    legend = list(orientation = 'h', x = 0.4, y = -0.1),
    title = "Daily Cumulative Returns",
    xaxis = list(title = '', automargin = 'true'),
    yaxis = list(title = 'Return €'))


# Sortino Ratio
s_ratios <- portf_types %>% # calculate for low and high strategies
  map_dbl(function(x) { # return numeric vectors
    daily_returns %>%
      dplyr::filter(portf_type == x) %>%
      pull(return) %>% # extract the single column
      getReturn() %>% # convert the normalised prices to returns
      SortinoRatio()
  })

data.frame(
  type = str_to_title(portf_types),
  s_ratios = round(s_ratios, 3)
) %>%
  kable(
    col.names = c('Type', 'Sortino Ratio'),
    format = 'html') %>%
  kable_styling(full_width = F)



# Best strategy

# Going back to the beginning with the parameters:
# 
# * Holding months of a portfolio (`holding_months`)
# * The portfolio size (`portf_size`)
# * The length of history for calculating the Beta (`beta_history`)
# 
# It is useful to understand how changing the value of them can impact the overall return of the strategy. This is made possible by exposing the parameters in functions, and with extensive use of the `map` function from [`purrr` package](https://purrr.tidyverse.org/) and the `expand_grid` to create combinations of the parameter values, we can get the overall returns of all scenarios.
# 
# For longer calculations apply parallel processing using `future_map` (instead of `map`) from the [`furrr` package](https://github.com/DavisVaughan/furrr).

# Set parameters
holding_months <- c(2:4, 6) # generates 2, 3, 4, 6
portf_size <- seq.int(10, 50, 10) # generates 10, 20, 30, 40, 50
beta_history <- seq.int(3, 12, 3) # generates 3, 6, 9, 12


# Select stocks
n_cores <- availableCores() # get the available cores of the machine
plan(multiprocess, workers = n_cores, gc = T) # use all the cores

beta_all <- param_grid %>%
  # future_map is a parallel version of map
  future_pmap(function(holding_months, portf_size, beta_history) { 
    cycle_cal <- prices %>%
      select(date) %>%
      getCycleCal(start_year, end_year, holding_months)
    
    expand_grid(
      target_year = start_year:end_year,
      target_cycle = 1:(12 / holding_months)) %>%
      pmap(function(target_year, target_cycle) {
        selectStocks(
          prices, index_col, cycle_cal, target_year, target_cycle,
          holding_months, portf_size,
          beta_history)
      }) %>% bind_rows()
  }) %>% bind_rows()


# Get performance
return_batch_grid <- expand_grid( # get all combinations of parameters
  h_mths = holding_months,
  b_hist = beta_history,
  p_size = portf_size,
  p_type = portf_types,
  p_wts = 'equal'
)

# set up parallel process
n_cores <- availableCores()
plan(multiprocess, workers = n_cores, gc = T)

return_all <- return_batch_grid %>%
  future_pmap(function(h_mths, b_hist, p_size, p_type, p_wts) { # parallel
    getStratRet(
      prices, beta_all, start_year, end_year,
      h_mths, b_hist, p_size, p_type, p_wts)
  })
future:::ClusterRegistry("stop") # stop the multi-core plan to release RAM


# Insight

# Here are some high level insight
# 
# 1. **Low Beta** is almost always better
# 2. Portfolio **holding period** is better at **either ends** (2 months or 6 months)
# 3. **Longer Beta history** is better, i.e. greater or equal to 6 months
# 4. **Medium to small sized portfolios** (<= 40 stocks) generally have higher Sortino Ratio

# Get daily returns
daily_returns_all <- return_all %>% # same as the quarterly returns, except using daily
  map(function(x) {
    x[['portf_daily_return']] %>%
      mutate(
        holding_months = x[['holding_months']],
        beta_history = x[['beta_history']],
        portf_size = x[['portf_size']],
        portf_type = x[['portf_type']]
      )
  }) %>% bind_rows()

s_ratios_all <- return_batch_grid %>% # calculate for low and high strategies
  pmap_dbl(function(h_mths, b_hist, p_size, p_type, p_wts) {
    daily_returns_all %>%
      filter(
        holding_months == h_mths &
          beta_history == b_hist &
          portf_size == p_size &
          portf_type == p_type) %>%
      pull(return) %>%
      getReturn() %>% # convert the normalised prices to returns
      SortinoRatio() %>%
      pluck(1, 1) # extract the number
  })

s_ratios_df <- return_batch_grid %>%
  mutate(sortino_ratio = s_ratios_all) # combine Sortino back to the param grid


# Visualise

beta_history %>%
  map(function(x) {
    s_ratios_df %>%
      filter(b_hist == x) %>% # arrange subplot by Beta month
      plot_ly(
        x = ~ h_mths, y = ~ sortino_ratio,
        size = ~ p_size * 2, type = 'scatter',
        mode = 'markers', colors = 'Dark2',
        # show the Beta month and type (high/low) in the legend
        name = ~ paste0('Beta: ', b_hist, '-month | ', str_to_title(p_type))) %>%
      layout(
        legend = list(orientation = 'h',  y = -0.2),
        title = "Strategy Sortino Ratio Comparison",
        xaxis = list(title = 'Holding Months', automargin = 'true'),
        yaxis = list(title = 'Sortino Ratio'))
  }) %>%
  subplot(
    nrows = 2,
    shareX = T, shareY = T)