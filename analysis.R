pacman::p_load(timeDate, tidyverse, lubridate, purrr, progress, ggplot2, furrr, plotly, knitr, kableExtra)
source('helpers.R')
raw_prices <- readRDS('data/Prices.rds')
prices <- raw_prices %>%
  as.data.frame() %>%
  rownames_to_column('date') %>%
  mutate(date = as.Date(date))


# params ----
start_year <- 2015
end_year <- 2019
holding_months <- 3
portf_size <- 30
beta_history <- 6
min_history <- 6
skip_na_days <- FALSE

raw_prices[1:10, 1:10]
class(raw_prices)
# cycle table ----
bh <- start_year:end_year %>%
  holidayLONDON() %>% as.Date()

cycle_df <- getCycleCal(
  prices %>% select(date),
  start_year = start_year, end_year = end_year,
  holding_months = holding_months
)


# get beta ----
ticker <- 'AMP IM'
target_year <- 2018
target_cycle <- 2
target_dates <- cycle_df %>%
  getCycleDates(target_year, target_cycle)
target_end_date <- target_dates[1] - days(1)

# + single spec ----
cycle_cal <- prices %>%
  select(date) %>%
  getCycleCal(start_year, end_year, holding_months)

stocks <- expand_grid(
  target_year = start_year:end_year,
  target_cycle = 1:(12 / holding_months)) %>%
  pmap(function(target_year, target_cycle) {
    message(target_year, ' - ', target_cycle)
    selectStocks(
      prices, index_col, cycle_cal,
      target_year, target_cycle,
      holding_months, portf_size,
      beta_history, T)
  }) %>% bind_rows()

# + batch ----
start_year <- 2015
end_year <- 2019
index_col <- 'Eurostoxx600'
holding_months <- c(2:4, 6)
portf_size <- seq.int(10, 50, 10)
beta_history <- seq.int(3, 12, 3)
portf_types <- c('low', 'high')
weights <- c('equal')#, 'by_beta', 'by_sharpe_r')
tickers <- colnames(raw_prices)[-1]

t1 <- Sys.time()
future:::ClusterRegistry("stop")
# options(future.globals.maxSize = 5 * 1024 * 1024 ^ 2)
n_cores <- availableCores()
plan(multiprocess, workers = n_cores, gc = T)


beta_all <- expand_grid(
  holding_months = holding_months,
  portf_size = portf_size,
  beta_history = beta_history
) %>%
  future_pmap(function(holding_months, portf_size, beta_history) {
    # message(paste(holding_months, portf_size, beta_history, sep = ' - '))
    cycle_cal <- prices %>%
      select(date) %>%
      getCycleCal(start_year, end_year, holding_months)
    
    expand_grid(
      target_year = start_year:end_year,
      target_cycle = 1:(12 / holding_months)) %>%
      pmap(function(target_year, target_cycle) {
        # message(paste0('  ', target_year, '-', target_cycle))
        selectStocks(
          prices, index_col, cycle_cal, target_year, target_cycle,
          holding_months, portf_size,
          beta_history)
      }) %>% bind_rows()
  }) %>% bind_rows()
t2 <- Sys.time()
saveRDS(beta_all, 'data/beta_all.RDS')
format(t2 - t1)
beta_summ <- beta_all %>%
  group_by(year, cycle, holding_months, portf_size, beta_history, beta_type) %>%
  summarise(
    n = n(),
    stocks = n_distinct(ticker),
    mean_beta = mean(beta),
    .groups = 'drop') %>%
  mutate(
    check1 = portf_size == n,
    check2 = portf_size == stocks
  )
table(beta_summ$check1)
table(beta_summ$check1)
beta_summ %>%
  ggplot(aes(beta_history, mean_beta, colour = beta_type)) +
  geom_point(aes(size = portf_size)) +
  facet_grid(year ~ cycle)

# Evaluate ----
beta_all <- readRDS('data/beta_all.RDS')

# + single spec ----
strat_return <- portf_types %>%
  map(function(x) {
    getStratRet(
      prices, stocks, start_year, end_year,
      holding_months, beta_history,
      portf_size, x, weights)
  })
saveRDS(strat_return, 'data/strat_return.RDS')

cycle_returns <- strat_return %>%
  map(function(x) {
    x[['portf_cycle_return']] %>%
      mutate(
        holding_months = x[['holding_months']],
        beta_history = x[['beta_history']],
        portf_size = x[['portf_size']],
        portf_type = x[['portf_type']]
      )
  }) %>% bind_rows()
daily_returns <- strat_return %>%
  map(function(x) {
    x[['portf_daily_return']] %>%
      mutate(
        holding_months = x[['holding_months']],
        beta_history = x[['beta_history']],
        portf_size = x[['portf_size']],
        portf_type = x[['portf_type']]
      )
  }) %>% bind_rows()

cycle_returns %>%
  mutate(year_cycle = paste0(t_year, '-', t_cycle)) %>%
  plot_ly(
    x = ~ year_cycle, y = ~ cycle_return,
    color = ~ portf_type, type = 'bar') %>%
  layout(
    legend = list(orientation = 'h', x = 0.5, y = -0.1),
    title = "Quarterly Cumulative Returns",
    xaxis = list(title = 'Quarter', automargin = 'true'),
    yaxis = list(title = 'Return (€m)'))

daily_returns %>%
  plot_ly(
    x = ~ date, y = ~ return,
    color = ~ portf_type,
    type = 'scatter', mode = 'lines') %>%
  layout(
    legend = list(orientation = 'h', x = 0.5, y = -0.1),
    title = "Daily Cumulative Returns",
    xaxis = list(title = '', automargin = 'true'),
    yaxis = list(title = 'Return (€m)'))

# + batch ----
start_year <- 2015
end_year <- 2019
holding_months <- c(2:4, 6)
portf_size <- seq.int(10, 50, 10)
beta_history <- seq.int(3, 12, 3)
portf_types <- c('low', 'high')
weights <- c('equal')#, 'by_beta', 'by_sharpe_r')
tickers <- colnames(raw_prices)[-1]
return_batch_grid <- expand_grid(
  h_mths = holding_months,
  b_hist = beta_history,
  p_size = portf_size,
  p_type = portf_types,
  p_wts = 'equal'
)

t1 <- Sys.time()
future:::ClusterRegistry("stop")
# options(future.globals.maxSize = 5 * 1024 * 1024 ^ 2)
n_cores <- availableCores()
plan(multiprocess, workers = n_cores, gc = T)

return_all <- return_batch_grid %>%
  future_pmap(function(h_mths, b_hist, p_size, p_type, p_wts) {
    getStratRet(
      prices, beta_all, start_year, end_year,
      h_mths, b_hist, p_size, p_type, p_wts)
  })
saveRDS(return_all, 'data/return_all.RDS')

cycle_returns <- return_all %>%
  map(function(x) {
    x[['portf_cycle_return']] %>%
      mutate(
        holding_months = x[['holding_months']],
        beta_history = x[['beta_history']],
        portf_size = x[['portf_size']],
        portf_type = x[['portf_type']]
      )
  }) %>% bind_rows()
daily_returns <- return_all %>%
  map(function(x) {
    x[['portf_daily_return']] %>%
      mutate(
        holding_months = x[['holding_months']],
        beta_history = x[['beta_history']],
        portf_size = x[['portf_size']],
        portf_type = x[['portf_type']]
      )
  }) %>% bind_rows()

test_beta_history <- 6
daily_returns %>%
  filter(beta_history == test_beta_history) %>%
  ggplot(aes(date, return, colour = portf_type)) +
  geom_line() +
  facet_grid(holding_months ~ portf_size)

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

daily_returns_all %>%
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
  mutate(sortino_ratio = s_ratios_all)
saveRDS(s_ratios_df, 'data/s_ratios_df.RDS')

beta_history %>%
  map(function(x) {
    s_ratios_df %>%
      filter(b_hist == x) %>% # arrange subplot by Beta month
      plot_ly(
        x = ~ h_mths, y = ~ sortino_ratio,
        size = ~ p_size, type = 'scatter',
        mode = 'markers', colors = 'Dark2',
        showLegend = T,
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
