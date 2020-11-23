index_col <- 'Eurostoxx600'
target_year <- 2019
target_cycle <- 1
holding_months <- 4
portf_size <- 30
beta_history <- 6
cycle_cal <- prices %>%
  select(date) %>%
  getCycleCal(start_year, end_year, holding_months)
test <- selectStocks(
  prices, index_col,
  cycle_cal, target_year, target_cycle,
  holding_months, portf_size,
  beta_history, T)

function(prices_df, beta_df, start_year, end_year, h_mths, b_hist, p_size, p_type, p_wts) 

prices_df <- prices
beta_df <- beta_all
h_mths <- 3
b_hist <- 6
p_size <- 30
p_type <- 'high'
p_wts <- 'equal'
# t_year <- 2019
# t_cycle <- 1

if (p_wts == 'equal') {
  weights <- rep(1, p_size) / p_size
} else {
  weights <- rep(1, p_size) / p_size
}

cycle_cal <-  prices_df %>%
  select(date) %>%
  getCycleCal(start_year, end_year, h_mths)

cycle_grid <- expand_grid(
  t_year = start_year:end_year,
  t_cycle = 1:(12 / h_mths))

tmp_return <- cycle_grid %>%
  pmap(function(t_year, t_cycle) {
    cycle_dates <- cycle_cal %>%
      getCycleDates(t_year, t_cycle)
    
    tickers <- beta_df %>%
      filter(
        year == t_year &
          cycle == t_cycle &
          holding_months == h_mths &
          portf_size == p_size &
          beta_history == b_hist &
          beta_type == p_type) %>%
      pull(ticker)
    
    prices_df %>%
      getPortfRet(
        tickers,
        cycle_dates[1], cycle_dates[2],
        weights)
    
  })

cycle_returns <- tmp_return %>%
  map_dbl(~ tail(.[['return']], 1))
cumul_returns <- cumprod(cycle_returns)
daily_returns <- list(
  df = tmp_return,
  ret = cumul_returns
) %>%
  pmap(function(df, ret) {
    df[['return']] <- df[['return']] * ret
    df
  }) %>%
  bind_rows()

test <- getStratRet(
  prices, beta_all, start_year, end_year,
  h_mths, b_hist, p_size, p_type, p_wts)


# Sortino ----
s_ratios <- portf_types %>% # calculate for low and high strategies
  map_dbl(function(x) {
    daily_returns %>%
      filter(portf_type == x) %>%
      pull(return) %>%
      getReturn() %>% # convert the normalised prices to returns
      SortinoRatio()
  })
# names(s_ratios) <- portf_types
data.frame(
  type = str_to_title(portf_types),
  s_ratios = round(s_ratios, 3)
) %>%
  kableExtra::kable(
    col.names = c('Type', 'Sortino Ratio'),
    format = 'html') %>%
  kable_styling(full_width = F)
